// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"encoding/hex"
	"flag"
	"fmt"
	goparser "go/parser"
	goscanner "go/scanner"
	"go/token"
	"math/rand"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"sort"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"
	"unicode"

	"github.com/pmezard/go-difflib/difflib"
	"modernc.org/mathutil"
)

func stack() string { return string(debug.Stack()) }

const (
	defaultSrc = "."
)

var (
	oBSrc   = flag.String("bsrc", runtime.GOROOT(), "")
	oHeap   = flag.Bool("heap", false, "")
	oRE     = flag.String("re", "", "")
	oReport = flag.Bool("report", false, "")
	oSrc    = flag.String("src", defaultSrc, "")
	oTrc    = flag.Bool("trc", false, "")

	digits  = expand(unicode.Nd)
	letters = expand(unicode.L)
	re      *regexp.Regexp
)

func TestMain(m *testing.M) {
	flag.BoolVar(&noBack, "noback", false, "panic on parser back")
	flag.BoolVar(&panicBack, "panicback", false, "panic on parser back")
	flag.BoolVar(&trcTODOs, "trctodo", false, "")
	flag.Parse()
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	os.Exit(m.Run())
}

func expand(cat *unicode.RangeTable) (r []rune) {
	for _, v := range cat.R16 {
		for x := v.Lo; x <= v.Hi; x += v.Stride {
			r = append(r, rune(x))
		}
	}
	for _, v := range cat.R32 {
		for x := v.Lo; x <= v.Hi; x += v.Stride {
			r = append(r, rune(x))
		}
	}
	s := rand.NewSource(42)
	rn := rand.New(s)
	for i := range r {
		j := rn.Intn(len(r))
		r[i], r[j] = r[j], r[i]
	}
	return r
}

type golden struct {
	a  []string
	f  *os.File
	mu sync.Mutex
	t  *testing.T

	discard bool
}

func newGolden(t *testing.T, fn string) *golden {
	if re != nil || *oReport || *oSrc != defaultSrc {
		return &golden{discard: true}
	}

	f, err := os.Create(filepath.FromSlash(fn))
	if err != nil { // Possibly R/O fs in a VM
		base := filepath.Base(filepath.FromSlash(fn))
		f, err = os.CreateTemp("", base)
		if err != nil {
			t.Fatal(err)
		}

		t.Logf("writing results to %s\n", f.Name())
	}

	return &golden{t: t, f: f}
}

func (g *golden) w(s string, args ...interface{}) {
	if g.discard {
		return
	}

	g.mu.Lock()

	defer g.mu.Unlock()

	if s = strings.TrimRight(s, " \t\n\r"); !strings.HasSuffix(s, "\n") {
		s += "\n"
	}
	g.a = append(g.a, fmt.Sprintf(s, args...))
}

func (g *golden) close() {
	if g.discard || g.f == nil {
		return
	}

	defer func() { g.f = nil }()

	sort.Strings(g.a)
	if _, err := g.f.WriteString(strings.Join(g.a, "")); err != nil {
		g.t.Fatal(err)
	}

	if err := g.f.Sync(); err != nil {
		g.t.Fatal(err)
	}

	if err := g.f.Close(); err != nil {
		g.t.Fatal(err)
	}
}

type testParallel struct {
	a                  *analyzer
	asts               []interface{}
	errors             []error
	limit              chan struct{}
	maxBacktrackOrigin string
	maxBacktrackPath   string
	maxBacktrackPos    string
	maxBacktracksPath  string
	maxBudgetPath      string
	maxDuration        time.Duration
	maxDurationPath    string
	minToksPath        string
	sync.Mutex
	wg sync.WaitGroup

	maxBacktrack      int
	maxBacktrackToks  int
	maxBacktracks     int
	maxBacktracksToks int
	maxDurationToks   int
	maxBudget         int
	maxBudgetToks     int
	minToks           int

	allToks int32
	fails   int32
	files   int32
	ok      int32
	skipped int32
}

func newParallel() *testParallel {
	return &testParallel{
		a:     newAnalyzer(),
		limit: make(chan struct{}, runtime.GOMAXPROCS(0)),
	}
}

func (p *testParallel) addFail()      { atomic.AddInt32(&p.fails, 1) }
func (p *testParallel) addFile()      { atomic.AddInt32(&p.files, 1) }
func (p *testParallel) addOk()        { atomic.AddInt32(&p.ok, 1) }
func (p *testParallel) addSkipped()   { atomic.AddInt32(&p.skipped, 1) }
func (p *testParallel) addToks(n int) { atomic.AddInt32(&p.allToks, int32(n)) }

func (p *testParallel) addAST(ast interface{}) {
	p.Lock()
	defer p.Unlock()

	p.asts = append(p.asts, ast)
}

func (p *testParallel) recordMaxDuration(path string, d time.Duration, toks int) {
	p.Lock()
	defer p.Unlock()

	if d > p.maxDuration {
		p.maxDuration = d
		p.maxDurationPath = path
		p.maxDurationToks = toks
	}
}

func (p *testParallel) recordMaxBacktrack(path string, back, toks int, pos, origin string) {
	p.Lock()
	defer p.Unlock()

	if back > p.maxBacktrack {
		p.maxBacktrack = back
		p.maxBacktrackOrigin = origin
		p.maxBacktrackPos = pos
		p.maxBacktrackPath = path
		p.maxBacktrackToks = toks
	}
}

func (p *testParallel) recordMaxBacktracks(path string, back, toks int) {
	p.Lock()
	defer p.Unlock()

	if back > p.maxBacktracks {
		p.maxBacktracks = back
		p.maxBacktracksPath = path
		p.maxBacktracksToks = toks
	}
}

func (p *testParallel) recordMaxBudget(path string, budget, toks int) {
	p.Lock()
	defer p.Unlock()

	if budget > p.maxBudget {
		p.maxBudget = budget
		p.maxBudgetToks = toks
		p.maxBudgetPath = path
	}
}

func (p *testParallel) recordMinToks(path string, toks int) {
	p.Lock()
	defer p.Unlock()

	if p.minToks == 0 || toks < p.minToks {
		p.minToks = toks
		p.minToksPath = path
	}
}

func (p *testParallel) err(err error) {
	if err == nil {
		return
	}

	s := err.Error()
	if x := strings.Index(s, "TODO"); x >= 0 {
		fmt.Println(s[x:])
	}
	p.Lock()
	p.errors = append(p.errors, err)
	p.Unlock()
}

func (p *testParallel) exec(run func() error) {
	p.limit <- struct{}{}
	p.wg.Add(1)

	go func() {
		defer func() {
			p.wg.Done()
			<-p.limit
		}()

		p.err(run())
	}()
}

func (p *testParallel) wait() error {
	p.wg.Wait()
	if len(p.errors) == 0 {
		return nil
	}

	var a []string
	for _, v := range p.errors {
		a = append(a, v.Error())
	}
	return fmt.Errorf("%s", strings.Join(a, "\n"))
}

func TestScanner(t *testing.T) {
	p := newParallel()
	t.Run("errors", func(t *testing.T) { testScanErrors(t) })
	t.Run("numbers", func(t *testing.T) { testNumbers(t) })
	t.Run("src", func(t *testing.T) { testScan(p, t, *oSrc) })
	t.Run("GOROOT", func(t *testing.T) { testScan(p, t, runtime.GOROOT()) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, ok %v, fail %v", h(p.files), h(p.ok), h(p.fails))
}

func testScan(p *testParallel, t *testing.T, root string) {
	if err := filepath.Walk(root, func(path0 string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		if re != nil && !re.MatchString(path0) {
			return nil
		}

		if filepath.Ext(path0) != ".go" {
			return nil
		}

		path := path0
		p.addFile()
		p.exec(func() error {
			if *oTrc {
				fmt.Fprintln(os.Stderr, path)
			}

			b, err := os.ReadFile(path)
			if err != nil {
				p.addFail()
				return err
			}

			fs := token.NewFileSet()
			fi := fs.AddFile(path, -1, len(b))
			var s0 goscanner.Scanner
			var err0 error
			s0.Init(fi, b, func(pos token.Position, msg string) {
				err0 = fmt.Errorf("%v: %s", pos, msg)
			}, 0)
			s := newScanner(path, b)
			for {
				pos0, tok0, lit0 := s0.Scan()
				position0 := fi.Position(pos0)
				eof0 := tok0 == token.EOF
				// trc("", position0, tok0, lit0, eof0)
				eof := !s.scan()
				// trc("", s.token().Position(), s.token().Ch(), s.token().Src(), s.eof)
				err := s.errs.Err()
				if g, e := s.token().Ch(), tok0; g != e {
					p.addFail()
					return fmt.Errorf("%v: token, got %v, expected %v", position0, g, e)
				}

				if g, e := err, err0; (g != nil) != (e != nil) {
					p.addFail()
					return fmt.Errorf("%v: error, got %v, expected %v", position0, g, e)
				}

				if err != nil {
					p.addOk()
					return nil
				}

				g, e := s.token().Src(), lit0
				if tok0 == token.STRING && strings.HasPrefix(e, "`") {
					// Specs: Carriage return characters ('\r') inside raw string literals are
					// discarded from the raw string value.
					g = strings.ReplaceAll(g, "\r", "")
				}
				if g != e {
					switch {
					case tok0 == token.SEMICOLON && lit0 != ";":
						// Ok, our result for injected semis is different.
					case noGoLit(s.token().Ch()):
						// Ok, go/scanner does not return the literal string.
					default:
						p.addFail()
						return fmt.Errorf("%v: source, got %q(`%[2]s`), expected %q(`%[3]s`)", position0, g, e)
					}
				}

				if g, e := s.token().Position().String(), position0.String(); g != e {
					ok := false
					switch {
					case eof || eof0:
						if a, b := s.token().Position().Offset, position0.Offset; a == b {
							ok = true
						}
					case tok0 == token.SEMICOLON && lit0 == "\n":
						ok = s.token().Position().Filename == position0.Filename && s.token().Position().Line == position0.Line
					}
					if !ok {
						p.addFail()
						return fmt.Errorf("%v: position, got %v (%v: %s %q)", e, g, path, tok0, lit0)
					}
				}

				if g, e := eof, eof0; g != e {
					p.addFail()
					return fmt.Errorf("%v: EOF, got %v, expected %v", position0, g, e)
				}

				if eof {
					break
				}
			}
			p.addOk()
			return nil
		})
		return nil
	}); err != nil {
		t.Error(err)
	}
}

func noGoLit(c token.Token) bool {
	switch c {
	case
		ADD,
		ADD_ASSIGN,
		AND,
		AND_ASSIGN,
		AND_NOT,
		AND_NOT_ASSIGN,
		ARROW,
		ASSIGN,
		COLON,
		COMMA,
		DEC,
		DEFINE,
		ELLIPSIS,
		EQL,
		GEQ,
		GTR,
		INC,
		LAND,
		LBRACE,
		LBRACK,
		LEQ,
		LOR,
		LPAREN,
		LSS,
		MUL,
		MUL_ASSIGN,
		NEQ,
		NOT,
		OR,
		OR_ASSIGN,
		PERIOD,
		QUO,
		QUO_ASSIGN,
		RBRACE,
		RBRACK,
		REM,
		REM_ASSIGN,
		RPAREN,
		SHL,
		SHL_ASSIGN,
		SHR,
		SHR_ASSIGN,
		SUB,
		SUB_ASSIGN,
		TILDE,
		XOR,
		XOR_ASSIGN:

		return true
	}

	return false
}

var falseNegatives = []string{
	"golang.org/x/tools/go/analysis/passes/unreachable/testdata/src/a/a.go",
}

func isKnownBad(fn string, pos token.Position) bool {
	fs := token.NewFileSet()
	ast, err := goparser.ParseFile(fs, fn, nil, goparser.ParseComments|goparser.DeclarationErrors)
	if err != nil {
		return true
	}

	for _, v := range ast.Comments {
		for _, w := range v.List {
			if strings.Contains(w.Text, "ERROR") && fs.PositionFor(w.Slash, true).Line == pos.Line {
				return true
			}
		}
	}

	s := filepath.ToSlash(fn)
	for _, k := range falseNegatives {
		if strings.Contains(s, k) {
			return true
		}
	}

	return false
}

func TestParser(t *testing.T) {
	gld := newGolden(t, "testdata/test_parse.golden")

	defer gld.close()

	var ms0, ms runtime.MemStats
	debug.FreeOSMemory()
	runtime.ReadMemStats(&ms0)
	p := newParallel()
	t.Run("src", func(t *testing.T) { testParser(p, t, *oSrc, gld) })
	t.Run("goroot", func(t *testing.T) { testParser(p, t, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, toks %v, skip %v, ok %v, fail %v", h(p.files), h(p.allToks), h(p.skipped), h(p.ok), h(p.fails))
	if p.fails != 0 {
		t.Logf("Shortest failing file: %s, %v tokens", p.minToksPath, p.minToks)
		return
	}

	t.Logf("Max backtrack: %s, %v for %v tokens\n\t%v (%v:)", p.maxBacktrackPath, h(p.maxBacktrack), h(p.maxBacktrackToks), p.maxBacktrackPos, p.maxBacktrackOrigin)
	t.Logf("Max backtracks: %s, %v for %v tokens", p.maxBacktracksPath, h(p.maxBacktracks), h(p.maxBacktracksToks))
	t.Logf("Max budget used: %s, %v for %v tokens", p.maxBudgetPath, h(p.maxBudget), h(p.maxBudgetToks))
	t.Logf("Max duration: %s, %v for %v tokens", p.maxDurationPath, p.maxDuration, h(p.maxDurationToks))
	if *oReport {
		t.Logf("\n%s", p.a.report())
	}
	debug.FreeOSMemory()
	runtime.ReadMemStats(&ms)
	if *oHeap && *oSrc == defaultSrc {
		t.Logf("ast count %v, heap %s", h(len(p.asts)), h(ms.HeapAlloc-ms0.HeapAlloc))
	}
}

func testParser(p *testParallel, t *testing.T, root string, gld *golden) {
	if err := filepath.Walk(filepath.FromSlash(root), func(path0 string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		if re != nil && !re.MatchString(path0) {
			return nil
		}

		if filepath.Ext(path0) != ".go" {
			return nil
		}

		p.addFile()
		path := path0
		p.exec(func() (err error) {
			if *oTrc {
				fmt.Fprintln(os.Stderr, path)
			}

			var pp *parser
			t0 := time.Now()

			defer func() {
				if err != nil {
					p.addFail()
					if pp != nil {
						p.recordMinToks(path, len(pp.s.toks))
					}
				}
				if pp != nil {
					p.recordMaxDuration(path, time.Since(t0), len(pp.s.toks))
					p.addToks(len(pp.s.toks))
					from := pp.s.toks[pp.maxBackRange[0]].position(pp.s.source)
					hi := mathutil.Min(pp.maxBackRange[1], len(pp.s.toks)-1)
					to := pp.s.toks[hi].position(pp.s.source)
					p.recordMaxBacktrack(path, pp.maxBack, len(pp.s.toks), fmt.Sprintf("%v: - %v:", from, to), pp.maxBackOrigin)
					p.recordMaxBacktracks(path, pp.backs, len(pp.s.toks))
					p.recordMaxBudget(path, parserBudget-pp.budget, len(pp.s.toks))
					if *oReport {
						p.a.merge(pp.a)
					}
				}
			}()

			b, err := os.ReadFile(path)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			pp = newParser(newScope(nil, scPackage), path, b, *oReport)
			pp.reportDeclarationErrors = true
			ast, err := pp.parse()
			if err != nil {
				if isKnownBad(path, pp.errPosition()) {
					t.Log(err)
					pp = nil
					p.addSkipped()
					return nil
				}

				return errorf("%s", err)
			}

			// trc("\n%s", dump(ast))
			srcA := string(b)
			srcB := ast.Source(true)
			if srcA != srcB {
				diff := difflib.UnifiedDiff{
					A:        difflib.SplitLines(srcA),
					B:        difflib.SplitLines(srcB),
					FromFile: "expected",
					ToFile:   "got",
					Context:  0,
				}
				s, _ := difflib.GetUnifiedDiffString(diff)
				return errorf(
					"%v: ast.Source differs\n%v\n--- expexted\n%s\n\n--- got\n%s\n\n--- expected\n%s\n--- got\n%s",
					path0, s, srcA, srcB, hex.Dump([]byte(srcA)), hex.Dump([]byte(srcB)),
				)
			}

			if *oHeap && *oSrc == defaultSrc {
				p.addAST(ast)
			}
			p.addOk()
			gld.w("%s\n", path)
			return nil
		})
		return nil
	}); err != nil {
		t.Error(err)
	}
}

func TestGoParser(t *testing.T) {
	gld := newGolden(t, "testdata/test_parse.go.golden")

	defer gld.close()

	var ms0, ms runtime.MemStats
	debug.FreeOSMemory()
	runtime.ReadMemStats(&ms0)
	p := newParallel()
	t.Run("src", func(t *testing.T) { testGoParser(p, t, *oSrc, gld) })
	t.Run("goroot", func(t *testing.T) { testGoParser(p, t, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, skip %v, ok %v, fail %v", h(p.files), h(p.skipped), h(p.ok), h(p.fails))
	t.Logf("Max duration: %s, %v for %v tokens", p.maxDurationPath, p.maxDuration, h(p.maxDurationToks))
	debug.FreeOSMemory()
	runtime.ReadMemStats(&ms)
	if *oHeap && *oSrc == defaultSrc {
		t.Logf("ast count %v, heap %s", h(len(p.asts)), h(ms.HeapAlloc-ms0.HeapAlloc))
	}
}

func testGoParser(p *testParallel, t *testing.T, root string, gld *golden) {
	if err := filepath.Walk(filepath.FromSlash(root), func(path0 string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}

		if re != nil && !re.MatchString(path0) {
			return nil
		}

		if filepath.Ext(path0) != ".go" {
			return nil
		}

		p.addFile()
		path := path0
		p.exec(func() (err error) {
			if *oTrc {
				fmt.Fprintln(os.Stderr, path)
			}

			t0 := time.Now()

			defer func() {
				if err != nil {
					p.addFail()
					return
				}

				p.recordMaxDuration(path, time.Since(t0), -1)
			}()

			b, err := os.ReadFile(path)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			ast, err := goparser.ParseFile(token.NewFileSet(), path, b, goparser.DeclarationErrors)
			if err != nil {
				if pos, ok := extractPos(err.Error()); !ok || isKnownBad(path, pos) {
					p.addSkipped()
					return nil
				}

				return errorf("%s", err)
			}

			if *oHeap && *oSrc == defaultSrc {
				p.addAST(ast)
			}
			p.addOk()
			gld.w("%s\n", path)
			return nil
		})
		return nil
	}); err != nil {
		t.Error(err)
	}
}

func BenchmarkParser(b *testing.B) {
	var sum int64
	root := *oBSrc
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if err := filepath.Walk(filepath.FromSlash(root), func(path0 string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if info.IsDir() {
				return nil
			}

			if re != nil && !re.MatchString(path0) {
				return nil
			}

			if filepath.Ext(path0) != ".go" {
				return nil
			}

			path := path0
			if err := func() (err error) {
				var pp *parser
				b, err := os.ReadFile(path)
				sum += int64(len(b))
				if err != nil {
					return errorf("%s: %v", path, err)
				}

				pp = newParser(newScope(nil, scPackage), path, b, *oReport)
				if _, err := pp.parse(); err != nil {
					if isKnownBad(path, pp.errPosition()) {
						return nil
					}

					return errorf("%s", err)
				}

				return nil
			}(); err != nil {
				b.Fatal(err)
			}
			return nil
		}); err != nil {
			b.Fatal(err)
		}
	}
	b.SetBytes(sum)
}

func BenchmarkGoParser(b *testing.B) {
	var sum int64
	root := *oBSrc
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if err := filepath.Walk(filepath.FromSlash(root), func(path0 string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if info.IsDir() {
				return nil
			}

			if re != nil && !re.MatchString(path0) {
				return nil
			}

			if filepath.Ext(path0) != ".go" {
				return nil
			}

			path := path0
			if err := func() (err error) {
				b, err := os.ReadFile(path)
				sum += int64(len(b))
				if err != nil {
					return errorf("%s: %v", path, err)
				}

				if _, err = goparser.ParseFile(token.NewFileSet(), path, b, goparser.DeclarationErrors); err != nil {
					if pos, ok := extractPos(err.Error()); !ok || isKnownBad(path, pos) {
						return nil
					}

					return errorf("%s", err)
				}

				return nil
			}(); err != nil {
				b.Fatal(err)
			}
			return nil
		}); err != nil {
			b.Fatal(err)
		}
	}
	b.SetBytes(sum)
}

func TestNewPackage(t *testing.T) {
	fs := os.DirFS(".")
	cfg, err := NewConfig(
		fs,
		runtime.GOOS, runtime.GOARCH,
		nil, nil,
		func(importPath string) (fsPath string, err error) {
			return ".", nil
		},
	)
	if err != nil {
		t.Fatal(err)
	}

	p, err := NewPackage(cfg, "")
	if err != nil {
		t.Fatal(err)
	}

	var a []string
	for k := range p.AST {
		a = append(a, k)
	}
	sort.Strings(a)
	t.Log(a)
	a = a[:0]
	for k := range p.Scope.nodes {
		if token.IsExported(k) {
			a = append(a, k)
		}
	}
	sort.Strings(a)
	if len(a) > 10 {
		a = append(a[:10], "...")
	}
	t.Log(a)
}
