// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
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
	"testing"
	"unicode"
)

func stack() string { return string(debug.Stack()) }

var (
	oRE     = flag.String("re", "", "")
	oReport = flag.Bool("report", false, "")
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
	if re != nil || *oReport {
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

func TestScanner(t *testing.T) {
	p := newParallel()
	t.Run("errors", func(t *testing.T) { testScanErrors(t) })
	t.Run("numbers", func(t *testing.T) { testNumbers(t) })
	t.Run(".", func(t *testing.T) { testScan(p, t, ".", "") })
	t.Run("GOROOT", func(t *testing.T) { testScan(p, t, runtime.GOROOT(), "/test/") })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, ok %v, fail %v", h(p.files), h(p.ok), h(p.fails))
}

func testScan(p *parallel, t *testing.T, root, skip string) {
	if err := filepath.Walk(root, func(path0 string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		switch {
		case re == nil:
			if strings.Contains(filepath.ToSlash(path0), "/errchk/") {
				return nil
			}

			if strings.Contains(filepath.ToSlash(path0), "/testdata/") {
				return nil
			}

			if skip != "" && strings.Contains(filepath.ToSlash(path0), skip) {
				return nil
			}
		default:
			if !re.MatchString(path0) {
				return nil
			}
		}

		if filepath.Ext(path0) != ".go" {
			return nil
		}

		path := path0
		p.addFile()
		p.exec(func() error {
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
				eof := !s.scan()
				err := s.errs.Err()
				if g, e := s.token().Ch(), tok0; g != e {
					p.addFail()
					return fmt.Errorf("%v: token, got %v, expected %v", position0, g, e)
				}

				if g, e := s.token().Src(), lit0; g != e {
					switch {
					case tok0 == token.SEMICOLON && lit0 != ";":
						// Ok, our result for injected semis is different.
					case noGoLit(s.token().Ch()):
						// Ok, go/scanner does not return the literal string.
					default:
						p.addFail()
						return fmt.Errorf("%v: source, got %q, expected %q", position0, g, e)
					}
				}

				if g, e := s.token().Position().String(), position0.String(); g != e {
					ok := false
					switch {
					case eof || eof0:
						if a, b := s.token().Position().Offset, position0.Offset; a-b == 1 || b-a == 1 {
							ok = true
						}
					case tok0 == token.SEMICOLON && lit0 == "\n":
						ok = s.token().Position().Filename == position0.Filename && s.token().Position().Line == position0.Line
					}
					if !ok {
						p.addFail()
						return fmt.Errorf("%v: got %v", e, g)
					}
				}

				if g, e := err, err0; (g != nil) != (e != nil) {
					p.addFail()
					return fmt.Errorf("%v: error, got %v, expected %v", position0, g, e)
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

func isKnownBad(fn string, pos token.Position) bool {
	fs := token.NewFileSet()
	ast, err := goparser.ParseFile(fs, fn, nil, goparser.SkipObjectResolution|goparser.ParseComments)
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

	return false
}

func TestParser(t *testing.T) {
	gld := newGolden(t, "testdata/test_parse.golden")

	defer gld.close()

	p := newParallel()
	t.Run("cd", func(t *testing.T) { testParser(p, t, ".", gld) })
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
	if *oReport {
		t.Logf("\n%s", p.a.report())
	}
}

func testParser(p *parallel, t *testing.T, root string, gld *golden) {
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

			defer func() {
				if err != nil {
					p.addFail()
					if pp != nil {
						p.recordMinToks(path, len(pp.s.toks))
					}
				}
				if pp != nil {
					p.addToks(len(pp.s.toks))
					from := pp.s.toks[pp.maxBackRange[0]].position(pp.s.source)
					to := pp.s.toks[pp.maxBackRange[1]].position(pp.s.source)
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

			pp = newParser(path, b, *oReport)
			if err := pp.parse(); err != nil {
				if isKnownBad(path, pp.errPosition()) {
					pp = nil
					p.addSkipped()
					return nil
				}

				return errorf("%s", err)
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

	p := newParallel()
	t.Run("cd", func(t *testing.T) { testGoParser(p, t, ".", gld) })
	t.Run("goroot", func(t *testing.T) { testGoParser(p, t, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, skip %v, ok %v, fail %v", h(p.files), h(p.skipped), h(p.ok), h(p.fails))
}

func testGoParser(p *parallel, t *testing.T, root string, gld *golden) {
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

			defer func() {
				if err != nil {
					p.addFail()
				}
			}()

			b, err := os.ReadFile(path)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			if _, err = goparser.ParseFile(token.NewFileSet(), path, b, goparser.SkipObjectResolution); err != nil {
				if pos, ok := extractPos(err.Error()); !ok || isKnownBad(path, pos) {
					p.addSkipped()
					return nil
				}

				return errorf("%s", err)
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
