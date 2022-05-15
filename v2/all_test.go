// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"bytes"
	"flag"
	"fmt"
	goscanner "go/scanner"
	gotoken "go/token"
	"io/fs"
	"io/ioutil"
	"math/rand"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"strings"
	"testing"
	"unicode"

	"modernc.org/scannertest"
	"modernc.org/y"
)

func caller(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(2)
	fmt.Fprintf(os.Stderr, "# caller: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	_, fn, fl, _ = runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "# \tcallee: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func dbg(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	pc, fn, fl, _ := runtime.Caller(1)
	f := runtime.FuncForPC(pc)
	fmt.Fprintf(os.Stderr, "# dbg %s:%d:%s: ", path.Base(fn), fl, f.Name())
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func stack() []byte { return debug.Stack() }

func use(...interface{}) {}

func init() {
	use(caller, dbg, stack) //TODOOK
}

// ----------------------------------------------------------------------------

const (
	maxTokenToken = iota + gotoken.TILDE
	tokenBODY
)

var (
	_ scannertest.Interface = (*testScanner)(nil)

	oRE  = flag.String("re", "", "")
	oTrc = flag.Bool("trc", false, "")

	digits    = expand(unicode.Nd)
	letters   = expand(unicode.L)
	re        *regexp.Regexp
	str2token = map[string]gotoken.Token{}
	tempDir   string
	yp0       *yParser
)

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

type yParser struct {
	*y.Parser
	reports   [][]byte
	terminals []*y.Symbol
	tok2sym   map[gotoken.Token]*y.Symbol
}

func newYParser(yaccFile string) *yParser {
	var closures bool
	var fn string
	for i, v := range os.Args {
		if i == 0 {
			continue
		}

		switch v {
		case "-closures":
			closures = true
		case "-out":
			fn = os.Args[i+1]
		}
	}
	fset := gotoken.NewFileSet()
	var out bytes.Buffer
	p, err := y.ProcessFile(fset, yaccFile, &y.Options{
		Closures:  closures,
		Reducible: true,
		Report:    &out,
	})
	if fn != "" {
		if err := ioutil.WriteFile(fn, out.Bytes(), 0644); err != nil {
			panic(err)
		}
	}

	if err != nil {
		panic(err)
	}

	reports := make([][]byte, len(p.States))
	rep := out.Bytes()
	sep := []byte("\ns") // "\nstate "
	s := 0
	for i := range reports {
		e := bytes.Index(rep[s:], sep)
		if e < 0 {
			e = len(rep[s:]) - 1
		}
		reports[i] = rep[s : s+e]
		s = s + e + 1
	}

	m := make(map[gotoken.Token]*y.Symbol, len(p.Syms))
	for k, v := range p.Syms {
		if !v.IsTerminal || k == "BODY" || k[0] == '_' {
			continue
		}

		switch {
		case k[0] >= 'A' && k[0] <= 'Z':
			if tok, ok := str2token[k]; ok {
				m[tok] = v
				break
			}

			l := v.LiteralString
			if l == "" {
				panic(fmt.Errorf("no token for %q", k))
			}

			if tok, ok := str2token[l[1:len(l)-1]]; ok {
				m[tok] = v
				break
			}

			panic(k)
		case k[0] == '\'':
			tok := str2token[k[1:2]]
			m[tok] = v
		default:
		}
	}
	m[gotoken.EOF] = p.Syms["$end"]
	m[tokenBODY] = p.Syms["BODY"]
	var t []*y.Symbol
	for _, v := range p.Syms {
		if v.IsTerminal {
			t = append(t, v)
		}
	}
	return &yParser{
		Parser:    p,
		reports:   reports,
		terminals: t,
		tok2sym:   m,
	}
}

func TestMain(m *testing.M) {
	extendedErrors = true
	for i := gotoken.IDENT; i <= maxTokenToken; i++ {
		s := strings.ToUpper(i.String())
		if _, ok := str2token[s]; ok {
			panic(todo(""))
		}

		str2token[s] = i
	}
	str2token["ILLEGAL"] = gotoken.ILLEGAL
	yp0 = newYParser("testdata/parser/parser.y")
	flag.Parse()
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}
	os.Exit(testMain(m))
}

func testMain(m *testing.M) int {
	var err error
	if tempDir, err = ioutil.TempDir("", "run-test-"); err != nil {
		panic(err) //TODOOK
	}

	defer os.RemoveAll(tempDir)

	return m.Run()
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
		p.file()
		p.exec(func() error {
			b, err := os.ReadFile(path)
			if err != nil {
				p.fail()
				return err
			}

			fs := gotoken.NewFileSet()
			fi := fs.AddFile(path, -1, len(b))
			var s0 goscanner.Scanner
			var err0 error
			s0.Init(fi, b, func(pos gotoken.Position, msg string) {
				err0 = fmt.Errorf("%v: %s", pos, msg)
			}, 0)
			s, err := NewScanner(path, b)
			if err != nil {
				p.fail()
				return err
			}

			for {
				pos0, tok0, lit0 := s0.Scan()
				position0 := fi.Position(pos0)
				eof0 := tok0 == gotoken.EOF
				eof := !s.Scan()
				err := s.Err()
				if g, e := s.Tok.token(), tok0; g != e {
					p.fail()
					return fmt.Errorf("%v: token, got %v, expected %v", position0, g, e)
				}

				if g, e := s.Tok.Src(), lit0; g != e {
					switch {
					case tok0 == gotoken.SEMICOLON && lit0 != ";":
						// Ok, our result for injected semis is different.
					case noGoLit(s.Tok.Ch):
						// Ok, go/scanner does not return the literal string.
					default:
						p.fail()
						return fmt.Errorf("%v: source, got %q, expected %q", position0, g, e)
					}
				}

				if g, e := s.Tok.Position().String(), position0.String(); g != e {
					ok := false
					switch {
					case eof || eof0:
						if a, b := s.Tok.Position().Offset, position0.Offset; a-b == 1 || b-a == 1 {
							ok = true
						}
					case tok0 == gotoken.SEMICOLON && lit0 == "\n":
						ok = s.Tok.Position().Filename == position0.Filename && s.Tok.Position().Line == position0.Line
					}
					if !ok {
						p.fail()
						return fmt.Errorf("%v: got %v:", e, g)
					}
				}

				if g, e := err, err0; (g != nil) != (e != nil) {
					p.fail()
					return fmt.Errorf("%v: error, got %v, expected %v", position0, g, e)
				}

				if g, e := eof, eof0; g != e {
					p.fail()
					return fmt.Errorf("%v: EOF, got %v, expected %v", position0, g, e)
				}

				if eof {
					break
				}
			}
			p.ok()
			return nil
		})
		return nil
	}); err != nil {
		t.Error(err)
	}
}

func noGoLit(c Ch) bool {
	switch c {
	case
		'!',
		'%',
		'&',
		'(',
		')',
		'*',
		'+',
		',',
		'-',
		'.',
		'/',
		':',
		'<',
		'=',
		'>',
		'[',
		']',
		'^',
		'{',
		'|',
		'}',
		'~',
		ADD_ASSIGN,
		AND_ASSIGN,
		AND_NOT,
		AND_NOT_ASSIGN,
		ARROW,
		DEC,
		DEFINE,
		ELLIPSIS,
		EQ,
		GE,
		INC,
		LAND,
		LE,
		LOR,
		MUL_ASSIGN,
		NE,
		OR_ASSIGN,
		QUO_ASSIGN,
		REM_ASSIGN,
		SHL,
		SHL_ASSIGN,
		SHR,
		SHR_ASSIGN,
		SUB_ASSIGN,
		XOR_ASSIGN:

		return true
	}

	return false
}

func TestScanner(t *testing.T) {
	p := newParallel()
	t.Run("states", func(t *testing.T) { testScanStates(t) })
	t.Run(".", func(t *testing.T) { testScan(p, t, ".", "") })
	t.Run("GOROOT", func(t *testing.T) { testScan(p, t, runtime.GOROOT(), "/test/") })
	t.Run("errors", func(t *testing.T) { testScanErrors(t) })
	t.Run("numbers", func(t *testing.T) { testNumbers(t) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, ok %v, fails %v", p.files, p.oks, p.fails)
}

type testScanner struct {
	inits int
	mod   int
	s     *Scanner
}

func newTestScanner() *testScanner {
	return &testScanner{}
}

func (s *testScanner) Init(name string, src []byte) (err error) {
	s.s, err = NewScanner(name, []byte(src))
	s.inits++
	return err
}

func (s *testScanner) Rune(c byte) (r rune, ok bool) {
	switch c {
	case 0:
		return -1, false
	case 0x80: // unicodeDigit
		r = digits[s.mod%len(digits)]
		s.mod++
		return r, true
	case 0x81: // unicodeLetter
		r = letters[s.mod%len(letters)]
		s.mod++
		return r, true
	}

	if c < 128 {
		return rune(c), true
	}

	return -1, false
}

func (s *testScanner) Scan() error {
	s.s.Tok.source = s.s.source
	s.s.scan()
	return s.s.Err()
}

func testScanStates(t *testing.T) {
	if testing.Short() {
		t.Skip("-short")
	}

	b, err := os.ReadFile(filepath.FromSlash("testdata/scanner/scanner.l"))
	if err != nil {
		t.Fatal(err)
	}

	s := newTestScanner()
	if err := scannertest.TestStates("scanner.l", bytes.NewReader(b), s); err != nil {
		t.Fatal(err)
	}

	t.Logf("%v test cases", s.inits)
}

func BenchmarkScanner(b *testing.B) {
	root := runtime.GOROOT()
	skip := filepath.ToSlash(root + "/test/")
	var sz int64
	files := 0
	debug.FreeOSMemory()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		err := filepath.Walk(runtime.GOROOT(), func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if info.IsDir() {
				return nil
			}

			if strings.HasPrefix(filepath.ToSlash(path), skip) {
				return nil
			}

			buf, err := ioutil.ReadFile(path)
			if err != nil {
				return err
			}

			if i == 0 {
				sz += int64(len(buf))
				files++
			}
			switch filepath.Ext(path) {
			case ".go":
				s, err := NewScanner(path, buf)
				if err != nil {
					return err
				}

				for s.Scan() {
				}
				if err := s.Err(); err != nil {
					b.Fatalf("%s: %v", path, err)
				}
			}
			return nil
		})
		if err != nil {
			b.Fatal(err)
		}
	}
	b.SetBytes(sz)
}

func BenchmarkGoScanner(b *testing.B) {
	root := runtime.GOROOT()
	skip := filepath.ToSlash(root + "/test/")
	var sz int64
	files := 0
	debug.FreeOSMemory()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		err := filepath.Walk(runtime.GOROOT(), func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if info.IsDir() {
				return nil
			}

			if strings.HasPrefix(filepath.ToSlash(path), skip) {
				return nil
			}

			buf, err := ioutil.ReadFile(path)
			if err != nil {
				return err
			}

			if i == 0 {
				sz += int64(len(buf))
				files++
			}
			switch filepath.Ext(path) {
			case ".go":
				fs := gotoken.NewFileSet()
				fi := fs.AddFile(path, -1, len(buf))
				var s goscanner.Scanner
				s.Init(fi, buf, nil, goscanner.ScanComments)
				for {
					_, tok, _ := s.Scan()
					if tok == gotoken.EOF {
						break
					}
				}
			}
			return nil
		})
		if err != nil {
			b.Fatal(err)
		}
	}
	b.SetBytes(sz)
}

func TestTokenSet(t *testing.T) {
	for itest, test := range []string{
		"a",
		"a b",
		"a b c",
		"a1",
		"a1 b2",
		"a1 b2 c2",
	} {
		ntoks := len(strings.Split(test, " "))
		for itok := 0; itok < ntoks; itok++ {
			s, err := NewScanner(fmt.Sprintf("%v.go", itest), []byte(test))
			if err != nil {
				t.Fatal(itest, err)
			}

			var toks []Token
			var seps, srcs []string
			for s.Scan() {
				toks = append(toks, s.Tok)
				seps = append(seps, s.Tok.Sep())
				srcs = append(srcs, s.Tok.Src())
			}

			for j, v := range []struct{ sep, src string }{
				{"", ""},
				{"x", ""},
				{"x", "y"},
				{"", "y"},
				{"xx", ""},
				{"xx", "y"},
				{"", "y"},
				{"xx", ""},
				{"xx", "yy"},
				{"", "yy"},
				{"x", ""},
				{"x", "yy"},
				{"", "yy"},
			} {
				toks[itok].Set(v.sep, v.src)
				var sep, src string
				for i, tok := range toks {
					switch {
					case i == itok:
						sep = v.sep
						src = v.src
					default:
						sep = seps[i]
						src = srcs[i]
					}
					if g, e := tok.Sep(), sep; g != e {
						t.Errorf("test %v, tok %v, j %v, got separator %q, expected %q", itest, itok, j, g, e)
					}
					if g, e := tok.Src(), src; g != e {
						t.Errorf("test %v, tok %v, j %v, got source %q, expected %q", itest, itok, j, g, e)
					}
				}
			}
		}
	}
}

func TestParser(t *testing.T) {
	return //TODO-
	p := newParallel()
	t.Run("Grammar", testParserGrammar)
	t.Run("RejectFollowSet", testParserRejectFS)
	//TODO t.Run(".", func(t *testing.T) { testParser(p, t, ".", "") })
	//TODO t.Run("GOROOT", func(t *testing.T) { testParser(p, t, runtime.GOROOT(), "/test/") })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, ok %v, fails %v", p.files, p.oks, p.fails)
}

func testParserGrammar(t *testing.T) {
	p := newParallel()
	files := findGoFiles(t, ".")
	files = append(files, findGoFiles(t, runtime.GOROOT())...)
	trc("", len(files))
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, ok %v, fails %v", p.files, p.oks, p.fails)
}

func testParserRejectFS(t *testing.T) {
	t.Logf("TODO")
}

func findGoFiles(t *testing.T, dir string) (r []string) {
	if err := filepath.Walk(dir, func(path string, info fs.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		switch filepath.Ext(path) {
		case ".go":
			r = append(r, path)
		}

		return nil
	}); err != nil {
		t.Fatal(err)
	}
	return r
}

func testParser(p *parallel, t *testing.T, root, skip string) {
	err := filepath.Walk(root, func(path0 string, info os.FileInfo, err error) error {
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

		if *oTrc {
			fmt.Println(path0)
		}
		path := path0
		p.file()
		p.exec(func() error {
			b, err := os.ReadFile(path)
			if err != nil {
				p.fail()
				return err
			}

			if _, err = ParseSourceFile(path, b); err != nil {
				p.fail()
				return err
			}

			p.ok()
			return nil
		})
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
}
