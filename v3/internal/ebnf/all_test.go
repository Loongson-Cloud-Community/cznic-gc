package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/scanner"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strings"
	"sync"
	"testing"

	"golang.org/x/exp/ebnf"
	"modernc.org/mathutil"
)

const (
	startProduction = "SourceFile"
)

var (
	spec, peg ebnf.Grammar

	oRE  = flag.String("re", "", "")
	oTrc = flag.Bool("trc", false, "")

	re *regexp.Regexp

	toks = map[string]token.Token{
		"!":             token.NOT,
		"!=":            token.NEQ,
		"%":             token.REM,
		"%=":            token.REM_ASSIGN,
		"&":             token.AND,
		"&&":            token.LAND,
		"&=":            token.AND_ASSIGN,
		"&^":            token.AND_NOT,
		"&^=":           token.AND_NOT_ASSIGN,
		"(":             token.LPAREN,
		")":             token.RPAREN,
		"*":             token.MUL,
		"*=":            token.MUL_ASSIGN,
		"+":             token.ADD,
		"++":            token.INC,
		"+=":            token.ADD_ASSIGN,
		",":             token.COMMA,
		"-":             token.SUB,
		"--":            token.DEC,
		"-=":            token.SUB_ASSIGN,
		".":             token.PERIOD,
		"...":           token.ELLIPSIS,
		"/":             token.QUO,
		"/=":            token.QUO_ASSIGN,
		":":             token.COLON,
		":=":            token.DEFINE,
		";":             token.SEMICOLON,
		"<":             token.LSS,
		"<-":            token.ARROW,
		"<<":            token.SHL,
		"<<=":           token.SHL_ASSIGN,
		"<=":            token.LEQ,
		"=":             token.ASSIGN,
		"==":            token.EQL,
		">":             token.GTR,
		">=":            token.GEQ,
		">>":            token.SHR,
		">>=":           token.SHR_ASSIGN,
		"[":             token.LBRACK,
		"]":             token.RBRACK,
		"^":             token.XOR,
		"^=":            token.XOR_ASSIGN,
		"float_lit":     token.FLOAT,
		"identifier":    token.IDENT,
		"imaginary_lit": token.IMAG,
		"int_lit":       token.INT,
		"rune_lit":      token.CHAR,
		"string_lit":    token.STRING,
		"{":             token.LBRACE,
		"|":             token.OR,
		"|=":            token.OR_ASSIGN,
		"||":            token.LOR,
		"}":             token.RBRACE,
		"~":             token.TILDE,
	}
)

func init() {
	var err error
	if spec, err = verifySpecEBNF(filepath.Join(runtime.GOROOT(), "doc", "go_spec.html"), startProduction, nil); err != nil {
		panic(err)
	}

	var b bytes.Buffer
	printEBNF(&b, spec)
	if err = os.WriteFile("spec.ebnf", b.Bytes(), 0660); err != nil {
		panic(err)
	}

	s, err := os.ReadFile("peg.ebnf")
	if err != nil {
		panic(err)
	}

	if peg, err = ebnf.Parse("peg.ebnf", bytes.NewBuffer(s)); err != nil {
		panic(err)
	}

	if ebnf.Verify(peg, startProduction); err != nil {
		panic(err)
	}
}

func TestMain(m *testing.M) {
	flag.BoolVar(&trcTODOs, "trctodo", false, "")
	flag.Parse()
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	os.Exit(m.Run())
}

func TestSpecEBNF(t *testing.T) {
	for _, v := range leftRecursive(spec, startProduction) {
		var a []string
		for _, w := range v {
			a = append(a, w.Name.String)
		}
		t.Logf("left recursive: %v", a)
	}
}

func TestPEGEBNF(t *testing.T) {
	for _, v := range leftRecursive(peg, startProduction) {
		var a []string
		for _, w := range v {
			a = append(a, w.Name.String)
		}
		t.Errorf("left recursive: %v", a)
	}
}

type golden struct {
	a  []string
	f  *os.File
	mu sync.Mutex
	t  *testing.T

	discard bool
}

func newGolden(t *testing.T, fn string) *golden {
	if re != nil {
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

func TestPEG(t *testing.T) {
	gld := newGolden(t, fmt.Sprintf("testdata/test_parse.golden"))

	defer gld.close()

	p := newParallel()
	t.Run("cd", func(t *testing.T) { testPEG(p, t, peg, ".", gld) })
	t.Run("goroot", func(t *testing.T) { testPEG(p, t, peg, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, skip %v, ok %v, fail %v", h(p.files), h(p.skipped), h(p.ok), h(p.fails))
}

func testPEG(p *parallel, t *testing.T, g ebnf.Grammar, root string, gld *golden) {
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
		p.exec(func() (err error) {
			if *oTrc {
				fmt.Fprintln(os.Stderr, path)
			}
			p.addFile()

			defer func() {
				if err != nil {
					p.addFail()
				}
			}()

			b, err := os.ReadFile(path)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			pp, err := newParser(g, path, b)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			if err := pp.parse(startProduction); err != nil {
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

type tok struct {
	pos token.Pos
	tok token.Token
	lit string
}

func (t tok) String() string { return fmt.Sprintf("%d: %v %q", t.pos, t.tok, t.lit) }

type parser struct {
	f    *token.File
	g    ebnf.Grammar
	path string
	toks []tok

	budget   int
	maxIndex int
}

func newParser(g ebnf.Grammar, path string, src []byte) (r *parser, err error) {
	r = &parser{
		budget: 1e7,
		g:      g,
		path:   path,
	}
	var s scanner.Scanner
	fs := token.NewFileSet()
	r.f = fs.AddFile(path, -1, len(src))
	s.Init(r.f, src, func(pos token.Position, msg string) { err = errorf("%v: %s", pos, msg) }, 0)
	for {
		pos, t, lit := s.Scan()
		r.toks = append(r.toks, tok{pos, t, lit})
		if err != nil {
			return nil, err
		}

		if t == token.EOF {
			return r, nil
		}
	}
}

func (p *parser) parse(start string) error {
	ix, ok := p.parseExpression(0, p.g[start].Expr)
	if p.budget == 0 {
		return errorf("%s: resources exhausted", p.path)
	}

	if !ok || ix < len(p.toks)-1 {
		t := p.toks[p.maxIndex]
		return errorf("%s: syntax error", p.f.PositionFor(t.pos, true))
	}

	return nil
}

func (p *parser) at(ix int) { p.maxIndex = mathutil.Max(p.maxIndex, ix) }

func (p *parser) c(ix int) tok {
	if p.budget == 0 {
		return p.toks[len(p.toks)-1]
	}

	p.budget--
	return p.toks[ix]
}

func (p *parser) tok(s string) token.Token {
	if r, ok := toks[s]; ok {
		return r
	}

	panic(todo("%q", s))
}

func (p *parser) parseExpression(ix int, e ebnf.Expression) (r int, ok bool) {
	r = ix
	// var b bytes.Buffer
	// printEBNFExpression(&b, e)
	// trc("%s[%d]: tok {%v}, expr %s", p.f.PositionFor(p.c(ix).pos, true), ix, p.c(ix), b.Bytes())
	p.at(ix)
	switch x := e.(type) {
	case ebnf.Sequence:
		for _, v := range x {
			n, ok := p.parseExpression(ix, v)
			if !ok {
				return r, false
			}

			ix = n
		}
		return ix, true
	case *ebnf.Name:
		switch nm := x.String; {
		case token.IsExported(nm):
			if e := p.g[nm].Expr; e != nil {
				return p.parseExpression(ix, p.g[nm].Expr)
			}

			return ix, true
		default:
			if p.c(ix).tok == p.tok(x.String) {
				ix++
			}
			return ix, ix != r
		}
	case *ebnf.Token:
		switch {
		case token.IsKeyword(x.String):
			if p.c(ix).lit == x.String {
				ix++
			}
		default:
			if p.c(ix).tok == p.tok(x.String) {
				ix++
			}
		}
		return ix, ix != r
	case *ebnf.Repetition:
		for {
			n, ok := p.parseExpression(ix, x.Body)
			if !ok {
				return n, true
			}

			ix = n
		}
	case *ebnf.Group:
		return p.parseExpression(ix, x.Body)
	case ebnf.Alternative:
		for _, v := range x {
			if n, ok := p.parseExpression(ix, v); ok {
				return n, true
			}
		}
		return r, false
	case *ebnf.Option:
		r, _ = p.parseExpression(ix, x.Body)
		return r, true
	default:
		panic(todo("%T", x))
	}
}
