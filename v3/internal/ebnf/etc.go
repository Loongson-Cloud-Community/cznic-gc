package main

import (
	"bytes"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/dustin/go-humanize"
	"golang.org/x/exp/ebnf"
	"modernc.org/mathutil"
)

const epsilon = -1

var (
	trcTODOs       bool
	extendedErrors = true

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
		"break":         token.BREAK,
		"case":          token.CASE,
		"chan":          token.CHAN,
		"const":         token.CONST,
		"continue":      token.CONTINUE,
		"default":       token.DEFAULT,
		"defer":         token.DEFER,
		"else":          token.ELSE,
		"fallthrough":   token.FALLTHROUGH,
		"float_lit":     token.FLOAT,
		"for":           token.FOR,
		"func":          token.FUNC,
		"go":            token.GO,
		"goto":          token.GOTO,
		"identifier":    token.IDENT,
		"if":            token.IF,
		"imaginary_lit": token.IMAG,
		"import":        token.IMPORT,
		"int_lit":       token.INT,
		"interface":     token.INTERFACE,
		"map":           token.MAP,
		"package":       token.PACKAGE,
		"range":         token.RANGE,
		"return":        token.RETURN,
		"rune_lit":      token.CHAR,
		"select":        token.SELECT,
		"string_lit":    token.STRING,
		"struct":        token.STRUCT,
		"switch":        token.SWITCH,
		"type":          token.TYPE,
		"var":           token.VAR,
		"{":             token.LBRACE,
		"|":             token.OR,
		"|=":            token.OR_ASSIGN,
		"||":            token.LOR,
		"}":             token.RBRACE,
		"~":             token.TILDE,
	}
)

// origin returns caller's short position, skipping skip frames.
func origin(skip int) string {
	pc, fn, fl, _ := runtime.Caller(skip)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
		if strings.HasPrefix(fns, "func") {
			num := true
			for _, c := range fns[len("func"):] {
				if c < '0' || c > '9' {
					num = false
					break
				}
			}
			if num {
				return origin(skip + 2)
			}
		}
	}
	return fmt.Sprintf("%s:%d:%s", filepath.Base(fn), fl, fns)
}

// todo prints and returns caller's position and an optional message tagged with TODO. Output goes to stderr.
func todo(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	r := fmt.Sprintf("%s\n\tTODO %s", origin(2), s)
	// fmt.Fprintf(os.Stderr, "%s\n", r)
	// os.Stdout.Sync()
	return r
}

// trc prints and returns caller's position and an optional message tagged with TRC. Output goes to stderr.
func trc(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	r := fmt.Sprintf("%s: TRC %s", origin(2), s)
	fmt.Fprintf(os.Stderr, "%s\n", r)
	os.Stderr.Sync()
	return r
}

func printEBNF(w io.Writer, g ebnf.Grammar) {
	var a []string
	for k := range g {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		p := g[k]
		fmt.Fprintf(w, "%s = ", p.Name.String)
		if p.Expr != nil {
			printEBNFExpression(w, p.Expr)
		}
		fmt.Fprintf(w, " .\n")
	}
}

func ebnfString(e ebnf.Expression) string {
	var b strings.Builder
	printEBNFExpression(&b, e)
	return b.String()
}

func printEBNFExpression(w io.Writer, e ebnf.Expression) {
	switch x := e.(type) {
	case ebnf.Sequence:
		for i, v := range x {
			if i != 0 {
				fmt.Fprintf(w, " ")
			}
			printEBNFExpression(w, v)
		}
	case *ebnf.Name:
		fmt.Fprintf(w, "%s", x.String)
	case *ebnf.Token:
		s := x.String
		s = strings.ReplaceAll(s, "&lt;", "<")
		s = strings.ReplaceAll(s, "&gt;", ">")
		s = strings.ReplaceAll(s, "&amp;", "&")
		fmt.Fprintf(w, "%q", s)
	case *ebnf.Option:
		fmt.Fprintf(w, "[ ")
		printEBNFExpression(w, x.Body)
		fmt.Fprintf(w, " ]")
	case *ebnf.Group:
		fmt.Fprintf(w, "( ")
		printEBNFExpression(w, x.Body)
		fmt.Fprintf(w, " )")
	case ebnf.Alternative:
		for i, v := range x {
			if i != 0 {
				fmt.Fprintf(w, " | ")
			}
			printEBNFExpression(w, v)
		}
	case *ebnf.Repetition:
		fmt.Fprintf(w, "{ ")
		printEBNFExpression(w, x.Body)
		fmt.Fprintf(w, " }")
	case *ebnf.Range:
		printEBNFExpression(w, x.Begin)
		fmt.Fprintf(w, " ... ")
		printEBNFExpression(w, x.End)
	case nil:
		// ok
	default:
		panic(todo("%T", x))
	}
}

func ebnfEpressionString(e ebnf.Expression) string {
	var b strings.Builder
	printEBNFExpression(&b, e)
	return b.String()
}

// errorf constructs an error value. If ExtendedErrors is true, the error will
// contain its origin.
func errorf(s string, args ...interface{}) error {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	if trcTODOs && strings.HasPrefix(s, "TODO") {
		fmt.Fprintf(os.Stderr, "%s (%v)\n", s, origin(2))
		os.Stderr.Sync()
	}
	switch {
	case extendedErrors:
		return fmt.Errorf("%s (%v: %v: %v)", s, origin(4), origin(3), origin(2))
	default:
		return fmt.Errorf("%s", s)
	}
}

func h(v interface{}) string {
	switch x := v.(type) {
	case int:
		return humanize.Comma(int64(x))
	case int32:
		return humanize.Comma(int64(x))
	case int64:
		return humanize.Comma(x)
	case uint32:
		return humanize.Comma(int64(x))
	case uint64:
		if x <= math.MaxInt64 {
			return humanize.Comma(int64(x))
		}
	}
	return fmt.Sprint(v)
}

type parallel struct {
	errors []error
	limit  chan struct{}
	sync.Mutex
	wg sync.WaitGroup

	fails   int32
	files   int32
	ok      int32
	skipped int32
}

func newParallel() *parallel {
	return &parallel{
		limit: make(chan struct{}, runtime.GOMAXPROCS(0)),
	}
}

func (p *parallel) addSkipped() { atomic.AddInt32(&p.skipped, 1) }
func (p *parallel) addFail()    { atomic.AddInt32(&p.fails, 1) }
func (p *parallel) addFile()    { atomic.AddInt32(&p.files, 1) }
func (p *parallel) addOk()      { atomic.AddInt32(&p.ok, 1) }

func (p *parallel) err(err error) {
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

func (p *parallel) exec(run func() error) {
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

func (p *parallel) wait() error {
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

func leftRecursive(g ebnf.Grammar, start string) (r [][]*ebnf.Production) {
	p := g[start]
	m := map[*ebnf.Production]int{p: 1}
	detected := map[*ebnf.Production]struct{}{}

	var f func(ebnf.Expression, int, []*ebnf.Production) int
	f = func(e ebnf.Expression, pos int, stack []*ebnf.Production) (npos int) {
		switch x := e.(type) {
		case ebnf.Sequence:
			for _, v := range x {
				pos = f(v, pos, stack)
			}
			return pos
		case *ebnf.Name:
			nm := x.String
			if first := nm[0]; first >= 'a' && first <= 'z' {
				return pos + 1
			}

			p := g[nm]
			if _, ok := detected[p]; ok {
				return pos
			}

			sv := m[p]
			defer func() { m[p] = sv }()

			if sv == pos {
				detected[p] = struct{}{}
				for sp := len(stack) - 1; ; sp-- {
					if stack[sp] == p {
						r = append(r, append([]*ebnf.Production(nil), stack[sp:]...))
						return pos
					}
				}
				panic(todo(""))
			}

			if sv != 0 {
				return pos
			}

			m[p] = pos
			return f(p.Expr, pos, append(stack, p))
		case *ebnf.Token:
			return pos + 1
		case ebnf.Alternative:
			moved := true
			for _, v := range x {
				if f(v, pos, stack) == pos {
					moved = false
				}
			}
			if moved {
				pos++
			}
			return pos
		case *ebnf.Repetition:
			f(x.Body, pos, stack)
			return pos
		case *ebnf.Group:
			return f(x.Body, pos, stack)
		case *ebnf.Option:
			f(x.Body, pos, stack)
			return pos
		case nil:
			return pos + 1
		default:
			panic(todo("%T", x))
		}
		panic(todo(""))
	}

	f(p.Expr, 1, []*ebnf.Production{p})
	return r
}

type followSet map[token.Token]struct{}

type grammar struct {
	ebnf       ebnf.Grammar
	followSets map[*ebnf.Production]followSet
}

func newGrammar(name, start string, src []byte) (r *grammar, err error) {
	g, err := ebnf.Parse(name, bytes.NewBuffer(src))
	if err != nil {
		return nil, err
	}

	if err = ebnf.Verify(g, start); err != nil {
		return nil, err
	}

	r = &grammar{ebnf: g, followSets: map[*ebnf.Production]followSet{}}
	for nm, p := range r.ebnf {
		if token.IsExported(nm) {
			r.followSets[p] = r.closure(p.Expr)
		}
	}
	return r, nil
}

func (g *grammar) closure(e ebnf.Expression) (r followSet) {
	// trc("%T = %s", e, ebnfString(e))
	// defer func() { trc("%s -> %q", ebnfString(e), r) }()
	switch x := e.(type) {
	case ebnf.Sequence:
		r = followSet{}
		for _, v := range x {
			s := g.closure(v)
			for k, v := range s {
				r[k] = v
			}
			if _, ok := s[epsilon]; !ok {
				delete(r, epsilon)
				break
			}
		}
		return r
	case *ebnf.Name:
		nm := x.String
		p := g.ebnf[nm]
		if r = g.followSets[p]; r != nil {
			return r
		}

		if e := g.ebnf[nm].Expr; e != nil {
			r = followSet{}
			g.followSets[p] = r
			for k := range g.closure(e) {
				r[k] = struct{}{}
			}
			return r
		}

		r = followSet{toks[nm]: struct{}{}}
		g.followSets[p] = r
		return r
	case ebnf.Alternative:
		r = followSet{}
		for _, v := range x {
			for k, v := range g.closure(v) {
				r[k] = v
			}
		}
		return r
	case *ebnf.Token:
		return followSet{toks[x.String]: struct{}{}}
	case *ebnf.Group:
		return g.closure(x.Body)
	case *ebnf.Option:
		r = followSet{}
		for k, v := range g.closure(x.Body) {
			r[k] = v
		}
		r[epsilon] = struct{}{}
		return r
	default:
		panic(todo("%T", x))
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
	g    *grammar
	path string
	toks []tok

	budget   int
	maxIndex int
	indentN  int

	trcPEG bool
}

func newParser(g *grammar, path string, src []byte, trcPEG bool) (r *parser, err error) {
	r = &parser{
		budget: 1e7,
		g:      g,
		path:   path,
		trcPEG: trcPEG,
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

func (p *parser) indent() (r string) {
	p.indentN++
	return strings.Repeat("· ", p.indentN-1)
}

func (p *parser) undent() string {
	p.indentN--
	return strings.Repeat("· ", p.indentN)
}

func (p *parser) tok(s string) token.Token {
	if r, ok := toks[s]; ok {
		return r
	}

	panic(todo("%q", s))
}

func (p *parser) at(ix int) { p.maxIndex = mathutil.Max(p.maxIndex, ix) }

func (p *parser) c(ix int) tok {
	if p.budget == 0 {
		return p.toks[len(p.toks)-1]
	}

	p.budget--
	return p.toks[ix]
}

func (p *parser) parse(start string) error {
	ix, ok := p.parseExpression(0, p.g.ebnf[start].Expr)
	if p.budget == 0 {
		return errorf("%s: resources exhausted", p.path)
	}

	if !ok || ix < len(p.toks)-1 {
		return errorf("%s: syntax error", p.errPosition())
	}

	return nil
}

func (p *parser) errPosition() token.Position { return p.f.PositionFor(p.toks[p.maxIndex].pos, true) }

func (p *parser) parseExpression(ix int, e ebnf.Expression) (r int, ok bool) {
	r = ix
	if p.trcPEG {
		trc("-> %s%s[%d]: tok {%v}, = %s", p.indent(), p.f.PositionFor(p.c(ix).pos, true), ix, p.c(ix), ebnfString(e))
		defer func(ix int) {
			s := "reject"
			if ok {
				s = "ACCEPT"
			}
			trc("<- %s%s[%d]: tok {%v}, = %s (%v, %s)", p.undent(), p.f.PositionFor(p.c(ix).pos, true), ix, p.c(ix), ebnfString(e), r, s)
		}(ix)
	}
	p.at(ix)
	switch x := e.(type) {
	case ebnf.Sequence:
		for _, v := range x {
			if ix, ok = p.parseExpression(ix, v); !ok {
				return r, false
			}
		}
		return ix, true
	case *ebnf.Name:
		switch nm := x.String; {
		case token.IsExported(nm):
			pr := p.g.ebnf[nm]
			fs := p.g.followSets[pr]
			if _, ok := fs[p.c(ix).tok]; ok {
				return p.parseExpression(ix, pr.Expr)
			}

			_, epsilon := fs[epsilon]
			if epsilon {
				return ix, true
			}

			return ix, false
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
			if ix, ok = p.parseExpression(ix, x.Body); !ok {
				return ix, true
			}
		}
	case *ebnf.Group:
		return p.parseExpression(ix, x.Body)
	case ebnf.Alternative:
		for _, v := range x {
			if ix, ok := p.parseExpression(ix, v); ok {
				return ix, true
			}
		}
		return r, false
	case *ebnf.Option:
		ix, _ = p.parseExpression(ix, x.Body)
		return ix, true
	default:
		panic(todo("%T", x))
	}
}
