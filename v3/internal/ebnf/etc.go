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

// The list of tokens.
const (
	// Special tokens
	ILLEGAL = token.ILLEGAL
	EOF     = token.EOF
	COMMENT = token.COMMENT

	// Identifiers and basic type literals
	// (these tokens stand for classes of literals)
	IDENT  = token.IDENT  // main
	INT    = token.INT    // 12345
	FLOAT  = token.FLOAT  // 123.45
	IMAG   = token.IMAG   // 123.45i
	CHAR   = token.CHAR   // 'a'
	STRING = token.STRING // "abc"

	// Operators and delimiters
	ADD = token.ADD // +
	SUB = token.SUB // -
	MUL = token.MUL // *
	QUO = token.QUO // /
	REM = token.REM // %

	AND     = token.AND     // &
	OR      = token.OR      // |
	XOR     = token.XOR     // ^
	SHL     = token.SHL     // <<
	SHR     = token.SHR     // >>
	AND_NOT = token.AND_NOT // &^

	ADD_ASSIGN = token.ADD_ASSIGN // +=
	SUB_ASSIGN = token.SUB_ASSIGN // -=
	MUL_ASSIGN = token.MUL_ASSIGN // *=
	QUO_ASSIGN = token.QUO_ASSIGN // /=
	REM_ASSIGN = token.REM_ASSIGN // %=

	AND_ASSIGN     = token.AND_ASSIGN     // &=
	OR_ASSIGN      = token.OR_ASSIGN      // |=
	XOR_ASSIGN     = token.XOR_ASSIGN     // ^=
	SHL_ASSIGN     = token.SHL_ASSIGN     // <<=
	SHR_ASSIGN     = token.SHR_ASSIGN     // >>=
	AND_NOT_ASSIGN = token.AND_NOT_ASSIGN // &^=

	LAND  = token.LAND  // &&
	LOR   = token.LOR   // ||
	ARROW = token.ARROW // <-
	INC   = token.INC   // ++
	DEC   = token.DEC   // --

	EQL    = token.EQL    // ==
	LSS    = token.LSS    // <
	GTR    = token.GTR    // >
	ASSIGN = token.ASSIGN // =
	NOT    = token.NOT    // !

	NEQ      = token.NEQ      // !=
	LEQ      = token.LEQ      // <=
	GEQ      = token.GEQ      // >=
	DEFINE   = token.DEFINE   // :=
	ELLIPSIS = token.ELLIPSIS // ...

	LPAREN = token.LPAREN // (
	LBRACK = token.LBRACK // [
	LBRACE = token.LBRACE // {
	COMMA  = token.COMMA  // ,
	PERIOD = token.PERIOD // .

	RPAREN    = token.RPAREN    // )
	RBRACK    = token.RBRACK    // ]
	RBRACE    = token.RBRACE    // }
	SEMICOLON = token.SEMICOLON // ;
	COLON     = token.COLON     // :

	// Keywords
	BREAK    = token.BREAK
	CASE     = token.CASE
	CHAN     = token.CHAN
	CONST    = token.CONST
	CONTINUE = token.CONTINUE

	DEFAULT     = token.DEFAULT
	DEFER       = token.DEFER
	ELSE        = token.ELSE
	FALLTHROUGH = token.FALLTHROUGH
	FOR         = token.FOR

	FUNC   = token.FUNC
	GO     = token.GO
	GOTO   = token.GOTO
	IF     = token.IF
	IMPORT = token.IMPORT

	INTERFACE = token.INTERFACE
	MAP       = token.MAP
	PACKAGE   = token.PACKAGE
	RANGE     = token.RANGE
	RETURN    = token.RETURN

	SELECT = token.SELECT
	STRUCT = token.STRUCT
	SWITCH = token.SWITCH
	TYPE   = token.TYPE
	VAR    = token.VAR

	// additional tokens, handled in an ad-hoc manner
	TILDE = token.TILDE
)

var (
	trcTODOs       bool
	extendedErrors = true

	toks = map[string]token.Token{
		"!":             NOT,
		"!=":            NEQ,
		"%":             REM,
		"%=":            REM_ASSIGN,
		"&":             AND,
		"&&":            LAND,
		"&=":            AND_ASSIGN,
		"&^":            AND_NOT,
		"&^=":           AND_NOT_ASSIGN,
		"(":             LPAREN,
		")":             RPAREN,
		"*":             MUL,
		"*=":            MUL_ASSIGN,
		"+":             ADD,
		"++":            INC,
		"+=":            ADD_ASSIGN,
		",":             COMMA,
		"-":             SUB,
		"--":            DEC,
		"-=":            SUB_ASSIGN,
		".":             PERIOD,
		"...":           ELLIPSIS,
		"/":             QUO,
		"/=":            QUO_ASSIGN,
		":":             COLON,
		":=":            DEFINE,
		";":             SEMICOLON,
		"<":             LSS,
		"<-":            ARROW,
		"<<":            SHL,
		"<<=":           SHL_ASSIGN,
		"<=":            LEQ,
		"=":             ASSIGN,
		"==":            EQL,
		">":             GTR,
		">=":            GEQ,
		">>":            SHR,
		">>=":           SHR_ASSIGN,
		"[":             LBRACK,
		"]":             RBRACK,
		"^":             XOR,
		"^=":            XOR_ASSIGN,
		"break":         BREAK,
		"case":          CASE,
		"chan":          CHAN,
		"const":         CONST,
		"continue":      CONTINUE,
		"default":       DEFAULT,
		"defer":         DEFER,
		"else":          ELSE,
		"fallthrough":   FALLTHROUGH,
		"float_lit":     FLOAT,
		"for":           FOR,
		"func":          FUNC,
		"go":            GO,
		"goto":          GOTO,
		"identifier":    IDENT,
		"if":            IF,
		"imaginary_lit": IMAG,
		"import":        IMPORT,
		"int_lit":       INT,
		"interface":     INTERFACE,
		"map":           MAP,
		"package":       PACKAGE,
		"range":         RANGE,
		"return":        RETURN,
		"rune_lit":      CHAR,
		"select":        SELECT,
		"string_lit":    STRING,
		"struct":        STRUCT,
		"switch":        SWITCH,
		"type":          TYPE,
		"var":           VAR,
		"{":             LBRACE,
		"|":             OR,
		"|=":            OR_ASSIGN,
		"||":            LOR,
		"}":             RBRACE,
		"~":             TILDE,
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
	wg      sync.WaitGroup
	minPath string

	minToks int
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

func (p *parallel) min(path string, toks int) {
	p.Lock()
	defer p.Unlock()

	if p.minToks == 0 || toks < p.minToks {
		p.minToks = toks
		p.minPath = path
	}
}

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
	n := map[*ebnf.Production]int{p: 1}
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
			defer func() { m[p] = sv; n[p] = 1 }()

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

			if n[p] != 0 {
				return pos + 1
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
	g          ebnf.Grammar
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

	r = &grammar{g: g, followSets: map[*ebnf.Production]followSet{}}
	for nm, p := range r.g {
		if token.IsExported(nm) {
			r.followSets[p] = r.closure(p.Expr)
		}
	}
	return r, nil
}

func (g *grammar) closure(e ebnf.Expression) (r followSet) {
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
		p := g.g[nm]
		if r = g.followSets[p]; r != nil {
			return r
		}

		if e := g.g[nm].Expr; e != nil {
			r = followSet{}
			g.followSets[p] = r
			for k := range g.closure(e) {
				r[k] = struct{}{}
			}
			return r
		}

		if token.IsExported(nm) {
			return followSet{epsilon: {}}
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
	case *ebnf.Repetition:
		r = followSet{}
		for k, v := range g.closure(x.Body) {
			r[k] = v
		}
		r[epsilon] = struct{}{}
		return r
	case nil:
		return followSet{epsilon: {}}
	default:
		panic(todo("%T %s", x, ebnfString(e)))
	}
}

type tok struct {
	pos token.Pos
	tok token.Token
	lit string
}

func (t tok) String() string {
	lit := t.lit
	if lit == "" {
		lit = fmt.Sprint(t.tok)
	}
	return fmt.Sprintf("%d: %v %q", t.pos, t.tok, lit)
}

func pos(p token.Position) token.Position {
	p.Filename = ""
	return p
}

func tokSource(t token.Token) string {
	if t == epsilon {
		return "ε"
	}

	switch t {
	case ILLEGAL:
		return "ILLEGAL"
	case EOF:
		return "EOF"
	case COMMENT:
		return "COMMENT"
	case IDENT:
		return "IDENT"
	case INT:
		return "INT"
	case FLOAT:
		return "FLOAT"
	case IMAG:
		return "IMAG"
	case CHAR:
		return "CHAR"
	case STRING:
		return "STRING"
	case ADD:
		return "ADD"
	case SUB:
		return "SUB"
	case MUL:
		return "MUL"
	case QUO:
		return "QUO"
	case REM:
		return "REM"
	case AND:
		return "AND"
	case OR:
		return "OR"
	case XOR:
		return "XOR"
	case SHL:
		return "SHL"
	case SHR:
		return "SHR"
	case AND_NOT:
		return "AND_NOT"
	case ADD_ASSIGN:
		return "ADD_ASSIGN"
	case SUB_ASSIGN:
		return "SUB_ASSIGN"
	case MUL_ASSIGN:
		return "MUL_ASSIGN"
	case QUO_ASSIGN:
		return "QUO_ASSIGN"
	case REM_ASSIGN:
		return "REM_ASSIGN"
	case AND_ASSIGN:
		return "AND_ASSIGN"
	case OR_ASSIGN:
		return "OR_ASSIGN"
	case XOR_ASSIGN:
		return "XOR_ASSIGN"
	case SHL_ASSIGN:
		return "SHL_ASSIGN"
	case SHR_ASSIGN:
		return "SHR_ASSIGN"
	case AND_NOT_ASSIGN:
		return "AND_NOT_ASSIGN"
	case LAND:
		return "LAND"
	case LOR:
		return "LOR"
	case ARROW:
		return "ARROW"
	case INC:
		return "INC"
	case DEC:
		return "DEC"
	case EQL:
		return "EQL"
	case LSS:
		return "LSS"
	case GTR:
		return "GTR"
	case ASSIGN:
		return "ASSIGN"
	case NOT:
		return "NOT"
	case NEQ:
		return "NEQ"
	case LEQ:
		return "LEQ"
	case GEQ:
		return "GEQ"
	case DEFINE:
		return "DEFINE"
	case ELLIPSIS:
		return "ELLIPSIS"
	case LPAREN:
		return "LPAREN"
	case LBRACK:
		return "LBRACK"
	case LBRACE:
		return "LBRACE"
	case COMMA:
		return "COMMA"
	case PERIOD:
		return "PERIOD"
	case RPAREN:
		return "RPAREN"
	case RBRACK:
		return "RBRACK"
	case RBRACE:
		return "RBRACE"
	case SEMICOLON:
		return "SEMICOLON"
	case COLON:
		return "COLON"
	case BREAK:
		return "BREAK"
	case CASE:
		return "CASE"
	case CHAN:
		return "CHAN"
	case CONST:
		return "CONST"
	case CONTINUE:
		return "CONTINUE"
	case DEFAULT:
		return "DEFAULT"
	case DEFER:
		return "DEFER"
	case ELSE:
		return "ELSE"
	case FALLTHROUGH:
		return "FALLTHROUGH"
	case FOR:
		return "FOR"
	case FUNC:
		return "FUNC"
	case GO:
		return "GO"
	case GOTO:
		return "GOTO"
	case IF:
		return "IF"
	case IMPORT:
		return "IMPORT"
	case INTERFACE:
		return "INTERFACE"
	case MAP:
		return "MAP"
	case PACKAGE:
		return "PACKAGE"
	case RANGE:
		return "RANGE"
	case RETURN:
		return "RETURN"
	case SELECT:
		return "SELECT"
	case STRUCT:
		return "STRUCT"
	case SWITCH:
		return "SWITCH"
	case TYPE:
		return "TYPE"
	case VAR:
		return "VAR"
	case TILDE:
		return "TILDE"
	default:
		panic(todo("", int(t), t))
	}
}

func tokString(t token.Token) string {
	if t == epsilon {
		return "ε"
	}

	s := t.String()
	if len(s) == 1 {
		return fmt.Sprintf("'%c'", s[0])
	}

	switch t {
	case SHL:
		return "SHL"
	case SHR:
		return "SHR"
	case AND_NOT:
		return "AND_NOT"
	case ADD_ASSIGN:
		return "ADD_ASSIGN"
	case SUB_ASSIGN:
		return "SUB_ASSIGN"
	case MUL_ASSIGN:
		return "MUL_ASSIGN"
	case QUO_ASSIGN:
		return "QUO_ASSIGN"
	case REM_ASSIGN:
		return "REM_ASSIGN"
	case AND_ASSIGN:
		return "AND_ASSIGN"
	case OR_ASSIGN:
		return "OR_ASSIGN"
	case XOR_ASSIGN:
		return "XOR_ASSIGN"
	case SHL_ASSIGN:
		return "SHL_ASSIGN"
	case SHR_ASSIGN:
		return "SHR_ASSIGN"
	case AND_NOT_ASSIGN:
		return "AND_NOT_ASSIGN"
	case LAND:
		return "LAND"
	case LOR:
		return "LOR"
	case ARROW:
		return "ARROW"
	case INC:
		return "INC"
	case DEC:
		return "DEC"
	case EQL:
		return "EQL"
	case NEQ:
		return "NEQ"
	case LEQ:
		return "LEQ"
	case GEQ:
		return "GEQ"
	case DEFINE:
		return "DEFINE"
	case ELLIPSIS:
		return "ELLIPSIS"
	default:
		return strings.ToUpper(s)
	}
}

type ebnfParser struct {
	f    *token.File
	g    *grammar
	path string
	toks []tok

	budget   int
	maxIndex int
	indentN  int

	trcPEG bool
}

func newEBNFParser(g *grammar, path string, src []byte, trcPEG bool) (r *ebnfParser, err error) {
	r = &ebnfParser{
		budget: 3e7,
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

func (p *ebnfParser) indent() (r string) {
	p.indentN++
	return strings.Repeat("· ", p.indentN-1)
}

func (p *ebnfParser) undent() string {
	p.indentN--
	return strings.Repeat("· ", p.indentN)
}

func (p *ebnfParser) tok(s string) token.Token {
	if r, ok := toks[s]; ok {
		return r
	}

	panic(todo("%q", s))
}

func (p *ebnfParser) c(ix int) tok {
	if p.budget == 0 {
		return p.toks[len(p.toks)-1]
	}

	p.budget--
	return p.toks[ix]
}

func (p *ebnfParser) parse(start string) error {
	ix, ok := p.parseExpression(0, p.g.g[start].Expr)
	if p.budget == 0 {
		return errorf("%s: resources exhausted", p.path)
	}

	if !ok || ix < len(p.toks)-1 {
		return errorf("%s: syntax error", p.errPosition())
	}

	return nil
}

func (p *ebnfParser) errPosition() token.Position {
	return p.f.PositionFor(p.toks[p.maxIndex].pos, true)
}

func (p *ebnfParser) parseExpression(ix int, e ebnf.Expression) (r int, ok bool) {
	var nm string
	r = ix

	defer func() {
		p.maxIndex = mathutil.Max(p.maxIndex, ix)
	}()

	if p.trcPEG {
		p.indent()
		defer func(ix int) {
			if nm == "" {
				nm = ebnfString(e)
			}
			switch {
			case ok:
				trc("%s ACCEPTED %s at %s: - %s:", p.undent(), nm, pos(p.f.PositionFor(p.c(ix).pos, true)), pos(p.f.PositionFor(p.c(r).pos, true)))
			default:
				trc("%s REJECTED %s at %s[%d]: tok {%v}", p.undent(), nm, p.f.PositionFor(p.c(ix).pos, true), ix, p.c(ix))
			}
		}(ix)
	}
	switch x := e.(type) {
	case ebnf.Sequence:
		for _, v := range x {
			if ix, ok = p.parseExpression(ix, v); !ok {
				return r, false
			}
		}
		return ix, true
	case *ebnf.Name:
		switch nm = x.String; {
		case token.IsExported(nm):
			pr := p.g.g[nm]
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
