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
	"strconv"
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

func unexport(s string) (r string) {
	switch r = strings.ToLower(s[:1]) + s[1:]; r {
	case "type":
		return "type1"
	default:
		return r
	}
}

func extractPos(s string) (p token.Position, ok bool) {
	var prefix string
	if len(s) > 1 && s[1] == ':' { // c:\foo
		prefix = s[:2]
		s = s[2:]
	}
	// "testdata/parser/bug/001.c:1193: ..."
	a := strings.Split(s, ":")
	// ["testdata/parser/bug/001.c" "1193" "..."]
	if len(a) < 2 {
		return p, false
	}

	line, err := strconv.Atoi(a[1])
	if err != nil {
		return p, false
	}

	col, err := strconv.Atoi(a[2])
	if err != nil {
		col = 1
	}

	return token.Position{Filename: prefix + a[0], Line: line, Column: col}, true
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

type closure map[token.Token]struct{}

func (c closure) caseStr() string {
	var a []string
	var e string
	for k := range c {
		switch k {
		case epsilon:
			e = " /* ε */"
		default:
			a = append(a, tokSource(k))
		}
	}
	sort.Strings(a)
	return strings.Join(a, ", ") + e
}

func (c closure) hasEpsilon() (r bool) { _, r = c[epsilon]; return r }

func (c *closure) add(t token.Token) closure {
	m := *c
	if m == nil {
		m = map[token.Token]struct{}{t: {}}
		*c = m
	}
	m[t] = struct{}{}
	return m
}

func (c *closure) union(d closure) closure {
	m := *c
	if d == nil {
		return m
	}

	if m == nil {
		m = map[token.Token]struct{}{}
		*c = m
	}
	for k, v := range d {
		m[k] = v
	}
	return m
}

type grammar struct {
	exprClosures    map[string]closure
	g               ebnf.Grammar
	leftRecursive   map[string]struct{}
	productClosures map[*ebnf.Production]closure
}

func newGrammar(name, start string, src []byte) (r *grammar, err error) {
	g, err := ebnf.Parse(name, bytes.NewBuffer(src))
	if err != nil {
		return nil, err
	}

	if err = ebnf.Verify(g, start); err != nil {
		return nil, err
	}

	r = &grammar{
		exprClosures:    map[string]closure{},
		g:               g,
		leftRecursive:   map[string]struct{}{},
		productClosures: map[*ebnf.Production]closure{},
	}
	for nm, p := range r.g {
		if token.IsExported(nm) {
			var c closure
			e := p.Expr
			switch {
			case e == nil:
				c.add(epsilon)
			default:
				c = r.closure0(nm, e, map[string]struct{}{})
			}
			r.exprClosures[ebnfString(p.Expr)] = c
			r.productClosures[p] = c
		}
	}
	return r, nil
}

func (p *grammar) tok(s string) token.Token {
	if r, ok := toks[s]; ok {
		return r
	}

	panic(todo("%q", s))
}

func (g *grammar) exprClosure(e ebnf.Expression) (r closure) {
	if e == nil {
		panic(todo(""))
	}

	k := ebnfString(e)
	if r, ok := g.exprClosures[k]; ok {
		return r
	}

	r = g.closure0("", e, map[string]struct{}{})
	g.exprClosures[k] = r
	return r
}

func (g *grammar) closure0(prod string, e ebnf.Expression, m map[string]struct{}) (r closure) {
	if e == nil {
		panic(todo(""))
	}

	k := ebnfString(e)
	if _, ok := m[k]; ok {
		return nil
	}

	m[k] = struct{}{}
	r = closure{}
	switch x := e.(type) {
	case ebnf.Alternative:
		r = closure{}
		for _, v := range x {
			r.union(g.closure0(prod, v, m))
		}
		return r
	case *ebnf.Group:
		return g.closure0(prod, x.Body, m)
	case *ebnf.Name:
		nm := x.String
		if nm == prod {
			g.leftRecursive[nm] = struct{}{}
		}
		p := g.g[nm]
		e := p.Expr
		if token.IsExported(nm) {
			if e == nil {
				return r.add(epsilon)
			}

			return r.union(g.closure0(prod, e, m))
		}

		if x, ok := toks[nm]; ok {
			r.add(x)
		}
		return r
	case *ebnf.Option:
		r.add(epsilon)
		return r.union(g.closure0(prod, x.Body, m))
	case *ebnf.Repetition:
		r.add(epsilon)
		return r.union(g.closure0(prod, x.Body, m))
	case ebnf.Sequence:
		for _, v := range x {
			s := g.closure0(prod, v, m)
			r.union(s)
			if _, ok := s[epsilon]; !ok {
				delete(r, epsilon)
				break
			}
		}
		return r
	case *ebnf.Token:
		return r.add(g.tok(x.String))
	}
	panic(todo("%q %T %s", prod, e, k))
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

//TODO- func tokString(t token.Token) string {
//TODO- 	if t == epsilon {
//TODO- 		return "ε"
//TODO- 	}
//TODO-
//TODO- 	s := t.String()
//TODO- 	if len(s) == 1 {
//TODO- 		return fmt.Sprintf("'%c'", s[0])
//TODO- 	}
//TODO-
//TODO- 	switch t {
//TODO- 	case SHL:
//TODO- 		return "SHL"
//TODO- 	case SHR:
//TODO- 		return "SHR"
//TODO- 	case AND_NOT:
//TODO- 		return "AND_NOT"
//TODO- 	case ADD_ASSIGN:
//TODO- 		return "ADD_ASSIGN"
//TODO- 	case SUB_ASSIGN:
//TODO- 		return "SUB_ASSIGN"
//TODO- 	case MUL_ASSIGN:
//TODO- 		return "MUL_ASSIGN"
//TODO- 	case QUO_ASSIGN:
//TODO- 		return "QUO_ASSIGN"
//TODO- 	case REM_ASSIGN:
//TODO- 		return "REM_ASSIGN"
//TODO- 	case AND_ASSIGN:
//TODO- 		return "AND_ASSIGN"
//TODO- 	case OR_ASSIGN:
//TODO- 		return "OR_ASSIGN"
//TODO- 	case XOR_ASSIGN:
//TODO- 		return "XOR_ASSIGN"
//TODO- 	case SHL_ASSIGN:
//TODO- 		return "SHL_ASSIGN"
//TODO- 	case SHR_ASSIGN:
//TODO- 		return "SHR_ASSIGN"
//TODO- 	case AND_NOT_ASSIGN:
//TODO- 		return "AND_NOT_ASSIGN"
//TODO- 	case LAND:
//TODO- 		return "LAND"
//TODO- 	case LOR:
//TODO- 		return "LOR"
//TODO- 	case ARROW:
//TODO- 		return "ARROW"
//TODO- 	case INC:
//TODO- 		return "INC"
//TODO- 	case DEC:
//TODO- 		return "DEC"
//TODO- 	case EQL:
//TODO- 		return "EQL"
//TODO- 	case NEQ:
//TODO- 		return "NEQ"
//TODO- 	case LEQ:
//TODO- 		return "LEQ"
//TODO- 	case GEQ:
//TODO- 		return "GEQ"
//TODO- 	case DEFINE:
//TODO- 		return "DEFINE"
//TODO- 	case ELLIPSIS:
//TODO- 		return "ELLIPSIS"
//TODO- 	default:
//TODO- 		return strings.ToUpper(s)
//TODO- 	}
//TODO- }

type ebnfParser struct {
	f    *token.File
	g    *grammar
	path string
	toks []tok

	budget   int
	indentN  int
	index    int
	maxIndex int

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

func (p *ebnfParser) c() tok {
	if p.budget == 0 {
		return p.toks[len(p.toks)-1]
	}

	p.budget--
	p.maxIndex = mathutil.Max(p.maxIndex, p.index)
	return p.toks[p.index]
}

func (p *ebnfParser) accept(b bool) bool {
	if b {
		p.index++
	}
	return b
}

func (p *ebnfParser) parse(start string) error {
	ok := p.parseExpression(p.g.g[start].Expr)
	if p.budget == 0 {
		return errorf("%s: resources exhausted", p.path)
	}

	if !ok || p.index < len(p.toks)-1 {
		return errorf("%s: syntax error", p.errPosition())
	}

	return nil
}

func (p *ebnfParser) errPosition() token.Position {
	return p.f.PositionFor(p.toks[p.maxIndex].pos, true)
}

func (p *ebnfParser) parseExpression(e ebnf.Expression) bool {
	index0 := p.index
out:
	switch x := e.(type) {
	case ebnf.Sequence:
		for _, v := range x {
			if !p.parseExpression(v) {
				break out
			}
		}
		return true
	case *ebnf.Name:
		switch nm := x.String; {
		case token.IsExported(nm):
			pr := p.g.g[nm]
			fs := p.g.productClosures[pr]
			if _, ok := fs[p.c().tok]; ok {
				if p.parseExpression(pr.Expr) {
					return true
				}

				break out
			}

			if fs.hasEpsilon() {
				return true
			}
		default:
			if p.accept(p.c().tok == p.tok(x.String)) {
				return true
			}
		}
	case *ebnf.Token:
		if p.accept(p.c().tok == p.tok(x.String)) {
			return true
		}
	case *ebnf.Repetition:
		for {
			if !p.parseExpression(x.Body) {
				return true
			}
		}
	case *ebnf.Group:
		if p.parseExpression(x.Body) {
			return true
		}
	case ebnf.Alternative:
		for _, v := range x {
			if p.parseExpression(v) {
				return true
			}
		}
	case *ebnf.Option:
		p.parseExpression(x.Body)
		return true
	default:
		panic(todo("%T", x))
	}
	p.index = index0
	return false
}

type parser struct {
	f    *token.File
	path string
	toks []tok

	budget int
	ix     int
	maxIx  int

	closed bool
}

func newParser(path string, src []byte) (r *parser, err error) {
	r = &parser{
		budget: 1e7,
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

		if t == EOF {
			return r, nil
		}
	}
}

func (p *parser) errPosition() token.Position {
	return p.f.PositionFor(p.toks[p.maxIx].pos, true)
}

func (p *parser) c() tok {
	if p.budget <= 0 {
		return p.toks[len(p.toks)-1]
	}

	p.maxIx = mathutil.Max(p.maxIx, p.ix)
	return p.toks[p.ix]
}

func (p *parser) back(ix int) { p.ix = ix }

func (p *parser) parse() (err error) {
	ast := p.sourceFile()
	if p.budget == 0 {
		return errorf("%s: resources exhausted", p.path)
	}

	if ast == nil || p.ix < len(p.toks)-1 {
		return errorf("%s: syntax error", p.errPosition())
	}

	return nil
}
