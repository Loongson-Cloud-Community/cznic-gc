// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//                                                                                       .  .  .
// BenchmarkParser-24      	       1	7727122462 ns/op	   9.08 MB/s	3227554712 B/op	47814969 allocs/op
// BenchmarkParser-24      	       1	7649451914 ns/op	   9.17 MB/s	3220208496 B/op	47814834 allocs/op
// BenchmarkParser-24      	       1	7546737165 ns/op	   9.30 MB/s	3215958992 B/op	47815030 allocs/op
// BenchmarkParser-24      	       1	7709228538 ns/op	   9.10 MB/s	3202184600 B/op	47814949 allocs/op
// BenchmarkParser-24      	       1	7410148019 ns/op	   9.47 MB/s	2843086096 B/op	47814548 allocs/op
// BenchmarkParser-24      	       1	7387526674 ns/op	   9.50 MB/s	2883234888 B/op	47814722 allocs/op
// BenchmarkParser-24      	       1	7335972425 ns/op	   9.57 MB/s	2825838144 B/op	44301449 allocs/op
// BenchmarkParser-24      	       1	6812696326 ns/op	  10.30 MB/s	2711099616 B/op	40692905 allocs/op
// BenchmarkParser-24      	       1	6539422838 ns/op	  10.73 MB/s	2605576712 B/op	37316600 allocs/op
// BenchmarkParser-24      	       1	6241464684 ns/op	  11.24 MB/s	2497851048 B/op	33960264 allocs/op
// BenchmarkParser-24      	       1	5901301945 ns/op	  11.89 MB/s	2337475008 B/op	30617910 allocs/op
// BenchmarkParser-24      	       1	5628186104 ns/op	  12.47 MB/s	2220355032 B/op	26966747 allocs/op
// BenchmarkParser-24      	       1	5645053005 ns/op	  12.43 MB/s	2195270208 B/op	26966953 allocs/op
// BenchmarkParser-24      	       1	5361272584 ns/op	  13.09 MB/s	2122389744 B/op	26966665 allocs/op
// BenchmarkParser-24      	       1	5402511361 ns/op	  12.99 MB/s	2138109528 B/op	25629078 allocs/op
// BenchmarkParser-24      	       1	5278073632 ns/op	  13.30 MB/s	2057989584 B/op	24870758 allocs/op
// BenchmarkParser-24      	       1	5097343024 ns/op	  13.77 MB/s	1987967968 B/op	23533027 allocs/op
// BenchmarkParser-24      	       1	5046280503 ns/op	  13.91 MB/s	1821836280 B/op	23532757 allocs/op
// BenchmarkParser-24      	       1	4868115159 ns/op	  14.41 MB/s	1798902776 B/op	23532845 allocs/op
// BenchmarkParser-24      	       1	4866396896 ns/op	  14.42 MB/s	1740396504 B/op	23532596 allocs/op
// BenchmarkParser-24      	       1	4559016831 ns/op	  15.39 MB/s	1594812888 B/op	20503295 allocs/op
// BenchmarkParser-24      	       1	4541016048 ns/op	  15.45 MB/s	1599650392 B/op	20531178 allocs/op	// added lexical scopes
// BenchmarkParser-24      	       1	4647439202 ns/op	  15.10 MB/s	1631258688 B/op	21010589 allocs/op	// declare consts
// BenchmarkParser-24      	       1	4667995357 ns/op	  15.03 MB/s	1638076720 B/op	21045826 allocs/op	// declare vars
// BenchmarkParser-24      	       1	4706816502 ns/op	  14.91 MB/s	1644401384 B/op	21059327 allocs/op	// declare functions
// BenchmarkParser-24      	       1	4742361793 ns/op	  14.80 MB/s	1679761352 B/op	21415726 allocs/op	// short var declarations
// BenchmarkParser-24      	       1	4821348660 ns/op	  14.55 MB/s	1681612136 B/op	21399555 allocs/op	// declare types

// e5-1650
// BenchmarkParser-12      	       1	5822301277 ns/op	  12.05 MB/s	1643143008 B/op	21388153 allocs/op	// declare consts
// BenchmarkParser-12      	       1	4445553964 ns/op	  15.78 MB/s	1648156648 B/op	21356971 allocs/op	// fix element list
// BenchmarkParser-12      	       1	4489738277 ns/op	  15.63 MB/s	1648484144 B/op	21353119 allocs/op	// fix param decl list
// BenchmarkParser-12      	       1	6033080321 ns/op	  11.63 MB/s	1658127928 B/op	21420972 allocs/op	// declare functions
// BenchmarkParser-12      	       1	5056769691 ns/op	  13.88 MB/s	1661280392 B/op	21441235 allocs/op	// declare types
// BenchmarkParser-12      	       1	4356147083 ns/op	  16.11 MB/s	1629649256 B/op	21256883 allocs/op
// BenchmarkParser-12      	       1	6151509861 ns/op	  11.41 MB/s	1674344960 B/op	21761297 allocs/op	// short var declarations

package gc // modernc.org/gc/v3

import (
	"go/token"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strings"

	"modernc.org/mathutil"
	"modernc.org/strutil"
)

const parserBudget = 1e7

var (
	_ Expression = (*BinaryExpression)(nil)

	noBack    bool
	panicBack bool
)

type scoped struct {
	n       Node
	visible int32 // first token index where n is visible
}

const (
	scZero = iota
	scUniverse
	scPackage
	scFile
	scOther
)

type Scope struct {
	nodes  map[string]scoped
	Parent *Scope

	kind int
}

func newScope(parent *Scope, kind int) *Scope { return &Scope{Parent: parent, kind: kind} }

func (s *Scope) declare(nm Token, n Node, visible int32, p *parser, initOK bool) (r Node) {
	snm := nm.Src()
	switch snm {
	case "_":
		return nil
	case "init":
		if s.kind == scPackage {
			if !initOK && p.reportDeclarationErrors {
				p.err(nm.Position(), "in the package block, the identifier init may only be used for init function declarations")
			}
			return nil
		}
	}

	if ex, ok := s.nodes[snm]; ok {
		return ex.n
	}

	if s.nodes == nil {
		s.nodes = map[string]scoped{}
	}
	// trc("%v: add %s %p", nm.Position(), snm, s)
	s.nodes[snm] = scoped{n, visible}
	return nil
}

type lexicalScoper struct{ s *Scope }

func newLexicalScoper(s *Scope) lexicalScoper { return lexicalScoper{s} }

func (n *lexicalScoper) LexicalScope() *Scope { return n.s }

// Node is an item of the CST tree.
type Node interface {
	Position() token.Position
	Source(full bool) string
}

var hooks = strutil.PrettyPrintHooks{
	reflect.TypeOf(Token{}): func(f strutil.Formatter, v interface{}, prefix, suffix string) {
		t := v.(Token)
		if !t.IsValid() {
			return
		}

		pos := t.Position()
		if pos.Filename != "" {
			pos.Filename = filepath.Base(pos.Filename)
		}
		f.Format(string(prefix)+"%10s %q %q\t(%v:)"+string(suffix), tokSource(t.Ch()), t.Sep(), t.Src(), pos)
	},
}

func dump(n Node) string { return strutil.PrettyString(n, "", "", hooks) }

// nodeSource returns the source text of n. If full is false, every non empty
// separator is replaced by a single space.
func nodeSource(n Node, full bool) string {
	var a []int32
	var t Token
	nodeSource0(&t.source, &a, n)
	if len(a) == 0 {
		return ""
	}

	var b strings.Builder
	sort.Slice(a, func(i, j int) bool { return a[i] < a[j] })
	for _, v := range a {
		t.index = v
		t.ch = t.source.toks[t.index].ch
		b.WriteString(t.Source(full))
	}
	return b.String()
}

func nodeSource0(ps **source, a *[]int32, n interface{}) {
	switch x := n.(type) {
	case nil:
		// nop
	case Token:
		if x.IsValid() {
			*ps = x.source
			*a = append(*a, x.index)
		}
	default:
		t := reflect.TypeOf(n)
		v := reflect.ValueOf(n)
		switch t.Kind() {
		case reflect.Pointer:
			if !v.IsZero() {
				nodeSource0(ps, a, v.Elem().Interface())
			}
		case reflect.Struct:
			for i := 0; i < t.NumField(); i++ {
				if token.IsExported(t.Field(i).Name) {
					nodeSource0(ps, a, v.Field(i).Interface())
				}
			}
		case reflect.Slice:
			for i := 0; i < v.Len(); i++ {
				nodeSource0(ps, a, v.Index(i).Interface())
			}
		default:
			panic(todo("", t.Kind()))
		}
	}
}

type AST struct {
	FileScope  *Scope
	SourceFile *SourceFileNode
	EOF        Token
}

func (n *AST) Position() token.Position { return n.SourceFile.Position() }
func (n *AST) Source(full bool) string  { return nodeSource(n, full) }

type parser struct {
	a             *analyzer
	fileScope     *Scope
	maxBackOrigin string
	maxBackRange  [2]int
	packageScope  *Scope
	path          string
	s             *scanner
	sc            *Scope

	backs   int
	budget  int
	ix      int
	maxBack int
	maxIx   int

	isClosed                bool
	record                  bool
	reportDeclarationErrors bool
}

func newParser(pkgScope *Scope, path string, src []byte, record bool) *parser {
	return &parser{
		a:            newAnalyzer(),
		budget:       parserBudget,
		fileScope:    newScope(pkgScope, scFile),
		packageScope: pkgScope,
		path:         path,
		record:       record,
		s:            newScanner(path, src),
		sc:           pkgScope,
	}
}

func (p *parser) c() token.Token { return p.peek(0) }
func (p *parser) closeScope() {
	p.sc = p.sc.Parent
	// trc("%v: closeScope %p (%v)", p.pos(), p.sc, origin(2))
}
func (p *parser) errPosition() token.Position { return p.s.toks[p.maxIx].position(p.s.source) }

func (p *parser) openScope() {
	p.sc = newScope(p.sc, scOther)
	// trc("%v: openScope %p (%v)", p.pos(), p.sc, origin(2))
}

func (p *parser) pos() token.Position {
	return p.s.toks[mathutil.MinInt32(int32(p.ix), int32(len(p.s.toks)-1))].position(p.s.source)
}

func (p *parser) err(pos token.Position, msg string, args ...interface{}) {
	p.s.errs.err(pos, msg, args...)
}

func (p *parser) declare(s *Scope, nm Token, n Node, visible int32, initOK bool) {
	if ex := s.declare(nm, n, visible, p, initOK); ex != nil && p.reportDeclarationErrors {
		p.err(nm.Position(), "%s redeclared, previous declaration at %v: (%p)", nm.Src(), ex.Position(), s)
	}
}

func (p *parser) consume() (r Token) {
	r = Token{p.s.source, p.s.toks[p.ix].ch, int32(p.ix)}
	p.ix++
	p.budget--
	return r
}

func (p *parser) accept(t token.Token) (r Token, _ bool) {
	if p.c() == t {
		return p.consume(), true
	}
	return r, false
}

func (p *parser) expect(t token.Token) (r Token) {
	var ok bool
	if r, ok = p.accept(t); !ok {
		p.isClosed = true
	}
	return r
}

func (p *parser) peek(n int) token.Token {
	for p.ix+n >= len(p.s.toks) {
		if p.budget <= 0 || p.isClosed {
			return EOF
		}

		p.s.scan()
		if p.s.isClosed {
			p.isClosed = true
		}
	}
	p.maxIx = mathutil.Max(p.maxIx, p.ix)
	return token.Token(p.s.toks[p.ix+n].ch)
}

func (p *parser) recordBacktrack(ix int, record bool) {
	delta := p.ix - ix
	p.backs += delta
	if delta > p.maxBack {
		p.maxBack = delta
		p.maxBackRange = [2]int{ix, p.ix}
		p.maxBackOrigin = origin(3)
	}
	p.ix = ix
	if p.record && record {
		if _, _, line, ok := runtime.Caller(2); ok {
			p.a.record(line, delta)
		}
	}
}

func (p *parser) back(ix int) {
	p.recordBacktrack(ix, true)
	if p.isClosed {
		return
	}

	if noBack {
		p.isClosed = true
	}
	if panicBack {
		panic(todo("%v: (%v:)", p.errPosition(), origin(2)))
	}
}

func (p *parser) parse() (ast *AST, err error) {
	if p.c() != PACKAGE {
		p.s.errs.err(p.errPosition(), "syntax error")
		return nil, p.s.errs
	}

	sourceFile := p.sourceFile()
	if p.budget <= 0 {
		return nil, errorf("%s: resources exhausted", p.path)
	}

	if eof, ok := p.accept(EOF); ok && p.ix == len(p.s.toks) {
		return &AST{p.fileScope, sourceFile, eof}, p.s.errs.Err()
	}

	p.s.errs.err(p.errPosition(), "syntax error")
	return nil, p.s.errs
}

type Expression interface {
	Node
	Type
	Value
}

type BinaryExpression struct {
	LHS Expression
	Op  Token
	RHS Expression
}

// Position implements Node.
func (n *BinaryExpression) Position() token.Position {
	return n.LHS.Position()
}

// Source implements Node.
func (n *BinaryExpression) Source(full bool) string { return nodeSource(n, full) }

func (p *parser) additiveExpression(preBlock bool) (r Expression) {
	var multiplicativeExpression Expression
	// ebnf.Sequence MultiplicativeExpression { ( "+" | "-" | "|" | "^" ) MultiplicativeExpression } ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name MultiplicativeExpression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if multiplicativeExpression = p.multiplicativeExpression(preBlock); multiplicativeExpression == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { ( "+" | "-" | "|" | "^" ) MultiplicativeExpression } ctx []
		r = multiplicativeExpression
	_0:
		{
			var op Token
			var multiplicativeExpression Expression
			switch p.c() {
			case ADD, OR, SUB, XOR:
				// ebnf.Sequence ( "+" | "-" | "|" | "^" ) MultiplicativeExpression ctx [ADD, OR, SUB, XOR]
				// *ebnf.Group ( "+" | "-" | "|" | "^" ) ctx [ADD, OR, SUB, XOR]
				// ebnf.Alternative "+" | "-" | "|" | "^" ctx [ADD, OR, SUB, XOR]
				op = p.consume()
				// *ebnf.Name MultiplicativeExpression ctx []
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if multiplicativeExpression = p.multiplicativeExpression(preBlock); multiplicativeExpression == nil {
						p.back(ix)
						goto _1
					}
				default:
					p.back(ix)
					goto _1
				}
				r = &BinaryExpression{r, op, multiplicativeExpression}
				goto _0
			}
		_1:
		}
	}
	return r
}

// AliasDeclNode represents the production
//
//	AliasDecl = identifier "=" Type .
type AliasDeclNode struct {
	lexicalScoper
	IDENT  Token
	ASSIGN Token
	Type   *TypeNode
}

// Source implements Node.
func (n *AliasDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *AliasDeclNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) aliasDecl() (r *AliasDeclNode) {
	var (
		identTok  Token
		assignTok Token
		typeNode  *TypeNode
	)
	// ebnf.Sequence identifier "=" Type ctx [IDENT]
	{
		if p.peek(1) != ASSIGN {
			return nil
		}
		ix := p.ix
		// *ebnf.Name identifier ctx [IDENT]
		identTok = p.expect(IDENT)
		// *ebnf.Token "=" ctx [ASSIGN]
		assignTok = p.expect(ASSIGN)
		// *ebnf.Name Type ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if typeNode = p.type1(); typeNode == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	r = &AliasDeclNode{
		lexicalScoper: newLexicalScoper(p.sc),
		IDENT:         identTok,
		ASSIGN:        assignTok,
		Type:          typeNode,
	}
	p.declare(p.sc, identTok, r, int32(p.ix), false)
	return r
}

// ArgumentsNode represents the production
//
//	Arguments = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
type ArgumentsNode struct {
	LPAREN          Token
	ExpressionList  *ExpressionListNode
	Type            *TypeNode
	COMMA           Token
	ExpressionList2 *ExpressionListNode
	ELLIPSIS        Token
	COMMA2          Token
	RPAREN          Token
}

// Source implements Node.
func (n *ArgumentsNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ArgumentsNode) Position() token.Position { return n.LPAREN.Position() }

func (p *parser) arguments() *ArgumentsNode {
	var (
		ok              bool
		lparenTok       Token
		expressionList  *ExpressionListNode
		typeNode        *TypeNode
		commaTok        Token
		expressionList2 *ExpressionListNode
		ellipsisTok     Token
		comma2Tok       Token
		rparenTok       Token
	)
	// ebnf.Sequence "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" ctx [LPAREN]
	{
		ix := p.ix
		// *ebnf.Token "(" ctx [LPAREN]
		lparenTok = p.expect(LPAREN)
		// *ebnf.Option [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// ebnf.Sequence ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Group ( ExpressionList | Type [ "," ExpressionList ] ) ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				// ebnf.Alternative ExpressionList | Type [ "," ExpressionList ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				switch p.c() {
				case ADD, AND, CHAR, FLOAT, IMAG, INT, NOT, STRING, SUB, XOR: // 0
					// *ebnf.Name ExpressionList ctx [ADD, AND, CHAR, FLOAT, IMAG, INT, NOT, STRING, SUB, XOR]
					if expressionList = p.expressionList(false); expressionList == nil {
						goto _2
					}
					break
				_2:
					expressionList = nil
					p.back(ix)
					goto _0
				case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 0 1
					// *ebnf.Name ExpressionList ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
					if expressionList = p.expressionList(false); expressionList == nil {
						goto _4
					}
					break
				_4:
					expressionList = nil
					// ebnf.Sequence Type [ "," ExpressionList ] ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
					{
						ix := p.ix
						// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
						if typeNode = p.type1(); typeNode == nil {
							p.back(ix)
							goto _5
						}
						// *ebnf.Option [ "," ExpressionList ] ctx []
						switch p.c() {
						case COMMA:
							// ebnf.Sequence "," ExpressionList ctx [COMMA]
							{
								switch p.peek(1) {
								case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
								default:
									goto _6
								}
								ix := p.ix
								// *ebnf.Token "," ctx [COMMA]
								commaTok = p.expect(COMMA)
								// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
								if expressionList2 = p.expressionList(false); expressionList2 == nil {
									p.back(ix)
									goto _6
								}
							}
						}
						goto _7
					_6:
						commaTok = Token{}
						expressionList2 = nil
					_7:
					}
					break
				_5:
					commaTok = Token{}
					expressionList2 = nil
					typeNode = nil
					p.back(ix)
					goto _0
				default:
					p.back(ix)
					goto _0
				}
				// *ebnf.Option [ "..." ] ctx []
				switch p.c() {
				case ELLIPSIS:
					// *ebnf.Token "..." ctx [ELLIPSIS]
					ellipsisTok = p.expect(ELLIPSIS)
				}
				// *ebnf.Option [ "," ] ctx []
				switch p.c() {
				case COMMA:
					// *ebnf.Token "," ctx [COMMA]
					comma2Tok = p.expect(COMMA)
				}
			}
		}
		goto _1
	_0:
		comma2Tok = Token{}
		ellipsisTok = Token{}
		expressionList = nil
	_1:
		// *ebnf.Token ")" ctx []
		if rparenTok, ok = p.accept(RPAREN); !ok {
			p.back(ix)
			return nil
		}
	}
	return &ArgumentsNode{
		LPAREN:          lparenTok,
		ExpressionList:  expressionList,
		Type:            typeNode,
		COMMA:           commaTok,
		ExpressionList2: expressionList2,
		ELLIPSIS:        ellipsisTok,
		COMMA2:          comma2Tok,
		RPAREN:          rparenTok,
	}
}

// ArrayLengthNode represents the production
//
//	ArrayLength = Expression .
type ArrayLengthNode struct {
	Expression Expression
}

// Source implements Node.
func (n *ArrayLengthNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ArrayLengthNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) arrayLength() *ArrayLengthNode {
	var (
		expression Expression
	)
	// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	if expression = p.expression(false); expression == nil {
		return nil
	}
	return &ArrayLengthNode{
		Expression: expression,
	}
}

// ArrayTypeNode represents the production
//
//	ArrayType = "[" ArrayLength "]" ElementType .
type ArrayTypeNode struct {
	LBRACK      Token
	ArrayLength *ArrayLengthNode
	RBRACK      Token
	ElementType *ElementTypeNode
}

// Source implements Node.
func (n *ArrayTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ArrayTypeNode) Position() token.Position { return n.LBRACK.Position() }

func (p *parser) arrayType() *ArrayTypeNode {
	var (
		ok          bool
		lbrackTok   Token
		arrayLength *ArrayLengthNode
		rbrackTok   Token
		elementType *ElementTypeNode
	)
	// ebnf.Sequence "[" ArrayLength "]" ElementType ctx [LBRACK]
	{
		switch p.peek(1) {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "[" ctx [LBRACK]
		lbrackTok = p.expect(LBRACK)
		// *ebnf.Name ArrayLength ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if arrayLength = p.arrayLength(); arrayLength == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "]" ctx []
		if rbrackTok, ok = p.accept(RBRACK); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ElementType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if elementType = p.elementType(); elementType == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ArrayTypeNode{
		LBRACK:      lbrackTok,
		ArrayLength: arrayLength,
		RBRACK:      rbrackTok,
		ElementType: elementType,
	}
}

// AssignmentNode represents the production
//
//	Assignment = ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" ) ExpressionList .
type AssignmentNode struct {
	Op             Token
	ExpressionList *ExpressionListNode
}

// Source implements Node.
func (n *AssignmentNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *AssignmentNode) Position() token.Position { panic("TODO") }

func (p *parser) assignment(preBlock bool) *AssignmentNode {
	var (
		op             Token
		expressionList *ExpressionListNode
	)
	// ebnf.Sequence ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" ) ExpressionList ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
	{
		ix := p.ix
		// *ebnf.Group ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" ) ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
		// ebnf.Alternative "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
		op = p.consume()
		// *ebnf.Name ExpressionList ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if expressionList = p.expressionList(preBlock); expressionList == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &AssignmentNode{
		Op:             op,
		ExpressionList: expressionList,
	}
}

// BaseTypeNode represents the production
//
//	BaseType = Type .
type BaseTypeNode struct {
	Type *TypeNode
}

// Source implements Node.
func (n *BaseTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *BaseTypeNode) Position() token.Position { return n.Type.Position() }

func (p *parser) baseType() *BaseTypeNode {
	var (
		typeNode *TypeNode
	)
	// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	if typeNode = p.type1(); typeNode == nil {
		return nil
	}
	return &BaseTypeNode{
		Type: typeNode,
	}
}

func (p *parser) basicLit() Expression {
	// ebnf.Alternative int_lit | float_lit | imaginary_lit | rune_lit | string_lit ctx [CHAR, FLOAT, IMAG, INT, STRING]
	return p.consume()
}

// BlockNode represents the production
//
//	Block = "{" StatementList "}" .
type BlockNode struct {
	LBRACE        Token
	StatementList *StatementListNode
	RBRACE        Token
}

// Source implements Node.
func (n *BlockNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *BlockNode) Position() token.Position { return n.LBRACE.Position() }

func (p *parser) block() *BlockNode {
	var (
		ok            bool
		lbraceTok     Token
		statementList *StatementListNode
		rbraceTok     Token
	)
	// ebnf.Sequence "{" StatementList "}" ctx [LBRACE]
	{
		p.openScope()

		defer p.closeScope()

		ix := p.ix
		// *ebnf.Token "{" ctx [LBRACE]
		lbraceTok = p.expect(LBRACE)
		// *ebnf.Name StatementList ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if statementList = p.statementList(); statementList == nil {
				p.back(ix)
				return nil
			}
		}
		// *ebnf.Token "}" ctx []
		if rbraceTok, ok = p.accept(RBRACE); !ok {
			p.back(ix)
			return nil
		}
	}
	return &BlockNode{
		LBRACE:        lbraceTok,
		StatementList: statementList,
		RBRACE:        rbraceTok,
	}
}

// BreakStmtNode represents the production
//
//	BreakStmt = "break" [ Label ] .
type BreakStmtNode struct {
	BREAK Token
	Label *LabelNode
}

// Source implements Node.
func (n *BreakStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *BreakStmtNode) Position() token.Position { return n.BREAK.Position() }

func (p *parser) breakStmt() *BreakStmtNode {
	var (
		breakTok Token
		label    *LabelNode
	)
	// ebnf.Sequence "break" [ Label ] ctx [BREAK]
	{
		// *ebnf.Token "break" ctx [BREAK]
		breakTok = p.expect(BREAK)
		// *ebnf.Option [ Label ] ctx []
		switch p.c() {
		case IDENT:
			// *ebnf.Name Label ctx [IDENT]
			if label = p.label(); label == nil {
				goto _0
			}
		}
		goto _1
	_0:
		label = nil
	_1:
	}
	return &BreakStmtNode{
		BREAK: breakTok,
		Label: label,
	}
}

// ChannelNode represents the production
//
//	Channel = Expression .
type ChannelNode struct {
	Expression Expression
}

// Source implements Node.
func (n *ChannelNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ChannelNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) channel() *ChannelNode {
	var (
		expression Expression
	)
	// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	if expression = p.expression(false); expression == nil {
		return nil
	}
	return &ChannelNode{
		Expression: expression,
	}
}

// ChannelTypeNode represents the production
//
//	ChannelType = ( "chan" "<-" | "chan" | "<-" "chan" ) ElementType .
type ChannelTypeNode struct {
	CHAN        Token
	ARROW       Token
	CHAN2       Token
	ARROW2      Token
	CHAN3       Token
	ElementType *ElementTypeNode
}

// Source implements Node.
func (n *ChannelTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ChannelTypeNode) Position() token.Position { panic("TODO") }

func (p *parser) channelType() *ChannelTypeNode {
	var (
		chanTok     Token
		arrowTok    Token
		chan2Tok    Token
		arrow2Tok   Token
		chan3Tok    Token
		elementType *ElementTypeNode
	)
	// ebnf.Sequence ( "chan" "<-" | "chan" | "<-" "chan" ) ElementType ctx [ARROW, CHAN]
	{
		ix := p.ix
		// *ebnf.Group ( "chan" "<-" | "chan" | "<-" "chan" ) ctx [ARROW, CHAN]
		// ebnf.Alternative "chan" "<-" | "chan" | "<-" "chan" ctx [ARROW, CHAN]
		switch p.c() {
		case CHAN: // 0 1
			// ebnf.Sequence "chan" "<-" ctx [CHAN]
			{
				if p.peek(1) != ARROW {
					goto _0
				}
				// *ebnf.Token "chan" ctx [CHAN]
				chanTok = p.expect(CHAN)
				// *ebnf.Token "<-" ctx [ARROW]
				arrowTok = p.expect(ARROW)
			}
			break
		_0:
			arrowTok = Token{}
			chanTok = Token{}
			// *ebnf.Token "chan" ctx [CHAN]
			chan2Tok = p.expect(CHAN)
			break
			p.back(ix)
			return nil
		case ARROW: // 2
			// ebnf.Sequence "<-" "chan" ctx [ARROW]
			{
				if p.peek(1) != CHAN {
					goto _2
				}
				// *ebnf.Token "<-" ctx [ARROW]
				arrow2Tok = p.expect(ARROW)
				// *ebnf.Token "chan" ctx [CHAN]
				chan3Tok = p.expect(CHAN)
			}
			break
		_2:
			arrow2Tok = Token{}
			chan3Tok = Token{}
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Name ElementType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if elementType = p.elementType(); elementType == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ChannelTypeNode{
		CHAN:        chanTok,
		ARROW:       arrowTok,
		CHAN2:       chan2Tok,
		ARROW2:      arrow2Tok,
		CHAN3:       chan3Tok,
		ElementType: elementType,
	}
}

// CommCaseNode represents the production
//
//	CommCase = "case" ( SendStmt | RecvStmt ) | "default" .
type CommCaseNode struct {
	CASE     Token
	SendStmt *SendStmtNode
	RecvStmt *RecvStmtNode
	DEFAULT  Token
}

// Source implements Node.
func (n *CommCaseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *CommCaseNode) Position() token.Position { panic("TODO") }

func (p *parser) commCase() *CommCaseNode {
	var (
		caseTok    Token
		sendStmt   *SendStmtNode
		recvStmt   *RecvStmtNode
		defaultTok Token
	)
	// ebnf.Alternative "case" ( SendStmt | RecvStmt ) | "default" ctx [CASE, DEFAULT]
	switch p.c() {
	case CASE: // 0
		// ebnf.Sequence "case" ( SendStmt | RecvStmt ) ctx [CASE]
		{
			switch p.peek(1) {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			default:
				goto _0
			}
			ix := p.ix
			// *ebnf.Token "case" ctx [CASE]
			caseTok = p.expect(CASE)
			// *ebnf.Group ( SendStmt | RecvStmt ) ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			// ebnf.Alternative SendStmt | RecvStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0 1
				// *ebnf.Name SendStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if sendStmt = p.sendStmt(); sendStmt == nil {
					goto _2
				}
				break
			_2:
				sendStmt = nil
				// *ebnf.Name RecvStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if recvStmt = p.recvStmt(); recvStmt == nil {
					goto _3
				}
				break
			_3:
				recvStmt = nil
				p.back(ix)
				goto _0
			default:
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
		caseTok = Token{}
		return nil
	case DEFAULT: // 1
		// *ebnf.Token "default" ctx [DEFAULT]
		defaultTok = p.expect(DEFAULT)
	default:
		return nil
	}
	return &CommCaseNode{
		CASE:     caseTok,
		SendStmt: sendStmt,
		RecvStmt: recvStmt,
		DEFAULT:  defaultTok,
	}
}

// CommClauseNode represents the production
//
//	CommClause = CommCase ":" StatementList .
type CommClauseNode struct {
	CommCase      *CommCaseNode
	COLON         Token
	StatementList *StatementListNode
}

// Source implements Node.
func (n *CommClauseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *CommClauseNode) Position() token.Position { return n.CommCase.Position() }

func (p *parser) commClause() *CommClauseNode {
	var (
		ok            bool
		commCase      *CommCaseNode
		colonTok      Token
		statementList *StatementListNode
	)
	// ebnf.Sequence CommCase ":" StatementList ctx [CASE, DEFAULT]
	{
		p.openScope()

		defer p.closeScope()

		ix := p.ix
		// *ebnf.Name CommCase ctx [CASE, DEFAULT]
		if commCase = p.commCase(); commCase == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token ":" ctx []
		if colonTok, ok = p.accept(COLON); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name StatementList ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if statementList = p.statementList(); statementList == nil {
				p.back(ix)
				return nil
			}
		}
	}
	return &CommClauseNode{
		CommCase:      commCase,
		COLON:         colonTok,
		StatementList: statementList,
	}
}

// CompositeLitNode represents the production
//
//	CompositeLit = LiteralType LiteralValue .
type CompositeLitNode struct {
	LiteralType  *LiteralTypeNode
	LiteralValue *LiteralValueNode
}

// Source implements Node.
func (n *CompositeLitNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *CompositeLitNode) Position() token.Position { return n.LiteralType.Position() }

func (p *parser) compositeLit() *CompositeLitNode {
	var (
		literalType  *LiteralTypeNode
		literalValue *LiteralValueNode
	)
	// ebnf.Sequence LiteralType LiteralValue ctx [LBRACK, MAP, STRUCT]
	{
		ix := p.ix
		// *ebnf.Name LiteralType ctx [LBRACK, MAP, STRUCT]
		if literalType = p.literalType(); literalType == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Name LiteralValue ctx []
		switch p.c() {
		case LBRACE:
			if literalValue = p.literalValue(); literalValue == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &CompositeLitNode{
		LiteralType:  literalType,
		LiteralValue: literalValue,
	}
}

// ConditionNode represents the production
//
//	Condition = Expression .
type ConditionNode struct {
	Expression Expression
}

// Source implements Node.
func (n *ConditionNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ConditionNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) condition(preBlock bool) *ConditionNode {
	var (
		expression Expression
	)
	// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	if expression = p.expression(preBlock); expression == nil {
		return nil
	}
	return &ConditionNode{
		Expression: expression,
	}
}

// ConstDeclNode represents the production
//
//	ConstDecl = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
type ConstDeclNode struct {
	CONST     Token
	ConstSpec *ConstSpecNode
	LPAREN    Token
	List      []struct {
		ConstSpec *ConstSpecNode
		SEMICOLON Token
	}
	RPAREN Token
}

// Source implements Node.
func (n *ConstDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ConstDeclNode) Position() token.Position { return n.CONST.Position() }

func (p *parser) constDecl() *ConstDeclNode {
	var (
		ok        bool
		constTok  Token
		constSpec *ConstSpecNode
		lparenTok Token
		list      []struct {
			ConstSpec *ConstSpecNode
			SEMICOLON Token
		}
		rparenTok Token
	)
	// ebnf.Sequence "const" ( ConstSpec | "(" { ConstSpec ";" } [ ConstSpec ] ")" ) ctx [CONST]
	{
		switch p.peek(1) {
		case IDENT, LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "const" ctx [CONST]
		constTok = p.expect(CONST)
		// *ebnf.Group ( ConstSpec | "(" { ConstSpec ";" } [ ConstSpec ] ")" ) ctx [IDENT, LPAREN]
		// ebnf.Alternative ConstSpec | "(" { ConstSpec ";" } [ ConstSpec ] ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name ConstSpec ctx [IDENT]
			if constSpec = p.constSpec(); constSpec == nil {
				goto _0
			}
			break
		_0:
			constSpec = nil
			p.back(ix)
			return nil
		case LPAREN: // 1
			// ebnf.Sequence "(" { ConstSpec ";" } [ ConstSpec ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				lparenTok = p.expect(LPAREN)
				// *ebnf.Repetition { ConstSpec ";" } ctx []
			_4:
				{
					var constSpec *ConstSpecNode
					var semicolonTok Token
					switch p.c() {
					case IDENT:
						// ebnf.Sequence ConstSpec ";" ctx [IDENT]
						ix := p.ix
						// *ebnf.Name ConstSpec ctx [IDENT]
						if constSpec = p.constSpec(); constSpec == nil {
							p.back(ix)
							goto _5
						}
						if p.c() == RPAREN {
							list = append(list, struct {
								ConstSpec *ConstSpecNode
								SEMICOLON Token
							}{ConstSpec: constSpec})
							break
						}

						// *ebnf.Token ";" ctx []
						if semicolonTok, ok = p.accept(SEMICOLON); !ok {
							p.back(ix)
							goto _5
						}
						list = append(list, struct {
							ConstSpec *ConstSpecNode
							SEMICOLON Token
						}{ConstSpec: constSpec, SEMICOLON: semicolonTok})
						goto _4
					}
				_5:
				}
				if rparenTok, ok = p.accept(RPAREN); !ok {
					p.back(ix)
					goto _2
				}
			}
			break
		_2:
			lparenTok = Token{}
			rparenTok = Token{}
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
	}
	return &ConstDeclNode{
		CONST:     constTok,
		ConstSpec: constSpec,
		LPAREN:    lparenTok,
		List:      list,
		RPAREN:    rparenTok,
	}
}

// ConstSpecNode represents the production
//
//	ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
type ConstSpecNode struct {
	lexicalScoper
	IdentifierList *IdentifierListNode
	Type           *TypeNode
	ASSIGN         Token
	ExpressionList *ExpressionListNode
}

// Source implements Node.
func (n *ConstSpecNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ConstSpecNode) Position() token.Position { return n.IdentifierList.Position() }

func (p *parser) constSpec() (r *ConstSpecNode) {
	var (
		ok             bool
		identifierList *IdentifierListNode
		typeNode       *TypeNode
		assignTok      Token
		expressionList *ExpressionListNode
	)
	// ebnf.Sequence IdentifierList [ [ Type ] "=" ExpressionList ] ctx [IDENT]
	{
		ix := p.ix
		// *ebnf.Name IdentifierList ctx [IDENT]
		if identifierList = p.identifierList(); identifierList == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ [ Type ] "=" ExpressionList ] ctx []
		switch p.c() {
		case ARROW, ASSIGN, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			// ebnf.Sequence [ Type ] "=" ExpressionList ctx [ARROW, ASSIGN, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			{
				ix := p.ix
				// *ebnf.Option [ Type ] ctx [ARROW, ASSIGN, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
				switch p.c() {
				case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
					// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
					if typeNode = p.type1(); typeNode == nil {
						goto _2
					}
				}
				goto _3
			_2:
				typeNode = nil
			_3:
				// *ebnf.Token "=" ctx []
				if assignTok, ok = p.accept(ASSIGN); !ok {
					p.back(ix)
					goto _0
				}
				// *ebnf.Name ExpressionList ctx []
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if expressionList = p.expressionList(false); expressionList == nil {
						p.back(ix)
						goto _0
					}
				default:
					p.back(ix)
					goto _0
				}
			}
		}
		goto _1
	_0:
		assignTok = Token{}
		expressionList = nil
		typeNode = nil
	_1:
	}
	sc := p.sc
	r = &ConstSpecNode{
		lexicalScoper:  newLexicalScoper(sc),
		IdentifierList: identifierList,
		Type:           typeNode,
		ASSIGN:         assignTok,
		ExpressionList: expressionList,
	}
	visible := int32(p.ix)
	for _, v := range identifierList.List {
		p.declare(sc, v.IDENT, r, visible, false)
	}
	return r
}

// ContinueStmtNode represents the production
//
//	ContinueStmt = "continue" [ Label ] .
type ContinueStmtNode struct {
	CONTINUE Token
	Label    *LabelNode
}

// Source implements Node.
func (n *ContinueStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ContinueStmtNode) Position() token.Position { return n.CONTINUE.Position() }

func (p *parser) continueStmt() *ContinueStmtNode {
	var (
		continueTok Token
		label       *LabelNode
	)
	// ebnf.Sequence "continue" [ Label ] ctx [CONTINUE]
	{
		// *ebnf.Token "continue" ctx [CONTINUE]
		continueTok = p.expect(CONTINUE)
		// *ebnf.Option [ Label ] ctx []
		switch p.c() {
		case IDENT:
			// *ebnf.Name Label ctx [IDENT]
			if label = p.label(); label == nil {
				goto _0
			}
		}
		goto _1
	_0:
		label = nil
	_1:
	}
	return &ContinueStmtNode{
		CONTINUE: continueTok,
		Label:    label,
	}
}

// ConversionNode represents the production
//
//	Conversion = Type "(" Expression [ "," ] ")" .
type ConversionNode struct {
	Type       *TypeNode
	LPAREN     Token
	Expression Expression
	COMMA      Token
	RPAREN     Token
}

// Source implements Node.
func (n *ConversionNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ConversionNode) Position() token.Position { return n.Type.Position() }

func (p *parser) conversion() *ConversionNode {
	var (
		ok         bool
		typeNode   *TypeNode
		lparenTok  Token
		expression Expression
		commaTok   Token
		rparenTok  Token
	)
	// ebnf.Sequence Type "(" Expression [ "," ] ")" ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	{
		ix := p.ix
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if typeNode = p.type1(); typeNode == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "(" ctx []
		if lparenTok, ok = p.accept(LPAREN); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Expression ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if expression = p.expression(false); expression == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ "," ] ctx []
		switch p.c() {
		case COMMA:
			// *ebnf.Token "," ctx [COMMA]
			commaTok = p.expect(COMMA)
		}
		// *ebnf.Token ")" ctx []
		if rparenTok, ok = p.accept(RPAREN); !ok {
			p.back(ix)
			return nil
		}
	}
	return &ConversionNode{
		Type:       typeNode,
		LPAREN:     lparenTok,
		Expression: expression,
		COMMA:      commaTok,
		RPAREN:     rparenTok,
	}
}

// DeclarationNode represents the production
//
//	Declaration = ConstDecl | TypeDecl | VarDecl .
type DeclarationNode struct {
	ConstDecl *ConstDeclNode
	TypeDecl  *TypeDeclNode
	VarDecl   *VarDeclNode
}

// Source implements Node.
func (n *DeclarationNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *DeclarationNode) Position() token.Position { panic("TODO") }

func (p *parser) declaration() *DeclarationNode {
	var (
		constDecl *ConstDeclNode
		typeDecl  *TypeDeclNode
		varDecl   *VarDeclNode
	)
	// ebnf.Alternative ConstDecl | TypeDecl | VarDecl ctx [CONST, TYPE, VAR]
	switch p.c() {
	case CONST: // 0
		// *ebnf.Name ConstDecl ctx [CONST]
		if constDecl = p.constDecl(); constDecl == nil {
			goto _0
		}
		break
	_0:
		constDecl = nil
		return nil
	case TYPE: // 1
		// *ebnf.Name TypeDecl ctx [TYPE]
		if typeDecl = p.typeDecl(); typeDecl == nil {
			goto _2
		}
		break
	_2:
		typeDecl = nil
		return nil
	case VAR: // 2
		// *ebnf.Name VarDecl ctx [VAR]
		if varDecl = p.varDecl(); varDecl == nil {
			goto _4
		}
		break
	_4:
		varDecl = nil
		return nil
	default:
		return nil
	}
	return &DeclarationNode{
		ConstDecl: constDecl,
		TypeDecl:  typeDecl,
		VarDecl:   varDecl,
	}
}

// DeferStmtNode represents the production
//
//	DeferStmt = "defer" Expression .
type DeferStmtNode struct {
	DEFER      Token
	Expression Expression
}

// Source implements Node.
func (n *DeferStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *DeferStmtNode) Position() token.Position { return n.DEFER.Position() }

func (p *parser) deferStmt() *DeferStmtNode {
	var (
		deferTok   Token
		expression Expression
	)
	// ebnf.Sequence "defer" Expression ctx [DEFER]
	{
		switch p.peek(1) {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "defer" ctx [DEFER]
		deferTok = p.expect(DEFER)
		// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if expression = p.expression(false); expression == nil {
			p.back(ix)
			return nil
		}
	}
	return &DeferStmtNode{
		DEFER:      deferTok,
		Expression: expression,
	}
}

// ElementNode represents the production
//
//	Element = Expression | LiteralValue .
type ElementNode struct {
	Expression   Expression
	LiteralValue *LiteralValueNode
}

// Source implements Node.
func (n *ElementNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ElementNode) Position() token.Position { panic("TODO") }

func (p *parser) element() *ElementNode {
	var (
		expression   Expression
		literalValue *LiteralValueNode
	)
	// ebnf.Alternative Expression | LiteralValue ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	switch p.c() {
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0
		// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if expression = p.expression(false); expression == nil {
			goto _0
		}
		break
	_0:
		expression = nil
		return nil
	case LBRACE: // 1
		// *ebnf.Name LiteralValue ctx [LBRACE]
		if literalValue = p.literalValue(); literalValue == nil {
			goto _2
		}
		break
	_2:
		literalValue = nil
		return nil
	default:
		return nil
	}
	return &ElementNode{
		Expression:   expression,
		LiteralValue: literalValue,
	}
}

// ElementTypeNode represents the production
//
//	ElementType = Type .
type ElementTypeNode struct {
	Type *TypeNode
}

// Source implements Node.
func (n *ElementTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ElementTypeNode) Position() token.Position { return n.Type.Position() }

func (p *parser) elementType() *ElementTypeNode {
	var (
		typeNode *TypeNode
	)
	// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	if typeNode = p.type1(); typeNode == nil {
		return nil
	}
	return &ElementTypeNode{
		Type: typeNode,
	}
}

// EmbeddedFieldNode represents the production
//
//	EmbeddedField = [ "*" ] TypeName [ TypeArgs ] .
type EmbeddedFieldNode struct {
	MUL      Token
	TypeName *TypeNameNode
	TypeArgs *TypeArgsNode
}

// Source implements Node.
func (n *EmbeddedFieldNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *EmbeddedFieldNode) Position() token.Position { panic("TODO") }

func (p *parser) embeddedField() *EmbeddedFieldNode {
	var (
		mulTok   Token
		typeName *TypeNameNode
		typeArgs *TypeArgsNode
	)
	// ebnf.Sequence [ "*" ] TypeName [ TypeArgs ] ctx [IDENT, MUL]
	{
		ix := p.ix
		// *ebnf.Option [ "*" ] ctx [IDENT, MUL]
		switch p.c() {
		case MUL:
			// *ebnf.Token "*" ctx [MUL]
			mulTok = p.expect(MUL)
		}
		// *ebnf.Name TypeName ctx []
		switch p.c() {
		case IDENT:
			if typeName = p.typeName(); typeName == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ TypeArgs ] ctx []
		switch p.c() {
		case LBRACK:
			// *ebnf.Name TypeArgs ctx [LBRACK]
			if typeArgs = p.typeArgs(); typeArgs == nil {
				goto _2
			}
		}
		goto _3
	_2:
		typeArgs = nil
	_3:
	}
	return &EmbeddedFieldNode{
		MUL:      mulTok,
		TypeName: typeName,
		TypeArgs: typeArgs,
	}
}

// EmptyStmtNode represents the production
//
//	EmptyStmt =  .
type EmptyStmtNode struct {
}

// Source implements Node.
func (n *EmptyStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *EmptyStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) emptyStmt() *EmptyStmtNode {
	return &EmptyStmtNode{}
}

// ExprCaseClauseNode represents the production
//
//	ExprCaseClause = ExprSwitchCase ":" StatementList .
type ExprCaseClauseNode struct {
	ExprSwitchCase *ExprSwitchCaseNode
	COLON          Token
	StatementList  *StatementListNode
}

// Source implements Node.
func (n *ExprCaseClauseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ExprCaseClauseNode) Position() token.Position { return n.ExprSwitchCase.Position() }

func (p *parser) exprCaseClause() *ExprCaseClauseNode {
	var (
		ok             bool
		exprSwitchCase *ExprSwitchCaseNode
		colonTok       Token
		statementList  *StatementListNode
	)
	// ebnf.Sequence ExprSwitchCase ":" StatementList ctx [CASE, DEFAULT]
	{
		p.openScope()

		defer p.closeScope()

		ix := p.ix
		// *ebnf.Name ExprSwitchCase ctx [CASE, DEFAULT]
		if exprSwitchCase = p.exprSwitchCase(); exprSwitchCase == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token ":" ctx []
		if colonTok, ok = p.accept(COLON); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name StatementList ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if statementList = p.statementList(); statementList == nil {
				p.back(ix)
				return nil
			}
		}
	}
	return &ExprCaseClauseNode{
		ExprSwitchCase: exprSwitchCase,
		COLON:          colonTok,
		StatementList:  statementList,
	}
}

// ExprSwitchCaseNode represents the production
//
//	ExprSwitchCase = "case" ExpressionList | "default" .
type ExprSwitchCaseNode struct {
	CASE           Token
	ExpressionList *ExpressionListNode
	DEFAULT        Token
}

// Source implements Node.
func (n *ExprSwitchCaseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ExprSwitchCaseNode) Position() token.Position { panic("TODO") }

func (p *parser) exprSwitchCase() *ExprSwitchCaseNode {
	var (
		caseTok        Token
		expressionList *ExpressionListNode
		defaultTok     Token
	)
	// ebnf.Alternative "case" ExpressionList | "default" ctx [CASE, DEFAULT]
	switch p.c() {
	case CASE: // 0
		// ebnf.Sequence "case" ExpressionList ctx [CASE]
		{
			switch p.peek(1) {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			default:
				goto _0
			}
			ix := p.ix
			// *ebnf.Token "case" ctx [CASE]
			caseTok = p.expect(CASE)
			// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expressionList = p.expressionList(false); expressionList == nil {
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
		caseTok = Token{}
		expressionList = nil
		return nil
	case DEFAULT: // 1
		// *ebnf.Token "default" ctx [DEFAULT]
		defaultTok = p.expect(DEFAULT)
	default:
		return nil
	}
	return &ExprSwitchCaseNode{
		CASE:           caseTok,
		ExpressionList: expressionList,
		DEFAULT:        defaultTok,
	}
}

// ExprSwitchStmtNode represents the production
//
//	ExprSwitchStmt = "switch" [ Expression ] "{" { ExprCaseClause } "}" | "switch" SimpleStmt ";" [ Expression ] "{" { ExprCaseClause } "}" .
type ExprSwitchStmtNode struct {
	SWITCH     Token
	Expression Expression
	LBRACE     Token
	List       []struct{ ExprCaseClause *ExprCaseClauseNode }
	RBRACE     Token
	SimpleStmt *SimpleStmtNode
	SEMICOLON  Token
}

// Source implements Node.
func (n *ExprSwitchStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ExprSwitchStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) exprSwitchStmt() *ExprSwitchStmtNode {
	var (
		ok           bool
		switchTok    Token
		expression   Expression
		lbraceTok    Token
		list         []struct{ ExprCaseClause *ExprCaseClauseNode }
		rbraceTok    Token
		simpleStmt   *SimpleStmtNode
		semicolonTok Token
	)
	// ebnf.Alternative "switch" [ Expression ] "{" { ExprCaseClause } "}" | "switch" SimpleStmt ";" [ Expression ] "{" { ExprCaseClause } "}" ctx [SWITCH]
	switch p.c() {
	case SWITCH: // 0 1
		// ebnf.Sequence "switch" [ Expression ] "{" { ExprCaseClause } "}" ctx [SWITCH]
		{
			ix := p.ix
			// *ebnf.Token "switch" ctx [SWITCH]
			switchTok = p.expect(SWITCH)
			// *ebnf.Option [ Expression ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expression = p.expression(true); expression == nil {
					goto _1
				}
			}
			goto _2
		_1:
			expression = nil
		_2:
			// *ebnf.Token "{" ctx []
			if lbraceTok, ok = p.accept(LBRACE); !ok {
				p.back(ix)
				goto _0
			}
			// *ebnf.Repetition { ExprCaseClause } ctx []
		_3:
			{
				var exprCaseClause *ExprCaseClauseNode
				switch p.c() {
				case CASE, DEFAULT:
					// *ebnf.Name ExprCaseClause ctx [CASE, DEFAULT]
					if exprCaseClause = p.exprCaseClause(); exprCaseClause == nil {
						goto _4
					}
					list = append(list, struct{ ExprCaseClause *ExprCaseClauseNode }{ExprCaseClause: exprCaseClause})
					goto _3
				}
			_4:
			}
			// *ebnf.Token "}" ctx []
			if rbraceTok, ok = p.accept(RBRACE); !ok {
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
		expression = nil
		lbraceTok = Token{}
		rbraceTok = Token{}
		switchTok = Token{}
		// ebnf.Sequence "switch" SimpleStmt ";" [ Expression ] "{" { ExprCaseClause } "}" ctx [SWITCH]
		{
			ix := p.ix
			// *ebnf.Token "switch" ctx [SWITCH]
			switchTok = p.expect(SWITCH)
			// *ebnf.Name SimpleStmt ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
				if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
					p.back(ix)
					goto _5
				}
			}
			// *ebnf.Token ";" ctx []
			if semicolonTok, ok = p.accept(SEMICOLON); !ok {
				p.back(ix)
				goto _5
			}
			// *ebnf.Option [ Expression ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expression = p.expression(true); expression == nil {
					goto _6
				}
			}
			goto _7
		_6:
			expression = nil
		_7:
			// *ebnf.Token "{" ctx []
			if lbraceTok, ok = p.accept(LBRACE); !ok {
				p.back(ix)
				goto _5
			}
			// *ebnf.Repetition { ExprCaseClause } ctx []
		_8:
			{
				var exprCaseClause *ExprCaseClauseNode
				switch p.c() {
				case CASE, DEFAULT:
					// *ebnf.Name ExprCaseClause ctx [CASE, DEFAULT]
					if exprCaseClause = p.exprCaseClause(); exprCaseClause == nil {
						goto _9
					}
					list = append(list, struct{ ExprCaseClause *ExprCaseClauseNode }{ExprCaseClause: exprCaseClause})
					goto _8
				}
			_9:
			}
			// *ebnf.Token "}" ctx []
			if rbraceTok, ok = p.accept(RBRACE); !ok {
				p.back(ix)
				goto _5
			}
		}
		break
	_5:
		expression = nil
		lbraceTok = Token{}
		rbraceTok = Token{}
		semicolonTok = Token{}
		simpleStmt = nil
		switchTok = Token{}
		return nil
	default:
		return nil
	}
	return &ExprSwitchStmtNode{
		SWITCH:     switchTok,
		Expression: expression,
		LBRACE:     lbraceTok,
		List:       list,
		RBRACE:     rbraceTok,
		SimpleStmt: simpleStmt,
		SEMICOLON:  semicolonTok,
	}
}

func (p *parser) expression(preBlock bool) (r Expression) {
	var logicalAndExpression Expression
	// ebnf.Sequence LogicalAndExpression { "||" LogicalAndExpression } ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name LogicalAndExpression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if logicalAndExpression = p.logicalAndExpression(preBlock); logicalAndExpression == nil {
			p.back(ix)
			return nil
		}
		r = logicalAndExpression
		// *ebnf.Repetition { "||" LogicalAndExpression } ctx []
	_0:
		{
			var lorTok Token
			var logicalAndExpression Expression
			switch p.c() {
			case LOR:
				// ebnf.Sequence "||" LogicalAndExpression ctx [LOR]
				switch p.peek(1) {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				default:
					goto _1
				}
				ix := p.ix
				// *ebnf.Token "||" ctx [LOR]
				lorTok = p.expect(LOR)
				// *ebnf.Name LogicalAndExpression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if logicalAndExpression = p.logicalAndExpression(preBlock); logicalAndExpression == nil {
					p.back(ix)
					goto _1
				}
				r = &BinaryExpression{r, lorTok, logicalAndExpression}
				goto _0
			}
		_1:
		}
	}
	return r
}

// ExpressionListNode represents the production
//
//	ExpressionList = Expression { "," Expression } .
type ExpressionListNode struct {
	Expression Expression
	List       []struct {
		COMMA      Token
		Expression Expression
	}
}

// Source implements Node.
func (n *ExpressionListNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ExpressionListNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) expressionList(preBlock bool) *ExpressionListNode {
	var (
		expression Expression
		list       []struct {
			COMMA      Token
			Expression Expression
		}
	)
	// ebnf.Sequence Expression { "," Expression } ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if expression = p.expression(preBlock); expression == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," Expression } ctx []
	_0:
		{
			var commaTok Token
			var expression Expression
			switch p.c() {
			case COMMA:
				// ebnf.Sequence "," Expression ctx [COMMA]
				switch p.peek(1) {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				default:
					goto _1
				}
				ix := p.ix
				// *ebnf.Token "," ctx [COMMA]
				commaTok = p.expect(COMMA)
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expression = p.expression(preBlock); expression == nil {
					p.back(ix)
					goto _1
				}
				list = append(list, struct {
					COMMA      Token
					Expression Expression
				}{COMMA: commaTok, Expression: expression})
				goto _0
			}
		_1:
		}
	}
	return &ExpressionListNode{
		Expression: expression,
		List:       list,
	}
}

// FallthroughStmtNode represents the production
//
//	FallthroughStmt = "fallthrough" .
type FallthroughStmtNode struct {
	FALLTHROUGH Token
}

// Source implements Node.
func (n *FallthroughStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *FallthroughStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) fallthroughStmt() *FallthroughStmtNode {
	var (
		fallthroughTok Token
	)
	// *ebnf.Token "fallthrough" ctx [FALLTHROUGH]
	fallthroughTok = p.expect(FALLTHROUGH)
	return &FallthroughStmtNode{
		FALLTHROUGH: fallthroughTok,
	}
}

// FieldDeclNode represents the production
//
//	FieldDecl = ( IdentifierList Type | EmbeddedField ) [ Tag ] .
type FieldDeclNode struct {
	IdentifierList *IdentifierListNode
	Type           *TypeNode
	EmbeddedField  *EmbeddedFieldNode
	Tag            *TagNode
}

// Source implements Node.
func (n *FieldDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *FieldDeclNode) Position() token.Position { panic("TODO") }

func (p *parser) fieldDecl() *FieldDeclNode {
	var (
		identifierList *IdentifierListNode
		typeNode       *TypeNode
		embeddedField  *EmbeddedFieldNode
		tag            *TagNode
	)
	// ebnf.Sequence ( IdentifierList Type | EmbeddedField ) [ Tag ] ctx [IDENT, MUL]
	{
		ix := p.ix
		// *ebnf.Group ( IdentifierList Type | EmbeddedField ) ctx [IDENT, MUL]
		// ebnf.Alternative IdentifierList Type | EmbeddedField ctx [IDENT, MUL]
		switch p.c() {
		case IDENT: // 0 1
			// ebnf.Sequence IdentifierList Type ctx [IDENT]
			{
				ix := p.ix
				// *ebnf.Name IdentifierList ctx [IDENT]
				if identifierList = p.identifierList(); identifierList == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Name Type ctx []
				switch p.c() {
				case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
					if typeNode = p.type1(); typeNode == nil {
						p.back(ix)
						goto _0
					}
				default:
					p.back(ix)
					goto _0
				}
			}
			break
		_0:
			identifierList = nil
			typeNode = nil
			// *ebnf.Name EmbeddedField ctx [IDENT]
			if embeddedField = p.embeddedField(); embeddedField == nil {
				goto _1
			}
			break
		_1:
			embeddedField = nil
			p.back(ix)
			return nil
		case MUL: // 1
			// *ebnf.Name EmbeddedField ctx [MUL]
			if embeddedField = p.embeddedField(); embeddedField == nil {
				goto _2
			}
			break
		_2:
			embeddedField = nil
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Tag ] ctx []
		switch p.c() {
		case STRING:
			// *ebnf.Name Tag ctx [STRING]
			if tag = p.tag(); tag == nil {
				goto _4
			}
		}
		goto _5
	_4:
		tag = nil
	_5:
	}
	return &FieldDeclNode{
		IdentifierList: identifierList,
		Type:           typeNode,
		EmbeddedField:  embeddedField,
		Tag:            tag,
	}
}

// ForClauseNode represents the production
//
//	ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
type ForClauseNode struct {
	InitStmt   *InitStmtNode
	SEMICOLON  Token
	Condition  *ConditionNode
	SEMICOLON2 Token
	PostStmt   *PostStmtNode
}

// Source implements Node.
func (n *ForClauseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ForClauseNode) Position() token.Position { panic("TODO") }

func (p *parser) forClause() *ForClauseNode {
	var (
		ok            bool
		initStmt      *InitStmtNode
		semicolonTok  Token
		condition     *ConditionNode
		semicolon2Tok Token
		postStmt      *PostStmtNode
	)
	// ebnf.Sequence [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Option [ InitStmt ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR]
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// *ebnf.Name InitStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if initStmt = p.initStmt(); initStmt == nil {
				goto _0
			}
		}
		goto _1
	_0:
		initStmt = nil
	_1:
		// *ebnf.Token ";" ctx []
		if semicolonTok, ok = p.accept(SEMICOLON); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Condition ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// *ebnf.Name Condition ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if condition = p.condition(false); condition == nil {
				goto _2
			}
		}
		goto _3
	_2:
		condition = nil
	_3:
		// *ebnf.Token ";" ctx []
		if semicolon2Tok, ok = p.accept(SEMICOLON); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ PostStmt ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
			// *ebnf.Name PostStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
			if postStmt = p.postStmt(); postStmt == nil {
				goto _4
			}
		}
		goto _5
	_4:
		postStmt = nil
	_5:
	}
	return &ForClauseNode{
		InitStmt:   initStmt,
		SEMICOLON:  semicolonTok,
		Condition:  condition,
		SEMICOLON2: semicolon2Tok,
		PostStmt:   postStmt,
	}
}

// ForStmtNode represents the production
//
//	ForStmt = "for" [ ForClause | RangeClause | Condition ] Block .
type ForStmtNode struct {
	FOR         Token
	ForClause   *ForClauseNode
	RangeClause *RangeClauseNode
	Condition   *ConditionNode
	Block       *BlockNode
}

// Source implements Node.
func (n *ForStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ForStmtNode) Position() token.Position { return n.FOR.Position() }

func (p *parser) forStmt() *ForStmtNode {
	var (
		forTok      Token
		forClause   *ForClauseNode
		rangeClause *RangeClauseNode
		condition   *ConditionNode
		block       *BlockNode
	)
	// ebnf.Sequence "for" [ ForClause | RangeClause | Condition ] Block ctx [FOR]
	{
		p.openScope()

		defer p.closeScope()

		ix := p.ix
		// *ebnf.Token "for" ctx [FOR]
		forTok = p.expect(FOR)
		// *ebnf.Option [ ForClause | RangeClause | Condition ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, SEMICOLON, STRING, STRUCT, SUB, XOR:
			// ebnf.Alternative ForClause | RangeClause | Condition ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, SEMICOLON, STRING, STRUCT, SUB, XOR]
			switch p.c() {
			case SEMICOLON: // 0
				// *ebnf.Name ForClause ctx [SEMICOLON]
				if forClause = p.forClause(); forClause == nil {
					goto _2
				}
				break
			_2:
				forClause = nil
				goto _0
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0 1 2
				// *ebnf.Name ForClause ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if forClause = p.forClause(); forClause == nil {
					goto _4
				}
				break
			_4:
				forClause = nil
				// *ebnf.Name RangeClause ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if rangeClause = p.rangeClause(); rangeClause == nil {
					goto _5
				}
				break
			_5:
				rangeClause = nil
				// *ebnf.Name Condition ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if condition = p.condition(true); condition == nil {
					goto _6
				}
				break
			_6:
				condition = nil
				goto _0
			case RANGE: // 1
				// *ebnf.Name RangeClause ctx [RANGE]
				if rangeClause = p.rangeClause(); rangeClause == nil {
					goto _7
				}
				break
			_7:
				rangeClause = nil
				goto _0
			default:
				goto _0
			}
		}
		goto _1
	_0:
		forClause = nil
		rangeClause = nil
	_1:
		// *ebnf.Name Block ctx []
		switch p.c() {
		case LBRACE:
			if block = p.block(); block == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ForStmtNode{
		FOR:         forTok,
		ForClause:   forClause,
		RangeClause: rangeClause,
		Condition:   condition,
		Block:       block,
	}
}

// FunctionBodyNode represents the production
//
//	FunctionBody = Block .
type FunctionBodyNode struct {
	Block *BlockNode
}

// Source implements Node.
func (n *FunctionBodyNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *FunctionBodyNode) Position() token.Position { return n.Block.Position() }

func (p *parser) functionBody() *FunctionBodyNode {
	var (
		block *BlockNode
	)
	// *ebnf.Name Block ctx [LBRACE]
	if block = p.block(); block == nil {
		return nil
	}
	return &FunctionBodyNode{
		Block: block,
	}
}

// FunctionDeclNode represents the production
//
//	FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
type FunctionDeclNode struct {
	lexicalScoper
	FUNC           Token
	FunctionName   *FunctionNameNode
	TypeParameters *TypeParametersNode
	Signature      *SignatureNode
	FunctionBody   *FunctionBodyNode
}

// Source implements Node.
func (n *FunctionDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *FunctionDeclNode) Position() token.Position { return n.FUNC.Position() }

func (p *parser) functionDecl() (r *FunctionDeclNode) {
	var (
		funcTok        Token
		functionName   *FunctionNameNode
		typeParameters *TypeParametersNode
		signature      *SignatureNode
		functionBody   *FunctionBodyNode
	)
	// ebnf.Sequence "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] ctx [FUNC]
	{
		if p.peek(1) != IDENT {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "func" ctx [FUNC]
		funcTok = p.expect(FUNC)
		// *ebnf.Name FunctionName ctx [IDENT]
		if functionName = p.functionName(); functionName == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ TypeParameters ] ctx []
		switch p.c() {
		case LBRACK:
			// *ebnf.Name TypeParameters ctx [LBRACK]
			if typeParameters = p.typeParameters(); typeParameters == nil {
				goto _0
			}
		}
		goto _1
	_0:
		typeParameters = nil
	_1:
		// *ebnf.Name Signature ctx []
		switch p.c() {
		case LPAREN:
			if signature = p.signature(); signature == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ FunctionBody ] ctx []
		switch p.c() {
		case LBRACE:
			// *ebnf.Name FunctionBody ctx [LBRACE]
			if functionBody = p.functionBody(); functionBody == nil {
				goto _2
			}
		}
		goto _3
	_2:
		functionBody = nil
	_3:
	}
	sc := p.sc
	r = &FunctionDeclNode{
		lexicalScoper:  newLexicalScoper(sc),
		FUNC:           funcTok,
		FunctionName:   functionName,
		TypeParameters: typeParameters,
		Signature:      signature,
		FunctionBody:   functionBody,
	}
	p.declare(sc, functionName.IDENT, r, 0, true)
	return r
}

// FunctionLitNode represents the production
//
//	FunctionLit = "func" Signature FunctionBody .
type FunctionLitNode struct {
	FUNC         Token
	Signature    *SignatureNode
	FunctionBody *FunctionBodyNode
}

// Source implements Node.
func (n *FunctionLitNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *FunctionLitNode) Position() token.Position { return n.FUNC.Position() }

func (p *parser) functionLit() *FunctionLitNode {
	var (
		funcTok      Token
		signature    *SignatureNode
		functionBody *FunctionBodyNode
	)
	// ebnf.Sequence "func" Signature FunctionBody ctx [FUNC]
	{
		switch p.peek(1) {
		case LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "func" ctx [FUNC]
		funcTok = p.expect(FUNC)
		// *ebnf.Name Signature ctx [LPAREN]
		if signature = p.signature(); signature == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Name FunctionBody ctx []
		switch p.c() {
		case LBRACE:
			if functionBody = p.functionBody(); functionBody == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &FunctionLitNode{
		FUNC:         funcTok,
		Signature:    signature,
		FunctionBody: functionBody,
	}
}

// FunctionNameNode represents the production
//
//	FunctionName = identifier .
type FunctionNameNode struct {
	IDENT Token
}

// Source implements Node.
func (n *FunctionNameNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *FunctionNameNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) functionName() *FunctionNameNode {
	var (
		identTok Token
	)
	// *ebnf.Name identifier ctx [IDENT]
	identTok = p.expect(IDENT)
	return &FunctionNameNode{
		IDENT: identTok,
	}
}

// FunctionTypeNode represents the production
//
//	FunctionType = "func" Signature .
type FunctionTypeNode struct {
	FUNC      Token
	Signature *SignatureNode
}

// Source implements Node.
func (n *FunctionTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *FunctionTypeNode) Position() token.Position { return n.FUNC.Position() }

func (p *parser) functionType() *FunctionTypeNode {
	var (
		funcTok   Token
		signature *SignatureNode
	)
	// ebnf.Sequence "func" Signature ctx [FUNC]
	{
		switch p.peek(1) {
		case LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "func" ctx [FUNC]
		funcTok = p.expect(FUNC)
		// *ebnf.Name Signature ctx [LPAREN]
		if signature = p.signature(); signature == nil {
			p.back(ix)
			return nil
		}
	}
	return &FunctionTypeNode{
		FUNC:      funcTok,
		Signature: signature,
	}
}

// GoStmtNode represents the production
//
//	GoStmt = "go" Expression .
type GoStmtNode struct {
	GO         Token
	Expression Expression
}

// Source implements Node.
func (n *GoStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *GoStmtNode) Position() token.Position { return n.GO.Position() }

func (p *parser) goStmt() *GoStmtNode {
	var (
		goTok      Token
		expression Expression
	)
	// ebnf.Sequence "go" Expression ctx [GO]
	{
		switch p.peek(1) {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "go" ctx [GO]
		goTok = p.expect(GO)
		// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if expression = p.expression(false); expression == nil {
			p.back(ix)
			return nil
		}
	}
	return &GoStmtNode{
		GO:         goTok,
		Expression: expression,
	}
}

// GotoStmtNode represents the production
//
//	GotoStmt = "goto" Label .
type GotoStmtNode struct {
	GOTO  Token
	Label *LabelNode
}

// Source implements Node.
func (n *GotoStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *GotoStmtNode) Position() token.Position { return n.GOTO.Position() }

func (p *parser) gotoStmt() *GotoStmtNode {
	var (
		gotoTok Token
		label   *LabelNode
	)
	// ebnf.Sequence "goto" Label ctx [GOTO]
	{
		if p.peek(1) != IDENT {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "goto" ctx [GOTO]
		gotoTok = p.expect(GOTO)
		// *ebnf.Name Label ctx [IDENT]
		if label = p.label(); label == nil {
			p.back(ix)
			return nil
		}
	}
	return &GotoStmtNode{
		GOTO:  gotoTok,
		Label: label,
	}
}

// IdentifierListNode represents the production
//
//	IdentifierList = identifier { "," identifier } .
type IdentifierListNode struct {
	IDENT Token
	List  []struct {
		COMMA Token
		IDENT Token
	}
}

// Source implements Node.
func (n *IdentifierListNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *IdentifierListNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) identifierList() *IdentifierListNode {
	var (
		identTok Token
		list     []struct {
			COMMA Token
			IDENT Token
		}
	)
	// ebnf.Sequence identifier { "," identifier } ctx [IDENT]
	{
		// *ebnf.Name identifier ctx [IDENT]
		identTok = p.expect(IDENT)
		// *ebnf.Repetition { "," identifier } ctx []
	_0:
		{
			var commaTok Token
			var identTok Token
			switch p.c() {
			case COMMA:
				// ebnf.Sequence "," identifier ctx [COMMA]
				if p.peek(1) != IDENT {
					goto _1
				}
				// *ebnf.Token "," ctx [COMMA]
				commaTok = p.expect(COMMA)
				// *ebnf.Name identifier ctx [IDENT]
				identTok = p.expect(IDENT)
				list = append(list, struct {
					COMMA Token
					IDENT Token
				}{COMMA: commaTok, IDENT: identTok})
				goto _0
			}
		_1:
		}
	}
	return &IdentifierListNode{
		IDENT: identTok,
		List:  list,
	}
}

// IfStmtNode represents the production
//
//	IfStmt = "if" Expression Block [ "else" ( IfStmt | Block ) ] | "if" SimpleStmt ";" Expression Block [ "else" ( IfStmt | Block ) ] .
type IfStmtNode struct {
	IF         Token
	Expression Expression
	Block      *BlockNode
	ELSE       Token
	IfStmt     *IfStmtNode
	Block2     *BlockNode
	SimpleStmt *SimpleStmtNode
	SEMICOLON  Token
}

// Source implements Node.
func (n *IfStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *IfStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) ifStmt() *IfStmtNode {
	var (
		ok           bool
		ifTok        Token
		expression   Expression
		block        *BlockNode
		elseTok      Token
		ifStmt       *IfStmtNode
		block2       *BlockNode
		simpleStmt   *SimpleStmtNode
		semicolonTok Token
	)
	// ebnf.Alternative "if" Expression Block [ "else" ( IfStmt | Block ) ] | "if" SimpleStmt ";" Expression Block [ "else" ( IfStmt | Block ) ] ctx [IF]
	switch p.c() {
	case IF: // 0 1
		p.openScope()

		defer p.closeScope()

		// ebnf.Sequence "if" Expression Block [ "else" ( IfStmt | Block ) ] ctx [IF]
		{
			switch p.peek(1) {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			default:
				goto _0
			}
			ix := p.ix
			// *ebnf.Token "if" ctx [IF]
			ifTok = p.expect(IF)
			// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expression = p.expression(true); expression == nil {
				p.back(ix)
				goto _0
			}
			// *ebnf.Name Block ctx []
			switch p.c() {
			case LBRACE:
				if block = p.block(); block == nil {
					p.back(ix)
					goto _0
				}
			default:
				p.back(ix)
				goto _0
			}
			// *ebnf.Option [ "else" ( IfStmt | Block ) ] ctx []
			switch p.c() {
			case ELSE:
				// ebnf.Sequence "else" ( IfStmt | Block ) ctx [ELSE]
				{
					switch p.peek(1) {
					case IF, LBRACE:
					default:
						goto _1
					}
					ix := p.ix
					// *ebnf.Token "else" ctx [ELSE]
					elseTok = p.expect(ELSE)
					// *ebnf.Group ( IfStmt | Block ) ctx [IF, LBRACE]
					// ebnf.Alternative IfStmt | Block ctx [IF, LBRACE]
					switch p.c() {
					case IF: // 0
						// *ebnf.Name IfStmt ctx [IF]
						if ifStmt = p.ifStmt(); ifStmt == nil {
							goto _3
						}
						break
					_3:
						ifStmt = nil
						p.back(ix)
						goto _1
					case LBRACE: // 1
						// *ebnf.Name Block ctx [LBRACE]
						if block2 = p.block(); block2 == nil {
							goto _5
						}
						break
					_5:
						block2 = nil
						p.back(ix)
						goto _1
					default:
						p.back(ix)
						goto _1
					}
				}
			}
			goto _2
		_1:
			block2 = nil
			elseTok = Token{}
			ifStmt = nil
		_2:
		}
		break
	_0:
		block = nil
		block2 = nil
		elseTok = Token{}
		expression = nil
		ifStmt = nil
		ifTok = Token{}
		// ebnf.Sequence "if" SimpleStmt ";" Expression Block [ "else" ( IfStmt | Block ) ] ctx [IF]
		{
			ix := p.ix
			// *ebnf.Token "if" ctx [IF]
			ifTok = p.expect(IF)
			// *ebnf.Name SimpleStmt ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
				if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
					p.back(ix)
					goto _7
				}
			}
			// *ebnf.Token ";" ctx []
			if semicolonTok, ok = p.accept(SEMICOLON); !ok {
				p.back(ix)
				goto _7
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expression = p.expression(true); expression == nil {
					p.back(ix)
					goto _7
				}
			default:
				p.back(ix)
				goto _7
			}
			// *ebnf.Name Block ctx []
			switch p.c() {
			case LBRACE:
				if block = p.block(); block == nil {
					p.back(ix)
					goto _7
				}
			default:
				p.back(ix)
				goto _7
			}
			// *ebnf.Option [ "else" ( IfStmt | Block ) ] ctx []
			switch p.c() {
			case ELSE:
				// ebnf.Sequence "else" ( IfStmt | Block ) ctx [ELSE]
				{
					switch p.peek(1) {
					case IF, LBRACE:
					default:
						goto _8
					}
					ix := p.ix
					// *ebnf.Token "else" ctx [ELSE]
					elseTok = p.expect(ELSE)
					// *ebnf.Group ( IfStmt | Block ) ctx [IF, LBRACE]
					// ebnf.Alternative IfStmt | Block ctx [IF, LBRACE]
					switch p.c() {
					case IF: // 0
						// *ebnf.Name IfStmt ctx [IF]
						if ifStmt = p.ifStmt(); ifStmt == nil {
							goto _10
						}
						break
					_10:
						ifStmt = nil
						p.back(ix)
						goto _8
					case LBRACE: // 1
						// *ebnf.Name Block ctx [LBRACE]
						if block2 = p.block(); block2 == nil {
							goto _12
						}
						break
					_12:
						block2 = nil
						p.back(ix)
						goto _8
					default:
						p.back(ix)
						goto _8
					}
				}
			}
			goto _9
		_8:
			block2 = nil
			elseTok = Token{}
			ifStmt = nil
		_9:
		}
		break
	_7:
		block = nil
		block2 = nil
		elseTok = Token{}
		expression = nil
		ifStmt = nil
		ifTok = Token{}
		semicolonTok = Token{}
		simpleStmt = nil
		return nil
	default:
		return nil
	}
	return &IfStmtNode{
		IF:         ifTok,
		Expression: expression,
		Block:      block,
		ELSE:       elseTok,
		IfStmt:     ifStmt,
		Block2:     block2,
		SimpleStmt: simpleStmt,
		SEMICOLON:  semicolonTok,
	}
}

// ImportDeclNode represents the production
//
//	ImportDecl = "import" ( ImportSpec | "(" { ImportSpec ";" } [ ImportSpec ] ")" ) .
type ImportDeclNode struct {
	IMPORT     Token
	ImportSpec *ImportSpecNode
	LPAREN     Token
	List       []struct {
		ImportSpec *ImportSpecNode
		SEMICOLON  Token
	}
	ImportSpec2 *ImportSpecNode
	RPAREN      Token
}

// Source implements Node.
func (n *ImportDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ImportDeclNode) Position() token.Position { return n.IMPORT.Position() }

func (p *parser) importDecl() *ImportDeclNode {
	var (
		ok         bool
		importTok  Token
		importSpec *ImportSpecNode
		lparenTok  Token
		list       []struct {
			ImportSpec *ImportSpecNode
			SEMICOLON  Token
		}
		importSpec2 *ImportSpecNode
		rparenTok   Token
	)
	// ebnf.Sequence "import" ( ImportSpec | "(" { ImportSpec ";" } [ ImportSpec ] ")" ) ctx [IMPORT]
	{
		switch p.peek(1) {
		case IDENT, LPAREN, PERIOD, STRING:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "import" ctx [IMPORT]
		importTok = p.expect(IMPORT)
		// *ebnf.Group ( ImportSpec | "(" { ImportSpec ";" } [ ImportSpec ] ")" ) ctx [IDENT, LPAREN, PERIOD, STRING]
		// ebnf.Alternative ImportSpec | "(" { ImportSpec ";" } [ ImportSpec ] ")" ctx [IDENT, LPAREN, PERIOD, STRING]
		switch p.c() {
		case IDENT, PERIOD, STRING: // 0
			// *ebnf.Name ImportSpec ctx [IDENT, PERIOD, STRING]
			if importSpec = p.importSpec(); importSpec == nil {
				goto _0
			}
			break
		_0:
			importSpec = nil
			p.back(ix)
			return nil
		case LPAREN: // 1
			// ebnf.Sequence "(" { ImportSpec ";" } [ ImportSpec ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				lparenTok = p.expect(LPAREN)
				// *ebnf.Repetition { ImportSpec ";" } ctx []
			_4:
				{
					var importSpec *ImportSpecNode
					var semicolonTok Token
					switch p.c() {
					case IDENT, PERIOD, STRING:
						// ebnf.Sequence ImportSpec ";" ctx [IDENT, PERIOD, STRING]
						ix := p.ix
						// *ebnf.Name ImportSpec ctx [IDENT, PERIOD, STRING]
						if importSpec = p.importSpec(); importSpec == nil {
							p.back(ix)
							goto _5
						}
						// *ebnf.Token ";" ctx []
						if semicolonTok, ok = p.accept(SEMICOLON); !ok {
							p.back(ix)
							goto _5
						}
						list = append(list, struct {
							ImportSpec *ImportSpecNode
							SEMICOLON  Token
						}{ImportSpec: importSpec, SEMICOLON: semicolonTok})
						goto _4
					}
				_5:
				}
				// *ebnf.Option [ ImportSpec ] ctx []
				switch p.c() {
				case IDENT, PERIOD, STRING:
					// *ebnf.Name ImportSpec ctx [IDENT, PERIOD, STRING]
					if importSpec2 = p.importSpec(); importSpec2 == nil {
						goto _6
					}
				}
				goto _7
			_6:
				importSpec2 = nil
			_7:
				// *ebnf.Token ")" ctx []
				if rparenTok, ok = p.accept(RPAREN); !ok {
					p.back(ix)
					goto _2
				}
			}
			break
		_2:
			importSpec2 = nil
			lparenTok = Token{}
			rparenTok = Token{}
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
	}
	return &ImportDeclNode{
		IMPORT:      importTok,
		ImportSpec:  importSpec,
		LPAREN:      lparenTok,
		List:        list,
		ImportSpec2: importSpec2,
		RPAREN:      rparenTok,
	}
}

// ImportPathNode represents the production
//
//	ImportPath = string_lit .
type ImportPathNode struct {
	STRING Token
}

// Source implements Node.
func (n *ImportPathNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ImportPathNode) Position() token.Position { return n.STRING.Position() }

func (p *parser) importPath() *ImportPathNode {
	var (
		stringTok Token
	)
	// *ebnf.Name string_lit ctx [STRING]
	stringTok = p.expect(STRING)
	return &ImportPathNode{
		STRING: stringTok,
	}
}

// ImportSpecNode represents the production
//
//	ImportSpec = [ "." | PackageName ] ImportPath .
type ImportSpecNode struct {
	PERIOD      Token
	PackageName *PackageNameNode
	ImportPath  *ImportPathNode
}

// Source implements Node.
func (n *ImportSpecNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ImportSpecNode) Position() token.Position { panic("TODO") }

func (p *parser) importSpec() *ImportSpecNode {
	var (
		periodTok   Token
		packageName *PackageNameNode
		importPath  *ImportPathNode
	)
	// ebnf.Sequence [ "." | PackageName ] ImportPath ctx [IDENT, PERIOD, STRING]
	{
		ix := p.ix
		// *ebnf.Option [ "." | PackageName ] ctx [IDENT, PERIOD, STRING]
		switch p.c() {
		case IDENT, PERIOD:
			// ebnf.Alternative "." | PackageName ctx [IDENT, PERIOD]
			switch p.c() {
			case PERIOD: // 0
				// *ebnf.Token "." ctx [PERIOD]
				periodTok = p.expect(PERIOD)
			case IDENT: // 1
				// *ebnf.Name PackageName ctx [IDENT]
				if packageName = p.packageName(); packageName == nil {
					goto _4
				}
				break
			_4:
				packageName = nil
				goto _0
			default:
				goto _0
			}
		}
		goto _1
	_0:
		packageName = nil
		periodTok = Token{}
	_1:
		// *ebnf.Name ImportPath ctx []
		switch p.c() {
		case STRING:
			if importPath = p.importPath(); importPath == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ImportSpecNode{
		PERIOD:      periodTok,
		PackageName: packageName,
		ImportPath:  importPath,
	}
}

// IndexNode represents the production
//
//	Index = "[" Expression "]" .
type IndexNode struct {
	LBRACK     Token
	Expression Expression
	RBRACK     Token
}

// Source implements Node.
func (n *IndexNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *IndexNode) Position() token.Position { return n.LBRACK.Position() }

func (p *parser) index() *IndexNode {
	var (
		ok         bool
		lbrackTok  Token
		expression Expression
		rbrackTok  Token
	)
	// ebnf.Sequence "[" Expression "]" ctx [LBRACK]
	{
		switch p.peek(1) {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "[" ctx [LBRACK]
		lbrackTok = p.expect(LBRACK)
		// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if expression = p.expression(false); expression == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "]" ctx []
		if rbrackTok, ok = p.accept(RBRACK); !ok {
			p.back(ix)
			return nil
		}
	}
	return &IndexNode{
		LBRACK:     lbrackTok,
		Expression: expression,
		RBRACK:     rbrackTok,
	}
}

// InitStmtNode represents the production
//
//	InitStmt = SimpleStmt .
type InitStmtNode struct {
	SimpleStmt *SimpleStmtNode
}

// Source implements Node.
func (n *InitStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *InitStmtNode) Position() token.Position { return n.SimpleStmt.Position() }

func (p *parser) initStmt() *InitStmtNode {
	var (
		simpleStmt *SimpleStmtNode
	)
	// *ebnf.Name SimpleStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
	if simpleStmt = p.simpleStmt(true); simpleStmt == nil {
		return nil
	}
	return &InitStmtNode{
		SimpleStmt: simpleStmt,
	}
}

// InterfaceElemNode represents the production
//
//	InterfaceElem = MethodElem | TypeElem .
type InterfaceElemNode struct {
	MethodElem *MethodElemNode
	TypeElem   *TypeElemNode
}

// Source implements Node.
func (n *InterfaceElemNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *InterfaceElemNode) Position() token.Position { panic("TODO") }

func (p *parser) interfaceElem() *InterfaceElemNode {
	var (
		methodElem *MethodElemNode
		typeElem   *TypeElemNode
	)
	// ebnf.Alternative MethodElem | TypeElem ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
	switch p.c() {
	case IDENT: // 0 1
		// *ebnf.Name MethodElem ctx [IDENT]
		if methodElem = p.methodElem(); methodElem == nil {
			goto _0
		}
		break
	_0:
		methodElem = nil
		// *ebnf.Name TypeElem ctx [IDENT]
		if typeElem = p.typeElem(); typeElem == nil {
			goto _1
		}
		break
	_1:
		typeElem = nil
		return nil
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE: // 1
		// *ebnf.Name TypeElem ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
		if typeElem = p.typeElem(); typeElem == nil {
			goto _2
		}
		break
	_2:
		typeElem = nil
		return nil
	default:
		return nil
	}
	return &InterfaceElemNode{
		MethodElem: methodElem,
		TypeElem:   typeElem,
	}
}

// InterfaceTypeNode represents the production
//
//	InterfaceType = "interface" "{" { InterfaceElem ";" } [ InterfaceElem ] "}" .
type InterfaceTypeNode struct {
	INTERFACE Token
	LBRACE    Token
	List      []struct {
		InterfaceElem *InterfaceElemNode
		SEMICOLON     Token
	}
	InterfaceElem *InterfaceElemNode
	RBRACE        Token
}

// Source implements Node.
func (n *InterfaceTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *InterfaceTypeNode) Position() token.Position { return n.INTERFACE.Position() }

func (p *parser) interfaceType() *InterfaceTypeNode {
	var (
		ok           bool
		interfaceTok Token
		lbraceTok    Token
		list         []struct {
			InterfaceElem *InterfaceElemNode
			SEMICOLON     Token
		}
		interfaceElem *InterfaceElemNode
		rbraceTok     Token
	)
	// ebnf.Sequence "interface" "{" { InterfaceElem ";" } [ InterfaceElem ] "}" ctx [INTERFACE]
	{
		if p.peek(1) != LBRACE {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "interface" ctx [INTERFACE]
		interfaceTok = p.expect(INTERFACE)
		// *ebnf.Token "{" ctx [LBRACE]
		lbraceTok = p.expect(LBRACE)
		// *ebnf.Repetition { InterfaceElem ";" } ctx []
	_0:
		{
			var interfaceElem *InterfaceElemNode
			var semicolonTok Token
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
				// ebnf.Sequence InterfaceElem ";" ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
				ix := p.ix
				// *ebnf.Name InterfaceElem ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
				if interfaceElem = p.interfaceElem(); interfaceElem == nil {
					p.back(ix)
					goto _1
				}
				// *ebnf.Token ";" ctx []
				if semicolonTok, ok = p.accept(SEMICOLON); !ok {
					p.back(ix)
					goto _1
				}
				list = append(list, struct {
					InterfaceElem *InterfaceElemNode
					SEMICOLON     Token
				}{InterfaceElem: interfaceElem, SEMICOLON: semicolonTok})
				goto _0
			}
		_1:
		}
		// *ebnf.Option [ InterfaceElem ] ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
			// *ebnf.Name InterfaceElem ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
			if interfaceElem = p.interfaceElem(); interfaceElem == nil {
				goto _2
			}
		}
		goto _3
	_2:
		interfaceElem = nil
	_3:
		// *ebnf.Token "}" ctx []
		if rbraceTok, ok = p.accept(RBRACE); !ok {
			p.back(ix)
			return nil
		}
	}
	return &InterfaceTypeNode{
		INTERFACE:     interfaceTok,
		LBRACE:        lbraceTok,
		List:          list,
		InterfaceElem: interfaceElem,
		RBRACE:        rbraceTok,
	}
}

// KeyTypeNode represents the production
//
//	KeyType = Type .
type KeyTypeNode struct {
	Type *TypeNode
}

// Source implements Node.
func (n *KeyTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *KeyTypeNode) Position() token.Position { return n.Type.Position() }

func (p *parser) keyType() *KeyTypeNode {
	var (
		typeNode *TypeNode
	)
	// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	if typeNode = p.type1(); typeNode == nil {
		return nil
	}
	return &KeyTypeNode{
		Type: typeNode,
	}
}

// KeyedElementNode represents the production
//
//	KeyedElement = Element [ ":" Element ] .
type KeyedElementNode struct {
	Element  *ElementNode
	COLON    Token
	Element2 *ElementNode
}

// Source implements Node.
func (n *KeyedElementNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *KeyedElementNode) Position() token.Position { return n.Element.Position() }

func (p *parser) keyedElement() *KeyedElementNode {
	var (
		element  *ElementNode
		colonTok Token
		element2 *ElementNode
	)
	// ebnf.Sequence Element [ ":" Element ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name Element ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if element = p.element(); element == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ ":" Element ] ctx []
		switch p.c() {
		case COLON:
			// ebnf.Sequence ":" Element ctx [COLON]
			{
				switch p.peek(1) {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				default:
					goto _0
				}
				ix := p.ix
				// *ebnf.Token ":" ctx [COLON]
				colonTok = p.expect(COLON)
				// *ebnf.Name Element ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if element2 = p.element(); element2 == nil {
					p.back(ix)
					goto _0
				}
			}
		}
		goto _1
	_0:
		colonTok = Token{}
		element2 = nil
	_1:
	}
	return &KeyedElementNode{
		Element:  element,
		COLON:    colonTok,
		Element2: element2,
	}
}

// LabelNode represents the production
//
//	Label = identifier .
type LabelNode struct {
	IDENT Token
}

// Source implements Node.
func (n *LabelNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *LabelNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) label() *LabelNode {
	var (
		identTok Token
	)
	// *ebnf.Name identifier ctx [IDENT]
	identTok = p.expect(IDENT)
	return &LabelNode{
		IDENT: identTok,
	}
}

// LabeledStmtNode represents the production
//
//	LabeledStmt = Label ":" Statement .
type LabeledStmtNode struct {
	Label     *LabelNode
	COLON     Token
	Statement Node
}

// Source implements Node.
func (n *LabeledStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *LabeledStmtNode) Position() token.Position { return n.Label.Position() }

func (p *parser) labeledStmt() *LabeledStmtNode {
	var (
		label     *LabelNode
		colonTok  Token
		statement Node
	)
	// ebnf.Sequence Label ":" Statement ctx [IDENT]
	{
		if p.peek(1) != COLON {
			return nil
		}
		ix := p.ix
		// *ebnf.Name Label ctx [IDENT]
		if label = p.label(); label == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token ":" ctx [COLON]
		colonTok = p.expect(COLON)
		// *ebnf.Name Statement ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if statement = p.statement(); statement == nil {
				p.back(ix)
				return nil
			}
		}
	}
	return &LabeledStmtNode{
		Label:     label,
		COLON:     colonTok,
		Statement: statement,
	}
}

func (p *parser) literal() Expression {
	var (
		basicLit     Expression
		compositeLit *CompositeLitNode
		functionLit  *FunctionLitNode
	)
	// ebnf.Alternative BasicLit | CompositeLit | FunctionLit ctx [CHAR, FLOAT, FUNC, IMAG, INT, LBRACK, MAP, STRING, STRUCT]
	switch p.c() {
	case CHAR, FLOAT, IMAG, INT, STRING: // 0
		// *ebnf.Name BasicLit ctx [CHAR, FLOAT, IMAG, INT, STRING]
		if basicLit = p.basicLit(); basicLit == nil {
			return nil
		}
		return basicLit
	case LBRACK, MAP, STRUCT: // 1
		// *ebnf.Name CompositeLit ctx [LBRACK, MAP, STRUCT]
		if compositeLit = p.compositeLit(); compositeLit == nil {
			return nil
		}
		return compositeLit
	case FUNC: // 2
		// *ebnf.Name FunctionLit ctx [FUNC]
		if functionLit = p.functionLit(); functionLit == nil {
			return nil
		}
		return functionLit
	default:
		return nil
	}
}

// LiteralTypeNode represents the production
//
//	LiteralType = StructType | ArrayType | "[" "..." "]" ElementType | SliceType | MapType .
type LiteralTypeNode struct {
	StructType  *StructTypeNode
	ArrayType   *ArrayTypeNode
	LBRACK      Token
	ELLIPSIS    Token
	RBRACK      Token
	ElementType *ElementTypeNode
	SliceType   *SliceTypeNode
	MapType     *MapTypeNode
}

// Source implements Node.
func (n *LiteralTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *LiteralTypeNode) Position() token.Position { panic("TODO") }

func (p *parser) literalType() *LiteralTypeNode {
	var (
		ok          bool
		structType  *StructTypeNode
		arrayType   *ArrayTypeNode
		lbrackTok   Token
		ellipsisTok Token
		rbrackTok   Token
		elementType *ElementTypeNode
		sliceType   *SliceTypeNode
		mapType     *MapTypeNode
	)
	// ebnf.Alternative StructType | ArrayType | "[" "..." "]" ElementType | SliceType | MapType ctx [LBRACK, MAP, STRUCT]
	switch p.c() {
	case STRUCT: // 0
		// *ebnf.Name StructType ctx [STRUCT]
		if structType = p.structType(); structType == nil {
			goto _0
		}
		break
	_0:
		structType = nil
		return nil
	case LBRACK: // 1 2 3
		// *ebnf.Name ArrayType ctx [LBRACK]
		if arrayType = p.arrayType(); arrayType == nil {
			goto _2
		}
		break
	_2:
		arrayType = nil
		// ebnf.Sequence "[" "..." "]" ElementType ctx [LBRACK]
		{
			if p.peek(1) != ELLIPSIS {
				goto _3
			}
			ix := p.ix
			// *ebnf.Token "[" ctx [LBRACK]
			lbrackTok = p.expect(LBRACK)
			// *ebnf.Token "..." ctx [ELLIPSIS]
			ellipsisTok = p.expect(ELLIPSIS)
			// *ebnf.Token "]" ctx []
			if rbrackTok, ok = p.accept(RBRACK); !ok {
				p.back(ix)
				goto _3
			}
			// *ebnf.Name ElementType ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if elementType = p.elementType(); elementType == nil {
					p.back(ix)
					goto _3
				}
			default:
				p.back(ix)
				goto _3
			}
		}
		break
	_3:
		elementType = nil
		ellipsisTok = Token{}
		lbrackTok = Token{}
		rbrackTok = Token{}
		// *ebnf.Name SliceType ctx [LBRACK]
		if sliceType = p.sliceType(); sliceType == nil {
			goto _4
		}
		break
	_4:
		sliceType = nil
		return nil
	case MAP: // 4
		// *ebnf.Name MapType ctx [MAP]
		if mapType = p.mapType(); mapType == nil {
			goto _5
		}
		break
	_5:
		mapType = nil
		return nil
	default:
		return nil
	}
	return &LiteralTypeNode{
		StructType:  structType,
		ArrayType:   arrayType,
		LBRACK:      lbrackTok,
		ELLIPSIS:    ellipsisTok,
		RBRACK:      rbrackTok,
		ElementType: elementType,
		SliceType:   sliceType,
		MapType:     mapType,
	}
}

// LiteralValueNode represents the production
//
//	LiteralValue = "{" { KeyedElement "," } "}" .
type LiteralValueNode struct {
	LBRACE Token
	List   []struct {
		KeyedElement *KeyedElementNode
		COMMA        Token
	}
	RBRACE Token
}

// Source implements Node.
func (n *LiteralValueNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *LiteralValueNode) Position() token.Position { return n.LBRACE.Position() }

func (p *parser) literalValue() *LiteralValueNode {
	var (
		ok        bool
		lbraceTok Token
		list      []struct {
			KeyedElement *KeyedElementNode
			COMMA        Token
		}
		keyedElement *KeyedElementNode
		rbraceTok    Token
	)
	// ebnf.Sequence "{" [ ElementList [ "," ] ] "}" ctx [LBRACE]
	ix := p.ix
	// *ebnf.Token "{" ctx [LBRACE]
	lbraceTok = p.expect(LBRACE)
	for {
		// *ebnf.Option [ ElementList [ "," ] ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			{
				ix := p.ix
				if keyedElement = p.keyedElement(); keyedElement == nil {
					p.back(ix)
					goto _1
				}
				switch p.c() {
				case COMMA:
					// *ebnf.Token "," ctx [COMMA]
					list = append(list, struct {
						KeyedElement *KeyedElementNode
						COMMA        Token
					}{keyedElement, p.consume()})
					continue
				case RBRACE:
					list = append(list, struct {
						KeyedElement *KeyedElementNode
						COMMA        Token
					}{KeyedElement: keyedElement})
				}
			}
		}
		goto _1
	}
_1:
	// *ebnf.Token "}" ctx []
	if rbraceTok, ok = p.accept(RBRACE); !ok {
		p.back(ix)
		return nil
	}
	return &LiteralValueNode{
		LBRACE: lbraceTok,
		List:   list,
		RBRACE: rbraceTok,
	}
}

func (p *parser) logicalAndExpression(preBlock bool) (r Expression) {
	var relationalExpression Expression
	// ebnf.Sequence RelationalExpression { "&&" RelationalExpression } ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name RelationalExpression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if relationalExpression = p.relationalExpression(preBlock); relationalExpression == nil {
			p.back(ix)
			return nil
		}
		r = relationalExpression
		// *ebnf.Repetition { "&&" RelationalExpression } ctx []
	_0:
		{
			var landTok Token
			var relationalExpression Expression
			switch p.c() {
			case LAND:
				// ebnf.Sequence "&&" RelationalExpression ctx [LAND]
				switch p.peek(1) {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				default:
					goto _1
				}
				ix := p.ix
				// *ebnf.Token "&&" ctx [LAND]
				landTok = p.expect(LAND)
				// *ebnf.Name RelationalExpression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if relationalExpression = p.relationalExpression(preBlock); relationalExpression == nil {
					p.back(ix)
					goto _1
				}
				r = &BinaryExpression{r, landTok, relationalExpression}
				goto _0
			}
		_1:
		}
	}
	return r
}

// MapTypeNode represents the production
//
//	MapType = "map" "[" KeyType "]" ElementType .
type MapTypeNode struct {
	MAP         Token
	LBRACK      Token
	KeyType     *KeyTypeNode
	RBRACK      Token
	ElementType *ElementTypeNode
}

// Source implements Node.
func (n *MapTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *MapTypeNode) Position() token.Position { return n.MAP.Position() }

func (p *parser) mapType() *MapTypeNode {
	var (
		ok          bool
		mapTok      Token
		lbrackTok   Token
		keyType     *KeyTypeNode
		rbrackTok   Token
		elementType *ElementTypeNode
	)
	// ebnf.Sequence "map" "[" KeyType "]" ElementType ctx [MAP]
	{
		if p.peek(1) != LBRACK {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "map" ctx [MAP]
		mapTok = p.expect(MAP)
		// *ebnf.Token "[" ctx [LBRACK]
		lbrackTok = p.expect(LBRACK)
		// *ebnf.Name KeyType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if keyType = p.keyType(); keyType == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "]" ctx []
		if rbrackTok, ok = p.accept(RBRACK); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ElementType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if elementType = p.elementType(); elementType == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &MapTypeNode{
		MAP:         mapTok,
		LBRACK:      lbrackTok,
		KeyType:     keyType,
		RBRACK:      rbrackTok,
		ElementType: elementType,
	}
}

// MethodDeclNode represents the production
//
//	MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
type MethodDeclNode struct {
	FUNC         Token
	Receiver     *ReceiverNode
	MethodName   *MethodNameNode
	Signature    *SignatureNode
	FunctionBody *FunctionBodyNode
}

// Source implements Node.
func (n *MethodDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *MethodDeclNode) Position() token.Position { return n.FUNC.Position() }

func (p *parser) methodDecl() *MethodDeclNode {
	var (
		funcTok      Token
		receiver     *ReceiverNode
		methodName   *MethodNameNode
		signature    *SignatureNode
		functionBody *FunctionBodyNode
	)
	// ebnf.Sequence "func" Receiver MethodName Signature [ FunctionBody ] ctx [FUNC]
	{
		switch p.peek(1) {
		case LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "func" ctx [FUNC]
		funcTok = p.expect(FUNC)
		// *ebnf.Name Receiver ctx [LPAREN]
		if receiver = p.receiver(); receiver == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Name MethodName ctx []
		switch p.c() {
		case IDENT:
			if methodName = p.methodName(); methodName == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Name Signature ctx []
		switch p.c() {
		case LPAREN:
			if signature = p.signature(); signature == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ FunctionBody ] ctx []
		switch p.c() {
		case LBRACE:
			// *ebnf.Name FunctionBody ctx [LBRACE]
			if functionBody = p.functionBody(); functionBody == nil {
				goto _0
			}
		}
		goto _1
	_0:
		functionBody = nil
	_1:
	}
	return &MethodDeclNode{
		FUNC:         funcTok,
		Receiver:     receiver,
		MethodName:   methodName,
		Signature:    signature,
		FunctionBody: functionBody,
	}
}

// MethodElemNode represents the production
//
//	MethodElem = MethodName Signature .
type MethodElemNode struct {
	MethodName *MethodNameNode
	Signature  *SignatureNode
}

// Source implements Node.
func (n *MethodElemNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *MethodElemNode) Position() token.Position { return n.MethodName.Position() }

func (p *parser) methodElem() *MethodElemNode {
	var (
		methodName *MethodNameNode
		signature  *SignatureNode
	)
	// ebnf.Sequence MethodName Signature ctx [IDENT]
	{
		switch p.peek(1) {
		case LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Name MethodName ctx [IDENT]
		if methodName = p.methodName(); methodName == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Signature ctx [LPAREN]
		if signature = p.signature(); signature == nil {
			p.back(ix)
			return nil
		}
	}
	return &MethodElemNode{
		MethodName: methodName,
		Signature:  signature,
	}
}

// MethodExprNode represents the production
//
//	MethodExpr = ReceiverType "." MethodName .
type MethodExprNode struct {
	ReceiverType *ReceiverTypeNode
	PERIOD       Token
	MethodName   *MethodNameNode
}

// Source implements Node.
func (n *MethodExprNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *MethodExprNode) Position() token.Position { return n.ReceiverType.Position() }

func (p *parser) methodExpr() *MethodExprNode {
	var (
		ok           bool
		receiverType *ReceiverTypeNode
		periodTok    Token
		methodName   *MethodNameNode
	)
	// ebnf.Sequence ReceiverType "." MethodName ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	{
		ix := p.ix
		// *ebnf.Name ReceiverType ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if receiverType = p.receiverType(); receiverType == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "." ctx []
		if periodTok, ok = p.accept(PERIOD); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name MethodName ctx []
		switch p.c() {
		case IDENT:
			if methodName = p.methodName(); methodName == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &MethodExprNode{
		ReceiverType: receiverType,
		PERIOD:       periodTok,
		MethodName:   methodName,
	}
}

// MethodNameNode represents the production
//
//	MethodName = identifier .
type MethodNameNode struct {
	IDENT Token
}

// Source implements Node.
func (n *MethodNameNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *MethodNameNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) methodName() *MethodNameNode {
	var (
		identTok Token
	)
	// *ebnf.Name identifier ctx [IDENT]
	identTok = p.expect(IDENT)
	return &MethodNameNode{
		IDENT: identTok,
	}
}

func (p *parser) multiplicativeExpression(preBlock bool) (r Expression) {
	var unaryExpr Expression
	// ebnf.Sequence UnaryExpr { ( "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) UnaryExpr } ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name UnaryExpr ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if unaryExpr = p.unaryExpr(preBlock); unaryExpr == nil {
			p.back(ix)
			return nil
		}
		r = unaryExpr
		// *ebnf.Repetition { ( "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) UnaryExpr } ctx []
	_0:
		{
			var op Token
			var unaryExpr Expression
			switch p.c() {
			case AND, AND_NOT, MUL, QUO, REM, SHL, SHR:
				// ebnf.Sequence ( "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) UnaryExpr ctx [AND, AND_NOT, MUL, QUO, REM, SHL, SHR]
				// *ebnf.Group ( "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) ctx [AND, AND_NOT, MUL, QUO, REM, SHL, SHR]
				// ebnf.Alternative "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ctx [AND, AND_NOT, MUL, QUO, REM, SHL, SHR]
				op = p.consume()
				// *ebnf.Name UnaryExpr ctx []
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if unaryExpr = p.unaryExpr(preBlock); unaryExpr == nil {
						p.back(ix)
						goto _1
					}
				default:
					p.back(ix)
					goto _1
				}
				r = &BinaryExpression{r, op, unaryExpr}
				goto _0
			}
		_1:
		}
	}
	return r
}

// OperandNode represents the production
//
//	Operand = Literal | OperandName [ TypeArgs ] [ LiteralValue ] | "(" Expression ")" .
type OperandNode struct {
	OperandName  *OperandNameNode
	TypeArgs     *TypeArgsNode
	LiteralValue *LiteralValueNode
}

// Source implements Node.
func (n *OperandNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *OperandNode) Position() token.Position { return n.OperandName.Position() }

type ParenthesizedExpression struct {
	LPAREN     Token
	Expression Expression
	RPAREN     Token
}

// Source implements Node.
func (n *ParenthesizedExpression) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ParenthesizedExpression) Position() token.Position { return n.LPAREN.Position() }

func (p *parser) operand(preBlock bool) Expression {
	var (
		ok           bool
		literal      Expression
		operandName  *OperandNameNode
		typeArgs     *TypeArgsNode
		literalValue *LiteralValueNode
		lparenTok    Token
		expression   Expression
		rparenTok    Token
	)
	// ebnf.Alternative Literal | OperandName [ TypeArgs ] [ LiteralValue ] | "(" Expression ")" ctx [CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, LPAREN, MAP, STRING, STRUCT]
	switch p.c() {
	case CHAR, FLOAT, FUNC, IMAG, INT, LBRACK, MAP, STRING, STRUCT: // 0
		// *ebnf.Name Literal ctx [CHAR, FLOAT, FUNC, IMAG, INT, LBRACK, MAP, STRING, STRUCT]
		if literal = p.literal(); literal == nil {
			return nil
		}
		return literal
	case IDENT: // 1
		// ebnf.Sequence OperandName [ TypeArgs ] [ LiteralValue ] ctx [IDENT]
		{
			ix := p.ix
			// *ebnf.Name OperandName ctx [IDENT]
			if operandName = p.operandName(); operandName == nil {
				p.back(ix)
				goto _2
			}
			// *ebnf.Option [ TypeArgs ] ctx []
			switch p.c() {
			case LBRACK:
				// *ebnf.Name TypeArgs ctx [LBRACK]
				if typeArgs = p.typeArgs(); typeArgs == nil {
					goto _4
				}
			}
			goto _5
		_4:
			typeArgs = nil
		_5:
			if !preBlock {
				// *ebnf.Option [ LiteralValue ] ctx []
				switch p.c() {
				case LBRACE:
					// *ebnf.Name LiteralValue ctx [LBRACE]
					if literalValue = p.literalValue(); literalValue == nil {
						goto _6
					}
				}
				goto _7
			_6:
				literalValue = nil
			_7:
			}
		}
		break
	_2:
		literalValue = nil
		operandName = nil
		typeArgs = nil
		return nil
	case LPAREN: // 2
		// ebnf.Sequence "(" Expression ")" ctx [LPAREN]
		{
			switch p.peek(1) {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			default:
				goto _8
			}
			ix := p.ix
			// *ebnf.Token "(" ctx [LPAREN]
			lparenTok = p.expect(LPAREN)
			// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expression = p.expression(false); expression == nil {
				p.back(ix)
				goto _8
			}
			// *ebnf.Token ")" ctx []
			if rparenTok, ok = p.accept(RPAREN); !ok {
				p.back(ix)
				goto _8
			}
		}
		return &ParenthesizedExpression{lparenTok, expression, rparenTok}
	_8:
		expression = nil
		lparenTok = Token{}
		rparenTok = Token{}
		return nil
	default:
		return nil
	}
	return &OperandNode{
		OperandName:  operandName,
		TypeArgs:     typeArgs,
		LiteralValue: literalValue,
	}
}

// OperandNameNode represents the production
//
//	OperandName = QualifiedIdent | identifier .
type OperandNameNode struct {
	QualifiedIdent *QualifiedIdentNode
	IDENT          Token
}

// Source implements Node.
func (n *OperandNameNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *OperandNameNode) Position() token.Position {
	if n.QualifiedIdent != nil {
		return n.QualifiedIdent.Position()
	}

	return n.IDENT.Position()
}

func (p *parser) operandName() *OperandNameNode {
	var (
		qualifiedIdent *QualifiedIdentNode
		identTok       Token
	)
	// ebnf.Alternative QualifiedIdent | identifier ctx [IDENT]
	switch p.c() {
	case IDENT: // 0 1
		// *ebnf.Name QualifiedIdent ctx [IDENT]
		if qualifiedIdent = p.qualifiedIdent(); qualifiedIdent == nil {
			goto _0
		}
		break
	_0:
		qualifiedIdent = nil
		// *ebnf.Name identifier ctx [IDENT]
		identTok = p.expect(IDENT)
		break
		return nil
	default:
		return nil
	}
	return &OperandNameNode{
		QualifiedIdent: qualifiedIdent,
		IDENT:          identTok,
	}
}

// PackageClauseNode represents the production
//
//	PackageClause = "package" PackageName .
type PackageClauseNode struct {
	PACKAGE     Token
	PackageName *PackageNameNode
}

// Source implements Node.
func (n *PackageClauseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *PackageClauseNode) Position() token.Position { return n.PACKAGE.Position() }

func (p *parser) packageClause() *PackageClauseNode {
	var (
		packageTok  Token
		packageName *PackageNameNode
	)
	// ebnf.Sequence "package" PackageName ctx [PACKAGE]
	{
		if p.peek(1) != IDENT {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "package" ctx [PACKAGE]
		packageTok = p.expect(PACKAGE)
		// *ebnf.Name PackageName ctx [IDENT]
		if packageName = p.packageName(); packageName == nil {
			p.back(ix)
			return nil
		}
	}
	return &PackageClauseNode{
		PACKAGE:     packageTok,
		PackageName: packageName,
	}
}

// PackageNameNode represents the production
//
//	PackageName = identifier .
type PackageNameNode struct {
	IDENT Token
}

// Source implements Node.
func (n *PackageNameNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *PackageNameNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) packageName() *PackageNameNode {
	var (
		identTok Token
	)
	// *ebnf.Name identifier ctx [IDENT]
	identTok = p.expect(IDENT)
	return &PackageNameNode{
		IDENT: identTok,
	}
}

// ParameterDeclNode represents the production
//
//	ParameterDecl = identifier "..." Type | identifier Type | "..." Type | Type .
type ParameterDeclNode struct {
	IDENT    Token
	ELLIPSIS Token
	Type     *TypeNode
}

// Source implements Node.
func (n *ParameterDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ParameterDeclNode) Position() token.Position { panic("TODO") }

func (p *parser) parameterDecl() *ParameterDeclNode {
	var (
		identTok    Token
		ellipsisTok Token
		typeNode    *TypeNode
	)
	// ebnf.Alternative identifier "..." Type | identifier Type | "..." Type | Type ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	switch p.c() {
	case IDENT: // 0 1 3
		// ebnf.Sequence identifier "..." Type ctx [IDENT]
		{
			if p.peek(1) != ELLIPSIS {
				goto _0
			}
			ix := p.ix
			// *ebnf.Name identifier ctx [IDENT]
			identTok = p.expect(IDENT)
			// *ebnf.Token "..." ctx [ELLIPSIS]
			ellipsisTok = p.expect(ELLIPSIS)
			// *ebnf.Name Type ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if typeNode = p.type1(); typeNode == nil {
					p.back(ix)
					goto _0
				}
			default:
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
		ellipsisTok = Token{}
		identTok = Token{}
		typeNode = nil
		// ebnf.Sequence identifier Type ctx [IDENT]
		{
			switch p.peek(1) {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			default:
				goto _1
			}
			ix := p.ix
			// *ebnf.Name identifier ctx [IDENT]
			identTok = p.expect(IDENT)
			// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if typeNode = p.type1(); typeNode == nil {
				p.back(ix)
				goto _1
			}
		}
		break
	_1:
		identTok = Token{}
		typeNode = nil
		// *ebnf.Name Type ctx [IDENT]
		if typeNode = p.type1(); typeNode == nil {
			goto _2
		}
		break
	_2:
		typeNode = nil
		return nil
	case ELLIPSIS: // 2
		// ebnf.Sequence "..." Type ctx [ELLIPSIS]
		{
			switch p.peek(1) {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			default:
				goto _3
			}
			ix := p.ix
			// *ebnf.Token "..." ctx [ELLIPSIS]
			ellipsisTok = p.expect(ELLIPSIS)
			// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if typeNode = p.type1(); typeNode == nil {
				p.back(ix)
				goto _3
			}
		}
		break
	_3:
		ellipsisTok = Token{}
		typeNode = nil
		return nil
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 3
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if typeNode = p.type1(); typeNode == nil {
			goto _5
		}
		break
	_5:
		typeNode = nil
		return nil
	default:
		return nil
	}
	return &ParameterDeclNode{
		IDENT:    identTok,
		ELLIPSIS: ellipsisTok,
		Type:     typeNode,
	}
}

// ParametersNode represents the production
//
//	Parameters = "(" { ParameterDecl  "," } ")" .
type ParametersNode struct {
	LPAREN Token
	List   []struct {
		ParameterDecl *ParameterDeclNode
		COMMA         Token
	}
	RPAREN Token
}

// Source implements Node.
func (n *ParametersNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ParametersNode) Position() token.Position { return n.LPAREN.Position() }

func (p *parser) parameters() *ParametersNode {
	var (
		ok            bool
		lparenTok     Token
		parameterDecl *ParameterDeclNode
		list          []struct {
			ParameterDecl *ParameterDeclNode
			COMMA         Token
		}
		rparenTok Token
	)
	ix := p.ix
	// *ebnf.Token "(" ctx [LPAREN]
	lparenTok = p.expect(LPAREN)
	for {
		// *ebnf.Option [ ParameterList [ "," ] ] ctx []
		switch p.c() {
		case ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			// ebnf.Sequence ParameterList [ "," ] ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			{
				ix := p.ix
				if parameterDecl = p.parameterDecl(); parameterDecl == nil {
					p.back(ix)
					goto _1
				}
				// *ebnf.Option [ "," ] ctx []
				switch p.c() {
				case COMMA:
					// *ebnf.Token "," ctx [COMMA]
					list = append(list, struct {
						ParameterDecl *ParameterDeclNode
						COMMA         Token
					}{parameterDecl, p.consume()})
					continue
				case RPAREN:
					list = append(list, struct {
						ParameterDecl *ParameterDeclNode
						COMMA         Token
					}{ParameterDecl: parameterDecl})
				}
			}
		}
		goto _1
	}
_1:
	// *ebnf.Token ")" ctx []
	if rparenTok, ok = p.accept(RPAREN); !ok {
		p.back(ix)
		return nil
	}
	return &ParametersNode{
		LPAREN: lparenTok,
		List:   list,
		RPAREN: rparenTok,
	}
}

// PointerTypeNode represents the production
//
//	PointerType = "*" BaseType .
type PointerTypeNode struct {
	MUL      Token
	BaseType *BaseTypeNode
}

// Source implements Node.
func (n *PointerTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *PointerTypeNode) Position() token.Position { return n.MUL.Position() }

func (p *parser) pointerType() *PointerTypeNode {
	var (
		mulTok   Token
		baseType *BaseTypeNode
	)
	// ebnf.Sequence "*" BaseType ctx [MUL]
	{
		switch p.peek(1) {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "*" ctx [MUL]
		mulTok = p.expect(MUL)
		// *ebnf.Name BaseType ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if baseType = p.baseType(); baseType == nil {
			p.back(ix)
			return nil
		}
	}
	return &PointerTypeNode{
		MUL:      mulTok,
		BaseType: baseType,
	}
}

// PostStmtNode represents the production
//
//	PostStmt = SimpleStmt .
type PostStmtNode struct {
	SimpleStmt *SimpleStmtNode
}

// Source implements Node.
func (n *PostStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *PostStmtNode) Position() token.Position { return n.SimpleStmt.Position() }

func (p *parser) postStmt() *PostStmtNode {
	var (
		simpleStmt *SimpleStmtNode
	)
	// *ebnf.Name SimpleStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
	if simpleStmt = p.simpleStmt(true); simpleStmt == nil {
		return nil
	}
	return &PostStmtNode{
		SimpleStmt: simpleStmt,
	}
}

// PrimaryExprNode represents the production
//
//	PrimaryExpr = ( Operand | Conversion | MethodExpr ) { Selector | Index | Slice | TypeAssertion | Arguments } .
type PrimaryExprNode struct {
	Operand Expression
	List    []Node
}

// Source implements Node.
func (n *PrimaryExprNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *PrimaryExprNode) Position() token.Position { return n.Operand.Position() }

func (p *parser) primaryExpr(preBlock bool) Expression {
	var (
		item0      Expression
		operand    Expression
		conversion *ConversionNode
		methodExpr *MethodExprNode
		list       []Node
	)
	// ebnf.Sequence ( Operand | Conversion | MethodExpr ) { Selector | Index | Slice | TypeAssertion | Arguments } ctx [ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT]
	{
		ix := p.ix
		// *ebnf.Group ( Operand | Conversion | MethodExpr ) ctx [ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT]
		// ebnf.Alternative Operand | Conversion | MethodExpr ctx [ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT]
		switch p.c() {
		case CHAR, FLOAT, IMAG, INT, STRING: // 0
			// *ebnf.Name Operand ctx [CHAR, FLOAT, IMAG, INT, STRING]
			if operand = p.operand(preBlock); operand == nil {
				goto _0
			}
			item0 = operand
			break
		_0:
			operand = nil
			p.back(ix)
			return nil
		case FUNC, IDENT, LBRACK, LPAREN, MAP, STRUCT: // 0 1 2
			// *ebnf.Name Operand ctx [FUNC, IDENT, LBRACK, LPAREN, MAP, STRUCT]
			if operand = p.operand(preBlock); operand == nil {
				goto _2
			}
			item0 = operand
			break
		_2:
			operand = nil
			// *ebnf.Name Conversion ctx [FUNC, IDENT, LBRACK, LPAREN, MAP, STRUCT]
			if conversion = p.conversion(); conversion == nil {
				goto _3
			}
			item0 = conversion
			break
		_3:
			conversion = nil
			// *ebnf.Name MethodExpr ctx [FUNC, IDENT, LBRACK, LPAREN, MAP, STRUCT]
			if methodExpr = p.methodExpr(); methodExpr == nil {
				goto _4
			}
			item0 = methodExpr
			break
		_4:
			methodExpr = nil
			p.back(ix)
			return nil
		case ARROW, CHAN, INTERFACE, MUL: // 1 2
			// *ebnf.Name Conversion ctx [ARROW, CHAN, INTERFACE, MUL]
			if conversion = p.conversion(); conversion == nil {
				goto _5
			}
			item0 = conversion
			break
		_5:
			conversion = nil
			// *ebnf.Name MethodExpr ctx [ARROW, CHAN, INTERFACE, MUL]
			if methodExpr = p.methodExpr(); methodExpr == nil {
				goto _6
			}
			item0 = methodExpr
			break
		_6:
			methodExpr = nil
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { Selector | Index | Slice | TypeAssertion | Arguments } ctx []
	_7:
		{
			var item Node
			var selector *SelectorNode
			var index *IndexNode
			var slice *SliceNode
			var typeAssertion *TypeAssertionNode
			var arguments *ArgumentsNode
			switch p.c() {
			case LBRACK, LPAREN, PERIOD:
				// ebnf.Alternative Selector | Index | Slice | TypeAssertion | Arguments ctx [LBRACK, LPAREN, PERIOD]
				switch p.c() {
				case PERIOD: // 0 3
					// *ebnf.Name Selector ctx [PERIOD]
					if selector = p.selector(); selector == nil {
						goto _9
					}
					item = selector
					break
				_9:
					// *ebnf.Name TypeAssertion ctx [PERIOD]
					if typeAssertion = p.typeAssertion(); typeAssertion == nil {
						goto _10
					}
					item = typeAssertion
					break
				_10:
					goto _8
				case LBRACK: // 1 2
					// *ebnf.Name Index ctx [LBRACK]
					if index = p.index(); index == nil {
						goto _11
					}
					item = index
					break
				_11:
					// *ebnf.Name Slice ctx [LBRACK]
					if slice = p.slice(); slice == nil {
						goto _12
					}
					item = slice
					break
				_12:
					goto _8
				case LPAREN: // 4
					// *ebnf.Name Arguments ctx [LPAREN]
					if arguments = p.arguments(); arguments == nil {
						goto _13
					}
					item = arguments
					break
				_13:
					goto _8
				default:
					goto _8
				}
				list = append(list, item)
				goto _7
			}
		_8:
		}
	}
	if len(list) == 0 {
		return item0
	}

	return &PrimaryExprNode{
		Operand: item0,
		List:    list,
	}
}

// QualifiedIdentNode represents the production
//
//	QualifiedIdent = PackageName "." identifier .
type QualifiedIdentNode struct {
	lexicalScoper
	PackageName *PackageNameNode
	PERIOD      Token
	IDENT       Token
}

// Source implements Node.
func (n *QualifiedIdentNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *QualifiedIdentNode) Position() token.Position { return n.PackageName.Position() }

func (p *parser) qualifiedIdent() *QualifiedIdentNode {
	var (
		ok          bool
		packageName *PackageNameNode
		periodTok   Token
		identTok    Token
	)
	// ebnf.Sequence PackageName "." identifier ctx [IDENT]
	{
		if p.peek(1) != PERIOD {
			return nil
		}
		ix := p.ix
		// *ebnf.Name PackageName ctx [IDENT]
		if packageName = p.packageName(); packageName == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "." ctx [PERIOD]
		periodTok = p.expect(PERIOD)
		// *ebnf.Name identifier ctx []
		if identTok, ok = p.accept(IDENT); !ok {
			p.back(ix)
			return nil
		}
	}
	return &QualifiedIdentNode{
		lexicalScoper: newLexicalScoper(p.sc),
		PackageName:   packageName,
		PERIOD:        periodTok,
		IDENT:         identTok,
	}
}

// RangeClauseNode represents the production
//
//	RangeClause = "range" Expression | ExpressionList "=" "range" Expression | IdentifierList ":=" "range" Expression .
type RangeClauseNode struct {
	RANGE          Token
	Expression     Expression
	ExpressionList *ExpressionListNode
	ASSIGN         Token
	IdentifierList *IdentifierListNode
	DEFINE         Token
}

// Source implements Node.
func (n *RangeClauseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *RangeClauseNode) Position() token.Position { panic("TODO") }

func (p *parser) rangeClause() *RangeClauseNode {
	var (
		ok             bool
		rangeTok       Token
		expression     Expression
		expressionList *ExpressionListNode
		assignTok      Token
		identifierList *IdentifierListNode
		defineTok      Token
	)
	// ebnf.Alternative "range" Expression | ExpressionList "=" "range" Expression | IdentifierList ":=" "range" Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, STRING, STRUCT, SUB, XOR]
	switch p.c() {
	case RANGE: // 0
		// ebnf.Sequence "range" Expression ctx [RANGE]
		{
			switch p.peek(1) {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			default:
				goto _0
			}
			ix := p.ix
			// *ebnf.Token "range" ctx [RANGE]
			rangeTok = p.expect(RANGE)
			// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expression = p.expression(true); expression == nil {
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
		expression = nil
		rangeTok = Token{}
		return nil
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 1
		// ebnf.Sequence ExpressionList "=" "range" Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		{
			ix := p.ix
			// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expressionList = p.expressionList(false); expressionList == nil {
				p.back(ix)
				goto _2
			}
			// *ebnf.Token "=" ctx []
			if assignTok, ok = p.accept(ASSIGN); !ok {
				p.back(ix)
				goto _2
			}
			// *ebnf.Token "range" ctx []
			if rangeTok, ok = p.accept(RANGE); !ok {
				p.back(ix)
				goto _2
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expression = p.expression(true); expression == nil {
					p.back(ix)
					goto _2
				}
			default:
				p.back(ix)
				goto _2
			}
		}
		break
	_2:
		assignTok = Token{}
		expressionList = nil
		expression = nil
		rangeTok = Token{}
		return nil
	case IDENT: // 1 2
		// ebnf.Sequence ExpressionList "=" "range" Expression ctx [IDENT]
		{
			ix := p.ix
			// *ebnf.Name ExpressionList ctx [IDENT]
			if expressionList = p.expressionList(false); expressionList == nil {
				p.back(ix)
				goto _4
			}
			// *ebnf.Token "=" ctx []
			if assignTok, ok = p.accept(ASSIGN); !ok {
				p.back(ix)
				goto _4
			}
			// *ebnf.Token "range" ctx []
			if rangeTok, ok = p.accept(RANGE); !ok {
				p.back(ix)
				goto _4
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expression = p.expression(true); expression == nil {
					p.back(ix)
					goto _4
				}
			default:
				p.back(ix)
				goto _4
			}
		}
		break
	_4:
		assignTok = Token{}
		expressionList = nil
		expression = nil
		rangeTok = Token{}
		// ebnf.Sequence IdentifierList ":=" "range" Expression ctx [IDENT]
		{
			ix := p.ix
			// *ebnf.Name IdentifierList ctx [IDENT]
			if identifierList = p.identifierList(); identifierList == nil {
				p.back(ix)
				goto _5
			}
			// *ebnf.Token ":=" ctx []
			if defineTok, ok = p.accept(DEFINE); !ok {
				p.back(ix)
				goto _5
			}
			// *ebnf.Token "range" ctx []
			if rangeTok, ok = p.accept(RANGE); !ok {
				p.back(ix)
				goto _5
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expression = p.expression(true); expression == nil {
					p.back(ix)
					goto _5
				}
			default:
				p.back(ix)
				goto _5
			}
		}
		break
	_5:
		defineTok = Token{}
		expression = nil
		identifierList = nil
		rangeTok = Token{}
		return nil
	default:
		return nil
	}
	return &RangeClauseNode{
		RANGE:          rangeTok,
		Expression:     expression,
		ExpressionList: expressionList,
		ASSIGN:         assignTok,
		IdentifierList: identifierList,
		DEFINE:         defineTok,
	}
}

// ReceiverNode represents the production
//
//	Receiver = Parameters .
type ReceiverNode struct {
	Parameters *ParametersNode
}

// Source implements Node.
func (n *ReceiverNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ReceiverNode) Position() token.Position { return n.Parameters.Position() }

func (p *parser) receiver() *ReceiverNode {
	var (
		parameters *ParametersNode
	)
	// *ebnf.Name Parameters ctx [LPAREN]
	if parameters = p.parameters(); parameters == nil {
		return nil
	}
	return &ReceiverNode{
		Parameters: parameters,
	}
}

// ReceiverTypeNode represents the production
//
//	ReceiverType = Type .
type ReceiverTypeNode struct {
	Type *TypeNode
}

// Source implements Node.
func (n *ReceiverTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ReceiverTypeNode) Position() token.Position { return n.Type.Position() }

func (p *parser) receiverType() *ReceiverTypeNode {
	var (
		typeNode *TypeNode
	)
	// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	if typeNode = p.type1(); typeNode == nil {
		return nil
	}
	return &ReceiverTypeNode{
		Type: typeNode,
	}
}

// RecvExprNode represents the production
//
//	RecvExpr = Expression .
type RecvExprNode struct {
	Expression Expression
}

// Source implements Node.
func (n *RecvExprNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *RecvExprNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) recvExpr() *RecvExprNode {
	var (
		expression Expression
	)
	// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	if expression = p.expression(false); expression == nil {
		return nil
	}
	return &RecvExprNode{
		Expression: expression,
	}
}

// RecvStmtNode represents the production
//
//	RecvStmt = [ ExpressionList "=" | IdentifierList ":=" ] RecvExpr .
type RecvStmtNode struct {
	ExpressionList *ExpressionListNode
	ASSIGN         Token
	IdentifierList *IdentifierListNode
	DEFINE         Token
	RecvExpr       *RecvExprNode
}

// Source implements Node.
func (n *RecvStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *RecvStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) recvStmt() *RecvStmtNode {
	var (
		ok             bool
		expressionList *ExpressionListNode
		assignTok      Token
		identifierList *IdentifierListNode
		defineTok      Token
		recvExpr       *RecvExprNode
	)
	// ebnf.Sequence [ ExpressionList "=" | IdentifierList ":=" ] RecvExpr ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Option [ ExpressionList "=" | IdentifierList ":=" ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		// ebnf.Alternative ExpressionList "=" | IdentifierList ":=" ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0
			// ebnf.Sequence ExpressionList "=" ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expressionList = p.expressionList(false); expressionList == nil {
					p.back(ix)
					goto _2
				}
				// *ebnf.Token "=" ctx []
				if assignTok, ok = p.accept(ASSIGN); !ok {
					p.back(ix)
					goto _2
				}
			}
			break
		_2:
			assignTok = Token{}
			expressionList = nil
			goto _0
		case IDENT: // 0 1
			// ebnf.Sequence ExpressionList "=" ctx [IDENT]
			{
				ix := p.ix
				// *ebnf.Name ExpressionList ctx [IDENT]
				if expressionList = p.expressionList(false); expressionList == nil {
					p.back(ix)
					goto _4
				}
				// *ebnf.Token "=" ctx []
				if assignTok, ok = p.accept(ASSIGN); !ok {
					p.back(ix)
					goto _4
				}
			}
			break
		_4:
			assignTok = Token{}
			expressionList = nil
			// ebnf.Sequence IdentifierList ":=" ctx [IDENT]
			{
				ix := p.ix
				// *ebnf.Name IdentifierList ctx [IDENT]
				if identifierList = p.identifierList(); identifierList == nil {
					p.back(ix)
					goto _5
				}
				// *ebnf.Token ":=" ctx []
				if defineTok, ok = p.accept(DEFINE); !ok {
					p.back(ix)
					goto _5
				}
			}
			break
		_5:
			defineTok = Token{}
			identifierList = nil
			goto _0
		default:
			goto _0
		}
		goto _1
	_0:
		assignTok = Token{}
		expressionList = nil
	_1:
		// *ebnf.Name RecvExpr ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if recvExpr = p.recvExpr(); recvExpr == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &RecvStmtNode{
		ExpressionList: expressionList,
		ASSIGN:         assignTok,
		IdentifierList: identifierList,
		DEFINE:         defineTok,
		RecvExpr:       recvExpr,
	}
}

func (p *parser) relationalExpression(preBlock bool) (r Expression) {
	var additiveExpression Expression
	// ebnf.Sequence AdditiveExpression { ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) AdditiveExpression } ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name AdditiveExpression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if additiveExpression = p.additiveExpression(preBlock); additiveExpression == nil {
			p.back(ix)
			return nil
		}
		r = additiveExpression
		// *ebnf.Repetition { ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) AdditiveExpression } ctx []
	_0:
		{
			var op Token
			var additiveExpression Expression
			switch p.c() {
			case EQL, GEQ, GTR, LEQ, LSS, NEQ:
				// ebnf.Sequence ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) AdditiveExpression ctx [EQL, GEQ, GTR, LEQ, LSS, NEQ]
				// *ebnf.Group ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) ctx [EQL, GEQ, GTR, LEQ, LSS, NEQ]
				// ebnf.Alternative "==" | "!=" | "<" | "<=" | ">" | ">=" ctx [EQL, GEQ, GTR, LEQ, LSS, NEQ]
				op = p.consume()
				// *ebnf.Name AdditiveExpression ctx []
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if additiveExpression = p.additiveExpression(preBlock); additiveExpression == nil {
						p.back(ix)
						goto _1
					}
				default:
					p.back(ix)
					goto _1
				}
				r = &BinaryExpression{r, op, additiveExpression}
				goto _0
			}
		_1:
		}
	}
	return r
}

// ResultNode represents the production
//
//	Result = Parameters | Type .
type ResultNode struct {
	Parameters *ParametersNode
	Type       *TypeNode
}

// Source implements Node.
func (n *ResultNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ResultNode) Position() token.Position { panic("TODO") }

func (p *parser) result() *ResultNode {
	var (
		parameters *ParametersNode
		typeNode   *TypeNode
	)
	// ebnf.Alternative Parameters | Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	switch p.c() {
	case LPAREN: // 0 1
		// *ebnf.Name Parameters ctx [LPAREN]
		if parameters = p.parameters(); parameters == nil {
			goto _0
		}
		break
	_0:
		parameters = nil
		// *ebnf.Name Type ctx [LPAREN]
		if typeNode = p.type1(); typeNode == nil {
			goto _1
		}
		break
	_1:
		typeNode = nil
		return nil
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 1
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		if typeNode = p.type1(); typeNode == nil {
			goto _2
		}
		break
	_2:
		typeNode = nil
		return nil
	default:
		return nil
	}
	return &ResultNode{
		Parameters: parameters,
		Type:       typeNode,
	}
}

// ReturnStmtNode represents the production
//
//	ReturnStmt = "return" [ ExpressionList ] .
type ReturnStmtNode struct {
	RETURN         Token
	ExpressionList *ExpressionListNode
}

// Source implements Node.
func (n *ReturnStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ReturnStmtNode) Position() token.Position { return n.RETURN.Position() }

func (p *parser) returnStmt() *ReturnStmtNode {
	var (
		returnTok      Token
		expressionList *ExpressionListNode
	)
	// ebnf.Sequence "return" [ ExpressionList ] ctx [RETURN]
	{
		// *ebnf.Token "return" ctx [RETURN]
		returnTok = p.expect(RETURN)
		// *ebnf.Option [ ExpressionList ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expressionList = p.expressionList(false); expressionList == nil {
				goto _0
			}
		}
		goto _1
	_0:
		expressionList = nil
	_1:
	}
	return &ReturnStmtNode{
		RETURN:         returnTok,
		ExpressionList: expressionList,
	}
}

// SelectStmtNode represents the production
//
//	SelectStmt = "select" "{" { CommClause } "}" .
type SelectStmtNode struct {
	SELECT Token
	LBRACE Token
	List   []struct{ CommClause *CommClauseNode }
	RBRACE Token
}

// Source implements Node.
func (n *SelectStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SelectStmtNode) Position() token.Position { return n.SELECT.Position() }

func (p *parser) selectStmt() *SelectStmtNode {
	var (
		ok        bool
		selectTok Token
		lbraceTok Token
		list      []struct{ CommClause *CommClauseNode }
		rbraceTok Token
	)
	// ebnf.Sequence "select" "{" { CommClause } "}" ctx [SELECT]
	{
		if p.peek(1) != LBRACE {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "select" ctx [SELECT]
		selectTok = p.expect(SELECT)
		// *ebnf.Token "{" ctx [LBRACE]
		lbraceTok = p.expect(LBRACE)
		// *ebnf.Repetition { CommClause } ctx []
	_0:
		{
			var commClause *CommClauseNode
			switch p.c() {
			case CASE, DEFAULT:
				// *ebnf.Name CommClause ctx [CASE, DEFAULT]
				if commClause = p.commClause(); commClause == nil {
					goto _1
				}
				list = append(list, struct{ CommClause *CommClauseNode }{CommClause: commClause})
				goto _0
			}
		_1:
		}
		// *ebnf.Token "}" ctx []
		if rbraceTok, ok = p.accept(RBRACE); !ok {
			p.back(ix)
			return nil
		}
	}
	return &SelectStmtNode{
		SELECT: selectTok,
		LBRACE: lbraceTok,
		List:   list,
		RBRACE: rbraceTok,
	}
}

// SelectorNode represents the production
//
//	Selector = "." identifier .
type SelectorNode struct {
	PERIOD Token
	IDENT  Token
}

// Source implements Node.
func (n *SelectorNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SelectorNode) Position() token.Position { return n.PERIOD.Position() }

func (p *parser) selector() *SelectorNode {
	var (
		periodTok Token
		identTok  Token
	)
	// ebnf.Sequence "." identifier ctx [PERIOD]
	{
		if p.peek(1) != IDENT {
			return nil
		}
		// *ebnf.Token "." ctx [PERIOD]
		periodTok = p.expect(PERIOD)
		// *ebnf.Name identifier ctx [IDENT]
		identTok = p.expect(IDENT)
	}
	return &SelectorNode{
		PERIOD: periodTok,
		IDENT:  identTok,
	}
}

// SendStmtNode represents the production
//
//	SendStmt = Channel "<-" Expression .
type SendStmtNode struct {
	Channel    *ChannelNode
	ARROW      Token
	Expression Expression
}

// Source implements Node.
func (n *SendStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SendStmtNode) Position() token.Position { return n.Channel.Position() }

func (p *parser) sendStmt() *SendStmtNode {
	var (
		ok         bool
		channel    *ChannelNode
		arrowTok   Token
		expression Expression
	)
	// ebnf.Sequence Channel "<-" Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name Channel ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if channel = p.channel(); channel == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "<-" ctx []
		if arrowTok, ok = p.accept(ARROW); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Expression ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if expression = p.expression(false); expression == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &SendStmtNode{
		Channel:    channel,
		ARROW:      arrowTok,
		Expression: expression,
	}
}

// ShortVarDeclNode represents the production
//
//	ShortVarDecl = IdentifierList ":=" ExpressionList .
type ShortVarDeclNode struct {
	lexicalScoper
	IdentifierList *IdentifierListNode
	DEFINE         Token
	ExpressionList *ExpressionListNode
}

// Source implements Node.
func (n *ShortVarDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *ShortVarDeclNode) Position() token.Position { return n.IdentifierList.Position() }

func (p *parser) shortVarDecl(lhs *ExpressionListNode, preBlock bool) (r *ShortVarDeclNode) {
	var (
		defineTok      Token
		expressionList *ExpressionListNode
	)
	// ebnf.Sequence ":=" ExpressionList ctx [DEFINE]
	{
		switch p.peek(1) {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token ":=" ctx [DEFINE]
		defineTok = p.expect(DEFINE)
		// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if expressionList = p.expressionList(preBlock); expressionList == nil {
			p.back(ix)
			return nil
		}
	}
	list := p.exprList2identList(lhs)
	sc := p.sc
	r = &ShortVarDeclNode{
		lexicalScoper:  newLexicalScoper(sc),
		IdentifierList: list,
		DEFINE:         defineTok,
		ExpressionList: expressionList,
	}
	visible := int32(p.ix)
	var redecl []struct {
		Token
		Node
	}
	if id := r.IdentifierList.IDENT; id.IsValid() {
		ex := sc.declare(id, r, visible, p, false)
		redecl = append(redecl, struct {
			Token
			Node
		}{id, ex})
	}
	for _, v := range r.IdentifierList.List {
		if id := v.IDENT; id.IsValid() {
			ex := sc.declare(id, r, visible, p, false)
			redecl = append(redecl, struct {
				Token
				Node
			}{id, ex})
		}
	}
	for _, v := range redecl {
		if v.Node == nil { // new
			return
		}
	}

	for _, v := range redecl {
		p.err(v.Token.Position(), "%s redeclared, previous declaration at %v: (%p)", v.Token.Src(), v.Node.Position(), sc)
	}
	return r
}
func (p *parser) exprList2identList(list *ExpressionListNode) (r *IdentifierListNode) {
	r = &IdentifierListNode{IDENT: p.expr2ident(list.Expression)}
	for _, v := range list.List {
		r.List = append(r.List, struct {
			COMMA Token
			IDENT Token
		}{COMMA: v.COMMA, IDENT: p.expr2ident(v.Expression)})
	}
	return r
}

func (p *parser) expr2ident(e Expression) (r Token) {
	switch x := e.(type) {
	case *OperandNode:
		if (x.TypeArgs != nil || x.LiteralValue != nil) && p.reportDeclarationErrors {
			p.err(x.Position(), "expected identifier")
			break
		}

		return p.expr2ident(x.OperandName)
	case *OperandNameNode:
		if x.QualifiedIdent != nil && p.reportDeclarationErrors {
			p.err(x.Position(), "expected identifier")
			break
		}

		return x.IDENT
	case *PrimaryExprNode:
		if len(x.List) != 0 && p.reportDeclarationErrors {
			p.err(x.Position(), "expected identifier")
			break
		}

		return p.expr2ident(x.Operand)
	default:
		if p.reportDeclarationErrors {
			p.err(x.Position(), "expected identifier")
		}
	}
	return r
}

// SignatureNode represents the production
//
//	Signature = Parameters [ Result ] .
type SignatureNode struct {
	Parameters *ParametersNode
	Result     *ResultNode
}

// Source implements Node.
func (n *SignatureNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SignatureNode) Position() token.Position { return n.Parameters.Position() }

func (p *parser) signature() *SignatureNode {
	var (
		parameters *ParametersNode
		result     *ResultNode
	)
	// ebnf.Sequence Parameters [ Result ] ctx [LPAREN]
	{
		ix := p.ix
		// *ebnf.Name Parameters ctx [LPAREN]
		if parameters = p.parameters(); parameters == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Result ] ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			// *ebnf.Name Result ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if result = p.result(); result == nil {
				goto _0
			}
		}
		goto _1
	_0:
		result = nil
	_1:
	}
	return &SignatureNode{
		Parameters: parameters,
		Result:     result,
	}
}

// SimpleStmtNode represents the production
//
//	SimpleStmt = ExpressionList [ Assignment | ShortVarDecl | "<-" Expression | "++" | "--" ] | EmptyStmt .
type SimpleStmtNode struct {
	ExpressionList *ExpressionListNode
	Assignment     *AssignmentNode
	ShortVarDecl   *ShortVarDeclNode
	ARROW          Token
	Expression     Expression
	INC            Token
	DEC            Token
	EmptyStmt      *EmptyStmtNode
}

// Source implements Node.
func (n *SimpleStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SimpleStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) simpleStmt(preBlock bool) *SimpleStmtNode {
	var (
		expressionList *ExpressionListNode
		assignment     *AssignmentNode
		shortVarDecl   *ShortVarDeclNode
		arrowTok       Token
		expression     Expression
		incTok         Token
		decTok         Token
		emptyStmt      *EmptyStmtNode
	)
	// ebnf.Alternative ExpressionList [ Assignment | ShortVarDecl | "<-" Expression | "++" | "--" ] | EmptyStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
	switch p.c() {
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0
		// ebnf.Sequence ExpressionList [ Assignment | ShortVarDecl | "<-" Expression | "++" | "--" ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		{
			ix := p.ix
			// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expressionList = p.expressionList(preBlock); expressionList == nil {
				p.back(ix)
				goto _0
			}
			// *ebnf.Option [ Assignment | ShortVarDecl | "<-" Expression | "++" | "--" ] ctx []
			switch p.c() {
			case ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ARROW, ASSIGN, DEC, DEFINE, INC, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
				// ebnf.Alternative Assignment | ShortVarDecl | "<-" Expression | "++" | "--" ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ARROW, ASSIGN, DEC, DEFINE, INC, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
				switch p.c() {
				case ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN: // 0
					// *ebnf.Name Assignment ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
					if assignment = p.assignment(preBlock); assignment == nil {
						goto _4
					}
					break
				_4:
					assignment = nil
					goto _2
				case DEFINE: // 1
					// *ebnf.Name ShortVarDecl ctx [DEFINE]
					if shortVarDecl = p.shortVarDecl(expressionList, preBlock); shortVarDecl == nil {
						goto _6
					}
					expressionList = nil
					break
				_6:
					shortVarDecl = nil
					goto _2
				case ARROW: // 2
					// ebnf.Sequence "<-" Expression ctx [ARROW]
					{
						switch p.peek(1) {
						case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
						default:
							goto _8
						}
						ix := p.ix
						// *ebnf.Token "<-" ctx [ARROW]
						arrowTok = p.expect(ARROW)
						// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
						if expression = p.expression(preBlock); expression == nil {
							p.back(ix)
							goto _8
						}
					}
					break
				_8:
					arrowTok = Token{}
					expression = nil
					goto _2
				case INC: // 3
					// *ebnf.Token "++" ctx [INC]
					incTok = p.expect(INC)
				case DEC: // 4
					// *ebnf.Token "--" ctx [DEC]
					decTok = p.expect(DEC)
				default:
					goto _2
				}
			}
			goto _3
		_2:
			arrowTok = Token{}
			assignment = nil
			decTok = Token{}
			expression = nil
			incTok = Token{}
			shortVarDecl = nil
		_3:
		}
		break
	_0:
		arrowTok = Token{}
		assignment = nil
		decTok = Token{}
		expression = nil
		expressionList = nil
		incTok = Token{}
		shortVarDecl = nil
		return nil
	default: //  /* ε */ 1
		// *ebnf.Name EmptyStmt ctx [ /* ε */]
		if emptyStmt = p.emptyStmt(); emptyStmt == nil {
			goto _14
		}
		goto _15
	_14:
		emptyStmt = nil
		return nil
	_15:
	}
	return &SimpleStmtNode{
		ExpressionList: expressionList,
		Assignment:     assignment,
		ShortVarDecl:   shortVarDecl,
		ARROW:          arrowTok,
		Expression:     expression,
		INC:            incTok,
		DEC:            decTok,
		EmptyStmt:      emptyStmt,
	}
}

// SliceNode represents the production
//
//	Slice = "[" [ Expression ] ":" [ Expression ] "]" | "[" [ Expression ] ":" Expression ":" Expression "]" .
type SliceNode struct {
	LBRACK      Token
	Expression  Expression
	COLON       Token
	Expression2 Expression
	RBRACK      Token
	COLON2      Token
	Expression3 Expression
}

// Source implements Node.
func (n *SliceNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SliceNode) Position() token.Position { panic("TODO") }

func (p *parser) slice() *SliceNode {
	var (
		ok          bool
		lbrackTok   Token
		expression  Expression
		colonTok    Token
		expression2 Expression
		rbrackTok   Token
		colon2Tok   Token
		expression3 Expression
	)
	// ebnf.Alternative "[" [ Expression ] ":" [ Expression ] "]" | "[" [ Expression ] ":" Expression ":" Expression "]" ctx [LBRACK]
	switch p.c() {
	case LBRACK: // 0 1
		// ebnf.Sequence "[" [ Expression ] ":" [ Expression ] "]" ctx [LBRACK]
		{
			ix := p.ix
			// *ebnf.Token "[" ctx [LBRACK]
			lbrackTok = p.expect(LBRACK)
			// *ebnf.Option [ Expression ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expression = p.expression(false); expression == nil {
					goto _1
				}
			}
			goto _2
		_1:
			expression = nil
		_2:
			// *ebnf.Token ":" ctx []
			if colonTok, ok = p.accept(COLON); !ok {
				p.back(ix)
				goto _0
			}
			// *ebnf.Option [ Expression ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expression2 = p.expression(false); expression2 == nil {
					goto _3
				}
			}
			goto _4
		_3:
			expression2 = nil
		_4:
			// *ebnf.Token "]" ctx []
			if rbrackTok, ok = p.accept(RBRACK); !ok {
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
		colonTok = Token{}
		expression = nil
		expression2 = nil
		lbrackTok = Token{}
		rbrackTok = Token{}
		// ebnf.Sequence "[" [ Expression ] ":" Expression ":" Expression "]" ctx [LBRACK]
		{
			ix := p.ix
			// *ebnf.Token "[" ctx [LBRACK]
			lbrackTok = p.expect(LBRACK)
			// *ebnf.Option [ Expression ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expression = p.expression(false); expression == nil {
					goto _6
				}
			}
			goto _7
		_6:
			expression = nil
		_7:
			// *ebnf.Token ":" ctx []
			if colonTok, ok = p.accept(COLON); !ok {
				p.back(ix)
				goto _5
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expression2 = p.expression(false); expression2 == nil {
					p.back(ix)
					goto _5
				}
			default:
				p.back(ix)
				goto _5
			}
			// *ebnf.Token ":" ctx []
			if colon2Tok, ok = p.accept(COLON); !ok {
				p.back(ix)
				goto _5
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expression3 = p.expression(false); expression3 == nil {
					p.back(ix)
					goto _5
				}
			default:
				p.back(ix)
				goto _5
			}
			// *ebnf.Token "]" ctx []
			if rbrackTok, ok = p.accept(RBRACK); !ok {
				p.back(ix)
				goto _5
			}
		}
		break
	_5:
		colon2Tok = Token{}
		colonTok = Token{}
		expression = nil
		expression2 = nil
		expression3 = nil
		lbrackTok = Token{}
		rbrackTok = Token{}
		return nil
	default:
		return nil
	}
	return &SliceNode{
		LBRACK:      lbrackTok,
		Expression:  expression,
		COLON:       colonTok,
		Expression2: expression2,
		RBRACK:      rbrackTok,
		COLON2:      colon2Tok,
		Expression3: expression3,
	}
}

// SliceTypeNode represents the production
//
//	SliceType = "[" "]" ElementType .
type SliceTypeNode struct {
	LBRACK      Token
	RBRACK      Token
	ElementType *ElementTypeNode
}

// Source implements Node.
func (n *SliceTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SliceTypeNode) Position() token.Position { return n.LBRACK.Position() }

func (p *parser) sliceType() *SliceTypeNode {
	var (
		lbrackTok   Token
		rbrackTok   Token
		elementType *ElementTypeNode
	)
	// ebnf.Sequence "[" "]" ElementType ctx [LBRACK]
	{
		if p.peek(1) != RBRACK {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "[" ctx [LBRACK]
		lbrackTok = p.expect(LBRACK)
		// *ebnf.Token "]" ctx [RBRACK]
		rbrackTok = p.expect(RBRACK)
		// *ebnf.Name ElementType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if elementType = p.elementType(); elementType == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &SliceTypeNode{
		LBRACK:      lbrackTok,
		RBRACK:      rbrackTok,
		ElementType: elementType,
	}
}

// SourceFileNode represents the production
//
//	SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
type SourceFileNode struct {
	PackageClause *PackageClauseNode
	SEMICOLON     Token
	List          []struct {
		ImportDecl *ImportDeclNode
		SEMICOLON  Token
	}
	List2 []struct {
		TopLevelDecl *TopLevelDeclNode
		SEMICOLON    Token
	}
}

// Source implements Node.
func (n *SourceFileNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SourceFileNode) Position() token.Position { return n.PackageClause.Position() }

func (p *parser) sourceFile() *SourceFileNode {
	var (
		ok            bool
		packageClause *PackageClauseNode
		semicolonTok  Token
		list          []struct {
			ImportDecl *ImportDeclNode
			SEMICOLON  Token
		}
		list2 []struct {
			TopLevelDecl *TopLevelDeclNode
			SEMICOLON    Token
		}
	)
	// ebnf.Sequence PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } ctx [PACKAGE]
	{
		ix := p.ix
		// *ebnf.Name PackageClause ctx [PACKAGE]
		if packageClause = p.packageClause(); packageClause == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token ";" ctx []
		if semicolonTok, ok = p.accept(SEMICOLON); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { ImportDecl ";" } ctx []
	_0:
		{
			var importDecl *ImportDeclNode
			var semicolonTok Token
			switch p.c() {
			case IMPORT:
				// ebnf.Sequence ImportDecl ";" ctx [IMPORT]
				ix := p.ix
				// *ebnf.Name ImportDecl ctx [IMPORT]
				if importDecl = p.importDecl(); importDecl == nil {
					p.back(ix)
					goto _1
				}
				// *ebnf.Token ";" ctx []
				if semicolonTok, ok = p.accept(SEMICOLON); !ok {
					p.back(ix)
					goto _1
				}
				list = append(list, struct {
					ImportDecl *ImportDeclNode
					SEMICOLON  Token
				}{ImportDecl: importDecl, SEMICOLON: semicolonTok})
				goto _0
			}
		_1:
		}
		// *ebnf.Repetition { TopLevelDecl ";" } ctx []
	_2:
		{
			var topLevelDecl *TopLevelDeclNode
			var semicolonTok Token
			switch p.c() {
			case CONST, FUNC, TYPE, VAR:
				// ebnf.Sequence TopLevelDecl ";" ctx [CONST, FUNC, TYPE, VAR]
				ix := p.ix
				// *ebnf.Name TopLevelDecl ctx [CONST, FUNC, TYPE, VAR]
				if topLevelDecl = p.topLevelDecl(); topLevelDecl == nil {
					p.back(ix)
					goto _3
				}
				// *ebnf.Token ";" ctx []
				if semicolonTok, ok = p.accept(SEMICOLON); !ok {
					p.back(ix)
					goto _3
				}
				list2 = append(list2, struct {
					TopLevelDecl *TopLevelDeclNode
					SEMICOLON    Token
				}{TopLevelDecl: topLevelDecl, SEMICOLON: semicolonTok})
				goto _2
			}
		_3:
		}
	}
	return &SourceFileNode{
		PackageClause: packageClause,
		SEMICOLON:     semicolonTok,
		List:          list,
		List2:         list2,
	}
}

func (p *parser) statement() Node {
	var (
		declaration     *DeclarationNode
		labeledStmt     *LabeledStmtNode
		goStmt          *GoStmtNode
		returnStmt      *ReturnStmtNode
		breakStmt       *BreakStmtNode
		continueStmt    *ContinueStmtNode
		gotoStmt        *GotoStmtNode
		fallthroughStmt *FallthroughStmtNode
		block           *BlockNode
		ifStmt          *IfStmtNode
		switchStmt      *SwitchStmtNode
		selectStmt      *SelectStmtNode
		forStmt         *ForStmtNode
		deferStmt       *DeferStmtNode
		simpleStmt      *SimpleStmtNode
	)
	// ebnf.Alternative Declaration | LabeledStmt | GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt | DeferStmt | SimpleStmt ctx [ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */]
	switch p.c() {
	case CONST, TYPE, VAR: // 0
		// *ebnf.Name Declaration ctx [CONST, TYPE, VAR]
		if declaration = p.declaration(); declaration == nil {
			return nil
		}
		return declaration
	case IDENT: // 1 14
		// *ebnf.Name LabeledStmt ctx [IDENT]
		if labeledStmt = p.labeledStmt(); labeledStmt == nil {
			goto _2
		}
		return labeledStmt
	_2:
		labeledStmt = nil
		// *ebnf.Name SimpleStmt ctx [IDENT]
		if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
			return nil
		}
		return simpleStmt
	case GO: // 2
		// *ebnf.Name GoStmt ctx [GO]
		if goStmt = p.goStmt(); goStmt == nil {
			return nil
		}
		return goStmt
	case RETURN: // 3
		// *ebnf.Name ReturnStmt ctx [RETURN]
		if returnStmt = p.returnStmt(); returnStmt == nil {
			return nil
		}
		return returnStmt
	case BREAK: // 4
		// *ebnf.Name BreakStmt ctx [BREAK]
		if breakStmt = p.breakStmt(); breakStmt == nil {
			return nil
		}
		return breakStmt
	case CONTINUE: // 5
		// *ebnf.Name ContinueStmt ctx [CONTINUE]
		if continueStmt = p.continueStmt(); continueStmt == nil {
			return nil
		}
		return continueStmt
	case GOTO: // 6
		// *ebnf.Name GotoStmt ctx [GOTO]
		if gotoStmt = p.gotoStmt(); gotoStmt == nil {
			return nil
		}
		return gotoStmt
	case FALLTHROUGH: // 7
		// *ebnf.Name FallthroughStmt ctx [FALLTHROUGH]
		if fallthroughStmt = p.fallthroughStmt(); fallthroughStmt == nil {
			return nil
		}
		return fallthroughStmt
	case LBRACE: // 8
		// *ebnf.Name Block ctx [LBRACE]
		if block = p.block(); block == nil {
			return nil
		}
		return block
		return nil
	case IF: // 9
		// *ebnf.Name IfStmt ctx [IF]
		if ifStmt = p.ifStmt(); ifStmt == nil {
			return nil
		}
		return ifStmt
	case SWITCH: // 10
		// *ebnf.Name SwitchStmt ctx [SWITCH]
		if switchStmt = p.switchStmt(); switchStmt == nil {
			return nil
		}
		return switchStmt
	case SELECT: // 11
		// *ebnf.Name SelectStmt ctx [SELECT]
		if selectStmt = p.selectStmt(); selectStmt == nil {
			return nil
		}
		return selectStmt
	case FOR: // 12
		// *ebnf.Name ForStmt ctx [FOR]
		if forStmt = p.forStmt(); forStmt == nil {
			return nil
		}
		return forStmt
	case DEFER: // 13
		// *ebnf.Name DeferStmt ctx [DEFER]
		if deferStmt = p.deferStmt(); deferStmt == nil {
			return nil
		}
		return deferStmt
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ : // 14
		// *ebnf.Name SimpleStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
		if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
			return nil
		}
		return simpleStmt
	}
	return nil
}

// StatementListNode represents the production
//
//	StatementList = { Statement ";" } .
type StatementListNode struct {
	List []struct {
		Statement Node
		SEMICOLON Token
	}
}

// Source implements Node.
func (n *StatementListNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *StatementListNode) Position() token.Position { panic("TODO") }

func (p *parser) statementList() *StatementListNode {
	var (
		statement Node
		list      []struct {
			Statement Node
			SEMICOLON Token
		}
	)
	for {
		ix := p.ix
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if statement = p.statement(); statement == nil {
				p.back(ix)
				goto _1
			}
		case SEMICOLON:
			list = append(list, struct {
				Statement Node
				SEMICOLON Token
			}{SEMICOLON: p.consume()})
			continue
		default:
			goto _1
		}

		if p.c() != SEMICOLON {
			list = append(list, struct {
				Statement Node
				SEMICOLON Token
			}{Statement: statement})
			goto _1
		}
		list = append(list, struct {
			Statement Node
			SEMICOLON Token
		}{Statement: statement, SEMICOLON: p.consume()})

	}
_1:
	if len(list) == 0 {
		return nil
	}

	return &StatementListNode{
		List: list,
	}
}

// StructTypeNode represents the production
//
//	StructType = "struct" "{" { FieldDecl ";" } [ FieldDecl ] "}" .
type StructTypeNode struct {
	STRUCT Token
	LBRACE Token
	List   []struct {
		FieldDecl *FieldDeclNode
		SEMICOLON Token
	}
	FieldDecl *FieldDeclNode
	RBRACE    Token
}

// Source implements Node.
func (n *StructTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *StructTypeNode) Position() token.Position { return n.STRUCT.Position() }

func (p *parser) structType() *StructTypeNode {
	var (
		ok        bool
		structTok Token
		lbraceTok Token
		list      []struct {
			FieldDecl *FieldDeclNode
			SEMICOLON Token
		}
		fieldDecl *FieldDeclNode
		rbraceTok Token
	)
	// ebnf.Sequence "struct" "{" { FieldDecl ";" } [ FieldDecl ] "}" ctx [STRUCT]
	{
		if p.peek(1) != LBRACE {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "struct" ctx [STRUCT]
		structTok = p.expect(STRUCT)
		// *ebnf.Token "{" ctx [LBRACE]
		lbraceTok = p.expect(LBRACE)
		// *ebnf.Repetition { FieldDecl ";" } ctx []
	_0:
		{
			var fieldDecl *FieldDeclNode
			var semicolonTok Token
			switch p.c() {
			case IDENT, MUL:
				// ebnf.Sequence FieldDecl ";" ctx [IDENT, MUL]
				ix := p.ix
				// *ebnf.Name FieldDecl ctx [IDENT, MUL]
				if fieldDecl = p.fieldDecl(); fieldDecl == nil {
					p.back(ix)
					goto _1
				}
				// *ebnf.Token ";" ctx []
				if semicolonTok, ok = p.accept(SEMICOLON); !ok {
					p.back(ix)
					goto _1
				}
				list = append(list, struct {
					FieldDecl *FieldDeclNode
					SEMICOLON Token
				}{FieldDecl: fieldDecl, SEMICOLON: semicolonTok})
				goto _0
			}
		_1:
		}
		// *ebnf.Option [ FieldDecl ] ctx []
		switch p.c() {
		case IDENT, MUL:
			// *ebnf.Name FieldDecl ctx [IDENT, MUL]
			if fieldDecl = p.fieldDecl(); fieldDecl == nil {
				goto _2
			}
		}
		goto _3
	_2:
		fieldDecl = nil
	_3:
		// *ebnf.Token "}" ctx []
		if rbraceTok, ok = p.accept(RBRACE); !ok {
			p.back(ix)
			return nil
		}
	}
	return &StructTypeNode{
		STRUCT:    structTok,
		LBRACE:    lbraceTok,
		List:      list,
		FieldDecl: fieldDecl,
		RBRACE:    rbraceTok,
	}
}

// SwitchStmtNode represents the production
//
//	SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
type SwitchStmtNode struct {
	ExprSwitchStmt *ExprSwitchStmtNode
	TypeSwitchStmt *TypeSwitchStmtNode
}

// Source implements Node.
func (n *SwitchStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *SwitchStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) switchStmt() *SwitchStmtNode {
	var (
		exprSwitchStmt *ExprSwitchStmtNode
		typeSwitchStmt *TypeSwitchStmtNode
	)
	p.openScope()

	defer p.closeScope()

	// ebnf.Alternative ExprSwitchStmt | TypeSwitchStmt ctx [SWITCH]
	switch p.c() {
	case SWITCH: // 0 1
		// *ebnf.Name ExprSwitchStmt ctx [SWITCH]
		if exprSwitchStmt = p.exprSwitchStmt(); exprSwitchStmt == nil {
			goto _0
		}
		break
	_0:
		exprSwitchStmt = nil
		p.closeScope()
		p.openScope()
		// *ebnf.Name TypeSwitchStmt ctx [SWITCH]
		if typeSwitchStmt = p.typeSwitchStmt(); typeSwitchStmt == nil {
			goto _1
		}
		break
	_1:
		typeSwitchStmt = nil
		return nil
	default:
		return nil
	}
	return &SwitchStmtNode{
		ExprSwitchStmt: exprSwitchStmt,
		TypeSwitchStmt: typeSwitchStmt,
	}
}

// TagNode represents the production
//
//	Tag = string_lit .
type TagNode struct {
	STRING Token
}

// Source implements Node.
func (n *TagNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TagNode) Position() token.Position { return n.STRING.Position() }

func (p *parser) tag() *TagNode {
	var (
		stringTok Token
	)
	// *ebnf.Name string_lit ctx [STRING]
	stringTok = p.expect(STRING)
	return &TagNode{
		STRING: stringTok,
	}
}

// TopLevelDeclNode represents the production
//
//	TopLevelDecl = Declaration | FunctionDecl | MethodDecl .
type TopLevelDeclNode struct {
	Declaration  *DeclarationNode
	FunctionDecl *FunctionDeclNode
	MethodDecl   *MethodDeclNode
}

// Source implements Node.
func (n *TopLevelDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TopLevelDeclNode) Position() token.Position { panic("TODO") }

func (p *parser) topLevelDecl() *TopLevelDeclNode {
	var (
		declaration  *DeclarationNode
		functionDecl *FunctionDeclNode
		methodDecl   *MethodDeclNode
	)
	// ebnf.Alternative Declaration | FunctionDecl | MethodDecl ctx [CONST, FUNC, TYPE, VAR]
	switch p.c() {
	case CONST, TYPE, VAR: // 0
		// *ebnf.Name Declaration ctx [CONST, TYPE, VAR]
		if declaration = p.declaration(); declaration == nil {
			goto _0
		}
		break
	_0:
		declaration = nil
		return nil
	case FUNC: // 1 2
		// *ebnf.Name FunctionDecl ctx [FUNC]
		if functionDecl = p.functionDecl(); functionDecl == nil {
			goto _2
		}
		break
	_2:
		functionDecl = nil
		// *ebnf.Name MethodDecl ctx [FUNC]
		if methodDecl = p.methodDecl(); methodDecl == nil {
			goto _3
		}
		break
	_3:
		methodDecl = nil
		return nil
	default:
		return nil
	}
	return &TopLevelDeclNode{
		Declaration:  declaration,
		FunctionDecl: functionDecl,
		MethodDecl:   methodDecl,
	}
}

// TypeNode represents the production
//
//	Type = TypeName [ TypeArgs ] | TypeLit | "(" Type ")" .
type TypeNode struct {
	TypeName *TypeNameNode
	TypeArgs *TypeArgsNode
	TypeLit  *TypeLitNode
	LPAREN   Token
	Type     *TypeNode
	RPAREN   Token
}

// Source implements Node.
func (n *TypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeNode) Position() token.Position { panic("TODO") }

func (p *parser) type1() *TypeNode {
	var (
		ok        bool
		typeName  *TypeNameNode
		typeArgs  *TypeArgsNode
		typeLit   *TypeLitNode
		lparenTok Token
		typeNode  *TypeNode
		rparenTok Token
	)
	// ebnf.Alternative TypeName [ TypeArgs ] | TypeLit | "(" Type ")" ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	switch p.c() {
	case IDENT: // 0
		// ebnf.Sequence TypeName [ TypeArgs ] ctx [IDENT]
		{
			ix := p.ix
			// *ebnf.Name TypeName ctx [IDENT]
			if typeName = p.typeName(); typeName == nil {
				p.back(ix)
				goto _0
			}
			// *ebnf.Option [ TypeArgs ] ctx []
			switch p.c() {
			case LBRACK:
				// *ebnf.Name TypeArgs ctx [LBRACK]
				if typeArgs = p.typeArgs(); typeArgs == nil {
					goto _2
				}
			}
			goto _3
		_2:
			typeArgs = nil
		_3:
		}
		break
	_0:
		typeArgs = nil
		typeName = nil
		return nil
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 1
		// *ebnf.Name TypeLit ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		if typeLit = p.typeLit(); typeLit == nil {
			goto _4
		}
		break
	_4:
		typeLit = nil
		return nil
	case LPAREN: // 2
		// ebnf.Sequence "(" Type ")" ctx [LPAREN]
		{
			switch p.peek(1) {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			default:
				goto _6
			}
			ix := p.ix
			// *ebnf.Token "(" ctx [LPAREN]
			lparenTok = p.expect(LPAREN)
			// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if typeNode = p.type1(); typeNode == nil {
				p.back(ix)
				goto _6
			}
			// *ebnf.Token ")" ctx []
			if rparenTok, ok = p.accept(RPAREN); !ok {
				p.back(ix)
				goto _6
			}
		}
		break
	_6:
		lparenTok = Token{}
		rparenTok = Token{}
		typeNode = nil
		return nil
	default:
		return nil
	}
	return &TypeNode{
		TypeName: typeName,
		TypeArgs: typeArgs,
		TypeLit:  typeLit,
		LPAREN:   lparenTok,
		Type:     typeNode,
		RPAREN:   rparenTok,
	}
}

// TypeArgsNode represents the production
//
//	TypeArgs = "[" TypeList [ "," ] "]" .
type TypeArgsNode struct {
	LBRACK   Token
	TypeList *TypeListNode
	COMMA    Token
	RBRACK   Token
}

// Source implements Node.
func (n *TypeArgsNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeArgsNode) Position() token.Position { return n.LBRACK.Position() }

func (p *parser) typeArgs() *TypeArgsNode {
	var (
		ok        bool
		lbrackTok Token
		typeList  *TypeListNode
		commaTok  Token
		rbrackTok Token
	)
	// ebnf.Sequence "[" TypeList [ "," ] "]" ctx [LBRACK]
	{
		switch p.peek(1) {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "[" ctx [LBRACK]
		lbrackTok = p.expect(LBRACK)
		// *ebnf.Name TypeList ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if typeList = p.typeList(); typeList == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ "," ] ctx []
		switch p.c() {
		case COMMA:
			// *ebnf.Token "," ctx [COMMA]
			commaTok = p.expect(COMMA)
		}
		// *ebnf.Token "]" ctx []
		if rbrackTok, ok = p.accept(RBRACK); !ok {
			p.back(ix)
			return nil
		}
	}
	return &TypeArgsNode{
		LBRACK:   lbrackTok,
		TypeList: typeList,
		COMMA:    commaTok,
		RBRACK:   rbrackTok,
	}
}

// TypeAssertionNode represents the production
//
//	TypeAssertion = "." "(" Type ")" .
type TypeAssertionNode struct {
	PERIOD Token
	LPAREN Token
	Type   *TypeNode
	RPAREN Token
}

// Source implements Node.
func (n *TypeAssertionNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeAssertionNode) Position() token.Position { return n.PERIOD.Position() }

func (p *parser) typeAssertion() *TypeAssertionNode {
	var (
		ok        bool
		periodTok Token
		lparenTok Token
		typeNode  *TypeNode
		rparenTok Token
	)
	// ebnf.Sequence "." "(" Type ")" ctx [PERIOD]
	{
		if p.peek(1) != LPAREN {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "." ctx [PERIOD]
		periodTok = p.expect(PERIOD)
		// *ebnf.Token "(" ctx [LPAREN]
		lparenTok = p.expect(LPAREN)
		// *ebnf.Name Type ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if typeNode = p.type1(); typeNode == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token ")" ctx []
		if rparenTok, ok = p.accept(RPAREN); !ok {
			p.back(ix)
			return nil
		}
	}
	return &TypeAssertionNode{
		PERIOD: periodTok,
		LPAREN: lparenTok,
		Type:   typeNode,
		RPAREN: rparenTok,
	}
}

// TypeCaseClauseNode represents the production
//
//	TypeCaseClause = TypeSwitchCase ":" StatementList .
type TypeCaseClauseNode struct {
	TypeSwitchCase *TypeSwitchCaseNode
	COLON          Token
	StatementList  *StatementListNode
}

// Source implements Node.
func (n *TypeCaseClauseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeCaseClauseNode) Position() token.Position { return n.TypeSwitchCase.Position() }

func (p *parser) typeCaseClause() *TypeCaseClauseNode {
	var (
		ok             bool
		typeSwitchCase *TypeSwitchCaseNode
		colonTok       Token
		statementList  *StatementListNode
	)
	// ebnf.Sequence TypeSwitchCase ":" StatementList ctx [CASE, DEFAULT]
	{
		p.openScope()

		defer p.closeScope()

		ix := p.ix
		// *ebnf.Name TypeSwitchCase ctx [CASE, DEFAULT]
		if typeSwitchCase = p.typeSwitchCase(); typeSwitchCase == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Token ":" ctx []
		if colonTok, ok = p.accept(COLON); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name StatementList ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if statementList = p.statementList(); statementList == nil {
				p.back(ix)
				return nil
			}
		}
	}
	return &TypeCaseClauseNode{
		TypeSwitchCase: typeSwitchCase,
		COLON:          colonTok,
		StatementList:  statementList,
	}
}

// TypeConstraintNode represents the production
//
//	TypeConstraint = TypeElem .
type TypeConstraintNode struct {
	TypeElem *TypeElemNode
}

// Source implements Node.
func (n *TypeConstraintNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeConstraintNode) Position() token.Position { return n.TypeElem.Position() }

func (p *parser) typeConstraint() *TypeConstraintNode {
	var (
		typeElem *TypeElemNode
	)
	// *ebnf.Name TypeElem ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
	if typeElem = p.typeElem(); typeElem == nil {
		return nil
	}
	return &TypeConstraintNode{
		TypeElem: typeElem,
	}
}

// TypeDeclNode represents the production
//
//	TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
type TypeDeclNode struct {
	TYPE     Token
	TypeSpec Node
	LPAREN   Token
	List     []struct {
		TypeSpec  Node
		SEMICOLON Token
	}
	RPAREN Token
}

// Source implements Node.
func (n *TypeDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeDeclNode) Position() token.Position { return n.TYPE.Position() }

func (p *parser) typeDecl() *TypeDeclNode {
	var (
		ok        bool
		typeTok   Token
		typeSpec  Node
		lparenTok Token
		list      []struct {
			TypeSpec  Node
			SEMICOLON Token
		}
		rparenTok Token
	)
	// ebnf.Sequence "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) ctx [TYPE]
	{
		switch p.peek(1) {
		case IDENT, LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "type" ctx [TYPE]
		typeTok = p.expect(TYPE)
		// *ebnf.Group ( TypeSpec | "(" { TypeSpec ";" } ")" ) ctx [IDENT, LPAREN]
		// ebnf.Alternative TypeSpec | "(" { TypeSpec ";" } ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name TypeSpec ctx [IDENT]
			if typeSpec = p.typeSpec(); typeSpec == nil {
				goto _0
			}
			break
		_0:
			typeSpec = nil
			p.back(ix)
			return nil
		case LPAREN: // 1
			// ebnf.Sequence "(" { TypeSpec ";" } ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				lparenTok = p.expect(LPAREN)
				// *ebnf.Repetition { TypeSpec ";" } ctx []
			_4:
				{
					var typeSpec Node
					var semicolonTok Token
					switch p.c() {
					case IDENT:
						// ebnf.Sequence TypeSpec ";" ctx [IDENT]
						ix := p.ix
						// *ebnf.Name TypeSpec ctx [IDENT]
						if typeSpec = p.typeSpec(); typeSpec == nil {
							p.back(ix)
							goto _5
						}
						// *ebnf.Token ";" ctx []
						if semicolonTok, ok = p.accept(SEMICOLON); !ok {
							p.back(ix)
							goto _5
						}
						list = append(list, struct {
							TypeSpec  Node
							SEMICOLON Token
						}{TypeSpec: typeSpec, SEMICOLON: semicolonTok})
						goto _4
					}
				_5:
				}
				// *ebnf.Token ")" ctx []
				if rparenTok, ok = p.accept(RPAREN); !ok {
					p.back(ix)
					goto _2
				}
			}
			break
		_2:
			lparenTok = Token{}
			rparenTok = Token{}
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
	}
	return &TypeDeclNode{
		TYPE:     typeTok,
		TypeSpec: typeSpec,
		LPAREN:   lparenTok,
		List:     list,
		RPAREN:   rparenTok,
	}
}

// TypeDefNode represents the production
//
//	TypeDef = identifier [ TypeParameters ] Type .
type TypeDefNode struct {
	lexicalScoper
	IDENT          Token
	TypeParameters *TypeParametersNode
	Type           *TypeNode
}

// Source implements Node.
func (n *TypeDefNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeDefNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) typeDef() (r *TypeDefNode) {
	var (
		identTok       Token
		typeParameters *TypeParametersNode
		typeNode       *TypeNode
	)
	// ebnf.Sequence identifier [ TypeParameters ] Type ctx [IDENT]
	{
		ix := p.ix
		// *ebnf.Name identifier ctx [IDENT]
		identTok = p.expect(IDENT)
		// *ebnf.Option [ TypeParameters ] ctx []
		switch p.c() {
		case LBRACK:
			// *ebnf.Name TypeParameters ctx [LBRACK]
			if typeParameters = p.typeParameters(); typeParameters == nil {
				goto _0
			}
		}
		goto _1
	_0:
		typeParameters = nil
	_1:
		// *ebnf.Name Type ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if typeNode = p.type1(); typeNode == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	r = &TypeDefNode{
		lexicalScoper:  newLexicalScoper(p.sc),
		IDENT:          identTok,
		TypeParameters: typeParameters,
		Type:           typeNode,
	}
	p.declare(p.sc, identTok, r, int32(p.ix), false)
	return r
}

// TypeElemNode represents the production
//
//	TypeElem = TypeTerm { "|" TypeTerm } .
type TypeElemNode struct {
	TypeTerm *TypeTermNode
	List     []struct {
		OR       Token
		TypeTerm *TypeTermNode
	}
}

// Source implements Node.
func (n *TypeElemNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeElemNode) Position() token.Position { return n.TypeTerm.Position() }

func (p *parser) typeElem() *TypeElemNode {
	var (
		typeTerm *TypeTermNode
		list     []struct {
			OR       Token
			TypeTerm *TypeTermNode
		}
	)
	// ebnf.Sequence TypeTerm { "|" TypeTerm } ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
	{
		ix := p.ix
		// *ebnf.Name TypeTerm ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
		if typeTerm = p.typeTerm(); typeTerm == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "|" TypeTerm } ctx []
	_0:
		{
			var orTok Token
			var typeTerm *TypeTermNode
			switch p.c() {
			case OR:
				// ebnf.Sequence "|" TypeTerm ctx [OR]
				switch p.peek(1) {
				case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
				default:
					goto _1
				}
				ix := p.ix
				// *ebnf.Token "|" ctx [OR]
				orTok = p.expect(OR)
				// *ebnf.Name TypeTerm ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
				if typeTerm = p.typeTerm(); typeTerm == nil {
					p.back(ix)
					goto _1
				}
				list = append(list, struct {
					OR       Token
					TypeTerm *TypeTermNode
				}{OR: orTok, TypeTerm: typeTerm})
				goto _0
			}
		_1:
		}
	}
	return &TypeElemNode{
		TypeTerm: typeTerm,
		List:     list,
	}
}

// TypeListNode represents the production
//
//	TypeList = Type { "," Type } .
type TypeListNode struct {
	Type *TypeNode
	List []struct {
		COMMA Token
		Type  *TypeNode
	}
}

// Source implements Node.
func (n *TypeListNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeListNode) Position() token.Position { return n.Type.Position() }

func (p *parser) typeList() *TypeListNode {
	var (
		typeNode *TypeNode
		list     []struct {
			COMMA Token
			Type  *TypeNode
		}
	)
	// ebnf.Sequence Type { "," Type } ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	{
		ix := p.ix
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if typeNode = p.type1(); typeNode == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," Type } ctx []
	_0:
		{
			var commaTok Token
			var typeNode *TypeNode
			switch p.c() {
			case COMMA:
				// ebnf.Sequence "," Type ctx [COMMA]
				switch p.peek(1) {
				case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				default:
					goto _1
				}
				ix := p.ix
				// *ebnf.Token "," ctx [COMMA]
				commaTok = p.expect(COMMA)
				// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
				if typeNode = p.type1(); typeNode == nil {
					p.back(ix)
					goto _1
				}
				list = append(list, struct {
					COMMA Token
					Type  *TypeNode
				}{COMMA: commaTok, Type: typeNode})
				goto _0
			}
		_1:
		}
	}
	return &TypeListNode{
		Type: typeNode,
		List: list,
	}
}

// TypeLitNode represents the production
//
//	TypeLit = ArrayType | StructType | PointerType | FunctionType | InterfaceType | SliceType | MapType | ChannelType .
type TypeLitNode struct {
	ArrayType     *ArrayTypeNode
	StructType    *StructTypeNode
	PointerType   *PointerTypeNode
	FunctionType  *FunctionTypeNode
	InterfaceType *InterfaceTypeNode
	SliceType     *SliceTypeNode
	MapType       *MapTypeNode
	ChannelType   *ChannelTypeNode
}

// Source implements Node.
func (n *TypeLitNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeLitNode) Position() token.Position { panic("TODO") }

func (p *parser) typeLit() *TypeLitNode {
	var (
		arrayType     *ArrayTypeNode
		structType    *StructTypeNode
		pointerType   *PointerTypeNode
		functionType  *FunctionTypeNode
		interfaceType *InterfaceTypeNode
		sliceType     *SliceTypeNode
		mapType       *MapTypeNode
		channelType   *ChannelTypeNode
	)
	// ebnf.Alternative ArrayType | StructType | PointerType | FunctionType | InterfaceType | SliceType | MapType | ChannelType ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT]
	switch p.c() {
	case LBRACK: // 0 5
		// *ebnf.Name ArrayType ctx [LBRACK]
		if arrayType = p.arrayType(); arrayType == nil {
			goto _0
		}
		break
	_0:
		arrayType = nil
		// *ebnf.Name SliceType ctx [LBRACK]
		if sliceType = p.sliceType(); sliceType == nil {
			goto _1
		}
		break
	_1:
		sliceType = nil
		return nil
	case STRUCT: // 1
		// *ebnf.Name StructType ctx [STRUCT]
		if structType = p.structType(); structType == nil {
			goto _2
		}
		break
	_2:
		structType = nil
		return nil
	case MUL: // 2
		// *ebnf.Name PointerType ctx [MUL]
		if pointerType = p.pointerType(); pointerType == nil {
			goto _4
		}
		break
	_4:
		pointerType = nil
		return nil
	case FUNC: // 3
		// *ebnf.Name FunctionType ctx [FUNC]
		if functionType = p.functionType(); functionType == nil {
			goto _6
		}
		break
	_6:
		functionType = nil
		return nil
	case INTERFACE: // 4
		// *ebnf.Name InterfaceType ctx [INTERFACE]
		if interfaceType = p.interfaceType(); interfaceType == nil {
			goto _8
		}
		break
	_8:
		interfaceType = nil
		return nil
	case MAP: // 6
		// *ebnf.Name MapType ctx [MAP]
		if mapType = p.mapType(); mapType == nil {
			goto _10
		}
		break
	_10:
		mapType = nil
		return nil
	case ARROW, CHAN: // 7
		// *ebnf.Name ChannelType ctx [ARROW, CHAN]
		if channelType = p.channelType(); channelType == nil {
			goto _12
		}
		break
	_12:
		channelType = nil
		return nil
	default:
		return nil
	}
	return &TypeLitNode{
		ArrayType:     arrayType,
		StructType:    structType,
		PointerType:   pointerType,
		FunctionType:  functionType,
		InterfaceType: interfaceType,
		SliceType:     sliceType,
		MapType:       mapType,
		ChannelType:   channelType,
	}
}

// TypeNameNode represents the production
//
//	TypeName = QualifiedIdent | identifier .
type TypeNameNode struct {
	QualifiedIdent *QualifiedIdentNode
	IDENT          Token
}

// Source implements Node.
func (n *TypeNameNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeNameNode) Position() token.Position { panic("TODO") }

func (p *parser) typeName() *TypeNameNode {
	var (
		qualifiedIdent *QualifiedIdentNode
		identTok       Token
	)
	// ebnf.Alternative QualifiedIdent | identifier ctx [IDENT]
	switch p.c() {
	case IDENT: // 0 1
		// *ebnf.Name QualifiedIdent ctx [IDENT]
		if qualifiedIdent = p.qualifiedIdent(); qualifiedIdent == nil {
			goto _0
		}
		break
	_0:
		qualifiedIdent = nil
		// *ebnf.Name identifier ctx [IDENT]
		identTok = p.expect(IDENT)
		break
		return nil
	default:
		return nil
	}
	return &TypeNameNode{
		QualifiedIdent: qualifiedIdent,
		IDENT:          identTok,
	}
}

// TypeParamDeclNode represents the production
//
//	TypeParamDecl = IdentifierList TypeConstraint .
type TypeParamDeclNode struct {
	IdentifierList *IdentifierListNode
	TypeConstraint *TypeConstraintNode
}

// Source implements Node.
func (n *TypeParamDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeParamDeclNode) Position() token.Position { return n.IdentifierList.Position() }

func (p *parser) typeParamDecl() *TypeParamDeclNode {
	var (
		identifierList *IdentifierListNode
		typeConstraint *TypeConstraintNode
	)
	// ebnf.Sequence IdentifierList TypeConstraint ctx [IDENT]
	{
		ix := p.ix
		// *ebnf.Name IdentifierList ctx [IDENT]
		if identifierList = p.identifierList(); identifierList == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Name TypeConstraint ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
			if typeConstraint = p.typeConstraint(); typeConstraint == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &TypeParamDeclNode{
		IdentifierList: identifierList,
		TypeConstraint: typeConstraint,
	}
}

// TypeParamListNode represents the production
//
//	TypeParamList = TypeParamDecl { "," TypeParamDecl } .
type TypeParamListNode struct {
	TypeParamDecl *TypeParamDeclNode
	List          []struct {
		COMMA         Token
		TypeParamDecl *TypeParamDeclNode
	}
}

// Source implements Node.
func (n *TypeParamListNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeParamListNode) Position() token.Position { return n.TypeParamDecl.Position() }

func (p *parser) typeParamList() *TypeParamListNode {
	var (
		typeParamDecl *TypeParamDeclNode
		list          []struct {
			COMMA         Token
			TypeParamDecl *TypeParamDeclNode
		}
	)
	// ebnf.Sequence TypeParamDecl { "," TypeParamDecl } ctx [IDENT]
	{
		ix := p.ix
		// *ebnf.Name TypeParamDecl ctx [IDENT]
		if typeParamDecl = p.typeParamDecl(); typeParamDecl == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," TypeParamDecl } ctx []
	_0:
		{
			var commaTok Token
			var typeParamDecl *TypeParamDeclNode
			switch p.c() {
			case COMMA:
				// ebnf.Sequence "," TypeParamDecl ctx [COMMA]
				switch p.peek(1) {
				case IDENT:
				default:
					goto _1
				}
				ix := p.ix
				// *ebnf.Token "," ctx [COMMA]
				commaTok = p.expect(COMMA)
				// *ebnf.Name TypeParamDecl ctx [IDENT]
				if typeParamDecl = p.typeParamDecl(); typeParamDecl == nil {
					p.back(ix)
					goto _1
				}
				list = append(list, struct {
					COMMA         Token
					TypeParamDecl *TypeParamDeclNode
				}{COMMA: commaTok, TypeParamDecl: typeParamDecl})
				goto _0
			}
		_1:
		}
	}
	return &TypeParamListNode{
		TypeParamDecl: typeParamDecl,
		List:          list,
	}
}

// TypeParametersNode represents the production
//
//	TypeParameters = "[" TypeParamList [ "," ] "]" .
type TypeParametersNode struct {
	LBRACK        Token
	TypeParamList *TypeParamListNode
	COMMA         Token
	RBRACK        Token
}

// Source implements Node.
func (n *TypeParametersNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeParametersNode) Position() token.Position { return n.LBRACK.Position() }

func (p *parser) typeParameters() *TypeParametersNode {
	var (
		ok            bool
		lbrackTok     Token
		typeParamList *TypeParamListNode
		commaTok      Token
		rbrackTok     Token
	)
	// ebnf.Sequence "[" TypeParamList [ "," ] "]" ctx [LBRACK]
	{
		switch p.peek(1) {
		case IDENT:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "[" ctx [LBRACK]
		lbrackTok = p.expect(LBRACK)
		// *ebnf.Name TypeParamList ctx [IDENT]
		if typeParamList = p.typeParamList(); typeParamList == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ "," ] ctx []
		switch p.c() {
		case COMMA:
			// *ebnf.Token "," ctx [COMMA]
			commaTok = p.expect(COMMA)
		}
		// *ebnf.Token "]" ctx []
		if rbrackTok, ok = p.accept(RBRACK); !ok {
			p.back(ix)
			return nil
		}
	}
	return &TypeParametersNode{
		LBRACK:        lbrackTok,
		TypeParamList: typeParamList,
		COMMA:         commaTok,
		RBRACK:        rbrackTok,
	}
}

func (p *parser) typeSpec() Node {
	var (
		aliasDecl *AliasDeclNode
		typeDef   *TypeDefNode
	)
	// ebnf.Alternative AliasDecl | TypeDef ctx [IDENT]
	switch p.c() {
	case IDENT: // 0 1
		// *ebnf.Name AliasDecl ctx [IDENT]
		if aliasDecl = p.aliasDecl(); aliasDecl == nil {
			goto _0
		}
		return aliasDecl
	_0:
		aliasDecl = nil
		// *ebnf.Name TypeDef ctx [IDENT]
		if typeDef = p.typeDef(); typeDef == nil {
			return nil
		}
		return typeDef
	default:
		return nil
	}
}

// TypeSwitchCaseNode represents the production
//
//	TypeSwitchCase = "case" TypeList | "default" .
type TypeSwitchCaseNode struct {
	CASE     Token
	TypeList *TypeListNode
	DEFAULT  Token
}

// Source implements Node.
func (n *TypeSwitchCaseNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeSwitchCaseNode) Position() token.Position { panic("TODO") }

func (p *parser) typeSwitchCase() *TypeSwitchCaseNode {
	var (
		caseTok    Token
		typeList   *TypeListNode
		defaultTok Token
	)
	// ebnf.Alternative "case" TypeList | "default" ctx [CASE, DEFAULT]
	switch p.c() {
	case CASE: // 0
		// ebnf.Sequence "case" TypeList ctx [CASE]
		{
			switch p.peek(1) {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			default:
				goto _0
			}
			ix := p.ix
			// *ebnf.Token "case" ctx [CASE]
			caseTok = p.expect(CASE)
			// *ebnf.Name TypeList ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if typeList = p.typeList(); typeList == nil {
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
		caseTok = Token{}
		typeList = nil
		return nil
	case DEFAULT: // 1
		// *ebnf.Token "default" ctx [DEFAULT]
		defaultTok = p.expect(DEFAULT)
	default:
		return nil
	}
	return &TypeSwitchCaseNode{
		CASE:     caseTok,
		TypeList: typeList,
		DEFAULT:  defaultTok,
	}
}

// TypeSwitchGuardNode represents the production
//
//	TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" .
type TypeSwitchGuardNode struct {
	IDENT       Token
	DEFINE      Token
	PrimaryExpr Expression
	PERIOD      Token
	LPAREN      Token
	TYPE        Token
	RPAREN      Token
}

// Source implements Node.
func (n *TypeSwitchGuardNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeSwitchGuardNode) Position() token.Position { panic("TODO") }

func (p *parser) typeSwitchGuard() *TypeSwitchGuardNode {
	var (
		ok          bool
		identTok    Token
		defineTok   Token
		primaryExpr Expression
		periodTok   Token
		lparenTok   Token
		typeTok     Token
		rparenTok   Token
	)
	// ebnf.Sequence [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" ctx [ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT]
	{
		ix := p.ix
		// *ebnf.Option [ identifier ":=" ] ctx [ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT]
		switch p.c() {
		case IDENT:
			// ebnf.Sequence identifier ":=" ctx [IDENT]
			{
				if p.peek(1) != DEFINE {
					goto _0
				}
				// *ebnf.Name identifier ctx [IDENT]
				identTok = p.expect(IDENT)
				// *ebnf.Token ":=" ctx [DEFINE]
				defineTok = p.expect(DEFINE)
			}
		}
		goto _1
	_0:
		defineTok = Token{}
		identTok = Token{}
	_1:
		// *ebnf.Name PrimaryExpr ctx []
		switch p.c() {
		case ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT:
			if primaryExpr = p.primaryExpr(false); primaryExpr == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "." ctx []
		if periodTok, ok = p.accept(PERIOD); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "(" ctx []
		if lparenTok, ok = p.accept(LPAREN); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "type" ctx []
		if typeTok, ok = p.accept(TYPE); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Token ")" ctx []
		if rparenTok, ok = p.accept(RPAREN); !ok {
			p.back(ix)
			return nil
		}
	}
	return &TypeSwitchGuardNode{
		IDENT:       identTok,
		DEFINE:      defineTok,
		PrimaryExpr: primaryExpr,
		PERIOD:      periodTok,
		LPAREN:      lparenTok,
		TYPE:        typeTok,
		RPAREN:      rparenTok,
	}
}

// TypeSwitchStmtNode represents the production
//
//	TypeSwitchStmt = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
type TypeSwitchStmtNode struct {
	SWITCH          Token
	SimpleStmt      *SimpleStmtNode
	SEMICOLON       Token
	TypeSwitchGuard *TypeSwitchGuardNode
	LBRACE          Token
	List            []struct{ TypeCaseClause *TypeCaseClauseNode }
	RBRACE          Token
}

// Source implements Node.
func (n *TypeSwitchStmtNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeSwitchStmtNode) Position() token.Position { return n.SWITCH.Position() }

func (p *parser) typeSwitchStmt() *TypeSwitchStmtNode {
	var (
		ok              bool
		switchTok       Token
		simpleStmt      *SimpleStmtNode
		semicolonTok    Token
		typeSwitchGuard *TypeSwitchGuardNode
		lbraceTok       Token
		list            []struct{ TypeCaseClause *TypeCaseClauseNode }
		rbraceTok       Token
	)
	// ebnf.Sequence "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" ctx [SWITCH]
	{
		ix := p.ix
		// *ebnf.Token "switch" ctx [SWITCH]
		switchTok = p.expect(SWITCH)
		// *ebnf.Option [ SimpleStmt ";" ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR:
			// ebnf.Sequence SimpleStmt ";" ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name SimpleStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR]
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
						p.back(ix)
						goto _0
					}
				default:
					p.back(ix)
					goto _0
				}
				// *ebnf.Token ";" ctx []
				if semicolonTok, ok = p.accept(SEMICOLON); !ok {
					p.back(ix)
					goto _0
				}
			}
		}
		goto _1
	_0:
		semicolonTok = Token{}
		simpleStmt = nil
	_1:
		// *ebnf.Name TypeSwitchGuard ctx []
		switch p.c() {
		case ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT:
			if typeSwitchGuard = p.typeSwitchGuard(); typeSwitchGuard == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "{" ctx []
		if lbraceTok, ok = p.accept(LBRACE); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { TypeCaseClause } ctx []
	_2:
		{
			var typeCaseClause *TypeCaseClauseNode
			switch p.c() {
			case CASE, DEFAULT:
				// *ebnf.Name TypeCaseClause ctx [CASE, DEFAULT]
				if typeCaseClause = p.typeCaseClause(); typeCaseClause == nil {
					goto _3
				}
				list = append(list, struct{ TypeCaseClause *TypeCaseClauseNode }{TypeCaseClause: typeCaseClause})
				goto _2
			}
		_3:
		}
		// *ebnf.Token "}" ctx []
		if rbraceTok, ok = p.accept(RBRACE); !ok {
			p.back(ix)
			return nil
		}
	}
	return &TypeSwitchStmtNode{
		SWITCH:          switchTok,
		SimpleStmt:      simpleStmt,
		SEMICOLON:       semicolonTok,
		TypeSwitchGuard: typeSwitchGuard,
		LBRACE:          lbraceTok,
		List:            list,
		RBRACE:          rbraceTok,
	}
}

// TypeTermNode represents the production
//
//	TypeTerm = Type | UnderlyingType .
type TypeTermNode struct {
	Type           *TypeNode
	UnderlyingType *UnderlyingTypeNode
}

// Source implements Node.
func (n *TypeTermNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *TypeTermNode) Position() token.Position { panic("TODO") }

func (p *parser) typeTerm() *TypeTermNode {
	var (
		typeNode       *TypeNode
		underlyingType *UnderlyingTypeNode
	)
	// ebnf.Alternative Type | UnderlyingType ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
	switch p.c() {
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 0
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if typeNode = p.type1(); typeNode == nil {
			goto _0
		}
		break
	_0:
		typeNode = nil
		return nil
	case TILDE: // 1
		// *ebnf.Name UnderlyingType ctx [TILDE]
		if underlyingType = p.underlyingType(); underlyingType == nil {
			goto _2
		}
		break
	_2:
		underlyingType = nil
		return nil
	default:
		return nil
	}
	return &TypeTermNode{
		Type:           typeNode,
		UnderlyingType: underlyingType,
	}
}

// UnaryExprNode represents the production
//
//	UnaryExpr = PrimaryExpr | ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr .
type UnaryExprNode struct {
	Op        Token
	UnaryExpr Expression
}

// Source implements Node.
func (n *UnaryExprNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *UnaryExprNode) Position() token.Position { panic("TODO") }

func (p *parser) unaryExpr(preBlock bool) Expression {
	var (
		primaryExpr Expression
		op          Token
		unaryExpr   Expression
	)
	// ebnf.Alternative PrimaryExpr | ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	switch p.c() {
	case CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT: // 0
		// *ebnf.Name PrimaryExpr ctx [CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT]
		if primaryExpr = p.primaryExpr(preBlock); primaryExpr == nil {
			return nil
		}
		return primaryExpr
	case ARROW, MUL: // 0 1
		// *ebnf.Name PrimaryExpr ctx [ARROW, MUL]
		if primaryExpr = p.primaryExpr(preBlock); primaryExpr == nil {
			goto _2
		}
		return primaryExpr
	_2:
		primaryExpr = nil
		// ebnf.Sequence ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx [ARROW, MUL]
		{
			ix := p.ix
			// *ebnf.Group ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) ctx [ARROW, MUL]
			// ebnf.Alternative "+" | "-" | "!" | "^" | "*" | "&" | "<-" ctx [ARROW, MUL]
			op = p.consume()
			// *ebnf.Name UnaryExpr ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if unaryExpr = p.unaryExpr(preBlock); unaryExpr == nil {
					p.back(ix)
					goto _3
				}
			default:
				p.back(ix)
				goto _3
			}
		}
		break
	_3:
		unaryExpr = nil
		return nil
	case ADD, AND, NOT, SUB, XOR: // 1
		// ebnf.Sequence ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx [ADD, AND, NOT, SUB, XOR]
		{
			ix := p.ix
			// *ebnf.Group ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) ctx [ADD, AND, NOT, SUB, XOR]
			// ebnf.Alternative "+" | "-" | "!" | "^" | "*" | "&" | "<-" ctx [ADD, AND, NOT, SUB, XOR]
			op = p.consume()
			// *ebnf.Name UnaryExpr ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if unaryExpr = p.unaryExpr(preBlock); unaryExpr == nil {
					p.back(ix)
					goto _8
				}
			default:
				p.back(ix)
				goto _8
			}
		}
		break
	_8:
		op = Token{}
		unaryExpr = nil
		return nil
	default:
		return nil
	}
	return &UnaryExprNode{
		Op:        op,
		UnaryExpr: unaryExpr,
	}
}

// UnderlyingTypeNode represents the production
//
//	UnderlyingType = "~" Type .
type UnderlyingTypeNode struct {
	TILDE Token
	Type  *TypeNode
}

// Source implements Node.
func (n *UnderlyingTypeNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *UnderlyingTypeNode) Position() token.Position { return n.TILDE.Position() }

func (p *parser) underlyingType() *UnderlyingTypeNode {
	var (
		tildeTok Token
		typeNode *TypeNode
	)
	// ebnf.Sequence "~" Type ctx [TILDE]
	{
		switch p.peek(1) {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "~" ctx [TILDE]
		tildeTok = p.expect(TILDE)
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if typeNode = p.type1(); typeNode == nil {
			p.back(ix)
			return nil
		}
	}
	return &UnderlyingTypeNode{
		TILDE: tildeTok,
		Type:  typeNode,
	}
}

// VarDeclNode represents the production
//
//	VarDecl = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
type VarDeclNode struct {
	VAR     Token
	VarSpec *VarSpecNode
	LPAREN  Token
	List    []struct {
		VarSpec   *VarSpecNode
		SEMICOLON Token
	}
	RPAREN Token
}

// Source implements Node.
func (n *VarDeclNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *VarDeclNode) Position() token.Position { return n.VAR.Position() }

func (p *parser) varDecl() *VarDeclNode {
	var (
		ok        bool
		varTok    Token
		varSpec   *VarSpecNode
		lparenTok Token
		list      []struct {
			VarSpec   *VarSpecNode
			SEMICOLON Token
		}
		rparenTok Token
	)
	// ebnf.Sequence "var" ( VarSpec | "(" { VarSpec ";" } ")" ) ctx [VAR]
	{
		switch p.peek(1) {
		case IDENT, LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "var" ctx [VAR]
		varTok = p.expect(VAR)
		// *ebnf.Group ( VarSpec | "(" { VarSpec ";" } ")" ) ctx [IDENT, LPAREN]
		// ebnf.Alternative VarSpec | "(" { VarSpec ";" } ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name VarSpec ctx [IDENT]
			if varSpec = p.varSpec(); varSpec == nil {
				goto _0
			}
			break
		_0:
			varSpec = nil
			p.back(ix)
			return nil
		case LPAREN: // 1
			// ebnf.Sequence "(" { VarSpec ";" } ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				lparenTok = p.expect(LPAREN)
				// *ebnf.Repetition { VarSpec ";" } ctx []
			_4:
				{
					var varSpec *VarSpecNode
					var semicolonTok Token
					switch p.c() {
					case IDENT:
						// ebnf.Sequence VarSpec ";" ctx [IDENT]
						ix := p.ix
						// *ebnf.Name VarSpec ctx [IDENT]
						if varSpec = p.varSpec(); varSpec == nil {
							p.back(ix)
							goto _5
						}
						// *ebnf.Token ";" ctx []
						if semicolonTok, ok = p.accept(SEMICOLON); !ok {
							p.back(ix)
							goto _5
						}
						list = append(list, struct {
							VarSpec   *VarSpecNode
							SEMICOLON Token
						}{VarSpec: varSpec, SEMICOLON: semicolonTok})
						goto _4
					}
				_5:
				}
				// *ebnf.Token ")" ctx []
				if rparenTok, ok = p.accept(RPAREN); !ok {
					p.back(ix)
					goto _2
				}
			}
			break
		_2:
			lparenTok = Token{}
			rparenTok = Token{}
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
	}
	return &VarDeclNode{
		VAR:     varTok,
		VarSpec: varSpec,
		LPAREN:  lparenTok,
		List:    list,
		RPAREN:  rparenTok,
	}
}

// VarSpecNode represents the production
//
//	VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
type VarSpecNode struct {
	lexicalScoper
	IdentifierList  *IdentifierListNode
	Type            *TypeNode
	ASSIGN          Token
	ExpressionList  *ExpressionListNode
	ASSIGN2         Token
	ExpressionList2 *ExpressionListNode
}

// Source implements Node.
func (n *VarSpecNode) Source(full bool) string { return nodeSource(n, full) }

// Position implements Node.
func (n *VarSpecNode) Position() token.Position { return n.IdentifierList.Position() }

func (p *parser) varSpec() (r *VarSpecNode) {
	var (
		identifierList  *IdentifierListNode
		typeNode        *TypeNode
		assignTok       Token
		expressionList  *ExpressionListNode
		assign2Tok      Token
		expressionList2 *ExpressionListNode
	)
	// ebnf.Sequence IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) ctx [IDENT]
	{
		ix := p.ix
		// *ebnf.Name IdentifierList ctx [IDENT]
		if identifierList = p.identifierList(); identifierList == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Group ( Type [ "=" ExpressionList ] | "=" ExpressionList ) ctx []
		// ebnf.Alternative Type [ "=" ExpressionList ] | "=" ExpressionList ctx [ARROW, ASSIGN, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 0
			// ebnf.Sequence Type [ "=" ExpressionList ] ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			{
				ix := p.ix
				// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
				if typeNode = p.type1(); typeNode == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Option [ "=" ExpressionList ] ctx []
				switch p.c() {
				case ASSIGN:
					// ebnf.Sequence "=" ExpressionList ctx [ASSIGN]
					{
						switch p.peek(1) {
						case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
						default:
							goto _2
						}
						ix := p.ix
						// *ebnf.Token "=" ctx [ASSIGN]
						assignTok = p.expect(ASSIGN)
						// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
						if expressionList = p.expressionList(false); expressionList == nil {
							p.back(ix)
							goto _2
						}
					}
				}
				goto _3
			_2:
				assignTok = Token{}
				expressionList = nil
			_3:
			}
			break
		_0:
			assignTok = Token{}
			expressionList = nil
			typeNode = nil
			p.back(ix)
			return nil
		case ASSIGN: // 1
			// ebnf.Sequence "=" ExpressionList ctx [ASSIGN]
			{
				switch p.peek(1) {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				default:
					goto _4
				}
				ix := p.ix
				// *ebnf.Token "=" ctx [ASSIGN]
				assign2Tok = p.expect(ASSIGN)
				// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expressionList2 = p.expressionList(false); expressionList2 == nil {
					p.back(ix)
					goto _4
				}
			}
			break
		_4:
			assign2Tok = Token{}
			expressionList2 = nil
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
	}
	sc := p.sc
	r = &VarSpecNode{
		lexicalScoper:   newLexicalScoper(sc),
		IdentifierList:  identifierList,
		Type:            typeNode,
		ASSIGN:          assignTok,
		ExpressionList:  expressionList,
		ASSIGN2:         assign2Tok,
		ExpressionList2: expressionList2,
	}
	//TODO visible := int32(p.ix)
	//TODO p.declare(sc, identifierList.IDENT, r, visible, false)
	//TODO for _, v := range identifierList.List {
	//TODO 	p.declare(sc, v.IDENT, r, visible, false)
	//TODO }
	return r
}
