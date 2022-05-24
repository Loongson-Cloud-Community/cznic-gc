// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"go/token"
)

var (
	_ = []Node{
		(*AliasDecl)(nil),
		(*Arguments)(nil),
		(*ArrayType)(nil),
		(*Assignment)(nil),
		(*BinaryExpression)(nil),
		(*Block)(nil),
		(*ChannelType)(nil),
		(*CompositeLit)(nil),
		(*ConstDecl)(nil),
		(*ConstSpec)(nil),
		(*Conversion)(nil),
		(*DeferStmt)(nil),
		(*EmbeddedField)(nil),
		(*ExpressionListItem)(nil),
		(*FieldDecl)(nil),
		(*ForClause)(nil),
		(*ForStmt)(nil),
		(*FunctionDecl)(nil),
		(*FunctionLit)(nil),
		(*FunctionType)(nil),
		(*GoStmt)(nil),
		(*IdentifierListItem)(nil),
		(*IfStmt)(nil),
		(*ImportDecl)(nil),
		(*ImportSpec)(nil),
		(*ImportSpecItem)(nil),
		(*Index)(nil),
		(*InterfaceType)(nil),
		(*KeyedElement)(nil),
		(*LiteralValue)(nil),
		(*MapType)(nil),
		(*MethodDecl)(nil),
		(*MethodElem)(nil),
		(*PackageClause)(nil),
		(*ParameterDecl)(nil),
		(*Parameters)(nil),
		(*PointerType)(nil),
		(*QualifiedIdent)(nil),
		(*RangeClause)(nil),
		(*ReturnStmt)(nil),
		(*Selector)(nil),
		(*ShortVarDecl)(nil),
		(*Signature)(nil),
		(*SliceType)(nil),
		(*SourceFile)(nil),
		(*Statement)(nil),
		(*StructType)(nil),
		(*TopLevelDecl)(nil),
		(*Type)(nil),
		(*TypeArgs)(nil),
		(*TypeAssertion)(nil),
		(*TypeDecl)(nil),
		(*TypeDef)(nil),
		(*TypeElem)(nil),
		(*TypeListItem)(nil),
		(*TypeName)(nil),
		(*TypeParamDecl)(nil),
		(*TypeParamListItem)(nil),
		(*TypeParameters)(nil),
		(*TypeSpecItem)(nil),
		(*TypeTerm)(nil),
		(*UnaryExpr)(nil),
		(*VarDecl)(nil),
		(*VarSpec)(nil),
	}

	notok Token

	opPrecedence = map[Ch]int{
		'%':     5,
		'&':     5,
		'*':     5,
		'+':     4,
		'-':     4,
		'/':     5,
		'<':     3,
		'>':     3,
		'^':     4,
		'|':     4,
		AND_NOT: 5,
		EQ:      3,
		GE:      3,
		LAND:    2,
		LE:      3,
		LOR:     1,
		NE:      3,
		SHL:     5,
		SHR:     5,
	}
)

// ParseSourceFileConfig configures ParseSourceFile.
type ParseSourceFileConfig struct {
	// Accept, if non nil, is called once the package clause and imports are
	// parsed. If Accept return false the parsing stops and an error is returned.
	// Passing nil Accept is the same as passing a function that always returns
	// true.
	Accept func(*SourceFile) bool

	AllErrors bool
}

type parser struct {
	cfg       *ParseSourceFileConfig
	loophacks []bool
	s         *Scanner

	loophack bool
}

func newParser(cfg *ParseSourceFileConfig, s *Scanner) *parser {
	s.Scan()
	return &parser{cfg: cfg, s: s}
}

func (p *parser) ch() Ch { return p.s.Tok.Ch }

func (p *parser) Err() error { return p.s.Err() }

func (p *parser) err(msg string, args ...interface{}) { p.errNode(p.s.Tok, msg, args...) }

func (p *parser) errNode(n Node, msg string, args ...interface{}) {
	p.s.errs.err(n.Position(), msg, args...)
	if !p.cfg.AllErrors && len(p.s.errs) >= 10 {
		p.s.close()
	}
}

func (p *parser) must(c Ch) (r Token) {
	if p.ch() != c {
		p.err("expected %v, got %v", c.str(), r.Ch.str())
	}
	return p.shift()
}

func (p *parser) opt(c Ch) (r Token) {
	if p.ch() == c {
		r = p.shift()
	}
	return r
}

func (p *parser) shift() (r Token) {
	r = p.s.Tok
	p.s.Scan()
	switch p.ch() {
	case FOR, IF, SELECT, SWITCH:
		p.loophack = true
	case '(', '[':
		if p.loophack || len(p.loophacks) != 0 {
			p.loophacks = append(p.loophacks, p.loophack)
			p.loophack = false
		}
	case ')', ']':
		if n := len(p.loophacks); n != 0 {
			p.loophack = p.loophacks[n-1]
			p.loophacks = p.loophacks[:n-1]
		}
	case '{':
		if p.loophack {
			p.s.Tok.Ch = body
			p.loophack = false
		}
	}
	return r
}

func (p *parser) fixlbr(lbr bool) {
	if lbr {
		p.loophack = true
	}
}

// SourceFile describes a source file.
//
//  SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
type SourceFile struct {
	PackageClause *PackageClause
	ImportDecls   []*ImportDecl
	TopLevelDecls []*TopLevelDecl
	EOF           Token
}

// Positions implements Node.
func (n *SourceFile) Position() token.Position { return n.PackageClause.Position() }

// ParseSourceFile parses buf and returns a *SourceFile or an error, if any.
// Positions are reported as if buf is coming from a file named name. The
// buffer becomes owned by the *SourceFile and must not be modified after
// calling ParseSourceFile.
func ParseSourceFile(cfg *ParseSourceFileConfig, name string, buf []byte) (r *SourceFile, err error) {
	s, err := NewScanner(name, buf)
	if err != nil {
		return nil, err
	}

	p := newParser(cfg, s)
	r = &SourceFile{PackageClause: p.packageClause(), ImportDecls: p.importDecls()}
	if err := p.s.errs.Err(); err != nil {
		return nil, err
	}

	if cfg.Accept != nil && !cfg.Accept(r) {
		p.err(errorf("rejected"))
		return nil, p.Err()
	}

	r.TopLevelDecls = p.topLevelDecls()
	r.EOF = p.must(EOF)
	if err := p.Err(); err != nil {
		return nil, err
	}

	return r, nil
}

// TopLevelDecl describes a top level declaration.
type TopLevelDecl struct {
	Decl      Node
	Semicolon Token
}

// Positions implements Node.
func (n *TopLevelDecl) Position() token.Position { return n.Decl.Position() }

func (p *parser) topLevelDecls() (r []*TopLevelDecl) {
	for {
		var n *TopLevelDecl
		switch p.ch() {
		case CONST:
			n = &TopLevelDecl{Decl: p.constDecl()}
		case FUNC:
			fn := p.shift()
			switch p.ch() {
			case '(':
				n = &TopLevelDecl{Decl: p.methodDecl(fn)}
			case IDENTIFIER:
				n = &TopLevelDecl{Decl: p.functionDecl(fn)}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case TYPE:
			n = &TopLevelDecl{Decl: p.typeDecl()}
		case VAR:
			n = &TopLevelDecl{Decl: p.varDecl()}
		default:
			return r
		}
		n.Semicolon = p.must(';')
		r = append(r, n)
	}
}

// MethodDecl describes a method declaration.
//
//  MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
type MethodDecl struct {
	Func         Token
	Receiver     *Parameters
	MethodName   Token
	Signature    *Signature
	FunctionBody *Block
}

// Positions implements Node.
func (n *MethodDecl) Position() token.Position { return n.Func.Position() }

// MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
func (p *parser) methodDecl(fn Token) (r *MethodDecl) {
	r = &MethodDecl{Func: fn, MethodName: p.must(IDENTIFIER), Signature: p.signature()}
	if p.ch() == '{' {
		r.FunctionBody = p.functionBody()
	}
	return r
}

// FunctionDecl describes a function declaration.
//
//  FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
type FunctionDecl struct {
	Func           Token
	FunctionName   Token
	TypeParameters *TypeParameters
	Signature      *Signature
	FunctionBody   *Block
}

// Positions implements Node.
func (n *FunctionDecl) Position() token.Position { return n.Func.Position() }

// FunctionDecl = "func" FunctionName TypeParameters Signature [ FunctionBody ]
// 	| "func" FunctionName Signature [ FunctionBody ] .
func (p *parser) functionDecl(fn Token) (r *FunctionDecl) {
	r = &FunctionDecl{Func: fn, FunctionName: p.must(IDENTIFIER)}
	switch p.ch() {
	case '(':
		r.Signature = p.signature()
	case '[':
		r.TypeParameters = p.typeParameters(notok, notok)
		r.Signature = p.signature()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	if p.ch() == '{' {
		r.FunctionBody = p.functionBody()
	}
	return r
}

// Block describes a compound statement.
//
//  Block = "{" StatementList "}" .
type Block struct {
	LBrace        Token
	StatementList []*Statement
	RBrace        Token
}

// Positions implements Node.
func (n *Block) Position() token.Position { return n.LBrace.Position() }

// Block = "{" StatementList "}" .
func (p *parser) functionBody() (r *Block) {
	return &Block{LBrace: p.must('{'), StatementList: p.statementList(), RBrace: p.must('}')}
}

// Statement describes a statement list item.
//
//  StatementList = { Statement ";" } .
type Statement struct {
	Statement Node
	Semicolon Token
}

// Positions implements Node.
func (n *Statement) Position() token.Position { return n.Statement.Position() }

// StatementList = { [ Statement ] ";" } [ Statement ] .
func (p *parser) statementList() (r []*Statement) {
	//        Statement case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, BREAK, CHAN, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT_LIT, FOR, FUNC, GO, GOTO, IDENTIFIER, IF, IMAG_LIT, INTERFACE, INT_LIT, MAP, RETURN, RUNE_LIT, SELECT, STRING_LIT, STRUCT, SWITCH, TYPE, VAR:
	for {
		var n *Statement
		switch p.ch() {
		case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, BREAK, CHAN, CONTINUE, FALLTHROUGH, FLOAT_LIT, FUNC, GOTO, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, SELECT, STRING_LIT, STRUCT:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		case CONST:
			n = &Statement{Statement: p.constDecl()}
		case DEFER:
			n = &Statement{Statement: p.deferStmt()}
		case FOR:
			n = &Statement{Statement: p.forStmt()}
		case GO:
			n = &Statement{Statement: &GoStmt{Go: p.shift(), Expression: p.expression(notok)}}
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			case DEFINE:
				// RecvStmt = IdentifierList ":=" RecvExpr
				// ShortVarDecl = IdentifierList ":=" ExpressionList .
				n = &Statement{Statement: &ShortVarDecl{IdentifierList: p.identifierList(id), Define: p.shift(), ExpressionList: p.expressionList(notok)}}
			case '.', '(':
				// MethodExpr = ReceiverType "." MethodName .
				// QualifiedIdent = PackageName "." identifier .
				expr := p.expression(id)
				switch p.ch() {
				case ';':
					n = &Statement{Statement: expr}
				case '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
					n = &Statement{Statement: &Assignment{LExpressionList: []*ExpressionListItem{{Expression: expr}}, AssOp: p.shift(), RExpressionList: p.expressionList(notok)}}
				default:
					p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
					p.shift()
					return r
				}
			case '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
				n = &Statement{Statement: &Assignment{LExpressionList: []*ExpressionListItem{{Expression: id}}, AssOp: p.shift(), RExpressionList: p.expressionList(notok)}}
			case ',', '[':
				el := p.expressionList(id)
				switch p.ch() {
				case DEFINE:
					il := p.exprList2IdList(p.s.Tok, el)
					n = &Statement{Statement: &ShortVarDecl{IdentifierList: il, Define: p.shift(), ExpressionList: p.expressionList(notok)}}
				case '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
					n = &Statement{Statement: &Assignment{LExpressionList: el, AssOp: p.shift(), RExpressionList: p.expressionList(notok)}}
				default:
					_ = el
					p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
					p.shift()
					return r
				}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case IF:
			n = &Statement{Statement: p.ifStmt()}
		case RETURN:
			n = &Statement{Statement: p.returnStmt()}
		case SWITCH:
			n = &Statement{Statement: p.switchStmt()}
		case TYPE:
			n = &Statement{Statement: p.typeDecl()}
		case VAR:
			n = &Statement{Statement: p.varDecl()}
		case '}':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
		r = append(r, n)
		switch p.ch() {
		case ';':
			n.Semicolon = p.shift()
		case '}':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
}

// GoStmt describes a go statement.
//
type GoStmt struct {
	Go         Token
	Expression Node
}

// Positions implements Node.
func (n *GoStmt) Position() token.Position { return n.Go.Position() }

// DeferStmt describes a defer statement.
//
//  DeferStmt = "defer" Expression .
type DeferStmt struct {
	Defer      Token
	Expression Node
}

// Positions implements Node.
func (n *DeferStmt) Position() token.Position { return n.Defer.Position() }

func (p *parser) deferStmt() (r *DeferStmt) {
	return &DeferStmt{Defer: p.must(DEFER), Expression: p.expression(notok)}
}

// Assignment describes a short variable declaration.
//
// Assignment = ExpressionList assign_op ExpressionList .
type Assignment struct {
	LExpressionList []*ExpressionListItem
	AssOp           Token
	RExpressionList []*ExpressionListItem
}

// Positions implements Node.
func (n *Assignment) Position() token.Position { return n.LExpressionList[0].Position() }

// ShortVarDecl describes a short variable declaration.
//
//  ShortVarDecl = IdentifierList ":=" ExpressionList .
type ShortVarDecl struct {
	IdentifierList []*IdentifierListItem
	Define         Token
	ExpressionList []*ExpressionListItem
}

// Positions implements Node.
func (n *ShortVarDecl) Position() token.Position { return n.IdentifierList[0].Position() }

// ForStmt describes a for statement.
//
//  ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
type ForStmt struct {
	For         Token
	ForClause   *ForClause
	RangeClause *RangeClause
	Block       *Block
}

// Positions implements Node.
func (n *ForStmt) Position() token.Position { return n.For.Position() }

// ForStmt = "for" ForClause LoopBody
// 	| "for" RangeClause LoopBody
// 	| "for" Condition LoopBody
// 	| "for" LoopBody .
func (p *parser) forStmt() (r *ForStmt) {
	//        Condition case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
	//        ForClause case '!', '&', '(', '*', '+', '-', ';', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
	//      RangeClause case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RANGE, RUNE_LIT, STRING_LIT, STRUCT:
	r = &ForStmt{For: p.must(FOR)}
	switch p.ch() {
	case ';':
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case RANGE:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		el := p.expressionList(notok)
		switch p.ch() {
		case DEFINE:
			def := p.shift()
			switch p.ch() {
			case RANGE:
				r.RangeClause = &RangeClause{ExpressionList: el, Assign: def, Range: p.shift(), Expression: p.expression(notok)}
			//       Expression
			case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
				ss := &ShortVarDecl{IdentifierList: p.exprList2IdList(def, el), Define: def, ExpressionList: p.expressionList(notok)}
				r.ForClause = p.forClause(ss)
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	r.Block = p.loopBody()
	return r
}

func (p *parser) loopBody() (r *Block) {
	r = &Block{LBrace: p.must(body), StatementList: p.statementList(), RBrace: p.must('}')}
	r.LBrace.Ch = '{'
	return r
}

// ForClause describes a for clause.
//
//  ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
type ForClause struct {
	InitStmt   Node
	Semicolon  Token
	Condition  Node
	Semicolon2 Token
	PostStmt   Node
}

// Positions implements Node.
func (n *ForClause) Position() token.Position {
	if n.InitStmt != nil {
		return n.InitStmt.Position()
	}

	return n.Semicolon.Position()
}

func (p *parser) forClause(initStmt Node) (r *ForClause) {
	r = &ForClause{}
	switch {
	case initStmt != nil:
		r.InitStmt = initStmt
		r.Semicolon = p.must(';')
		if p.ch() != ';' {
			r.Condition = p.expression(notok)
		}
		r.Semicolon2 = p.must(';')
		switch p.ch() {
		//         PostStmt
		case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
			r.PostStmt = p.simpleStmt()
		}
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// RangeClause describes a range clause.
//
type RangeClause struct {
	ExpressionList []*ExpressionListItem
	Assign         Token
	Range          Token
	Expression     Node
}

// Positions implements Node.
func (n *RangeClause) Position() token.Position { return n.ExpressionList[0].Position() }

// SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
// ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] body { ExprCaseClause } "}" .
// TypeSwitchStmt = "switch" [ SimpleStmt ";" ] TypeSwitchGuard body { TypeCaseClause } "}" .
func (p *parser) switchStmt() (r Node) {
	sw := p.must(SWITCH)
	switch p.ch() {
	default:
		_ = sw
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// ReturnStmt describes a return statement.
//
//  ReturnStmt = "return" [ ExpressionList ] .
type ReturnStmt struct {
	Return         Token
	ExpressionList []*ExpressionListItem
}

// Positions implements Node.
func (n *ReturnStmt) Position() token.Position { return n.Return.Position() }

func (p *parser) returnStmt() (r *ReturnStmt) {
	r = &ReturnStmt{Return: p.must(RETURN)}
	switch p.ch() {
	//   ExpressionList
	case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		r.ExpressionList = p.expressionList(notok)
	case ';':
		// ok
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
	}
	return r
}

// IfStmt describes an if statement.
//
//  IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
type IfStmt struct {
	If         Token
	SimpleStmt Node
	Semicolon  Token
	Expression Node
	Block      *Block
}

// Positions implements Node.
func (n *IfStmt) Position() token.Position { return n.If.Position() }

// IfStmt = "if" [ SimpleStmt ";" ] Expression LoopBody [
// 		 "else" ( IfStmt | Block )
// 	  ] .
func (p *parser) ifStmt() (r *IfStmt) {
	//       SimpleStmt case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
	//       Expression case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
	r = &IfStmt{If: p.must(IF)}
	switch p.ch() {
	case IDENTIFIER:
		ss := p.simpleStmt()
		switch p.ch() {
		case body:
			r.Expression = ss
		case ';':
			r.SimpleStmt = ss
			r.Semicolon = p.shift()
			r.Expression = p.expression(notok)
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	case '!', '&', '(', '*', '+', '-', '[', '^', CHAN, FLOAT_LIT, FUNC, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		expr := p.expression(notok)
		switch p.ch() {
		case body:
			r.Expression = expr
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	r.Block = p.loopBody()
	switch p.ch() {
	case ';':
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// SimpleStmt = IncDecStmt
// 	| ShortVarDecl
// 	| Assignment
// 	| SendStmt
// 	| ExpressionStmt .
func (p *parser) simpleStmt() (r Node) {
	switch p.ch() {
	case IDENTIFIER:
		el := p.expressionList(notok)
		switch p.ch() {
		case body:
			if len(el) != 1 {
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}

			return el[0]
		case DEFINE:
			return &ShortVarDecl{IdentifierList: p.exprList2IdList(p.s.Tok, el), Define: p.shift(), ExpressionList: p.expressionList(notok)}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

func (p *parser) exprList2IdList(n Node, el []*ExpressionListItem) (r []*IdentifierListItem) {
	if len(el) == 0 {
		p.errNode(n, errorf("TODO %v", p.s.Tok.Ch.str()))
		return r
	}

	for _, v := range el {
		switch x := v.Expression.(type) {
		case Token:
			if x.Ch != IDENTIFIER {
				p.errNode(x, errorf("TODO %v", p.s.Tok.Ch.str()))
				return r
			}
			r = append(r, &IdentifierListItem{Ident: x, Comma: v.Comma})
		default:
			p.errNode(v, errorf("TODO %T", x))
			return r
		}
	}
	return r
}

// Signature describes a function signature.
//
//  Signature = Parameters [ Result ] .
type Signature struct {
	Parameters *Parameters
	Result     Node
}

// Positions implements Node.
func (n *Signature) Position() token.Position { return n.Parameters.Position() }

// Signature = Parameters [ Result ] .
func (p *parser) signature() (r *Signature) {
	r = &Signature{Parameters: p.parameters()}
	switch p.ch() {
	case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		r.Result = p.result()
	}
	return r
}

func (p *parser) result() (r Node) {
	switch p.ch() {
	case '(':
		par := p.parameters()
		r = par
		if par != nil && len(par.ParameterList) == 1 {
			pd := par.ParameterList[0]
			if len(pd.IdentifierList) == 0 && !pd.ELLIPSIS.IsValid() {
				//TODO convert Parameters to (Type)
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		}
	case '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		return p.type1()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
	}
	return r
}

// Parameters describes function parameters or a function result.
//
//  Parameters = "(" [ ParameterList [ "," ] ] ")" .
//  ParameterList = ParameterDecl { "," ParameterDecl } .
type Parameters struct {
	LParen        Token
	ParameterList []*ParameterDecl
	Comma         Token
	RParen        Token
}

// Positions implements Node.
func (n *Parameters) Position() token.Position { return n.LParen.Position() }

// Parameters = "(" [ ParameterList [ "," ] ] ")" .
func (p *parser) parameters() (r *Parameters) {
	r = &Parameters{LParen: p.must('(')}
	switch p.ch() {
	case '(', '*', '[', ARROW, CHAN, ELLIPSIS, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		r.ParameterList = p.parameterList()
	}
	r.Comma = p.opt(',')
	r.RParen = p.must(')')
	return r
}

// ParameterDecl describes a parameter declaration.
//
//  ParameterDecl = [ IdentifierList ] [ "..." ] Type .
type ParameterDecl struct {
	IdentifierList []*IdentifierListItem
	ELLIPSIS       Token
	Type           *Type
	Comma          Token
}

// Positions implements Node.
func (n *ParameterDecl) Position() token.Position {
	switch {
	case len(n.IdentifierList) != 0:
		return n.IdentifierList[0].Position()
	case n.ELLIPSIS.IsValid():
		return n.ELLIPSIS.Position()
	default:
		return n.Type.Position()
	}
}

// ParameterDecl = identifier "..." Type
// 	| identifier Type
// 	| "..." Type
// 	| Type .
func (p *parser) parameterDecl() (r *ParameterDecl) {
	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	p.shift()
	return r
}

func (p *parser) parameterList() (r []*ParameterDecl) {
	for {
		var n *ParameterDecl
		switch p.ch() {
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			case ELLIPSIS:
				n = &ParameterDecl{IdentifierList: []*IdentifierListItem{{Ident: id}}, ELLIPSIS: p.shift(), Type: p.type1()}
			case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
				n = &ParameterDecl{IdentifierList: []*IdentifierListItem{{Ident: id}}, Type: p.type1()}
			case ',', ')':
				n = &ParameterDecl{Type: &Type{Type: &QualifiedIdent{Ident: id}}}
			case '.':
				n = &ParameterDecl{Type: &Type{Type: &QualifiedIdent{PackageName: id, Dot: p.shift(), Ident: p.must(IDENTIFIER)}}}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case ELLIPSIS:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		case '(', '*', '[', ARROW, CHAN, FUNC, INTERFACE, MAP, STRUCT:
			n = &ParameterDecl{Type: p.type1()}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
		n.Comma = p.opt(',')
		r = append(r, n)
		if p.ch() == ')' {
			return r
		}
	}
}

// Type describes a type.
//
//  Type = TypeName | TypeLit | "(" Type ")" .
type Type struct {
	LParen Token
	Type   Node // One of TypeName, TypeLit, *Type.
	RParen Token
}

// Positions implements Node.
func (n *Type) Position() token.Position {
	if n.LParen.IsValid() {
		return n.LParen.Position()
	}

	return n.Type.Position()
}

// Type = TypeName
// 	| TypeLit
// 	| "(" Type ")" .
func (p *parser) type1() (r *Type) {
	switch p.ch() {
	case '(':
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case '*':
		return &Type{Type: p.pointerType()}
	case '[':
		lbracket := p.shift()
		switch p.ch() {
		case ']':
			return &Type{Type: p.sliceType(lbracket)}
		case ELLIPSIS:
			return &Type{Type: &ArrayType{LBracket: lbracket, ArrayLength: p.shift(), RBracket: p.must(']'), ElementType: p.type1()}}
		//       Expression
		case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
			return &Type{Type: &ArrayType{LBracket: lbracket, ArrayLength: p.expression(notok), RBracket: p.must(']'), ElementType: p.type1()}}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	case ARROW:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case CHAN:
		return &Type{Type: p.channelType(notok)}
	case FUNC:
		return &Type{Type: p.functionType()}
	case IDENTIFIER:
		return &Type{Type: p.typeName()}
	case INTERFACE:
		return &Type{Type: p.interfaceType()}
	case MAP:
		return &Type{Type: p.mapType()}
	case STRUCT:
		return &Type{Type: p.structType()}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// ArrayType describes a channel type.
//
type ArrayType struct {
	LBracket    Token
	ArrayLength Node
	RBracket    Token
	ElementType *Type
}

// Positions implements Node.
func (n *ArrayType) Position() token.Position { return n.LBracket.Position() }

// ChannelType describes a channel type.
//
//  ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
type ChannelType struct {
	ArrowPre    Token
	Chan        Token
	ArrayPost   Token
	ElementType *Type
}

// Positions implements Node.
func (n *ChannelType) Position() token.Position {
	if n.ArrowPre.IsValid() {
		return n.ArrowPre.Position()
	}

	return n.Chan.Position()
}

func (p *parser) channelType(arrowPre Token) (r *ChannelType) {
	switch {
	case arrowPre.IsValid():
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
	default:
		switch p.ch() {
		case CHAN:
			r = &ChannelType{Chan: p.shift()}
			if p.ch() == ARROW {
				r.ArrayPost = p.shift()
			}
			r.ElementType = p.type1()
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
		}
	}
	return r
}

// FunctionType describes a function type.
//
//  FunctionType = "func" Signature .
type FunctionType struct {
	Func      Token
	Signature *Signature
}

// Positions implements Node.
func (n *FunctionType) Position() token.Position { return n.Func.Position() }

func (p *parser) functionType() (r *FunctionType) {
	return &FunctionType{Func: p.must(FUNC), Signature: p.signature()}
}

// MapType describes a map type.
//
//  MapType = "map" "[" KeyType "]" ElementType .
type MapType struct {
	Map         Token
	LBracket    Token
	KeyType     *Type
	RBracket    Token
	ElementType *Type
}

// Positions implements Node.
func (n *MapType) Position() token.Position { return n.Map.Position() }

func (p *parser) mapType() (r *MapType) {
	return &MapType{Map: p.must(MAP), LBracket: p.must('['), KeyType: p.type1(), RBracket: p.must(']'), ElementType: p.type1()}
}

// SliceType describes a slice type.
//
//  SliceType = "[" "]" ElementType .
type SliceType struct {
	LBracket    Token
	RBracket    Token
	ElementType *Type
}

// Positions implements Node.
func (n *SliceType) Position() token.Position { return n.LBracket.Position() }

func (p *parser) sliceType(lbracket Token) (r *SliceType) {
	return &SliceType{LBracket: lbracket, RBracket: p.must(']'), ElementType: p.type1()}
}

// InterfaceType describes an interface type.
//
//  InterfaceType = "interface" "{" { InterfaceElem ";" } "}" .
type InterfaceType struct {
	Interface      Token
	LBrace         Token
	InterfaceElems []Node
	RBrace         Token
}

// Positions implements Node.
func (n *InterfaceType) Position() token.Position { return n.Interface.Position() }

// InterfaceType = "interface" lbrace "#fixlbr" "}"
// 	| "interface" lbrace InterfaceElem { ";" InterfaceElem } [ ";" ] "#fixlbr" "}" .
//
// InterfaceElem = MethodElem | TypeElem .
// MethodElem = MethodName Signature .
// MethodName = identifier .
// TypeElem = TypeTerm { "|" TypeTerm } .
// TypeTerm = Type | UnderlyingType .
func (p *parser) interfaceType() (r *InterfaceType) {
	//       MethodElem case IDENTIFIER:
	//         TypeElem case '(', '*', '[', '~', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	var lbr bool
	r = &InterfaceType{Interface: p.must(INTERFACE), LBrace: p.lbrace(&lbr)}
	for {
		switch p.ch() {
		case '(', '*', '[', '~', ARROW, CHAN, FUNC, INTERFACE, MAP, STRUCT:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			case '(':
				n := &MethodElem{MethodName: id, Signature: p.signature(), Semicolon: p.opt(';')}
				r.InterfaceElems = append(r.InterfaceElems, n)
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case '}':
			p.fixlbr(lbr)
			r.RBrace = p.shift()
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
}

// MethodElem describes a method element.
//
type MethodElem struct {
	MethodName Token
	Signature  *Signature
	Semicolon  Token
}

// Positions implements Node.
func (n *MethodElem) Position() token.Position { return n.MethodName.Position() }

func (p *parser) lbrace(lbr *bool) (r Token) {
	switch p.ch() {
	case '{':
		return p.shift()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// EmbeddedField describes an embeded field.
//
//  EmbeddedField = [ "*" ] TypeName .
type EmbeddedField struct {
	Star     Token
	TypeName *QualifiedIdent
}

// Positions implements Node.
func (n *EmbeddedField) Position() token.Position {
	if n.Star.IsValid() {
		return n.Star.Position()
	}

	return n.TypeName.Position()
}

// FieldDecl describes a field declaration.
//
// FieldDecl = (IdentifierList Type | EmbeddedField) [ Tag ] .
type FieldDecl struct {
	IdentifierList []*IdentifierListItem
	Type           *Type
	EmbeddedField  *EmbeddedField
	Tag            Token
	Semicolon      Token
}

// Positions implements Node.
func (n *FieldDecl) Position() token.Position {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return n.EmbeddedField.Position()
}

// StructType describes a struct type.
//
//  StructTyp = "struct" "{" { FieldDecl ";" } "}" .
type StructType struct {
	Struct     Token
	LBrace     Token
	FieldDecls []*FieldDecl
	RBrace     Token
}

// Positions implements Node.
func (n *StructType) Position() token.Position { return n.Struct.Position() }

// StructType = "struct" lbrace "#fixlbr" "}"
// 	| "struct" lbrace FieldDecl { ";" FieldDecl } [ ";" ] "#fixlbr" "}" .
func (p *parser) structType() (r *StructType) {
	//        FieldDecl case '*', IDENTIFIER:
	var lbr bool
	r = &StructType{Struct: p.must(STRUCT), LBrace: p.lbrace(&lbr)}
	for {
		// FieldDecl = IdentifierList Type [ Tag ]
		// 	| EmbeddedField [ Tag ] .
		var n *FieldDecl
		switch p.ch() {
		case '*':
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			//             Type
			case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
				n = &FieldDecl{IdentifierList: p.identifierList(id), Type: p.type1()}
			case ',':
				n = &FieldDecl{IdentifierList: p.identifierList(id), Type: p.type1()}
			case ';':
				n = &FieldDecl{EmbeddedField: &EmbeddedField{TypeName: &QualifiedIdent{Ident: id}}}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case '}':
			p.fixlbr(lbr)
			r.RBrace = p.must('}')
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
		n.Semicolon = p.opt(';')
		r.FieldDecls = append(r.FieldDecls, n)
	}
}

// TypeListItem describes a type name.
//
type TypeListItem struct {
	Type  *Type
	Comma Token
}

// Positions implements Node.
func (n *TypeListItem) Position() token.Position { return n.Type.Position() }

// TypeName describes a type name.
//
//  TypeName = QualifiedIdent [ TypeArgs ]
//  	| identifier [ TypeArgs ] .
type TypeName struct {
	Name     *QualifiedIdent
	TypeArgs *TypeArgs
}

// Positions implements Node.
func (n *TypeName) Position() token.Position { return n.Name.Position() }

// TypeName = QualifiedIdent [ TypeArgs ]
// 	| identifier [ TypeArgs ] .
func (p *parser) typeName() (r *TypeName) {
	switch p.ch() {
	case IDENTIFIER:
		r = &TypeName{Name: p.qualifiedIdent()}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	switch p.ch() {
	case '[':
		r.TypeArgs = p.typeArgs()
	}
	return r
}

// TypeArgs describes a type name.
//
//  TypeArgs = "[" TypeList [ "," ] "]" .
type TypeArgs struct {
	LBracket Token
	TypeList []*TypeListItem
	RBracket Token
}

// Positions implements Node.
func (n *TypeArgs) Position() token.Position { return n.LBracket.Position() }

// TypeArgs = "[" TypeList [ "," ] "]" .
func (p *parser) typeArgs() (r *TypeArgs) {
	r = &TypeArgs{LBracket: p.must('[')}
	for {
		var n *TypeListItem
		switch p.ch() {
		//         TypeList
		case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
			n = &TypeListItem{Type: p.type1()}
		case ']':
			r.RBracket = p.shift()
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
		n.Comma = p.opt(',')
		r.TypeList = append(r.TypeList, n)
	}
}

// QualifiedIdent describes an optionally qualified identifier.
//
//  QualifiedIdent = PackageName "." identifier .
type QualifiedIdent struct {
	PackageName Token
	Dot         Token
	Ident       Token
}

// Positions implements Node.
func (n *QualifiedIdent) Position() token.Position {
	if n.PackageName.IsValid() {
		return n.PackageName.Position()
	}

	return n.Ident.Position()
}

func (p *parser) qualifiedIdent() (r *QualifiedIdent) {
	switch p.ch() {
	case IDENTIFIER:
		id := p.shift()
		switch p.ch() {
		case '.':
			return &QualifiedIdent{PackageName: id, Dot: p.shift(), Ident: p.must(IDENTIFIER)}
		default:
			return &QualifiedIdent{Ident: id}
		}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// PointerType describes a pointer type.
//
//  PointerType = "*" BaseType .
type PointerType struct {
	Star     Token
	BaseType *Type
}

// Positions implements Node.
func (n *PointerType) Position() token.Position { return n.Star.Position() }

// PointerType = "*" BaseType .
func (p *parser) pointerType() (r *PointerType) {
	return &PointerType{Star: p.must('*'), BaseType: p.type1()}
}

// VarDecl describes a variable declaration.
//
//  VarDecl = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
type VarDecl struct {
	Var      Token
	LParen   Token
	VarSpecs []*VarSpec
	RParen   Token
}

// Positions implements Node.
func (n *VarDecl) Position() token.Position { return n.Var.Position() }

// VarDecl = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
func (p *parser) varDecl() (r *VarDecl) {
	v := p.must(VAR)
	switch p.ch() {
	case '(':
		return &VarDecl{Var: v, LParen: p.shift(), VarSpecs: p.varSpecs(), RParen: p.must(')')}
	case IDENTIFIER:
		return &VarDecl{Var: v, VarSpecs: []*VarSpec{p.varSpec()}}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// VarSpec describes a variable specification.
//
//  VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
type VarSpec struct {
	IdentifierList []*IdentifierListItem
	Type           *Type
	Eq             Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
}

// Positions implements Node.
func (n *VarSpec) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

func (p *parser) varSpecs() (r []*VarSpec) {
	for p.ch() == IDENTIFIER {
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	return r
}

// VarSpec = IdentifierList Type [ "=" ExpressionList ]
// 	| IdentifierList "=" ExpressionList .
func (p *parser) varSpec() (r *VarSpec) {
	r = &VarSpec{IdentifierList: p.identifierList(notok)}
	if p.ch() != '=' {
		r.Type = p.type1()
		if p.ch() != '=' {
			return r
		}
	}
	r.Eq = p.must('=')
	switch p.ch() {
	//   ExpressionList
	case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		r.ExpressionList = p.expressionList(notok)
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// ExpressionListItem describes an item of an expression list
//
// ExpressionList = Expression { "," Expression } .
type ExpressionListItem struct {
	Expression Node
	Comma      Token
}

// Positions implements Node.
func (n *ExpressionListItem) Position() (r token.Position) { return n.Expression.Position() }

func (p *parser) expressionList(id Token) (r []*ExpressionListItem) {
	for {
		var n *ExpressionListItem
		switch {
		case id.IsValid():
			n = &ExpressionListItem{Expression: p.expression(id)}
			id = notok
		default:
			switch p.ch() {
			case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
				n = &ExpressionListItem{Expression: p.expression(notok)}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		}
		r = append(r, n)
		switch p.ch() {
		case ',':
			n.Comma = p.shift()
		default:
			return r
		}
	}
}

// Expression = UnaryExpr { binary_op Expression } .
func (p *parser) expression(id Token) (r Node) {
	switch {
	case id.IsValid():
		r = p.primaryExpr(id)
	default:
		switch p.ch() {
		//        UnaryExpr
		case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
			r = p.unaryExpr()
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
	return p.expression2(r)
}

func (p *parser) expression2(lhs Node) (r Node) {
	r = lhs
	for {
		switch p.ch() {
		case '%', '&', '*', '+', '-', '/', '<', '>', '^', '|', AND_NOT, EQ, GE, LAND, LE, LOR, NE, SHL, SHR:
			op := p.shift()
			rhs := p.expression(notok)
			rprec := 0
			switch x := r.(type) {
			case *BinaryExpression:
				_ = x
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			default:
				_ = op
				_ = rhs
				_ = rprec
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		default:
			return r
		}
	}
}

// BinaryExpression describes a binary expression.
//
type BinaryExpression struct {
	LExpression Node
	Op          Token
	RExpressio  Node
}

// Positions implements Node.
func (n *BinaryExpression) Position() token.Position { return n.LExpression.Position() }

// UnaryExpr describes an unary expression.
//
//  UnaryExpr = PrimaryExpr | unary_op UnaryExpr .
type UnaryExpr struct {
	UnaryOp   Token
	UnaryExpr Node
}

// Positions implements Node.
func (n *UnaryExpr) Position() token.Position { return n.UnaryOp.Position() }

// UnaryExpr = PrimaryExpr
// 	| unary_op UnaryExpr .
func (p *parser) unaryExpr() (r Node) {
	//      PrimaryExpr case '(', '*', '[', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
	//         unary_op case '!', '&', '*', '+', '-', '^', ARROW:
	switch p.ch() {
	case '(', '[', CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		return p.primaryExpr(notok)
	case '!', '&', '+', '-', '^':
		return &UnaryExpr{UnaryOp: p.shift(), UnaryExpr: p.unaryExpr()}
	case '*':
		star := p.shift()
		switch p.ch() {
		default:
			_ = star
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	case ARROW:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// PrimaryExpr = Operand {
// 		  Arguments
// 		| Index
// 		| Selector
// 		| Slice
// 		| TypeAssertion
// 	  }
// 	| Conversion {
// 		  Arguments
// 		| Index
// 		| Selector
// 		| Slice
// 		| TypeAssertion
// 	  }
// 	| MethodExpr {
// 		  Arguments
// 		| Index
// 		| Selector
// 		| Slice
// 		| TypeAssertion
// 	  } .
// Operand = Literal
// 	| OperandName [ TypeArgs ]
// 	| "(" Expression ")" .
// CompositeLit = LiteralType1 LiteralValue1
// 	| LiteralType2 LiteralValue2 .
func (p *parser) primaryExpr(id Token) (r Node) {
	switch {
	case id.IsValid():
		switch p.ch() {
		case '(', '.', ',', '[':
			r = id
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	default:
		switch p.ch() {
		//          Operand case '(', '*', '[', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		//       Conversion case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		//       MethodExpr case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		case '[':
			// Operand = Literal
			// Conversion = Type "(" Expression [ "," ] ")" .
			// MethodExpr = ReceiverType "." MethodName .
			t := p.type1()
			switch p.ch() {
			case '{':
				r = &CompositeLit{LiteralType: t, LiteralValue: p.literalValue1()}
			case '(':
				r = &Conversion{Type: t, LParen: p.shift(), Expression: p.expression(notok), Comma: p.opt(','), RParen: p.must(')')}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case FLOAT_LIT, IMAG_LIT, INT_LIT, RUNE_LIT, STRING_LIT:
			r = p.shift()
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			case '(', '.', ';', ':', ',', DEFINE, '}', ')', ']', '<', '>', EQ, GE, LE, NE, body, '[', '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN, '%', '&', '*', '+', '-', '/', '^', '|', AND_NOT, LAND, LOR, SHL, SHR:
				r = id
			case '{':
				r = &CompositeLit{LiteralType: &Type{Type: &QualifiedIdent{Ident: id}}, LiteralValue: p.literalValue2()}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case MAP:
			r = &CompositeLit{LiteralType: p.type1(), LiteralValue: p.literalValue1()}
		case FUNC:
			f := p.shift()
			sig := p.signature()
			switch p.ch() {
			case '{':
				r = p.functionLit(f, sig)
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case '(':
			switch x := p.parenExprOrType().(type) {
			default:
				p.err(errorf("TODO %T", x))
				p.shift()
				return r
			}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
	for {
		switch p.ch() {
		case '.':
			dot := p.shift()
			switch p.ch() {
			case IDENTIFIER:
				r = &Selector{PrimaryExpr: r, Dot: dot, Ident: p.shift()}
			case '(':
				r = &TypeAssertion{PrimaryExpr: r, Dot: dot, LParen: p.shift(), Type: p.type1(), RParen: p.must(')')}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case '(':
			r = p.arguments(r)
		case ';', '}', ',', ':', ')', ']', DEFINE, '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN, '<', '>', EQ, GE, LE, NE, body, '%', '&', '*', '+', '-', '/', '^', '|', AND_NOT, LAND, LOR, SHL, SHR:
			return r
		case '{':
			r = &CompositeLit{LiteralType: &Type{Type: r}, LiteralValue: p.literalValue2()}
		case '[':
			r = &Index{PrimaryExpr: r, LBracket: p.shift(), Expression: p.expression(notok), RBracket: p.must(']')}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
}

// Conversion describes a conversion.
//
//  Conversion = Type "(" Expression [ "," ] ")" .
type Conversion struct {
	Type       *Type
	LParen     Token
	Expression Node
	Comma      Token
	RParen     Token
}

// Positions implements Node.
func (n *Conversion) Position() token.Position { return n.Type.Position() }

// Index describes an index.
//
//  Index = "[" Expression "]" .
type Index struct {
	PrimaryExpr Node
	LBracket    Token
	Expression  Node
	RBracket    Token
}

// Positions implements Node.
func (n *Index) Position() token.Position { return n.PrimaryExpr.Position() }

// TypeAssertion describes a type assertion.
//
//  TypeAssertion = PrimaryExpr "." "(" Type ")" .
type TypeAssertion struct {
	PrimaryExpr Node
	Dot         Token
	LParen      Token
	Type        *Type
	RParen      Token
}

// Positions implements Node.
func (n *TypeAssertion) Position() token.Position { return n.PrimaryExpr.Position() }

// Selector describes a selector.
//
//  Selector = PrimaryExpr "." identifier .
type Selector struct {
	PrimaryExpr Node
	Dot         Token
	Ident       Token
}

// Positions implements Node.
func (n *Selector) Position() token.Position { return n.PrimaryExpr.Position() }

// (expr) or (type)
func (p *parser) parenExprOrType() (r Node) {
	lparen := p.must('(')
	switch p.ch() {
	case '*':
		switch x := p.exprOrType().(type) {
		default:
			p.err(errorf("TODO %T", x))
			p.shift()
			return r
		}
	default:
		_ = lparen
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

func (p *parser) exprOrType() (r Node) {
	//       Expression case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
	//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	switch p.ch() {
	case '!', '&', '+', '-', '^', FLOAT_LIT, IMAG_LIT, INT_LIT, RUNE_LIT, STRING_LIT:
		// Expression
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case '(', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		// Expression or Type
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case '*':
		return p.exprOrPointerType()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// p.ch() == '*'
func (p *parser) exprOrPointerType() (r Node) {
	star := p.must('*')
	switch p.ch() {
	default:
		_ = star
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// FunctionLit describes a function literal.
//
//  FunctionLit = "func" Signature FunctionBody .
type FunctionLit struct {
	Func         Token
	Signature    *Signature
	FunctionBody *Block
}

// Positions implements Node.
func (n *FunctionLit) Position() token.Position { return n.Func.Position() }

// FunctionLit = "func" Signature lbrace StatementList "#fixlbr" "}" .
func (p *parser) functionLit(f Token, sig *Signature) (r *FunctionLit) {
	r = &FunctionLit{Func: f, Signature: sig}
	var lbr bool
	r.FunctionBody = &Block{LBrace: p.lbrace(&lbr), StatementList: p.statementList()}
	p.fixlbr(lbr)
	r.FunctionBody.RBrace = p.must('}')
	return r
}

// Arguments describes a call or conversion.
//
//  Arguments = PrimaryExpr "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
type Arguments struct {
	PrimaryExpr    Node
	LParen         Token
	Type           *Type
	ExpressionList []*ExpressionListItem
	Comma          Token
	Comma2         Token
	RParen         Token
}

// Positions implements Node.
func (n *Arguments) Position() token.Position { return n.PrimaryExpr.Position() }

// Arguments = "(" ")"
// 	| "(" ExpressionList [ "..." ] [ "," ] ")"
// 	| "(" Type "," ExpressionList [ "..." ] [ "," ] ")"
// 	| "(" Type [ "..." ] [ "," ] ")" .
func (p *parser) arguments(primaryExpr Node) (r *Arguments) {
	//   ExpressionList case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
	//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	r = &Arguments{PrimaryExpr: primaryExpr, LParen: p.must('(')}
	switch p.ch() {
	case '!', '&', '+', '-', '^', FLOAT_LIT, IMAG_LIT, INT_LIT, RUNE_LIT, STRING_LIT:
		// ExpressionList
		r.ExpressionList = p.expressionList(notok)
	case '*', ARROW:
		// ExpressionList or Type
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case IDENTIFIER:
		// ExpressionList or Type
		r.ExpressionList = p.expressionList(notok)
		switch p.ch() {
		case ')':
			// ok
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	case '(':
		// ExpressionList or Type
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case '[', CHAN, FUNC, INTERFACE, MAP, STRUCT:
		// Type or a Literal
		t := p.type1()
		switch p.ch() {
		case ',':
			r.Type = t
			r.Comma = p.shift()
			switch p.ch() {
			//   ExpressionList
			case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
				r.ExpressionList = p.expressionList(notok)
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case ')':
			// ok
		case '{':
			expr := &CompositeLit{LiteralType: t, LiteralValue: p.literalValue1()}
			switch p.ch() {
			default:
				_ = expr
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case '(':
			expr := &Conversion{Type: t, LParen: p.shift(), Expression: p.expression(notok), RParen: p.must(')')}
			switch p.ch() {
			default:
				_ = expr
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	case ')':
		// ok
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	if p.ch() == ELLIPSIS {
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	if p.ch() == ',' {
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	r.RParen = p.must(')')
	return r
}

// LiteralValue describes a composite literal value.
//
//  LiteralValue = "{" [ ElementList [ "," ] ] "}" .
type LiteralValue struct {
	LBrace      Token
	ElementList []*KeyedElement
	RBrace      Token
}

// Positions implements Node.
func (n *LiteralValue) Position() token.Position { return n.LBrace.Position() }

// LiteralValue1 = lbrace ElementList [ "," ] "#fixlbr" "}"
// 	| lbrace "#fixlbr" "}" .
func (p *parser) literalValue1() (r *LiteralValue) {
	var lbr bool
	r = &LiteralValue{LBrace: p.lbrace(&lbr)}
	switch p.ch() {
	//      ElementList
	case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT, body:
		r.ElementList = p.keyedElements()
	case '}':
		// ok
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	switch p.ch() {
	case '}':
		p.fixlbr(lbr)
		r.RBrace = p.shift()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
	}
	return r
}

// LiteralValue2 = "{" "}"
// 	| "{" ElementList [ "," ] "}" .
func (p *parser) literalValue2() (r *LiteralValue) {
	r = &LiteralValue{LBrace: p.must('{')}
	switch p.ch() {
	//      ElementList
	case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT, body:
		r.ElementList = p.keyedElements()
	case '}':
		// ok
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	switch p.ch() {
	case '}':
		r.RBrace = p.shift()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
	}
	return r
}

// KeyedElement describes an optionally keyed element.
//
//  KeyedElement = [ Key ":" ] Element .
type KeyedElement struct {
	Key     Node
	Colon   Token
	Element Node
	Comma   Token
}

// Positions implements Node.
func (n *KeyedElement) Position() token.Position {
	if n.Key != nil {
		return n.Key.Position()
	}

	return n.Element.Position()
}

// Element = Expression | LiteralValue1 .
// Key = Expression | LiteralValue1 .
func (p *parser) exprOrLiteralValue1() (r Node) {
	switch p.ch() {
	case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT, body:
		expr := p.expression(notok)
		switch p.ch() {
		case '}', ',', ':':
			return expr
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	case '{':
		lv := p.literalValue1()
		switch p.ch() {
		case '}', ',':
			return lv
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

func (p *parser) keyedElements() (r []*KeyedElement) {
	//              Key case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT, body:
	//      ElementList case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT, body:
	for p.ch() != '}' {
		switch p.ch() {
		case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT, body:
			n := &KeyedElement{Element: p.exprOrLiteralValue1()}
			if p.ch() == ':' {
				n.Key = n.Element
				n.Colon = p.shift()
				n.Element = p.exprOrLiteralValue1()
			}
			n.Comma = p.opt(',')
			r = append(r, n)
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
	return r
}

// CompositeLit describes a composite literal.
//
//  CompositeLit = LiteralType LiteralValue .
type CompositeLit struct {
	LiteralType  *Type
	LiteralValue *LiteralValue
}

// Positions implements Node.
func (n *CompositeLit) Position() token.Position { return n.LiteralType.Position() }

// ConstDecl describes a constant declaration.
//
//  ConstDecl = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
type ConstDecl struct {
	Const      Token
	LParen     Token
	ConstSpecs []*ConstSpec
	RParen     Token
}

// Positions implements Node.
func (n *ConstDecl) Position() token.Position { return n.Const.Position() }

// ConstDecl = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
func (p *parser) constDecl() (r *ConstDecl) {
	c := p.must(CONST)
	switch p.ch() {
	case '(':
		return &ConstDecl{Const: c, LParen: p.shift(), ConstSpecs: p.constSpecs(), RParen: p.must(')')}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// ConstSpec describes a constant specification.
//
//  ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
type ConstSpec struct {
	IdentifierList []*IdentifierListItem
	Type           *Type
	Eq             Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
}

// Positions implements Node.
func (n *ConstSpec) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

// ConstSpec = IdentifierList "=" ExpressionList
// 	| IdentifierList Type "=" ExpressionList
// 	| IdentifierList .
func (p *parser) constSpecs() (r []*ConstSpec) {
	//        ConstSpec case IDENTIFIER:
	for p.ch() == IDENTIFIER {
		n := &ConstSpec{IdentifierList: p.identifierList(notok)}
		switch p.ch() {
		case '=':
			n.Eq = p.shift()
			n.ExpressionList = p.expressionList(notok)
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
		n.Semicolon = p.opt(';')
		r = append(r, n)
	}
	return r
}

// TypeDecl describes a type declaration.
//
//  TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
type TypeDecl struct {
	Type      Token
	LParen    Token
	TypeSpecs []*TypeSpecItem
	RParen    Token
}

// Positions implements Node.
func (n *TypeDecl) Position() token.Position { return n.Type.Position() }

// TypeDecl = "type" TypeSpec
// 	| "type" "(" { TypeSpec ";" } ")" .
func (p *parser) typeDecl() (r *TypeDecl) {
	typ := p.must(TYPE)
	switch p.ch() {
	case '(':
		return &TypeDecl{Type: typ, LParen: p.shift(), TypeSpecs: p.typeSpecs(), RParen: p.must(')')}
		return r
	default:
		return &TypeDecl{Type: typ, TypeSpecs: []*TypeSpecItem{{TypeSpec: p.typeSpec()}}}
	}
}

// AliasDecl describes a type alias.
//
//  AliasDecl = identifier "=" Type .
type AliasDecl struct {
	Ident Token
	Eq    Token
	Type  *Type
}

// Positions implements Node.
func (n *AliasDecl) Position() token.Position { return n.Ident.Position() }

// TypeSpec = AliasDecl | TypeDef .
// TypeDef = identifier [ TypeParameters ] Type .
// AliasDecl = identifier "=" Type .
func (p *parser) typeSpec() (r Node) {
	id := p.must(IDENTIFIER)
	switch p.ch() {
	//   TypeParameters case '[':
	//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	case '[':
		switch x := p.typeOrTypeParamaters().(type) {
		case *TypeParameters:
			return &TypeDef{Ident: id, TypeParameters: x, Type: p.type1()}
		default:
			return &TypeDef{Ident: id, Type: &Type{Type: x}}
		}
	case '=':
		return &AliasDecl{Ident: id, Eq: p.shift(), Type: p.type1()}
	case '(', '*', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		return &TypeDef{Ident: id, Type: p.type1()}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// p.s.Tok.Ch == '['
func (p *parser) typeOrTypeParamaters() (r Node) {
	//   TypeParameters case '[':
	//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	switch p.ch() {
	case '[':
		lbracket := p.shift()
		// '[' .
		switch p.ch() {
		case IDENTIFIER:
			id := p.shift()
			// '[' IDENTIFIER  .
			// TypeParameters = "[" . TypeParamList [ "," ] "]" .
			// TypeParamList = . TypeParamDecl { "," TypeParamDecl } .
			// TypeParamDecl = . IdentifierList TypeConstraint .
			// ArrayType = "[" . ArrayLength "]" ElementType .
			switch p.ch() {
			case IDENTIFIER:
				// '[' IDENTIFIER  . IDENTIFIER -> TypeParameters
				return p.typeParameters(lbracket, id)
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case ']':
			return &SliceType{LBracket: lbracket, RBracket: p.shift(), ElementType: p.type1()}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	case '(', '*', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// TypeParameters describes type parameters.
//
//  TypeParameters = "[" TypeParamList [ "," ] "]" .
type TypeParameters struct {
	LBracket      Token
	TypeParamList []*TypeParamListItem
	RBracket      Token
}

// Positions implements Node.
func (n *TypeParameters) Position() token.Position { return n.LBracket.Position() }

func (p *parser) typeParameters(lbracket, id Token) (r *TypeParameters) {
	if !lbracket.IsValid() {
		lbracket = p.must('[')
	}
	return &TypeParameters{LBracket: lbracket, TypeParamList: p.typeParamList(id), RBracket: p.must(']')}
}

// TypeParamListItem describes an item of a type parameter list.
//
//  TypeParamList = TypeParamDecl { "," TypeParamDecl } .
type TypeParamListItem struct {
	TypeParamDecl *TypeParamDecl
	Comma         Token
}

// Positions implements Node.
func (n *TypeParamListItem) Position() token.Position { return n.TypeParamDecl.Position() }

func (p *parser) typeParamList(id Token) (r []*TypeParamListItem) {
	for {
		var n *TypeParamListItem
		switch {
		case id.IsValid():
			n = &TypeParamListItem{TypeParamDecl: p.typeParamDecl(id)}
			id = Token{}
		default:
			switch p.ch() {
			case IDENTIFIER:
				n = &TypeParamListItem{TypeParamDecl: p.typeParamDecl(notok)}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		}
		n.Comma = p.opt(',')
		r = append(r, n)
		switch p.ch() {
		case ']':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
}

// TypeTerm describes a type term.
//
//  TypeTerm = Type | UnderlyingType .
//  UnderlyingType = "~" Type .
type TypeTerm struct {
	Tilde Token
	Type  *Type
}

// Positions implements Node.
func (n *TypeTerm) Position() (r token.Position) {
	if n.Tilde.IsValid() {
		return n.Tilde.Position()
	}

	return n.Type.Position()
}

func (p *parser) typeTerm() (r *TypeTerm) {
	switch p.ch() {
	case '~':
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		return &TypeTerm{Type: p.type1()}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// TypeElem describes a type element.
//
//  TypeElem = TypeTerm { "|" TypeTerm } .
type TypeElem struct {
	TypeTerm *TypeTerm
	Pipe     Token
}

// Positions implements Node.
func (n *TypeElem) Position() (r token.Position) { return n.TypeTerm.Position() }

func (p *parser) typeElem(id Token) (r *TypeElem) {
	panic(todo(""))
}

func (p *parser) typeConstraint() (r []*TypeElem) {
	for {
		n := &TypeElem{TypeTerm: p.typeTerm()}
		r = append(r, n)
		switch p.ch() {
		case '|':
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		default:
			return r
		}
	}
}

// IdentifierListItem describes an item of an identifier list.
type IdentifierListItem struct {
	Ident Token
	Comma Token
}

// Positions implements Node.
func (n *IdentifierListItem) Position() (r token.Position) { return n.Ident.Position() }

func (p *parser) identifierList(id Token) (r []*IdentifierListItem) {
	if id.IsValid() {
		n := &IdentifierListItem{Ident: id}
		r = append(r, n)
		if p.ch() != ',' {
			return r
		}

		n.Comma = p.shift()
	}
	for {
		var n *IdentifierListItem
		switch p.ch() {
		case IDENTIFIER:
			n = &IdentifierListItem{Ident: p.shift()}
			r = append(r, n)
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		case ',':
			n.Comma = p.shift()
		default:
			return r
		}
	}
}

// TypeParamDecl describes an item of a type parameter list.
//
//  TypeParamDecl = IdentifierList TypeConstraint .
type TypeParamDecl struct {
	IdentifierList []*IdentifierListItem
	TypeConstraint []*TypeElem
}

// Positions implements Node.
func (n *TypeParamDecl) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

func (p *parser) typeParamDecl(id Token) (r *TypeParamDecl) {
	return &TypeParamDecl{IdentifierList: p.identifierList(id), TypeConstraint: p.typeConstraint()}
}

// TypeDef describes a type definition.
//
//  TypeDef = identifier [ TypeParameters ] Type .
type TypeDef struct {
	Ident          Token
	TypeParameters *TypeParameters
	Type           *Type
}

// Positions implements Node.
func (n *TypeDef) Position() token.Position { return n.Ident.Position() }

type TypeSpecItem struct {
	TypeSpec  Node
	Semicolon Token
}

// Positions implements Node.
func (n *TypeSpecItem) Position() token.Position { return n.TypeSpec.Position() }

func (p *parser) typeSpecs() (r []*TypeSpecItem) {
	for p.ch() == IDENTIFIER {
		r = append(r, &TypeSpecItem{TypeSpec: p.typeSpec(), Semicolon: p.opt(';')})
	}
	return r
}

// ImportDecl describes an import declaration.
//
//  ImportDecl = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
type ImportDecl struct {
	Import      Token
	LParen      Token
	ImportSpecs []*ImportSpecItem
	RParen      Token
	Semicolon   Token
}

// Positions implements Node.
func (n *ImportDecl) Position() token.Position { return n.Import.Position() }

// ImportDecl = "import" ImportSpec
// 	| "import" "(" { ImportSpec ";" } ")" .
func (p *parser) importDecls() (r []*ImportDecl) {
	for p.ch() == IMPORT {
		var n *ImportDecl
		im := p.shift()
		switch p.ch() {
		case '(':
			n = &ImportDecl{Import: im, LParen: p.shift(), ImportSpecs: p.importSpecs(), RParen: p.must(')')}
		case '.', IDENTIFIER, STRING_LIT:
			n = &ImportDecl{Import: im, LParen: p.shift(), ImportSpecs: []*ImportSpecItem{{ImportSpec: p.importSpec()}}}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
		n.Semicolon = p.must(';')
		r = append(r, n)
	}
	return r
}

type ImportSpecItem struct {
	ImportSpec *ImportSpec
	Semicolon  Token
}

// Positions implements Node.
func (n *ImportSpecItem) Position() token.Position { return n.ImportSpec.Position() }

func (p *parser) importSpecs() (r []*ImportSpecItem) {
	for {
		switch p.ch() {
		case '.', IDENTIFIER, STRING_LIT:
			r = append(r, &ImportSpecItem{ImportSpec: p.importSpec(), Semicolon: p.must(';')})
		default:
			return r
		}
	}
}

// ImportSpec describes an import specification.
//
//  ImportSpec = [ "." | PackageName ] ImportPath .
type ImportSpec struct {
	Qualifier  Token
	ImportPath Token
}

// Positions implements Node.
func (n *ImportSpec) Position() token.Position {
	if n.Qualifier.IsValid() {
		return n.Qualifier.Position()
	}

	return n.ImportPath.Position()
}

// ImportSpec = "." ImportPath
// 	| PackageName ImportPath
// 	| ImportPath .
func (p *parser) importSpec() (r *ImportSpec) {
	switch p.ch() {
	case '.':
		return &ImportSpec{Qualifier: p.shift(), ImportPath: p.must(STRING_LIT)}
	case IDENTIFIER:
		return &ImportSpec{Qualifier: p.shift(), ImportPath: p.must(STRING_LIT)}
	case STRING_LIT:
		return &ImportSpec{ImportPath: p.shift()}
	default:
		p.err(errorf("expected import specification"))
		p.shift()
		return nil
	}
}

// PackageClause describes the package clause.
//
//  PackageClause = "package" PackageName .
type PackageClause struct {
	Package     Token
	PackageName Token
	Semicolon   Token
}

// Positions implements Node.
func (n *PackageClause) Position() token.Position { return n.Package.Position() }

// PackageClause = "package" PackageName .
func (p *parser) packageClause() (r *PackageClause) {
	return &PackageClause{Package: p.must(PACKAGE), PackageName: p.must(IDENTIFIER), Semicolon: p.must(';')}
}
