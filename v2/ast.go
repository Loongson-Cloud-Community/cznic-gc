// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"go/token"
)

var (
	_ = []Node{
		(*Arguments)(nil),
		(*Block)(nil),
		(*CallExpr)(nil),
		(*CompositeLit)(nil),
		(*ConstDecl)(nil),
		(*ConstSpec)(nil),
		(*ElementListItem)(nil),
		(*FieldDecl)(nil),
		(*FieldDeclItem)(nil),
		(*FunctionDecl)(nil),
		(*IdentifierListItem)(nil),
		(*IfStmt)(nil),
		(*ImportDecl)(nil),
		(*ImportSpec)(nil),
		(*InterfaceType)(nil),
		(*KeyedElement)(nil),
		(*LiteralValue)(nil),
		(*MapType)(nil),
		(*MethodDecl)(nil),
		(*MethodDecl)(nil),
		(*PackageClause)(nil),
		(*ParameterDecl)(nil),
		(*Parameters)(nil),
		(*PointerType)(nil),
		(*ReturnStmt)(nil),
		(*SliceType)(nil),
		(*SourceFile)(nil),
		(*StatementListItem)(nil),
		(*StructType)(nil),
		(*TopLevelDecl)(nil),
		(*TypeDef)(nil),
		(*TypeSpec)(nil),
		(*VarSpec)(nil),
	}
)

type parser struct {
	s *Scanner
}

func newParser(s *Scanner) *parser {
	return &parser{s: s}
}

func (p *parser) ch() Ch { return p.s.Tok.Ch }

func (p *parser) Err() error { return p.s.Err() }

func (p *parser) err(msg string, args ...interface{}) { p.errNode(p.s.Tok, msg, args...) }

func (p *parser) errNode(n Node, msg string, args ...interface{}) {
	p.s.errs.err(n.Position(), msg, args...)
}

func (p *parser) must(c Ch) (r Token) {
	r = p.s.Tok
	if r.Ch != c {
		p.err("expected %v, got %v", c, r.Ch)
	}
	p.s.Scan()
	return r
}

func (p *parser) shift() (r Token) {
	r = p.s.Tok
	p.s.Scan()
	return r
}

// ParseSourceFile parses buf and returns a *SourceFile or an error, if any.
// Positions are reported as if buf is coming from a file named name. The
// buffer becomes owned by the *SourceFile and must not be modified after
// calling ParseSourceFile.
func ParseSourceFile(name string, buf []byte) (*SourceFile, error) {
	s, err := NewScanner(name, buf)
	if err != nil {
		return nil, err
	}

	p := newParser(s)
	sf := p.sourceFile()
	if err := p.Err(); err != nil {
		return nil, err
	}

	return sf, nil
}

// SourceFile represents a Go source file.
//
//	SourceFile    = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
//	Declaration   = ConstDecl | TypeDecl | VarDecl .

type SourceFile struct {
	PackageClause *PackageClause
	Semicolon     Token
	ImportDecls   []*ImportDecl
	TopLevelDecls []*TopLevelDecl
}

// Position implements Node.
func (n *SourceFile) Position() token.Position { return n.PackageClause.Position() }

func (p *parser) sourceFile() *SourceFile {
	p.s.Scan()
	return &SourceFile{p.packageClause(), p.must(';'), p.importDecls(), p.topLevelDecls()}
}

// PackageClause represents the package clause of a source file.
//
//	PackageClause  = "package" PackageName .
//	PackageName    = identifier .
type PackageClause struct {
	Package     Token
	PackageName Token
}

// Position implements Node.
func (n *PackageClause) Position() token.Position { return n.Package.Position() }

func (p *parser) packageClause() *PackageClause {
	return &PackageClause{p.must(PACKAGE), p.must(IDENTIFIER)}
}

// ImportDecl represents an import declaration of a source file.
//
//	ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
type ImportDecl struct {
	Import      Token
	LParen      Token
	ImportSpecs []*ImportSpec
	RParen      Token
	Semicolon   Token
}

// Position implements Node.
func (n *ImportDecl) Position() token.Position { return n.Import.Position() }

func (p *parser) importDecls() (r []*ImportDecl) {
	for p.ch() == IMPORT {
		n := &ImportDecl{Import: p.shift()}
		switch p.ch() {
		case '(':
			n.LParen = p.shift()
			n.ImportSpecs = p.importSpecs()
			n.RParen = p.must(')')
			n.Semicolon = p.must(';')
		case STRING_LIT:
			n.ImportSpecs = p.importSpecs()
		default:
			return r
		}
		r = append(r, n)
	}
	return r
}

// ImportSpec represents a single import specification.
//
//	ImportSpec       = [ "." | PackageName ] ImportPath .
//	ImportPath       = string_lit .
type ImportSpec struct {
	Qualifier  Token
	ImportPath Token
	Semicolon  Token
}

// Position implements Node.
func (n *ImportSpec) Position() token.Position {
	if n.Qualifier.IsValid() {
		return n.Qualifier.Position()
	}

	return n.ImportPath.Position()
}

func (p *parser) importSpecs() (r []*ImportSpec) {
	for {
		switch p.ch() {
		case STRING_LIT:
			r = append(r, &ImportSpec{ImportPath: p.shift(), Semicolon: p.must(';')})
		case IDENTIFIER, '.':
			r = append(r, &ImportSpec{Qualifier: p.shift(), ImportPath: p.must(STRING_LIT), Semicolon: p.must(';')})
		default:
			return r
		}
	}
}

// TopLevelDecl represents a top-level declaration.
//
//	TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
type TopLevelDecl struct {
	Decl      Node
	Semicolon Token
}

// Position implements Node.
func (n *TopLevelDecl) Position() token.Position { return n.Decl.Position() }

func (p *parser) topLevelDecls() (r []*TopLevelDecl) {
	for p.ch() != EOF {
		var n *TopLevelDecl
		switch p.ch() {
		case FUNC:
			n = &TopLevelDecl{Decl: p.functionOrMethodDecl()}
		case CONST:
			n = &TopLevelDecl{Decl: p.constDecl()}
		case TYPE:
			n = &TopLevelDecl{Decl: p.typeDecl()}
		case VAR:
			n = &TopLevelDecl{Decl: p.varDecl()}
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		case ';':
			n.Semicolon = p.shift()
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
	return r
}

// VarDecl represents a variable declaration.
//
//	VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
type VarDecl struct {
	Var      Token
	LParen   Token
	VarSpecs []*VarSpec
	RParen   Token
}

// Position implements Node.
func (n *VarDecl) Position() token.Position { return n.Var.Position() }

func (p *parser) varDecl() (r *VarDecl) {
	r = &VarDecl{Var: p.must(VAR)}
	switch p.ch() {
	case '(':
		r.LParen = p.shift()
		r.VarSpecs = p.varSpecs(false)
		r.RParen = p.must(')')
		return r
	case IDENTIFIER:
		r.VarSpecs = p.varSpecs(true)
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// VarSpec represents a variable specification.
//
//	VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
type VarSpec struct {
	IdentifierList []*IdentifierListItem
	Type           Node
	Eq             Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
}

// Position implements Node.
func (n *VarSpec) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

func (p *parser) varSpecs(one bool) (r []*VarSpec) {
	for {
		var n *VarSpec
		switch p.ch() {
		case IDENTIFIER:
			n = &VarSpec{IdentifierList: p.identifierList(Token{})}
			switch p.ch() {
			case '=':
				n.Eq = p.shift()
				n.ExpressionList = p.expressionList()
			default:
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			}
			if one {
				return r
			}
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
}

// ExpressionListItem represents an item of an expression list.
type ExpressionListItem struct {
	Expression Node
	Comma      Token
}

// Position implements Node.
func (n *ExpressionListItem) Position() token.Position { return n.Expression.Position() }

func (p *parser) expressionList() (r []*ExpressionListItem) {
	for {
		var n *ExpressionListItem
		switch p.ch() {
		case
			'-',
			'[',
			IDENTIFIER,
			INT_LIT:

			n = &ExpressionListItem{Expression: p.expr()}
			// case '&':
			// 	amp := p.shift()
			// 	switch p.ch() {
			// 	default:
			// 		_ = amp
			// 		p.err(errorf("TODO %v", p.s.Tok.str()))
			// 		p.shift()
			// 		return r
			// 	}
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		case ';':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
}

func (p *parser) expr() (r Node) {
	switch p.ch() {
	case '[':
		typ := p.type1()
		switch p.ch() {
		case '{':
			return p.compositeLiteral(typ)
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
	case IDENTIFIER:
		id := p.shift()
		switch p.ch() {
		case '.':
			dot := p.shift()
			switch p.ch() {
			default:
				_ = dot
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			}
		case ';':
			return id
		case '(':
			return p.callExpr(id)
		case '{':
			return p.compositeLiteral(id)
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
	case '&':
		op := p.shift()
		switch p.ch() {
		default:
			_ = op
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
	case INT_LIT:
		return p.shift()
	case '-':
		op := p.shift()
		switch p.ch() {
		default:
			_ = op
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// CallExpr represents a call expression or a conversion.
//
//	CallExpr = Expression Arguments.
type CallExpr struct {
	Expr      Node
	Arguments *Arguments
}

// Position implements Node.
func (n *CallExpr) Position() token.Position { return n.Expr.Position() }

func (p *parser) callExpr(expr Node) (r *CallExpr) {
	return &CallExpr{Expr: expr, Arguments: p.arguments()}
}

// Arguments represents the arguments of a call or of a conversion.
//
//	Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
type Arguments struct {
	LParen Token
	//TODO
	RParen Token
}

// Position implements Node.
func (n *Arguments) Position() token.Position { return n.LParen.Position() }

func (p *parser) arguments() (r *Arguments) {
	r = &Arguments{LParen: p.must('(')}
	switch p.ch() {
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// CompositeLit represents a composite literal.
//
//	CompositeLit  = LiteralType LiteralValue .
type CompositeLit struct {
	LiteralType  Node
	LiteralValue *LiteralValue
}

// Position implements Node.
func (n *CompositeLit) Position() token.Position { return n.LiteralType.Position() }

func (p *parser) compositeLiteral(typ Node) (r *CompositeLit) {
	return &CompositeLit{LiteralType: typ, LiteralValue: p.literalValue()}
}

// LiteralValue represents the value of a composite literal.
//
// LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
type LiteralValue struct {
	LBrace      Token
	ElementList []*ElementListItem
	RBrace      Token
}

// Position implements Node.
func (n *LiteralValue) Position() token.Position { return n.LBrace.Position() }

func (p *parser) literalValue() (r *LiteralValue) {
	return &LiteralValue{LBrace: p.must('{'), ElementList: p.elementList(), RBrace: p.must('}')}
}

// ElementListItem is an item of an element list.
type ElementListItem struct {
	KeyedElement *KeyedElement
	Comma        Token
}

// Position implements Node.
func (n *ElementListItem) Position() token.Position { return n.KeyedElement.Position() }

func (p *parser) elementList() (r []*ElementListItem) {
	for {
		var n *ElementListItem
		switch p.ch() {
		case '{':
			n = &ElementListItem{KeyedElement: p.keyedElement()}
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
}

// KeyedElement represents an optionally keyed element of a literal value.
//
//	KeyedElement  = [ Key ":" ] Element .
type KeyedElement struct {
	Key     Node
	Element Node
}

// Position implements Node.
func (n *KeyedElement) Position() token.Position {
	if n.Key != nil {
		return n.Key.Position()
	}

	return n.Element.Position()
}

func (p *parser) keyedElement() (r *KeyedElement) {
	switch p.ch() {
	case '{':
		return &KeyedElement{Element: p.literalValue()}
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// TypeDecl represents a type declaration.
//
//	TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
type TypeDecl struct {
	Type      Token
	LParen    Token
	TypeSpecs []*TypeSpec
	RParen    Token
}

// Position implements Node.
func (n *TypeDecl) Position() token.Position { return n.Type.Position() }

func (p *parser) typeDecl() (r *TypeDecl) {
	r = &TypeDecl{Type: p.must(TYPE)}
	switch p.ch() {
	case '(':
		r.LParen = p.shift()
		r.TypeSpecs = p.typeSpecs(false)
		r.RParen = p.must(')')
		return r
	case IDENTIFIER:
		r.TypeSpecs = p.typeSpecs(true)
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// TypeSpec represents a type specification,
//
//	TypeSpec = AliasDecl | TypeDef .
type TypeSpec struct {
	Decl      Node
	Semicolon Token
}

// Position implements Node.
func (n *TypeSpec) Position() token.Position { return n.Decl.Position() }

func (p *parser) typeSpecs(one bool) (r []*TypeSpec) {
	for {
		var n *TypeSpec
		switch p.ch() {
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			case
				IDENTIFIER,
				INTERFACE,
				STRUCT:

				n = &TypeSpec{Decl: p.typeDef(id)}
			default:
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			}
			if one {
				return r
			}
		case ')':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		case ';':
			n.Semicolon = p.shift()
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
}

// TypeDef represents a type definition
//
//	TypeDef = identifier [ TypeParameters ] Type .
type TypeDef struct {
	Ident Token
	//TODO TypeParameters []*TypeParameter
	Type Node
}

// Position implements Node.
func (n *TypeDef) Position() token.Position { return n.Ident.Position() }

func (p *parser) typeDef(id Token) (r *TypeDef) {
	r = &TypeDef{Ident: id}
	switch p.ch() {
	case
		IDENTIFIER,
		INTERFACE,
		STRUCT:

		r.Type = p.type1()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// ConstDecl represents a constant declaration.
//
//	ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
type ConstDecl struct {
	Const      Token
	LParen     Token
	ConstSpecs []*ConstSpec
	RParen     Token
}

// Position implements Node.
func (n *ConstDecl) Position() token.Position { return n.Const.Position() }

func (p *parser) constDecl() (r *ConstDecl) {
	r = &ConstDecl{Const: p.must(CONST)}
	switch p.ch() {
	case '(':
		r.LParen = p.shift()
		r.ConstSpecs = p.constSpecs(false)
		r.RParen = p.must(')')
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// ConstSpec represents a constant declaration.
//
//	ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
type ConstSpec struct {
	IdentifierList []*IdentifierListItem
	Type           Node
	Eq             Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
}

// Position implements Node.
func (n *ConstSpec) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

func (p *parser) constSpecs(one bool) (r []*ConstSpec) {
	for {
		var n *ConstSpec
		switch p.ch() {
		case IDENTIFIER:
			n = &ConstSpec{IdentifierList: p.identifierList(Token{})}
			switch p.ch() {
			case '=':
				n.Eq = p.shift()
				n.ExpressionList = p.expressionList()
			default:
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			}
			if one {
				return r
			}
		case ')':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		case ';':
			n.Semicolon = p.shift()
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
}

type IdentifierListItem struct {
	Ident Token
	Comma Token
}

// Position implements Node.
func (n *IdentifierListItem) Position() token.Position { return n.Ident.Position() }

func (p *parser) identifierList(id Token) (r []*IdentifierListItem) {
	for {
		var n *IdentifierListItem
		switch {
		case id.IsValid():
			n = &IdentifierListItem{Ident: id}
			id = Token{}
		default:
			switch p.ch() {
			case IDENTIFIER:
				n = &IdentifierListItem{Ident: p.shift()}
			default:
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			}
		}
		switch p.ch() {
		case ',':
			n.Comma = p.shift()
		default:
			return r
		}
		r = append(r, n)
	}
}

func (p *parser) functionOrMethodDecl() (r Node) {
	fn := p.must(FUNC)
	switch p.ch() {
	case IDENTIFIER:
		return p.function(fn)
	case '(':
		return p.method(fn)
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// MethodDecl represents a method declaration.
//
//	MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
//	Receiver   = Parameters .
type MethodDecl struct {
	Func         Token
	Receiver     *Parameters
	MethodName   Token
	Signature    *Signature
	FunctionBody *Block
}

// Position implements Node.
func (n *MethodDecl) Position() token.Position { return n.Func.Position() }

func (p *parser) method(fn Token) (r *MethodDecl) {
	r = &MethodDecl{Func: fn, Receiver: p.parameters(), MethodName: p.must(IDENTIFIER), Signature: p.signature()}
	switch p.ch() {
	case '{':
		r.FunctionBody = p.block()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// FunctionDecl represents a function declaration.
//
//	FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
//	FunctionName = identifier .
//	FunctionBody = Block .
type FunctionDecl struct {
	Func         Token
	FunctionName Token
	//TODO TypeParameters []*TypeParameter
	Signature    *Signature
	FunctionBody *Block
}

// Position implements Node.
func (n *FunctionDecl) Position() token.Position { return n.Func.Position() }

func (p *parser) function(fn Token) (r *FunctionDecl) {
	r = &FunctionDecl{Func: fn, FunctionName: p.must(IDENTIFIER)}
	switch p.ch() {
	case '(':
		r.Signature = p.signature()
		if p.ch() == '{' {
			r.FunctionBody = p.block()
		}
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// Block represents a function body or a compound statement.
//
// Block = "{" StatementList "}" .
type Block struct {
	LBrace        Token
	StatementList []*StatementListItem
	RBrace        Token
}

// Position implements Node.
func (n *Block) Position() token.Position { return n.LBrace.Position() }

func (p *parser) block() (r *Block) {
	return &Block{LBrace: p.must('{'), StatementList: p.statementList(), RBrace: p.must('}')}
}

// StatementListItem represents an item of a statement list.
type StatementListItem struct {
	Statement Node
	Semicolon Token
}

// Position implements Node.
func (n *StatementListItem) Position() token.Position {
	if n.Statement != nil {
		return n.Statement.Position()
	}

	return n.Semicolon.Position()
}

func (p *parser) statementList() (r []*StatementListItem) {
	for {
		var n *StatementListItem
		switch p.ch() {
		case IF:
			n = &StatementListItem{Statement: p.ifStatement()}
		case RETURN:
			n = &StatementListItem{Statement: p.returnStatement()}
		case VAR:
			n = &StatementListItem{Statement: p.varDecl()}
		case SWITCH:
			sw := p.shift()
			switch p.ch() {
			default:
				_ = sw
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			}
		case CONST:
			n = &StatementListItem{Statement: p.constDecl()}
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
}

// ReturnStmt represents a return statement.
//
//	ReturnStmt = "return" [ ExpressionList ] .
type ReturnStmt struct {
	Return         Token
	ExpressionList []*ExpressionListItem
}

// Position implements Node.
func (n *ReturnStmt) Position() token.Position { return n.Return.Position() }

func (p *parser) returnStatement() (r *ReturnStmt) {
	r = &ReturnStmt{Return: p.must(RETURN)}
	switch p.ch() {
	case IDENTIFIER:
		r.ExpressionList = p.expressionList()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// IfStmt represents an 'if' statment.
//
//	IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
type IfStmt struct {
	If         Token
	SimpleStmt Node
	Semicolon  Token
	Expression Node
	Block      *Block
	//ElsePart *ElsePart
}

// Position implements Node.
func (n *IfStmt) Position() token.Position { return n.If.Position() }

func (p *parser) ifStatement() (r *IfStmt) {
	r = &IfStmt{If: p.must(IF)}
	switch p.ch() {
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// Signature represents a function signature.
//
//	Signature      = Parameters [ Result ] .
//	Result         = Parameters | Type .
type Signature struct {
	Parameters *Parameters
	Result     Node
}

// Position implements Node.
func (n *Signature) Position() token.Position { return n.Parameters.Position() }

func (p *parser) signature() (r *Signature) {
	r = &Signature{Parameters: p.parameters()}
	switch p.ch() {
	case '{':
		return r
	case '(':
		r.Result = p.parameters()
		return r
	case IDENTIFIER, '*':
		r.Result = p.type1()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// Parameters represent the formal arguments of a function/method or the return
// arguments of the same.
//
//	Parameters     = "(" [ ParameterList [ "," ] ] ")" .
type Parameters struct {
	LParen        Token
	ParameterList []*ParameterDecl
	Comma         Token
	RParen        Token
}

// Position implements Node.
func (n *Parameters) Position() token.Position { return n.LParen.Position() }

func (p *parser) parameters() (r *Parameters) {
	r = &Parameters{LParen: p.must('(')}
	switch p.ch() {
	case IDENTIFIER:
		r.ParameterList = p.parameterList()
		switch p.ch() {
		case ',':
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
		case ')':
			r.RParen = p.shift()
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
		}
		return r
	case ')':
		r.RParen = p.shift()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// ParameterDecl represents a parameter declaration.
//
//	ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
type ParameterDecl struct {
	IdentifierList []*IdentifierListItem
	DDD            Token
	Type           Node
	Comma          Token
}

// Position implements Node.
func (n *ParameterDecl) Position() token.Position {
	switch {
	case len(n.IdentifierList) != 0:
		return n.IdentifierList[0].Position()
	case n.DDD.IsValid():
		return n.DDD.Position()
	default:
		return n.Type.Position()
	}
}

func (p *parser) parameterList() (r []*ParameterDecl) {
	for {
		var n *ParameterDecl
		switch p.ch() {
		case IDENTIFIER:
			n = &ParameterDecl{IdentifierList: p.identifierList(Token{})}
			switch p.ch() {
			case ELLIPSIS:
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			case ')':
				if len(r) != 0 {
					p.err(errorf("TODO %v", p.s.Tok.str()))
					p.shift()
					return r
				}

				for _, v := range n.IdentifierList {
					r = append(r, &ParameterDecl{Type: v.Ident, Comma: v.Comma})
				}
				return r
			}

			n.Type = p.type1()
			switch p.ch() {
			case ')':
				return r
			case ',':
				n.Comma = p.shift()
			default:
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			}
		case ')':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
}

//	Type      = TypeName [ TypeArgs ] | TypeLit | "(" Type ")" .
//	TypeName  = identifier | QualifiedIdent .
//	TypeArgs  = "[" TypeList [ "," ] "]" .
//	TypeList  = Type { "," Type } .
//	TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
//		    SliceType | MapType | ChannelType .

func (p *parser) type1() (r Node) {
	switch p.ch() {
	case '*':
		return p.pointerType()
	case IDENTIFIER:
		id := p.shift()
		switch p.ch() {
		case '.':
			return &QualifiedIdent{PackageName: id, Dot: p.shift(), Ident: p.must(IDENTIFIER)}
		default:
			return id
		}
	case '[':
		lb := p.shift()
		switch p.ch() {
		case ']':
			return p.sliceType(lb)
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
	case STRUCT:
		return p.structType()
	case INTERFACE:
		return p.interfaceType()
	case MAP:
		return p.mapType()
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// MapType represents a map type.
//
//	MapType     = "map" "[" KeyType "]" ElementType .
//	KeyType     = Type .
type MapType struct {
	LBracket    Token
	KeyType     Node
	RBracket    Token
	ElementType Node
}

// Position implements Node.
func (n *MapType) Position() token.Position { return n.LBracket.Position() }

func (p *parser) mapType() (r *MapType) {
	return &MapType{LBracket: p.must('['), KeyType: p.type1(), RBracket: p.must(']'), ElementType: p.type1()}
}

// InterfaceType represents an interface type.
//
// InterfaceType  = "interface" "{" { InterfaceElem ";" } "}" .
type InterfaceType struct {
	Interface Token
	LBrace    Token
	//TODO
	RBrace Token
}

// Position implements Node.
func (n *InterfaceType) Position() token.Position { return n.Interface.Position() }

func (p *parser) interfaceType() (r *InterfaceType) {
	r = &InterfaceType{Interface: p.must(INTERFACE), LBrace: p.must('{')}
	switch p.ch() {
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// StructType represents a struct type.
//
//	StructType    = "struct" "{" { FieldDecl ";" } "}" .
type StructType struct {
	Struct Token
	LBrace Token
	Fields []*FieldDeclItem
	RBrace Token
}

// Position implements Node.
func (n *StructType) Position() token.Position { return n.Struct.Position() }

func (p *parser) structType() (r *StructType) {
	return &StructType{Struct: p.must(STRUCT), LBrace: p.must('{'), Fields: p.fields(), RBrace: p.must('}')}
}

// FieldDeclItem represents an item of a field declaration list.
type FieldDeclItem struct {
	FieldDecl *FieldDecl
	Semicolon Token
}

// Position implements Node.
func (n *FieldDeclItem) Position() token.Position { return n.FieldDecl.Position() }

func (p *parser) fields() (r []*FieldDeclItem) {
	for {
		var n *FieldDeclItem
		switch p.ch() {
		case IDENTIFIER:
			n = &FieldDeclItem{FieldDecl: p.fieldDecl()}
		case '}':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		switch p.ch() {
		case ';':
			n.Semicolon = p.shift()
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		r = append(r, n)
	}
}

// FieldDecl represents a field declaration.
//
//	FieldDecl     = (IdentifierList Type | EmbeddedField) [ Tag ] .
type FieldDecl struct {
	IdentifierList []*IdentifierListItem
	Type           Node
	Tag            Token
}

// Position implements Node.
func (n *FieldDecl) Position() (r token.Position) {
	switch {
	case len(n.IdentifierList) != 0:
		return n.IdentifierList[0].Position()
	case n.Type != nil:
		return n.Type.Position()
	default:
		return r
	}
}

func (p *parser) fieldDecl() (r *FieldDecl) {
	switch p.ch() {
	case IDENTIFIER:
		id := p.shift()
		switch p.ch() {
		case
			'*',
			'[',
			IDENTIFIER,
			MAP:

			return &FieldDecl{
				IdentifierList: []*IdentifierListItem{{Ident: id}},
				Type:           p.type1(),
			}
		case ',':
			return &FieldDecl{IdentifierList: p.identifierList(id), Type: p.type1()}
		default:
			trc("", p.s.Tok)
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// SliceType represents a slice type.
//
//	SliceType = "[" "]" ElementType .
type SliceType struct {
	LBracket    Token
	ElementType Node
	RBracket    Token
}

// Position implements Node.
func (n *SliceType) Position() token.Position { return n.LBracket.Position() }

func (p *parser) sliceType(lbracket Token) (r *SliceType) {
	return &SliceType{LBracket: lbracket, RBracket: p.must(']'), ElementType: p.type1()}
}

// QualifiedIdent represents an identifier qualified by a package name.
//
//	QualifiedIdent = PackageName "." identifier .
type QualifiedIdent struct {
	PackageName Token
	Dot         Token
	Ident       Token
}

// Position implements Node.
func (n *QualifiedIdent) Position() token.Position { return n.PackageName.Position() }

// PointerType represents a pointer type.
//
//	PointerType = "*" BaseType .
//	BaseType    = Type .
type PointerType struct {
	Star Token
	Type Node
}

// Position implements Node.
func (n *PointerType) Position() token.Position { return n.Star.Position() }

func (p *parser) pointerType() (r *PointerType) {
	return &PointerType{Star: p.must('*'), Type: p.type1()}
}
