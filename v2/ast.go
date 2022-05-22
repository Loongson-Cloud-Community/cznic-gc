// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"go/token"
)

var (
	_ = []Node{
		(*ConstDecl)(nil),
		(*ConstSpec)(nil),
		(*ConstSpecItem)(nil),
		(*FunctionBody)(nil),
		(*FunctionDecl)(nil),
		(*ImportDecl)(nil),
		(*ImportSpec)(nil),
		(*ImportSpecItem)(nil),
		(*InterfaceType)(nil),
		(*MethodDecl)(nil),
		(*PackageClause)(nil),
		(*ParameterDecl)(nil),
		(*Parameters)(nil),
		(*PointerType)(nil),
		(*QualifiedIdent)(nil),
		(*Signature)(nil),
		(*SourceFile)(nil),
		(*Statement)(nil),
		(*StructType)(nil),
		(*TopLevelDecl)(nil),
		(*Type)(nil),
		(*TypeDecl)(nil),
		(*TypeDef)(nil),
		(*TypeName)(nil),
		(*TypeSpecItem)(nil),
		(*VarDecl)(nil),
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
	cfg *ParseSourceFileConfig
	s   *Scanner

	loophacks []bool

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

func (p *parser) fixlbr() {
	if n := len(p.loophacks); n != 0 {
		p.loophack = p.loophacks[n-1]
		p.loophacks = p.loophacks[:n-1]
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
	r = &SourceFile{PackageClause: p.packageClause()}
	if err := p.s.errs.Err(); err != nil {
		return nil, err
	}

	r.ImportDecls = p.importDecls()
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
				p.err(errorf("TODO %v", p.s.Tok.str()))
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

// MethodDecl describes a function declaration.
//
//  MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
type MethodDecl struct {
	Func         Token
	Receiver     *Parameters
	MethodName   Token
	Signature    *Signature
	FunctionBody *FunctionBody
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
	Func         Token
	FunctionName Token
	Signature    *Signature
	FunctionBody *FunctionBody
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
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
	if p.ch() == '{' {
		r.FunctionBody = p.functionBody()
	}
	return r
}

// FunctionBody describes a function body.
//
//  FunctionBody = Block .
//  Block = "{" StatementList "}" .
type FunctionBody struct {
	LBrace        Token
	StatementList []*Statement
	RBrace        Token
}

// Positions implements Node.
func (n *FunctionBody) Position() token.Position { return n.LBrace.Position() }

// FunctionBody = Block .
// Block = "{" StatementList "}" .
func (p *parser) functionBody() (r *FunctionBody) {
	return &FunctionBody{LBrace: p.must('{'), StatementList: p.statementList(), RBrace: p.must('}')}
}

// Statement describes a statement list item.
//
//  StatementList = { Statement ";" } .
type Statement struct {
	Statement Node
	Semicolon Token
}

// StatementList = { [ Statement ] ";" } [ Statement ] .
func (p *parser) statementList() (r []*Statement) {
	for {
		switch p.ch() {
		case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, BREAK, CHAN, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT_LIT, FOR, FUNC, GO, GOTO, IDENTIFIER, IF, IMAG_LIT, INTERFACE, INT_LIT, MAP, RETURN, RUNE_LIT, SELECT, STRING_LIT, STRUCT, SWITCH, TYPE, VAR:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
	}
	return r
}

// Positions implements Node.
func (n *Statement) Position() token.Position { return n.Statement.Position() }

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
	r = &Signature{Parameters: p.paramaters()}
	switch p.ch() {
	case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		r.Result = p.result()
	}
	return r
}

func (p *parser) result() (r Node) {
	switch p.ch() {
	case '(':
		lparen := p.shift()
		switch p.ch() {
		default:
			_ = lparen
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
	case '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// Parameters describes function paramaters or a function result.
//
//  Parameters    = "(" [ ParameterList [ "," ] ] ")" .
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
func (p *parser) paramaters() (r *Parameters) {
	r = &Parameters{LParen: p.must('(')}
	switch p.ch() {
	case '(', '*', '[', ARROW, CHAN, ELLIPSIS, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		r.ParameterList = p.parameterList()
	}
	if p.ch() == ',' {
		r.Comma = p.shift()
	}
	r.RParen = p.must(')')
	return r
}

// ParameterDecl describes a parameter declaration.
//
//  ParameterDecl = [ IdentifierList ] [ "..." ] Type .
type ParameterDecl struct {
	IdentifierList []Token
	DDD            Token
	Type           *Type
	Comma          Token
}

// Positions implements Node.
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

// ParameterDecl = identifier "..." Type
// 	| identifier Type
// 	| "..." Type
// 	| Type .
func (p *parser) parameterDecl() (r *ParameterDecl) {
	p.err(errorf("TODO %v", p.s.Tok.str()))
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
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
				n = &ParameterDecl{IdentifierList: []Token{id}, Type: p.type1()}
			case ',':
				n = &ParameterDecl{Type: &Type{Type: &QualifiedIdent{Ident: id}}}
			default:
				p.err(errorf("TODO %v", p.s.Tok.str()))
				p.shift()
				return r
			}
		case ELLIPSIS:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		case '(', '*', '[', ARROW, CHAN, FUNC, INTERFACE, MAP, STRUCT:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.str()))
			p.shift()
			return r
		}
		if p.ch() == ',' {
			n.Comma = p.shift()
		}
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
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	case '*':
		return &Type{Type: p.pointerType()}
	case '[':
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	case ARROW:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	case CHAN:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	case FUNC:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	case IDENTIFIER:
		return &Type{Type: p.typeName()}
	case INTERFACE:
		return &Type{Type: p.structType()}
	case MAP:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	case STRUCT:
		return &Type{Type: p.structType()}
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// InterfaceType describes a struct type.
//
//  InterfaceType = "interface" "{" { InterfaceElem ";" } "}" .
type InterfaceType struct {
	Interface Token
	LBrace    Token
	//TODO
	RBrace Token
}

// Positions implements Node.
func (n *InterfaceType) Position() token.Position { return n.Interface.Position() }

// InterfaceType = "interface" lbrace "#fixlbr" "}"
// 	| "interface" lbrace InterfaceElem { ";" InterfaceElem } [ ";" ] "#fixlbr" "}" .
func (p *parser) interfaceType() (r *InterfaceType) {
	var fixlbr bool
	r = &InterfaceType{Interface: p.must(INTERFACE), LBrace: p.lbrace(&fixlbr)}
	switch p.ch() {
	default:
		_ = fixlbr
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

func (p *parser) lbrace(fixlbr *bool) (r Token) {
	switch p.ch() {
	case '{':
		return p.shift()
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// StructType describes a struct type.
//
//  StructTyp = "struct" "{" { FieldDecl ";" } "}" .
type StructType struct {
	Struct Token
	LBrace Token
	//TODO
	RBrace Token
}

// Positions implements Node.
func (n *StructType) Position() token.Position { return n.Struct.Position() }

// StructType = "struct" lbrace "#fixlbr" "}"
// 	| "struct" lbrace FieldDecl { ";" FieldDecl } [ ";" ] "#fixlbr" "}" .
func (p *parser) structType() (r *StructType) {
	var fixlbr bool
	r = &StructType{Struct: p.must(STRUCT), LBrace: p.lbrace(&fixlbr)}
	switch p.ch() {
	default:
		_ = fixlbr
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// TypeName describes a type name.
//
//  TypeName = QualifiedIdent [ TypeArgs ]
//  	| identifier [ TypeArgs ] .
type TypeName struct {
	Name *QualifiedIdent
	// TypeArgs
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
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
	switch p.ch() {
	case '[':
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
	}
	return r
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
		p.err(errorf("TODO %v", p.s.Tok.str()))
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
	Var    Token
	LParen Token
	// VarSpecs []*VarSpecItem
	RParen Token
}

// Positions implements Node.
func (n *VarDecl) Position() token.Position { return n.Var.Position() }

// VarDecl = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
func (p *parser) varDecl() (r *VarDecl) {
	v := p.must(VAR)
	switch p.ch() {
	case '(':
		_ = v
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// ConstDecl describes a constant declaration.
//
//  ConstDecl = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
type ConstDecl struct {
	Const      Token
	LParen     Token
	ConstSpecs []*ConstSpecItem
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
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// ConstSpecItem describes an item of ConstDecl.ConstSpecs.
type ConstSpecItem struct {
	ConstSpec *ConstSpec
	Semicolon Token
}

// Positions implements Node.
func (n *ConstSpecItem) Position() token.Position { return n.ConstSpec.Position() }

func (p *parser) constSpecs() (r []*ConstSpecItem) {
	for p.ch() == IDENTIFIER {
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
	return r
}

// ConstSpec describes a constant specification.
//
//  ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
type ConstSpec struct {
	IdentifierList []Token
	//TODO
}

// Positions implements Node.
func (n *ConstSpec) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
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
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	default:
		return &TypeDecl{Type: typ, TypeSpecs: []*TypeSpecItem{{TypeSpec: p.typeSpec()}}}
	}
}

// TypeSpec = AliasDecl | TypeDef .
// TypeDef = identifier [ TypeParameters ] Type .
// AliasDecl = identifier "=" Type .
func (p *parser) typeSpec() (r Node) {
	id := p.must(IDENTIFIER)
	switch p.ch() {
	//   TypeParameters case '[':
	//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	case '[':
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	case '=':
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	case '(', '*', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		return &TypeDef{Ident: id, Type: p.type1()}
	default:
		_ = id
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
	}
}

// TypeDef describes a type definition.
//
//  TypeDef = identifier [ TypeParameters ] Type .
type TypeDef struct {
	Ident Token
	// TypeParameters
	Type *Type
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
	p.err(errorf("TODO %v", p.s.Tok.str()))
	p.shift()
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
			p.err(errorf("TODO %v", p.s.Tok.str()))
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
		p.err(errorf("TODO %v", p.s.Tok.str()))
		p.shift()
		return r
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
