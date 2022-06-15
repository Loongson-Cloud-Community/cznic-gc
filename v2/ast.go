// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"bytes"
	"go/token"
)

var (
	_ = []Node{
		(*AliasDecl)(nil),
		(*AliasType)(nil),
		(*Arguments)(nil),
		(*ArrayType)(nil),
		(*ArrayTypeNode)(nil),
		(*Assignment)(nil),
		(*BasicLit)(nil),
		(*BinaryExpression)(nil),
		(*Block)(nil),
		(*BreakStmt)(nil),
		(*ChannelType)(nil),
		(*ChannelTypeNode)(nil),
		(*CommCase)(nil),
		(*CommClause)(nil),
		(*CompositeLit)(nil),
		(*ConstDecl)(nil),
		(*ConstSpec)(nil),
		(*Constant)(nil),
		(*ContinueStmt)(nil),
		(*Conversion)(nil),
		(*DeferStmt)(nil),
		(*EmbeddedField)(nil),
		(*EmptyStmt)(nil),
		(*ExprCaseClause)(nil),
		(*ExprSwitchCase)(nil),
		(*ExpressionListItem)(nil),
		(*ExpressionStmt)(nil),
		(*ExpressionSwitchStmt)(nil),
		(*FallthroughStmt)(nil),
		(*FieldDecl)(nil),
		(*ForClause)(nil),
		(*ForStmt)(nil),
		(*FunctionDecl)(nil),
		(*FunctionLit)(nil),
		(*FunctionType)(nil),
		(*FunctionTypeNode)(nil),
		(*GenericOperand)(nil),
		(*GoStmt)(nil),
		(*GotoStmt)(nil),
		(*Ident)(nil),
		(*IdentifierListItem)(nil),
		(*IfStmt)(nil),
		(*ImportDecl)(nil),
		(*ImportSpec)(nil),
		(*IncDecStmt)(nil),
		(*Index)(nil),
		(*InterfaceType)(nil),
		(*InterfaceTypeNode)(nil),
		(*KeyedElement)(nil),
		(*LabeledStmt)(nil),
		(*LiteralValue)(nil),
		(*MapType)(nil),
		(*MapTypeNode)(nil),
		(*MethodDecl)(nil),
		(*MethodElem)(nil),
		(*MethodExpr)(nil),
		(*Package)(nil),
		(*PackageClause)(nil),
		(*ParameterDecl)(nil),
		(*Parameters)(nil),
		(*ParenExpr)(nil),
		(*ParenType)(nil),
		(*PointerType)(nil),
		(*PointerTypeNode)(nil),
		(*QualifiedIdent)(nil),
		(*RangeClause)(nil),
		(*ReturnStmt)(nil),
		(*SelectStmt)(nil),
		(*Selector)(nil),
		(*SendStmt)(nil),
		(*ShortVarDecl)(nil),
		(*Signature)(nil),
		(*SliceExpr)(nil),
		(*SliceType)(nil),
		(*SliceTypeNode)(nil),
		(*SourceFile)(nil),
		(*StructType)(nil),
		(*StructTypeNode)(nil),
		(*TypeArgs)(nil),
		(*TypeAssertion)(nil),
		(*TypeAssertion)(nil),
		(*TypeCaseClause)(nil),
		(*TypeDecl)(nil),
		(*TypeDef)(nil),
		(*TypeElem)(nil),
		(*TypeListItem)(nil),
		(*TypeName)(nil),
		(*TypeNameNode)(nil),
		(*TypeParamDecl)(nil),
		(*TypeParameters)(nil),
		(*TypeSwitchGuard)(nil),
		(*TypeSwitchStmt)(nil),
		(*TypeTerm)(nil),
		(*UnaryExpr)(nil),
		(*VarDecl)(nil),
		(*VarSpec)(nil),
		(*Variable)(nil),
	}

	_ = []typeNode{
		(*StructTypeNode)(nil),
		(*PointerTypeNode)(nil),
		(*TypeNameNode)(nil),
		(*SliceTypeNode)(nil),
		(*InterfaceTypeNode)(nil),
		(*ArrayTypeNode)(nil),
		(*ChannelTypeNode)(nil),
		(*FunctionTypeNode)(nil),
		(*MapTypeNode)(nil),
		(*ParenType)(nil),
	}
)

type typeNode interface {
	Node
	isTypeNode()
}

type typeNoder struct{}

func (typeNoder) isTypeNode() {}

type simpleStmt interface {
	Node
	isSimpleStmt()
	semi(p *parser)
}

type simpleStmter struct{}

func (simpleStmter) isSimpleStmt() {}

// PackageClause describes the package clause.
//
//  PackageClause = "package" PackageName .
type PackageClause struct {
	Package     Token
	PackageName Token
	Semicolon   Token
}

// Position implements Node.
func (n *PackageClause) Position() (r token.Position) {
	return n.Package.Position()
}

// Source implements Node.
func (n *PackageClause) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ImportSpec describes an import specification.
//
//  ImportSpec = [ "." | PackageName ] ImportPath .
type ImportSpec struct {
	Qualifier  Token
	ImportPath Token
	Semicolon  Token
}

// Position implements Node.
func (n *ImportSpec) Position() (r token.Position) {
	if n.Qualifier.IsValid() {
		return n.Qualifier.Position()
	}

	return n.ImportPath.Position()
}

// Source implements Node.
func (n *ImportSpec) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ImportDecl describes an import declaration.
//
//  ImportDecl = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
type ImportDecl struct {
	Import      Token
	LParen      Token
	ImportSpecs []*ImportSpec
	RParen      Token
	Semicolon   Token
}

// Position implements Node.
func (n *ImportDecl) Position() (r token.Position) {
	return n.Import.Position()
}

// Source implements Node.
func (n *ImportDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// SourceFile describes a source file.
//
//  SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
type SourceFile struct {
	PackageClause *PackageClause
	ImportDecls   []*ImportDecl
	TopLevelDecls []Node
	EOF           Token
	Scope         Scope
}

// Position implements Node.
func (n *SourceFile) Position() (r token.Position) {
	return n.PackageClause.Position()
}

// Source implements Node.
func (n *SourceFile) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// FunctionDecl describes a function declaration.
//
//  FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
type FunctionDecl struct {
	guard
	typer
	Func           Token
	FunctionName   Token
	TypeParameters *TypeParameters
	Signature      *Signature
	FunctionBody   *Block
	Semicolon      Token
}

// Position implements Node.
func (n *FunctionDecl) Position() (r token.Position) {
	return n.Func.Position()
}

// Source implements Node.
func (n *FunctionDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// Signature describes a function signature.
//
//  Signature = Parameters [ Result ] .
type Signature struct {
	typer
	Parameters *Parameters
	Result     Node
}

// Position implements Node.
func (n *Signature) Position() (r token.Position) {
	return n.Parameters.Position()
}

// Source implements Node.
func (n *Signature) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

func (n *Signature) check(c *ctx) (r *FunctionType) {
	r = &FunctionType{}
	for _, v := range n.Parameters.ParameterList {
		t := newTyper(c.check(v.Type))
		if len(v.IdentifierList) == 0 {
			r.Parameters = append(r.Parameters, &Parameter{typer: t})
			continue
		}

		for _, w := range v.IdentifierList {
			r.Parameters = append(r.Parameters, &Parameter{Name: w.Ident.Src(), typer: t})
		}
	}
	switch x := n.Result.(type) {
	default:
		c.err(x, errorf("TODO %T", x))
	}
	n.typ = r
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

// Position implements Node.
func (n *Parameters) Position() (r token.Position) {
	return n.LParen.Position()
}

// Source implements Node.
func (n *Parameters) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// TypeDecl describes a type declaration.
//
//  TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
type TypeDecl struct {
	TypeTok   Token
	LParen    Token
	TypeSpecs []Node
	RParen    Token
	Semicolon Token
}

// Position implements Node.
func (n *TypeDecl) Position() (r token.Position) {
	return n.TypeTok.Position()
}

// Source implements Node.
func (n *TypeDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// TypeDef describes a type definition.
//
//  TypeDef = identifier [ TypeParameters ] Type .
type TypeDef struct {
	typer
	Ident          Token
	TypeParameters *TypeParameters
	TypeNode       Node
	Semicolon      Token
}

// Position implements Node.
func (n *TypeDef) Position() (r token.Position) {
	return n.Ident.Position()
}

// Source implements Node.
func (n *TypeDef) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ParameterDecl describes a parameter declaration.
//
//  ParameterDecl = [ IdentifierList ] [ "..." ] Type .
type ParameterDecl struct {
	IdentifierList []*IdentifierListItem
	Ellipsis       Token
	Type           Node
	Comma          Token
}

// Position implements Node.
func (n *ParameterDecl) Position() (r token.Position) {
	switch {
	case len(n.IdentifierList) != 0:
		return n.IdentifierList[0].Position()
	case n.Ellipsis.IsValid():
		return n.Ellipsis.Position()
	default:
		return n.Type.Position()
	}
}

// Source implements Node.
func (n *ParameterDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// IdentifierListItem describes an item of an identifier list.
type IdentifierListItem struct {
	Ident Token
	Comma Token
}

// Position implements Node.
func (n *IdentifierListItem) Position() (r token.Position) {
	return n.Ident.Position()
}

// Source implements Node.
func (n *IdentifierListItem) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// VarDecl describes a variable declaration.
//
//  VarDecl = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
type VarDecl struct {
	Var       Token
	LParen    Token
	VarSpecs  []*VarSpec
	RParen    Token
	Semicolon Token
}

// Position implements Node.
func (n *VarDecl) Position() (r token.Position) {
	return n.Var.Position()
}

// Source implements Node.
func (n *VarDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ConstDecl describes a constant declaration.
//
//  ConstDecl = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
type ConstDecl struct {
	Const      Token
	LParen     Token
	ConstSpecs []*ConstSpec
	RParen     Token
	Semicolon  Token
}

// Position implements Node.
func (n *ConstDecl) Position() (r token.Position) {
	return n.Const.Position()
}

// Source implements Node.
func (n *ConstDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// Block describes a compound statement.
//
//  Block = "{" StatementList "}" .
type Block struct {
	LBrace        Token
	StatementList []Node
	RBrace        Token
	Semicolon     Token
}

// Position implements Node.
func (n *Block) Position() (r token.Position) {
	return n.LBrace.Position()
}

// Source implements Node.
func (n *Block) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// StructTypeNode describes a struct type.
//
//  StructTyp = "struct" "{" { FieldDecl ";" } "}" .
type StructTypeNode struct {
	guard
	typer
	typeNoder
	Struct     Token
	LBrace     Token
	FieldDecls []Node
	RBrace     Token
}

// Position implements Node.
func (n *StructTypeNode) Position() (r token.Position) {
	return n.Struct.Position()
}

// Source implements Node.
func (n *StructTypeNode) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// FieldDecl describes a field declaration.
//
// FieldDecl = (IdentifierList Type | EmbeddedField) [ Tag ] .
type FieldDecl struct {
	IdentifierList []*IdentifierListItem
	Type           Node
	EmbeddedField  *EmbeddedField
	Tag            Token
	Semicolon      Token
}

// Position implements Node.
func (n *FieldDecl) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

// Source implements Node.
func (n *FieldDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// EmbeddedField describes an embeded field.
//
//  EmbeddedField = [ "*" ] TypeName .
type EmbeddedField struct {
	Star     Token
	TypeName *TypeNameNode
}

// Position implements Node.
func (n *EmbeddedField) Position() (r token.Position) {
	if n.Star.IsValid() {
		return n.Star.Position()
	}

	return n.TypeName.Position()
}

// Source implements Node.
func (n *EmbeddedField) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// VarSpec describes a variable specification.
//
//  VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
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

// Source implements Node.
func (n *VarSpec) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// PointerTypeNode describes a pointer type.
//
//  PointerTypeNode = "*" BaseType .
type PointerTypeNode struct {
	guard
	typer
	typeNoder
	Star     Token
	BaseType Node
}

// Position implements Node.
func (n *PointerTypeNode) Position() (r token.Position) {
	return n.Star.Position()
}

// Source implements Node.
func (n *PointerTypeNode) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// TypeNameNode describes a type name.
//
//  TypeNameNode = QualifiedIdent [ TypeArgs ]
//  	| identifier [ TypeArgs ] .
type TypeNameNode struct {
	guard
	typer
	typeNoder
	Name     *QualifiedIdent
	TypeArgs *TypeArgs
}

// Position implements Node.
func (n *TypeNameNode) Position() (r token.Position) {
	return n.Name.Position()
}

// Source implements Node.
func (n *TypeNameNode) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// QualifiedIdent describes an optionally qualified identifier.
//
//  QualifiedIdent = PackageName "." identifier .
type QualifiedIdent struct {
	guard
	typer
	valuer
	PackageName Token
	Dot         Token
	Ident       Token
}

// Position implements Node.
func (n *QualifiedIdent) Position() (r token.Position) {
	if n.PackageName.IsValid() {
		return n.PackageName.Position()
	}

	return n.Ident.Position()
}

// Source implements Node.
func (n *QualifiedIdent) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// ConstSpec describes a constant specification.
//
//  ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
type ConstSpec struct {
	IdentifierList []*IdentifierListItem
	Type           Node
	Eq             Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
	iota           int64
	expressionList []*ExpressionListItem
	typ            Node
}

// Position implements Node.
func (n *ConstSpec) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

// Source implements Node.
func (n *ConstSpec) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ExpressionListItem describes an item of an expression list.
//
// ExpressionList = Expression { "," Expression } .
type ExpressionListItem struct {
	Expression Expression
	Comma      Token
}

// Position implements Node.
func (n *ExpressionListItem) Position() (r token.Position) {
	return n.Expression.Position()
}

// Source implements Node.
func (n *ExpressionListItem) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// ExpressionStmt describes an expression statement.
//
type ExpressionStmt struct {
	Expression Expression
	Semicolon  Token
}

// Position implements Node.
func (n *ExpressionStmt) Position() (r token.Position) {
	return n.Expression.Position()
}

// Source implements Node.
func (n *ExpressionStmt) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// BinaryExpression describes a binary expression.
//
type BinaryExpression struct {
	guard
	typer
	valuer
	A  Expression
	Op Token
	B  Expression
}

// Position implements Node.
func (n *BinaryExpression) Position() (r token.Position) {
	return n.A.Position()
}

// Source implements Node.
func (n *BinaryExpression) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// ShortVarDecl describes a short variable declaration.
//
//  ShortVarDecl = IdentifierList ":=" ExpressionList .
type ShortVarDecl struct {
	simpleStmter
	IdentifierList []*IdentifierListItem
	Define         Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
}

// Position implements Node.
func (n *ShortVarDecl) Position() (r token.Position) {
	return n.IdentifierList[0].Position()
}

// Source implements Node.
func (n *ShortVarDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

func (n *ShortVarDecl) semi(p *parser) { n.Semicolon = p.semi(true) }

// MethodDecl describes a method declaration.
//
//  MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
type MethodDecl struct {
	guard
	typer
	Func         Token
	Receiver     *Parameters
	MethodName   Token
	Signature    *Signature
	FunctionBody *Block
	Semicolon    Token
}

// Position implements Node.
func (n *MethodDecl) Position() (r token.Position) {
	return n.Func.Position()
}

// Source implements Node.
func (n *MethodDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ReturnStmt describes a return statement.
//
//  ReturnStmt = "return" [ ExpressionList ] .
type ReturnStmt struct {
	Return         Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
}

// Position implements Node.
func (n *ReturnStmt) Position() (r token.Position) {
	return n.Return.Position()
}

// Source implements Node.
func (n *ReturnStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// Selector describes a selector.
//
//  Selector = PrimaryExpr "." identifier .
type Selector struct {
	guard
	typer
	valuer
	PrimaryExpr Expression
	Dot         Token
	Ident       Token
}

// Position implements Node.
func (n *Selector) Position() (r token.Position) {
	return n.PrimaryExpr.Position()
}

// Source implements Node.
func (n *Selector) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// Arguments describes a call or conversion.
//
//  Arguments = PrimaryExpr "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
type Arguments struct {
	guard
	typer
	valuer
	PrimaryExpr    Expression
	LParen         Token
	TypeArg        Node
	Comma          Token
	ExpressionList []*ExpressionListItem
	Ellipsis       Token
	Comma2         Token
	RParen         Token
}

// Position implements Node.
func (n *Arguments) Position() (r token.Position) {
	return n.PrimaryExpr.Position()
}

// Source implements Node.
func (n *Arguments) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// IfStmt describes an if statement.
//
//  IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
type IfStmt struct {
	If         Token
	SimpleStmt Node
	Semicolon  Token
	Expression Expression
	Block      *Block
	Else       Token
	ElsePart   Node
	Semicolon2 Token
}

// Position implements Node.
func (n *IfStmt) Position() (r token.Position) {
	return n.If.Position()
}

// Source implements Node.
func (n *IfStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// SliceTypeNode describes a slice type.
//
//  SliceTypeNode = "[" "]" ElementType .
type SliceTypeNode struct {
	typeNoder
	LBracket    Token
	RBracket    Token
	ElementType Node
}

// Position implements Node.
func (n *SliceTypeNode) Position() (r token.Position) {
	return n.LBracket.Position()
}

// Source implements Node.
func (n *SliceTypeNode) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// Assignment describes a short variable declaration.
//
// Assignment = ExpressionList assign_op ExpressionList .
type Assignment struct {
	simpleStmter
	LExpressionList []*ExpressionListItem
	AssOp           Token
	RExpressionList []*ExpressionListItem
	Semicolon       Token
}

// Position implements Node.
func (n *Assignment) Position() (r token.Position) {
	return n.LExpressionList[0].Position()
}

// Source implements Node.
func (n *Assignment) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

func (n *Assignment) semi(p *parser) { n.Semicolon = p.semi(true) }

// UnaryExpr describes an unary expression.
//
//  UnaryExpr = PrimaryExpr | unary_op UnaryExpr .
type UnaryExpr struct {
	guard
	typer
	valuer
	UnaryOp   Token
	UnaryExpr Expression
}

// Position implements Node.
func (n *UnaryExpr) Position() (r token.Position) {
	return n.UnaryOp.Position()
}

// Source implements Node.
func (n *UnaryExpr) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// CompositeLit describes a composite literal.
//
//  CompositeLit = LiteralType LiteralValue .
type CompositeLit struct {
	guard
	typer
	valuer
	LiteralType  Node
	LiteralValue *LiteralValue
}

// Position implements Node.
func (n *CompositeLit) Position() (r token.Position) {
	return n.LiteralType.Position()
}

// Source implements Node.
func (n *CompositeLit) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// LiteralValue describes a composite literal value.
//
//  LiteralValue = "{" [ ElementList [ "," ] ] "}" .
type LiteralValue struct {
	LBrace      Token
	ElementList []*KeyedElement
	RBrace      Token
}

// Position implements Node.
func (n *LiteralValue) Position() (r token.Position) {
	return n.LBrace.Position()
}

// Source implements Node.
func (n *LiteralValue) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// KeyedElement describes an optionally keyed element.
//
//  KeyedElement = [ Key ":" ] Element .
type KeyedElement struct {
	Key     Node
	Colon   Token
	Element Node
	Comma   Token
}

// Position implements Node.
func (n *KeyedElement) Position() (r token.Position) {
	if n.Key != nil {
		return n.Key.Position()
	}

	return n.Element.Position()
}

// Source implements Node.
func (n *KeyedElement) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// InterfaceTypeNode describes an interface type.
//
//  InterfaceTypeNode = "interface" "{" { InterfaceElem ";" } "}" .
type InterfaceTypeNode struct {
	typeNoder
	Interface      Token
	LBrace         Token
	InterfaceElems []Node
	RBrace         Token
}

// Position implements Node.
func (n *InterfaceTypeNode) Position() (r token.Position) {
	return n.Interface.Position()
}

// Source implements Node.
func (n *InterfaceTypeNode) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// ForStmt describes a for statement.
//
//  ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
type ForStmt struct {
	For         Token
	ForClause   *ForClause
	RangeClause *RangeClause
	Block       *Block
	Semicolon   Token
}

// Position implements Node.
func (n *ForStmt) Position() (r token.Position) {
	return n.For.Position()
}

// Source implements Node.
func (n *ForStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ForClause describes a for clause.
//
//  ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
type ForClause struct {
	InitStmt   Node
	Semicolon  Token
	Condition  Expression
	Semicolon2 Token
	PostStmt   Node
}

// Position implements Node.
func (n *ForClause) Position() (r token.Position) {
	if n.InitStmt != nil {
		return n.InitStmt.Position()
	}

	return n.Semicolon.Position()
}

// Source implements Node.
func (n *ForClause) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// RangeClause describes a range clause.
//
//  RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
type RangeClause struct {
	ExpressionList []*ExpressionListItem
	Assign         Token
	Range          Token
	Expression     Expression
}

// Position implements Node.
func (n *RangeClause) Position() (r token.Position) {
	return n.ExpressionList[0].Position()
}

// Source implements Node.
func (n *RangeClause) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// MethodElem describes a method element.
//
// MethodElem     = MethodName Signature .
type MethodElem struct {
	MethodName Token
	Signature  *Signature
	Semicolon  Token
}

// Position implements Node.
func (n *MethodElem) Position() (r token.Position) {
	return n.MethodName.Position()
}

// Source implements Node.
func (n *MethodElem) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// MethodExpr describes a method expression.
//
//  MethodExpr    = ReceiverType "." MethodName .
type MethodExpr struct {
	guard
	typer
	valuer
	Receiver Node
	Dot      Token
	Ident    Token
}

// Position implements Node.
func (n *MethodExpr) Position() (r token.Position) {
	return n.Receiver.Position()
}

// Source implements Node.
func (n *MethodExpr) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// TypeParameters describes type parameters.
//
//  TypeParameters = "[" TypeParamList [ "," ] "]" .
type TypeParameters struct {
	LBracket      Token
	TypeParamList []*TypeParamDecl
	RBracket      Token
}

// Position implements Node.
func (n *TypeParameters) Position() (r token.Position) {
	return n.LBracket.Position()
}

// Source implements Node.
func (n *TypeParameters) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// TypeParamDecl describes an item of a type parameter list.
//
//  TypeParamDecl = IdentifierList TypeConstraint .
type TypeParamDecl struct {
	IdentifierList []*IdentifierListItem
	TypeConstraint *TypeElem
	Comma          Token
}

// Position implements Node.
func (n *TypeParamDecl) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

// Source implements Node.
func (n *TypeParamDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// TypeElem describes a type element.
//
//  TypeElem = TypeTerm { "|" TypeTerm } .
type TypeElem struct {
	TypeTerms []*TypeTerm
	Semicolon Token
}

// Position implements Node.
func (n *TypeElem) Position() (r token.Position) {
	if len(n.TypeTerms) != 0 {
		return n.TypeTerms[0].Position()
	}

	return r
}

// Source implements Node.
func (n *TypeElem) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// TypeTerm describes a type term.
//
//  TypeTerm = Type | UnderlyingType .
//  UnderlyingType = "~" Type .
type TypeTerm struct {
	Tilde Token
	Type  Node
	Pipe  Token
}

// Position implements Node.
func (n *TypeTerm) Position() (r token.Position) {
	if n.Tilde.IsValid() {
		return n.Tilde.Position()
	}

	return n.Type.Position()
}

// Source implements Node.
func (n *TypeTerm) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// Index describes an index.
//
//  Index = "[" Expression "]" .
type Index struct {
	guard
	typer
	valuer
	PrimaryExpr Expression
	LBracket    Token
	Expression  Expression
	RBracket    Token
}

// Position implements Node.
func (n *Index) Position() (r token.Position) {
	return n.PrimaryExpr.Position()
}

// Source implements Node.
func (n *Index) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// DeferStmt describes a defer statement.
//
//  DeferStmt = "defer" Expression .
type DeferStmt struct {
	Defer      Token
	Expression Expression
	Semicolon  Token
}

// Position implements Node.
func (n *DeferStmt) Position() (r token.Position) {
	return n.Defer.Position()
}

// Source implements Node.
func (n *DeferStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// EmptyStmt describes an empty statement.
//
//  EmptyStmt = .
type EmptyStmt struct {
	Semicolon Token
}

// Position implements Node.
func (n *EmptyStmt) Position() (r token.Position) {
	return n.Semicolon.Position()
}

// Source implements Node.
func (n *EmptyStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// FunctionLit describes a function literal.
//
//  FunctionLit = "func" Signature FunctionBody .
type FunctionLit struct {
	guard
	typer
	valuer
	Func         Token
	Signature    *Signature
	FunctionBody *Block
}

// Position implements Node.
func (n *FunctionLit) Position() (r token.Position) {
	return n.Func.Position()
}

// Source implements Node.
func (n *FunctionLit) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ExpressionSwitchStmt describes an expression switch statement.
//
//  ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
type ExpressionSwitchStmt struct {
	Switch          Token
	SimpleStmt      Node
	Semicolon       Token
	Expression      Expression
	LBrace          Token
	ExprCaseClauses []*ExprCaseClause
	RBrace          Token
	Semicolon2      Token
}

// Position implements Node.
func (n *ExpressionSwitchStmt) Position() (r token.Position) {
	return n.Switch.Position()
}

// Source implements Node.
func (n *ExpressionSwitchStmt) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// TypeSwitchStmt describes a type switch statement.
//
//  TypeSwitchStmt  = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
type TypeSwitchStmt struct {
	Switch          Token
	SimpleStmt      Node
	Semicolon       Token
	TypeSwitchGuard *TypeSwitchGuard
	LBrace          Token
	TypeCaseClauses []*TypeCaseClause
	RBrace          Token
	Semicolon2      Token
}

// Position implements Node.
func (n *TypeSwitchStmt) Position() (r token.Position) {
	return n.Switch.Position()
}

// Source implements Node.
func (n *TypeSwitchStmt) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// TypeSwitchGuard describes a type switch guard.
//
//  TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" .
type TypeSwitchGuard struct {
	guard
	typer
	valuer
	Ident       Token
	Define      Token
	PrimaryExpr Expression
	Dot         Token
	LParen      Token
	TypeToken   Token
	RParen      Token
}

// Position implements Node.
func (n *TypeSwitchGuard) Position() (r token.Position) {
	if n.Ident.IsValid() {
		return n.Ident.Position()
	}

	return n.PrimaryExpr.Position()
}

// Source implements Node.
func (n *TypeSwitchGuard) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// TypeCaseClause describes a type switch case clause.
//
//  TypeCaseClause  = TypeSwitchCase ":" StatementList .
type TypeCaseClause struct {
	TypeSwitchCase *TypeSwitchCase
	Colon          Token
	StatementList  []Node
}

// Position implements Node.
func (n *TypeCaseClause) Position() (r token.Position) {
	return n.TypeSwitchCase.Position()
}

// Source implements Node.
func (n *TypeCaseClause) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// TypeSwitchCase describes an expression switch case.
//
//  TypeSwitchCase  = "case" TypeList | "default" .
type TypeSwitchCase struct {
	CaseOrDefault Token
	TypeList      []*TypeListItem
}

// Position implements Node.
func (n *TypeSwitchCase) Position() (r token.Position) {
	return n.CaseOrDefault.Position()
}

// Source implements Node.
func (n *TypeSwitchCase) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// TypeAssertion describes a type assertion.
//
//  TypeAssertion = PrimaryExpr "." "(" Type ")" .
type TypeAssertion struct {
	guard
	typer
	valuer
	PrimaryExpr Expression
	Dot         Token
	LParen      Token
	AssertType  Node
	RParen      Token
}

// Position implements Node.
func (n *TypeAssertion) Position() (r token.Position) {
	return n.PrimaryExpr.Position()
}

// Source implements Node.
func (n *TypeAssertion) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// CommClause describes an select statement communication clause.
//
//  CommClause = CommCase ":" StatementList .
type CommClause struct {
	CommCase      *CommCase
	Colon         Token
	StatementList []Node
}

// Position implements Node.
func (n *CommClause) Position() (r token.Position) {
	return n.CommCase.Position()
}

// Source implements Node.
func (n *CommClause) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// CommCase describes an communication clause case.
//
//  CommCase   = "case" ( SendStmt | RecvStmt ) | "default" .
type CommCase struct {
	CaseOrDefault Token
	Statement     Node
}

// Position implements Node.
func (n *CommCase) Position() (r token.Position) {
	return n.CaseOrDefault.Position()
}

// Source implements Node.
func (n *CommCase) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ExprCaseClause describes an expression switch case clause.
//
//  ExprCaseClause = ExprSwitchCase ":" StatementList .
type ExprCaseClause struct {
	ExprSwitchCase *ExprSwitchCase
	Colon          Token
	StatementList  []Node
}

// Position implements Node.
func (n *ExprCaseClause) Position() (r token.Position) {
	return n.ExprSwitchCase.Position()
}

// Source implements Node.
func (n *ExprCaseClause) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// ExprSwitchCase describes an expression switch case.
//
//  ExprSwitchCase = "case" ExpressionList | "default" .
type ExprSwitchCase struct {
	CaseOrDefault  Token
	ExpressionList []*ExpressionListItem
}

// Position implements Node.
func (n *ExprSwitchCase) Position() (r token.Position) {
	return n.CaseOrDefault.Position()
}

// Source implements Node.
func (n *ExprSwitchCase) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// SliceExpr describes a slice expression.
//
//  SliceExpr = "[" [ Expression ] ":" [ Expression ] "]" | "[" [ Expression ] ":" Expression ":" Expression "]" .
type SliceExpr struct {
	guard
	typer
	valuer
	PrimaryExpr Expression
	LBracket    Token
	Expression  Expression
	Colon       Token
	Expression2 Expression
	Colon2      Token
	Expression3 Expression
	RBracket    Token
}

// Position implements Node.
func (n *SliceExpr) Position() (r token.Position) {
	return n.LBracket.Position()
}

// Source implements Node.
func (n *SliceExpr) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// SelectStmt describes a select statement.
//
//  SelectStmt = "select" "{" { CommClause } "}" .
type SelectStmt struct {
	Select      Token
	LBrace      Token
	CommClauses []*CommClause
	RBrace      Token
	Semicolon   Token
}

// Position implements Node.
func (n *SelectStmt) Position() (r token.Position) {
	return n.Select.Position()
}

// Source implements Node.
func (n *SelectStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// SendStmt describes a send statement.
//
//  SendStmt = Channel "<-" Expression .
type SendStmt struct {
	simpleStmter
	Channel    Node
	Arrow      Token
	Expression Expression
	Semicolon  Token
}

// Position implements Node.
func (n *SendStmt) Position() (r token.Position) {
	return n.Channel.Position()
}

// Source implements Node.
func (n *SendStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

func (n *SendStmt) semi(p *parser) { n.Semicolon = p.semi(true) }

// BreakStmt describes a continue statement.
//
//  BreakStmt = "break" [ Label ] .
type BreakStmt struct {
	Break     Token
	Label     Token
	Semicolon Token
}

// Position implements Node.
func (n *BreakStmt) Position() (r token.Position) {
	return n.Break.Position()
}

// Source implements Node.
func (n *BreakStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ContinueStmt describes a continue statement.
//
//  ContinueStmt = "continue" [ Label ] .
type ContinueStmt struct {
	Continue  Token
	Label     Token
	Semicolon Token
}

// Position implements Node.
func (n *ContinueStmt) Position() (r token.Position) {
	return n.Continue.Position()
}

// Source implements Node.
func (n *ContinueStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// FallthroughStmt describes a fallthrough statement.
//
//  FallthroughStmt = "fallthrough" .
type FallthroughStmt struct {
	Fallthrough Token
	Semicolon   Token
}

// Position implements Node.
func (n *FallthroughStmt) Position() (r token.Position) {
	return n.Fallthrough.Position()
}

// Source implements Node.
func (n *FallthroughStmt) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// Conversion describes a conversion.
//
//  Conversion = Type "(" Expression [ "," ] ")" .
type Conversion struct {
	guard
	typer
	valuer
	ConvertType Node
	LParen      Token
	Expression  Expression
	Comma       Token
	RParen      Token
}

// Position implements Node.
func (n *Conversion) Position() (r token.Position) {
	return n.ConvertType.Position()
}

// Source implements Node.
func (n *Conversion) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// AliasDecl describes a type alias.
//
//  AliasDecl = identifier "=" Type .
type AliasDecl struct {
	Ident     Token
	Eq        Token
	Type      Node
	Semicolon Token
}

// Position implements Node.
func (n *AliasDecl) Position() (r token.Position) {
	return n.Ident.Position()
}

// Source implements Node.
func (n *AliasDecl) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ArrayTypeNode describes a channel type.
//
//  ArrayType   = "[" ArrayLength "]" ElementType .
//  ArrayLength = Expression | "..."
type ArrayTypeNode struct {
	typeNoder
	LBracket    Token
	ArrayLength Expression
	Ellipsis    Token
	RBracket    Token
	ElementType Node
}

// Position implements Node.
func (n *ArrayTypeNode) Position() (r token.Position) {
	return n.LBracket.Position()
}

// Source implements Node.
func (n *ArrayTypeNode) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ChannelTypeNode describes a channel type.
//
//  ChannelTypeNode = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
type ChannelTypeNode struct {
	typeNoder
	ArrowPre    Token
	Chan        Token
	ArrayPost   Token
	ElementType Node
}

// Position implements Node.
func (n *ChannelTypeNode) Position() (r token.Position) {
	if n.ArrowPre.IsValid() {
		return n.ArrowPre.Position()
	}

	return n.Chan.Position()
}

// Source implements Node.
func (n *ChannelTypeNode) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// FunctionTypeNode describes a function type.
//
//  FunctionTypeNode = "func" Signature .
type FunctionTypeNode struct {
	typeNoder
	Func      Token
	Signature *Signature
}

// Position implements Node.
func (n *FunctionTypeNode) Position() (r token.Position) {
	return n.Func.Position()
}

// Source implements Node.
func (n *FunctionTypeNode) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// MapTypeNode describes a map type.
//
//  MapTypeNode = "map" "[" KeyType "]" ElementType .
type MapTypeNode struct {
	typeNoder
	Map         Token
	LBracket    Token
	KeyType     Node
	RBracket    Token
	ElementType Node
}

// Position implements Node.
func (n *MapTypeNode) Position() (r token.Position) {
	return n.Map.Position()
}

// Source implements Node.
func (n *MapTypeNode) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// GoStmt describes a go statement.
//
// GoStmt = "go" Expression .
type GoStmt struct {
	Go         Token
	Expression Expression
	Semicolon  Token
}

// Position implements Node.
func (n *GoStmt) Position() (r token.Position) {
	return n.Go.Position()
}

// Source implements Node.
func (n *GoStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// GenericOperand describes an operand name and type arguments.
//
// GenericOperand = OperandName TypeArgs .
type GenericOperand struct {
	guard
	typer
	valuer
	OperandName Node
	TypeArgs    *TypeArgs
}

// Position implements Node.
func (n *GenericOperand) Position() (r token.Position) {
	return n.OperandName.Position()
}

// Source implements Node.
func (n *GenericOperand) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n, full).Bytes()
}

// GotoStmt describes a goto statement.
//
//  GotoStmt = "goto" Label .
type GotoStmt struct {
	Goto      Token
	Label     Token
	Semicolon Token
}

// Position implements Node.
func (n *GotoStmt) Position() (r token.Position) {
	return n.Goto.Position()
}

// Source implements Node.
func (n *GotoStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// LabeledStmt describes a labeled statement.
//
//  LabeledStmt = Label ":" Statement .
type LabeledStmt struct {
	Label     Token
	Colon     Token
	Statement Node
}

// Position implements Node.
func (n *LabeledStmt) Position() (r token.Position) {
	return n.Label.Position()
}

// Source implements Node.
func (n *LabeledStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// TypeArgs describes a type name.
//
//  TypeArgs = "[" TypeList [ "," ] "]" .
type TypeArgs struct {
	LBracket Token
	TypeList []*TypeListItem
	RBracket Token
	Comma    Token
}

// Position implements Node.
func (n *TypeArgs) Position() (r token.Position) {
	return n.LBracket.Position()
}

// Source implements Node.
func (n *TypeArgs) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// TypeListItem describes an item of a type list.
//
type TypeListItem struct {
	Type  Node
	Comma Token
}

// Position implements Node.
func (n *TypeListItem) Position() (r token.Position) {
	return n.Type.Position()
}

// Source implements Node.
func (n *TypeListItem) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// IncDecStmt describes an increment or decrement statemen.
//
//  IncDecStmt = Expression ( "++" | "--" ) .
type IncDecStmt struct {
	simpleStmter
	Expression Expression
	Op         Token
	Semicolon  Token
}

// Position implements Node.
func (n *IncDecStmt) Position() (r token.Position) {
	return n.Expression.Position()
}

// Source implements Node.
func (n *IncDecStmt) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

func (n *IncDecStmt) semi(p *parser) { n.Semicolon = p.semi(true) }

// ParenExpr describes a parenthesized expression.
//
// ParenExpr = "(" Expression ")" .
type ParenExpr struct {
	guard
	typer
	valuer
	LParen     Token
	Expression Expression
	RParen     Token
}

// Position implements Node.
func (n *ParenExpr) Position() (r token.Position) {
	return n.LParen.Position()
}

// Source implements Node.
func (n *ParenExpr) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// ParenType describes a parenthesized type.
//
// ParenType = "(" Type ")" .
type ParenType struct {
	typeNoder
	LParen Token
	Type   Node
	RParen Token
}

// Position implements Node.
func (n *ParenType) Position() (r token.Position) {
	return n.LParen.Position()
}

// Source implements Node.
func (n *ParenType) Source(full bool) []byte { return nodeSource(&bytes.Buffer{}, n, full).Bytes() }

// Constant represents a Go constant.
type Constant struct {
	guard
	node *ConstSpec
	typer
	valuer
	Expr  Expression
	Ident Token
}

// Position implements Node.
func (n *Constant) Position() (r token.Position) { return n.Ident.Position() }

// Source implements Node.
func (n *Constant) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n.Expr, full).Bytes()
}

// Variable represents a Go variable.
type Variable struct {
	guard
	typer
	Expr  Expression
	Ident Token
}

// Position implements Node.
func (n *Variable) Position() (r token.Position) { return n.Ident.Position() }

// Source implements Node.
func (n *Variable) Source(full bool) []byte {
	return nodeSource(&bytes.Buffer{}, n.Expr, full).Bytes()
}

// BasicLit represents a basic literal.
type BasicLit struct {
	guard
	typer
	valuer
	Token Token
}

// Position implements Node.
func (n *BasicLit) Position() (r token.Position) { return n.Token.Position() }

// Source implements Node.
func (n *BasicLit) Source(full bool) []byte { return n.Token.src() }

// Ident represents an unqualified operand name.
type Ident struct {
	guard
	typer
	valuer
	Token Token
}

// Position implements Node.
func (n *Ident) Position() (r token.Position) { return n.Token.Position() }

// Source implements Node.
func (n *Ident) Source(full bool) []byte { return n.Token.src() }
