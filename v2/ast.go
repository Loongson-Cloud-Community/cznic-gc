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
		(*ContinueStmt)(nil),
		(*Conversion)(nil),
		(*DeferStmt)(nil),
		(*EmbeddedField)(nil),
		(*ExprCaseClause)(nil),
		(*ExprSwitchCase)(nil),
		(*ExpressionListItem)(nil),
		(*ExpressionStmt)(nil),
		(*ExpressionSwitchStmt)(nil),
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
		(*IncDecStmt)(nil),
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
		(*SendStmt)(nil),
		(*ShortVarDecl)(nil),
		(*Signature)(nil),
		(*Slice)(nil),
		(*SliceType)(nil),
		(*SourceFile)(nil),
		(*StructType)(nil),
		(*TypeArgs)(nil),
		(*TypeAssertion)(nil),
		(*TypeDecl)(nil),
		(*TypeDef)(nil),
		(*TypeElem)(nil),
		(*TypeListItem)(nil),
		(*TypeName)(nil),
		(*TypeParamDecl)(nil),
		(*TypeParameters)(nil),
		(*TypeTerm)(nil),
		(*UnaryExpr)(nil),
		(*VarDecl)(nil),
		(*VarSpec)(nil),
	}
)

// PackageClause describes the package clause.
//
//  PackageClause = "package" PackageName .
type PackageClause struct {
	Package     Token
	PackageName Token
	Semicolon   Token
}

// ImportSpec describes an import specification.
//
//  ImportSpec = [ "." | PackageName ] ImportPath .
type ImportSpec struct {
	Qualifier  Token
	ImportPath Token
	Semicolon  Token
}

// Positions implements Node.
func (n *ImportSpec) Position() (r token.Position) {
	if n.Qualifier.IsValid() {
		return n.Qualifier.Position()
	}

	return n.ImportPath.Position()
}

// Positions implements Node.
func (n *PackageClause) Position() (r token.Position) { return n.Package.Position() }

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

// Positions implements Node.
func (n *ImportDecl) Position() (r token.Position) { return n.Import.Position() }

// SourceFile describes a source file.
//
//  SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
type SourceFile struct {
	PackageClause *PackageClause
	ImportDecls   []*ImportDecl
	TopLevelDecls []Node
	EOF           Token
}

// Positions implements Node.
func (n *SourceFile) Position() (r token.Position) { return n.PackageClause.Position() }

// FunctionDecl describes a function declaration.
//
//  FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
type FunctionDecl struct {
	Func           Token
	FunctionName   Token
	TypeParameters Node //TODO *TypeParameters
	Signature      *Signature
	FunctionBody   Node //TODO *Block
	Semicolon      Token
}

// Positions implements Node.
func (n *FunctionDecl) Position() (r token.Position) { return n.Func.Position() }

// Signature describes a function signature.
//
//  Signature = Parameters [ Result ] .
type Signature struct {
	Parameters Node //TODO *Parameters
	Result     Node
}

// Positions implements Node.
func (n *Signature) Position() (r token.Position) { return n.Parameters.Position() }

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
func (n *Parameters) Position() (r token.Position) { return n.LParen.Position() }

// TypeDecl describes a type declaration.
//
//  TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
type TypeDecl struct {
	Type      Token
	LParen    Token
	TypeSpecs []Node
	RParen    Token
	Semicolon Token
}

// Positions implements Node.
func (n *TypeDecl) Position() (r token.Position) { return n.Type.Position() }

// TypeDef describes a type definition.
//
//  TypeDef = identifier [ TypeParameters ] Type .
type TypeDef struct {
	Ident          Token
	TypeParameters Node //TODO *TypeParameters
	Type           Node
	Semicolon      Token
}

// Positions implements Node.
func (n *TypeDef) Position() (r token.Position) { return n.Ident.Position() }

// ParameterDecl describes a parameter declaration.
//
//  ParameterDecl = [ IdentifierList ] [ "..." ] Type .
type ParameterDecl struct {
	IdentifierList []*IdentifierListItem
	ELLIPSIS       Token
	Type           Node
	Comma          Token
}

// Positions implements Node.
func (n *ParameterDecl) Position() (r token.Position) {
	switch {
	case len(n.IdentifierList) != 0:
		return n.IdentifierList[0].Position()
	case n.ELLIPSIS.IsValid():
		return n.ELLIPSIS.Position()
	default:
		return n.Type.Position()
	}
}

// IdentifierListItem describes an item of an identifier list.
type IdentifierListItem struct {
	Ident Token
	Comma Token
}

// Positions implements Node.
func (n *IdentifierListItem) Position() (r token.Position) { return n.Ident.Position() }

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

// Positions implements Node.
func (n *VarDecl) Position() (r token.Position) { return n.Var.Position() }

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

// Positions implements Node.
func (n *ConstDecl) Position() (r token.Position) { return n.Const.Position() }

// Block describes a compound statement.
//
//  Block = "{" StatementList "}" .
type Block struct {
	LBrace        Token
	StatementList []Node
	RBrace        Token
}

// Positions implements Node.
func (n *Block) Position() (r token.Position) { return n.LBrace.Position() }

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
func (n *StructType) Position() (r token.Position) { return n.Struct.Position() }

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

// Positions implements Node.
func (n *VarSpec) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

// PointerType describes a pointer type.
//
//  PointerType = "*" BaseType .
type PointerType struct {
	Star     Token
	BaseType Node
}

// Positions implements Node.
func (n *PointerType) Position() (r token.Position) { return n.Star.Position() }

// TypeName describes a type name.
//
//  TypeName = QualifiedIdent [ TypeArgs ]
//  	| identifier [ TypeArgs ] .
type TypeName struct {
	Name     *QualifiedIdent
	TypeArgs *TypeArgs
}

// Positions implements Node.
func (n *TypeName) Position() (r token.Position) { return n.Name.Position() }

// QualifiedIdent describes an optionally qualified identifier.
//
//  QualifiedIdent = PackageName "." identifier .
type QualifiedIdent struct {
	PackageName Token
	Dot         Token
	Ident       Token
}

// Positions implements Node.
func (n *QualifiedIdent) Position() (r token.Position) {
	if n.PackageName.IsValid() {
		return n.PackageName.Position()
	}

	return n.Ident.Position()
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
}

// Positions implements Node.
func (n *ConstSpec) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

// ExpressionListItem describes an item of an expression list.
//
// ExpressionList = Expression { "," Expression } .
type ExpressionListItem struct {
	Expression Node
	Comma      Token
}

// Positions implements Node.
func (n *ExpressionListItem) Position() (r token.Position) { return n.Expression.Position() }

// ExpressionStmt describes an expression statement.
//
type ExpressionStmt struct {
	Expression Node
	Semicolon  Token
}

// Positions implements Node.
func (n *ExpressionStmt) Position() (r token.Position) { return n.Expression.Position() }

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

// Positions implements Node.
func (n *FieldDecl) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
}

// BinaryExpression describes a binary expression.
//
type BinaryExpression struct {
	A  Node
	Op Token
	B  Node
}

// Positions implements Node.
func (n *BinaryExpression) Position() (r token.Position) { return n.A.Position() }

// ShortVarDecl describes a short variable declaration.
//
//  ShortVarDecl = IdentifierList ":=" ExpressionList .
type ShortVarDecl struct {
	IdentifierList []*IdentifierListItem
	Define         Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
}

// Positions implements Node.
func (n *ShortVarDecl) Position() (r token.Position) { return n.IdentifierList[0].Position() }

// MethodDecl describes a method declaration.
//
//  MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
type MethodDecl struct {
	Func         Token
	Receiver     *Parameters
	MethodName   Token
	Signature    *Signature
	FunctionBody *Block
	Semicolon    Token
}

// Positions implements Node.
func (n *MethodDecl) Position() (r token.Position) { return n.Func.Position() }

// ReturnStmt describes a return statement.
//
//  ReturnStmt = "return" [ ExpressionList ] .
type ReturnStmt struct {
	Return         Token
	ExpressionList []*ExpressionListItem
	Semicolon      Token
}

// Positions implements Node.
func (n *ReturnStmt) Position() (r token.Position) { return n.Return.Position() }

// Selector describes a selector.
//
//  Selector = PrimaryExpr "." identifier .
type Selector struct {
	PrimaryExpr Node
	Dot         Token
	Ident       Token
}

// Positions implements Node.
func (n *Selector) Position() (r token.Position) { return n.PrimaryExpr.Position() }

// Arguments describes a call or conversion.
//
//  Arguments = PrimaryExpr "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
type Arguments struct {
	PrimaryExpr    Node
	LParen         Token
	Type           Node
	ExpressionList []*ExpressionListItem
	Comma          Token
	Comma2         Token
	RParen         Token
}

// Positions implements Node.
func (n *Arguments) Position() (r token.Position) { return n.PrimaryExpr.Position() }

// IfStmt describes an if statement.
//
//  IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
type IfStmt struct {
	If         Token
	SimpleStmt Node
	Semicolon  Token
	Expression Node
	Block      *Block
	Semicolon2 Token
}

// Positions implements Node.
func (n *IfStmt) Position() (r token.Position) { return n.If.Position() }

// SliceType describes a slice type.
//
//  SliceType = "[" "]" ElementType .
type SliceType struct {
	LBracket    Token
	RBracket    Token
	ElementType Node
}

// Positions implements Node.
func (n *SliceType) Position() (r token.Position) { return n.LBracket.Position() }

// Assignment describes a short variable declaration.
//
// Assignment = ExpressionList assign_op ExpressionList .
type Assignment struct {
	LExpressionList []*ExpressionListItem
	AssOp           Token
	RExpressionList []*ExpressionListItem
	Semicolon       Token
}

// Positions implements Node.
func (n *Assignment) Position() (r token.Position) { return n.LExpressionList[0].Position() }

// UnaryExpr describes an unary expression.
//
//  UnaryExpr = PrimaryExpr | unary_op UnaryExpr .
type UnaryExpr struct {
	UnaryOp   Token
	UnaryExpr Node
}

// Positions implements Node.
func (n *UnaryExpr) Position() (r token.Position) { return n.UnaryOp.Position() }

// CompositeLit describes a composite literal.
//
//  CompositeLit = LiteralType LiteralValue .
type CompositeLit struct {
	LiteralType  Node
	LiteralValue *LiteralValue
}

// Positions implements Node.
func (n *CompositeLit) Position() (r token.Position) { return n.LiteralType.Position() }

// LiteralValue describes a composite literal value.
//
//  LiteralValue = "{" [ ElementList [ "," ] ] "}" .
type LiteralValue struct {
	LBrace      Token
	ElementList []*KeyedElement
	RBrace      Token
}

// Positions implements Node.
func (n *LiteralValue) Position() (r token.Position) { return n.LBrace.Position() }

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
func (n *KeyedElement) Position() (r token.Position) {
	if n.Key != nil {
		return n.Key.Position()
	}

	return n.Element.Position()
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
func (n *InterfaceType) Position() (r token.Position) { return n.Interface.Position() }

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
func (n *ForStmt) Position() (r token.Position) { return n.For.Position() }

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
func (n *ForClause) Position() (r token.Position) {
	if n.InitStmt != nil {
		return n.InitStmt.Position()
	}

	return n.Semicolon.Position()
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
func (n *RangeClause) Position() (r token.Position) { return n.ExpressionList[0].Position() }

// MethodElem describes a method element.
//
type MethodElem struct {
	MethodName Token
	Signature  *Signature
	Semicolon  Token
}

// Positions implements Node.
func (n *MethodElem) Position() (r token.Position) { return n.MethodName.Position() }

// TypeParameters describes type parameters.
//
//  TypeParameters = "[" TypeParamList [ "," ] "]" .
type TypeParameters struct {
	LBracket      Token
	TypeParamList []*TypeParamDecl
	RBracket      Token
}

// Positions implements Node.
func (n *TypeParameters) Position() (r token.Position) { return n.LBracket.Position() }

// TypeParamDecl describes an item of a type parameter list.
//
//  TypeParamDecl = IdentifierList TypeConstraint .
type TypeParamDecl struct {
	IdentifierList []*IdentifierListItem
	TypeConstraint []*TypeElem
	Comma          Token
}

// Positions implements Node.
func (n *TypeParamDecl) Position() (r token.Position) {
	if len(n.IdentifierList) != 0 {
		return n.IdentifierList[0].Position()
	}

	return r
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

// TypeTerm describes a type term.
//
//  TypeTerm = Type | UnderlyingType .
//  UnderlyingType = "~" Type .
type TypeTerm struct {
	Tilde Token
	Type  Node
}

// Positions implements Node.
func (n *TypeTerm) Position() (r token.Position) {
	if n.Tilde.IsValid() {
		return n.Tilde.Position()
	}

	return n.Type.Position()
}

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
func (n *Index) Position() (r token.Position) { return n.PrimaryExpr.Position() }

// DeferStmt describes a defer statement.
//
//  DeferStmt = "defer" Expression .
type DeferStmt struct {
	Defer      Token
	Expression Node
	Semicolon  Token
}

// Positions implements Node.
func (n *DeferStmt) Position() (r token.Position) { return n.Defer.Position() }

// FunctionLit describes a function literal.
//
//  FunctionLit = "func" Signature FunctionBody .
type FunctionLit struct {
	Func         Token
	Signature    *Signature
	FunctionBody *Block
}

// Positions implements Node.
func (n *FunctionLit) Position() (r token.Position) { return n.Func.Position() }

// ExpressionSwitchStmt describes an expression switch statement.
//
//  ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
type ExpressionSwitchStmt struct {
	Switch          Token
	SimpleStmt      Node
	Semicolon       Token
	Expression      Node
	LBrace          Token
	ExprCaseClauses []*ExprCaseClause
	RBrace          Token
	Semicolon2      Token
}

// Positions implements Node.
func (n *ExpressionSwitchStmt) Position() (r token.Position) { return n.Switch.Position() }

// TypeAssertion describes a type assertion.
//
//  TypeAssertion = PrimaryExpr "." "(" Type ")" .
type TypeAssertion struct {
	PrimaryExpr Node
	Dot         Token
	LParen      Token
	Type        Node
	RParen      Token
}

// Positions implements Node.
func (n *TypeAssertion) Position() (r token.Position) { return n.PrimaryExpr.Position() }

// ExprCaseClause describes an expression switch case clause.
//
//  ExprCaseClause = ExprSwitchCase ":" StatementList .
type ExprCaseClause struct {
	ExprSwitchCase *ExprSwitchCase
	Colon          Token
	StatementList  []Node
}

// Positions implements Node.
func (n *ExprCaseClause) Position() (r token.Position) { return n.ExprSwitchCase.Position() }

// ExprSwitchCase describes an expression switch case.
//
//  ExprSwitchCase = "case" ExpressionList | "default" .
type ExprSwitchCase struct {
	CaseOrDefault  Token
	ExpressionList []*ExpressionListItem
}

// Positions implements Node.
func (n *ExprSwitchCase) Position() (r token.Position) { return n.CaseOrDefault.Position() }

// Slice describes a slice expression.
//
//  Slice = "[" [ Expression ] ":" [ Expression ] "]" | "[" [ Expression ] ":" Expression ":" Expression "]" .
type Slice struct {
	PrimaryExpr Node
	LBracket    Token
	Expression  Node
	Colon       Token
	Expression2 Node
	Colon2      Token
	Expression3 Node
	RBracket    Token
}

// Positions implements Node.
func (n *Slice) Position() (r token.Position) { return n.LBracket.Position() }

// SendStmt describes a send statement.
//
//  SendStmt = Channel "<-" Expression .
type SendStmt struct {
	Channel    Node
	Arrow      Token
	Expression Node
	Semicolon  Token
}

// Positions implements Node.
func (n *SendStmt) Position() (r token.Position) { return n.Channel.Position() }

// ContinueStmt describes a continue statement.
//
type ContinueStmt struct {
	Continue Token
	Label    Token
}

// Positions implements Node.
func (n *ContinueStmt) Position() (r token.Position) { return n.Continue.Position() }

// Conversion describes a conversion.
//
//  Conversion = Type "(" Expression [ "," ] ")" .
type Conversion struct {
	Type       Node
	LParen     Token
	Expression Node
	Comma      Token
	RParen     Token
}

// Positions implements Node.
func (n *Conversion) Position() (r token.Position) { return n.Type.Position() }

// AliasDecl describes a type alias.
//
//  AliasDecl = identifier "=" Type .
type AliasDecl struct {
	Ident Token
	Eq    Token
	Type  Node
}

// Positions implements Node.
func (n *AliasDecl) Position() (r token.Position) { return n.Ident.Position() }

// ArrayType describes a channel type.
//
type ArrayType struct {
	LBracket    Token
	ArrayLength Node
	RBracket    Token
	ElementType Node
}

// Positions implements Node.
func (n *ArrayType) Position() (r token.Position) { return n.LBracket.Position() }

// ChannelType describes a channel type.
//
//  ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
type ChannelType struct {
	ArrowPre    Token
	Chan        Token
	ArrayPost   Token
	ElementType Node
}

// Positions implements Node.
func (n *ChannelType) Position() (r token.Position) {
	if n.ArrowPre.IsValid() {
		return n.ArrowPre.Position()
	}

	return n.Chan.Position()
}

// FunctionType describes a function type.
//
//  FunctionType = "func" Signature .
type FunctionType struct {
	Func      Token
	Signature *Signature
}

// Positions implements Node.
func (n *FunctionType) Position() (r token.Position) { return n.Func.Position() }

func (p *parser) typeElem(id Token) (r *TypeElem) {
	panic(todo(""))
}

// MapType describes a map type.
//
//  MapType = "map" "[" KeyType "]" ElementType .
type MapType struct {
	Map         Token
	LBracket    Token
	KeyType     Node
	RBracket    Token
	ElementType Node
}

// Positions implements Node.
func (n *MapType) Position() (r token.Position) { return n.Map.Position() }

// EmbeddedField describes an embeded field.
//
//  EmbeddedField = [ "*" ] TypeName .
type EmbeddedField struct {
	Star     Token
	TypeName *QualifiedIdent
}

// Positions implements Node.
func (n *EmbeddedField) Position() (r token.Position) {
	if n.Star.IsValid() {
		return n.Star.Position()
	}

	return n.TypeName.Position()
}

// GoStmt describes a go statement.
//
// GoStmt = "go" Expression .
type GoStmt struct {
	Go         Token
	Expression Node
}

// Positions implements Node.
func (n *GoStmt) Position() (r token.Position) { return n.Go.Position() }

// TypeArgs describes a type name.
//
//  TypeArgs = "[" TypeList [ "," ] "]" .
type TypeArgs struct {
	LBracket Token
	TypeList []*TypeListItem
	RBracket Token
}

// Positions implements Node.
func (n *TypeArgs) Position() (r token.Position) { return n.LBracket.Position() }

// TypeListItem describes an item of a type list.
//
type TypeListItem struct {
	Type  Node
	Comma Token
}

// Positions implements Node.
func (n *TypeListItem) Position() (r token.Position) { return n.Type.Position() }

// IncDecStmt describes an increment or decrement statemen.
//
type IncDecStmt struct {
	Expression Node
	Op         Token
}

// Positions implements Node.
func (n *IncDecStmt) Position() (r token.Position) { return n.Expression.Position() }
