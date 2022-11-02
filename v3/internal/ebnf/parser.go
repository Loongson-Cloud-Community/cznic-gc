package main

// SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
func (p *parser) parse() (err error) {
	if !(p.packageClause() && p.must(SEMICOLON)) {
		return errorf("%s: syntax error", p.errPosition())
	}

	for p.importDecl() && p.must(SEMICOLON) {
	}
	for p.topLevelDecl() && p.must(SEMICOLON) {
	}

	if p.c() != EOF {
		return errorf("%s: syntax error", p.errPosition())
	}

	return nil
}

// TopLevelDecl = Declaration
// 	| FunctionDecl
// 	| MethodDecl .
func (p *parser) topLevelDecl() bool {
	// TopLevelDecl case CONST, FUNC, TYPE, VAR:
	// Declaration case CONST, TYPE, VAR:
	// FunctionDecl case FUNC:
	// MethodDecl case FUNC:
	switch p.c() {
	case CONST, TYPE, VAR:
		return p.declaration()
	case FUNC:
		return p.functionDeclOrMethodDecl()
	default:
		return false
	}
}

// FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
// MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
func (p *parser) functionDeclOrMethodDecl() bool {
	// FunctionDecl case FUNC:
	// MethodDecl case FUNC:
	if p.c() != FUNC {
		return false
	}

	p.n()
	// FunctionName case IDENT:
	// Receiver case LPAREN:
	switch p.c() {
	case IDENT:
		return p.functionDecl()
	case LPAREN:
		return false
	default:
		return false
	}

}

// FunctionDecl = "func" . FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
func (p *parser) functionDecl() bool {
	// FunctionName case IDENT:
	if p.c() != IDENT {
		return false
	}

	p.n()
	// TypeParameters case LBRACK:
	// Signature case LPAREN:
	switch p.c() {
	case LBRACK:
		return false
	case LPAREN:
		// ok
	default:
		return false
	}

	// Signature case LPAREN:
	switch p.c() {
	case LPAREN:
		if !p.signature() {
			return false
		}
	default:
		return false
	}

	// FunctionBody case LBRACE:
	switch p.c() {
	case LBRACE:
		return p.functionBody()
	default:
		return true
	}
}

// FunctionBody = Block .
func (p *parser) functionBody() bool {
	// FunctionBody case LBRACE:
	if p.c() != LBRACE {
		return false
	}

	return p.block()
}

// Block = "{" StatementList "}" .
func (p *parser) block() bool {
	// Block case LBRACE:
	if p.c() != LBRACE {
		return false
	}

	p.n()
	// StatementList case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR, ε:
	switch p.c() {
	case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR:
		return false
	}

	return p.must(RBRACE)
}

// Signature = Parameters [ Result ] .
func (p *parser) signature() bool {
	// Signature case LPAREN:
	if p.c() != LPAREN {
		return false
	}

	if !p.parameters() {
		return false
	}

	// Result case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		return p.result()
	default:
		return true
	}
}

// Result = Parameters | Type .
func (p *parser) result() bool {
	// Result case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	// Parameters case LPAREN:
	// Type case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case LPAREN:
		return false
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT:
		return p.type1()
	default:
		return false
	}
}

// Parameters = "(" [ ParameterList [ "," ] ] ")" .
func (p *parser) parameters() bool {
	// Parameters case LPAREN:
	if p.c() != LPAREN {
		return false
	}

	p.n()
	// ParameterList case ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		return false
	case RPAREN:
		// ok
	default:
		return false
	}

	switch p.c() {
	case RPAREN:
		p.n()
		return true
	default:
		return false
	}
}

// Declaration =
// 	  ConstDecl
// 	| TypeDecl
// 	| VarDecl .
func (p *parser) declaration() bool {
	// Declaration case CONST, TYPE, VAR:
	switch p.c() {
	case CONST:
		return p.constDecl()
	case TYPE:
		return p.typeDecl()
	case VAR:
		return p.varDecl()
	default:
		return false
	}
}

// ConstDecl = "const" ( ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ) .
func (p *parser) constDecl() bool {
	// ConstDecl case CONST:
	if p.c() != CONST {
		return false
	}

	p.n()
	// ConstSpec case IDENT:
	switch p.c() {
	case IDENT:
		return p.constSpec()
	case LPAREN:
		return false
	default:
		return false
	}
}

// ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
func (p *parser) constSpec() bool {
	// ConstSpec case IDENT:
	if p.c() != IDENT {
		return false
	}

	if !p.identifierList() {
		return false
	}

	// Type case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		return false
	case ASSIGN:
		// ok
	default:
		return false
	}

	if p.c() != ASSIGN {
		return true
	}

	p.n()
	return p.expressionList()
}

// TypeDecl =
// 	  "type" ( TypeSpec
// 	| "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ) .
func (p *parser) typeDecl() bool {
	// TypeDecl case TYPE:
	if p.c() != TYPE {
		return false
	}

	p.n()
	// TypeSpec case IDENT:
	switch p.c() {
	case LPAREN:
		return false
	case IDENT:
		return p.typeSpec()
	default:
		return false
	}
}

// TypeSpec =
// 	  AliasDecl
// 	| TypeDef .
func (p *parser) typeSpec() bool {
	// AliasDecl case IDENT:
	// TypeSpec case IDENT:
	if p.c() != IDENT {
		return false
	}

	p.n()
	// TypeParameters case LBRACK:
	// Type case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case ASSIGN:
		return p.aliasDecl()
	case LBRACK:
		return false
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LPAREN, MAP, MUL, STRUCT:
		return p.type1()
	default:
		return false
	}
}

// AliasDecl = identifier . "=" Type .
func (p *parser) aliasDecl() bool {
	if p.c() != ASSIGN {
		return false
	}

	p.n()
	// Type case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		return p.type1()
	default:
		return false
	}
}

// VarDecl =
// 	  "var" ( VarSpec
// 	| "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ) .
func (p *parser) varDecl() bool {
	// VarDecl case VAR:
	if p.c() != VAR {
		return false
	}

	p.n()
	// VarSpec case IDENT:
	switch p.c() {
	case LPAREN:
		return false
	case IDENT:
		return p.varSpec()
	default:
		return false
	}
}

// VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
func (p *parser) varSpec() bool {
	// VarSpec case IDENT:
	if p.c() != IDENT {
		return false
	}

	if !p.identifierList() {
		return false
	}

	// Type case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case ASSIGN:
		p.n()
		return p.expressionList()
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		return p.type1()
	default:
		return false
	}
}

// ExpressionList = Expression { "," Expression } .
func (p *parser) expressionList() bool {
	// ExpressionList case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
	for {
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if !p.expression() {
				return false
			}

			if p.c() != COMMA {
				return true
			}

			p.n()
		default:
			return false
		}
	}
}

// Expression = LogicalAndExpression { "||" LogicalAndExpression } .
func (p *parser) expression() bool {
	// Expression case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
	for {
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if !p.logicalAndExpression() {
				return false
			}

			if p.c() != LOR {
				return true
			}

			p.n()
		default:
			return false
		}
	}
}

// LogicalAndExpression = RelationalExpression { "&&" RelationalExpression } .
func (p *parser) logicalAndExpression() bool {
	// LogicalAndExpression case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
	for {
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if !p.relationalExpression() {
				return false
			}

			if p.c() != LAND {
				return true
			}

			p.n()
		default:
			return false
		}
	}
}

// RelationalExpression = AdditiveExpression { RelOp AdditiveExpression } .
func (p *parser) relationalExpression() bool {
	// RelationalExpression case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
	for {
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if !p.additiveExpression() {
				return false
			}

			if !p.relOp() {
				return true
			}

			p.n()
		default:
			return false
		}
	}
}

// RelOp = "==" | "!=" | "<" | "<=" | ">" | ">=" .
func (p *parser) relOp() bool {
	//	RelOp case EQL, GEQ, GTR, LEQ, LSS, NEQ:
	switch p.c() {
	case EQL, GEQ, GTR, LEQ, LSS, NEQ:
		p.n()
		return true
	default:
		return false
	}
}

// AdditiveExpression = MultiplicativeExpression { AddOp MultiplicativeExpression } .
func (p *parser) additiveExpression() bool {
	// AdditiveExpression case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
	for {
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if !p.multiplicativeExpression() {
				return false
			}

			if !p.addOp() {
				return true
			}

			p.n()
		default:
			return false
		}
	}
}

// AddOp = "+" | "-" | "|" | "^" .
func (p *parser) addOp() bool {
	// AddOp case ADD, OR, SUB, XOR:
	switch p.c() {
	case ADD, OR, SUB, XOR:
		p.n()
		return true
	default:
		return false
	}
}

// MultiplicativeExpression = UnaryExpr { MulOp UnaryExpr } .
func (p *parser) multiplicativeExpression() bool {
	// MultiplicativeExpression case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
	for {
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if !p.unaryExpr() {
				return false
			}

			if !p.mulOp() {
				return true
			}

			p.n()
		default:
			return false
		}
	}
}

// MulOp = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
func (p *parser) mulOp() bool {
	// MulOp case AND, AND_NOT, MUL, QUO, REM, SHL, SHR:
	switch p.c() {
	case AND, AND_NOT, MUL, QUO, REM, SHL, SHR:
		p.n()
		return true
	default:
		return false
	}
}

// UnaryExpr = PrimaryExpr | UnaryOp UnaryExpr .
func (p *parser) unaryExpr() bool {
	// UnaryExpr case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
	// PrimaryExpr case ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT:
	// UnaryOp case ADD, AND, ARROW, MUL, NOT, SUB, XOR:
	switch p.c() {
	case CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT:
		return p.primaryExpr()
	case ADD, AND, NOT, SUB, XOR:
		p.n()
		return p.unaryExpr()
	case ARROW:
		return false
	case MUL:
		return false
	default:
		return false
	}

}

// PrimaryExpr = ( Operand | Conversion | MethodExpr ) { Selector | Index | Slice | TypeAssertion | Arguments } .
func (p *parser) primaryExpr() bool {
	// PrimaryExpr case ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT:
	// Operand case CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, LPAREN, MAP, STRING, STRUCT:
	// Conversion case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	// MethodExpr case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case CHAR, FLOAT, IMAG, INT, STRING:
		if !p.operand() {
			return false
		}
	case ARROW, CHAN, INTERFACE, MUL:
		return false
	case FUNC, LBRACK, LPAREN, MAP, STRUCT:
		return false
	case IDENT:
		// Operand = Literal | OperandName [ TypeArgs ] | "(" Expression ")" .
		// Conversion = Type "(" Expression [ "," ] ")" .
		// MethodExpr = ReceiverType "." MethodName .
		switch p.peek(1).tok {
		case PERIOD:
			return false
		case LPAREN:
			return false
		case LBRACE:
			return false
		case LBRACK:
			return false
		default:
			p.n()
			return true
		}
	default:
		return false
	}

	for {
		// Selector case PERIOD:
		// Index case LBRACK:
		// Slice case LBRACK:
		// TypeAssertion case PERIOD:
		// Arguments case LPAREN:
		switch p.c() {
		case PERIOD:
			return false
		case LBRACK:
			return false
		case LPAREN:
			return false
		default:
			return true
		}
	}
}

// Operand = Literal | OperandName [ TypeArgs ] | "(" Expression ")" .
func (p *parser) operand() bool {
	// Operand case CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, LPAREN, MAP, STRING, STRUCT:
	// Literal case CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, MAP, STRING, STRUCT:
	// OperandName case IDENT:
	switch p.c() {
	case CHAR, FLOAT, FUNC, IMAG, INT, LBRACK, MAP, STRING, STRUCT:
		return p.literal()
	case IDENT:
		switch p.peek(1).tok {
		case PERIOD:
			return false
		case LBRACK:
			return false
		case LBRACE:
			return false
		default:
			p.n()
			return true
		}
	case LPAREN:
		return false
	default:
		return false
	}
}

// Literal = BasicLit | CompositeLit | FunctionLit .
func (p *parser) literal() bool {
	// Literal case CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, MAP, STRING, STRUCT:
	// BasicLit case CHAR, FLOAT, IMAG, INT, STRING:
	// CompositeLit case IDENT, LBRACK, MAP, STRUCT:
	// FunctionLit case FUNC:
	switch p.c() {
	case CHAR, FLOAT, IMAG, INT, STRING:
		return p.basicLit()
	case IDENT, LBRACK, MAP, STRUCT:
		return false
	case FUNC:
		return false
	default:
		return false
	}
}

// BasicLit = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
func (p *parser) basicLit() bool {
	// BasicLit case CHAR, FLOAT, IMAG, INT, STRING:
	switch p.c() {
	case CHAR, FLOAT, IMAG, INT, STRING:
		p.n()
		return true
	default:
		return false
	}
}

// Type =
// 	  TypeName [ TypeArgs ]
// 	| TypeLit
// 	| "(" Type ")" .
func (p *parser) type1() bool {
	// Type case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	// TypeName case IDENT:
	// TypeLit case ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT:
	switch p.c() {
	case IDENT:
		if !p.typeName() {
			return false
		}

		// TypeArgs case LBRACK:
		if p.c() == LBRACK {
			return false
		}

		return true
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT:
		return p.typeLit()
	case LPAREN:
		return false
	default:
		return false
	}
}

// TypeLit = ArrayType | StructType | PointerType | FunctionType | InterfaceType | SliceType | MapType | ChannelType .
func (p *parser) typeLit() bool {
	// TypeLit case ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT:
	// ArrayType case LBRACK:
	// StructType case STRUCT:
	// PointerType case MUL:
	// FunctionType case FUNC:
	// InterfaceType case INTERFACE:
	// SliceType case LBRACK:
	// MapType case MAP:
	// ChannelType case ARROW, CHAN:
	switch p.c() {
	case LBRACK:
		return false
	case STRUCT:
		return p.structType()
	case MUL:
		return p.pointerType()
	case FUNC:
		return false
	case INTERFACE:
		return p.interfaceType()
	case MAP:
		return false
	case ARROW, CHAN:
		return false
	default:
		return false
	}
}

// StructType = "struct" "{" [ FieldDecl { ";" FieldDecl } [ ";" ] ] "}" .
func (p *parser) structType() bool {
	// StructType case STRUCT:
	if p.c() != STRUCT {
		return false
	}

	p.n()
	if !p.must(LBRACE) {
		return false
	}

	// FieldDecl case IDENT, MUL:
more:
	switch p.c() {
	case IDENT, MUL:
		if !p.fieldDecl() {
			return false
		}

		switch p.c() {
		case SEMICOLON:
			p.n()
			switch p.c() {
			case RBRACE:
				p.n()
				return true
			case IDENT, MUL:
				goto more
			default:
				return false
			}
		case RBRACE:
			p.n()
			return true
		default:
			return false
		}
	case RBRACE:
		// ok
	default:
		return false
	}

	return p.must(RBRACE)

}

// FieldDecl = ( IdentifierList Type | EmbeddedField ) [ Tag ] .
func (p *parser) fieldDecl() bool {
	// FieldDecl case IDENT, MUL:
	// IdentifierList case IDENT:
	// EmbeddedField case IDENT, MUL:
	switch p.c() {
	case IDENT:
		switch p.peek(1).tok {
		case RBRACE:
			return p.embeddedField()
		default:
			return false
		}
	case MUL:
		return false
	default:
		return false
	}
}

// EmbeddedField = [ "*" ] TypeName [ TypeArgs ] .
func (p *parser) embeddedField() bool {
	// EmbeddedField case IDENT, MUL:
	switch p.c() {
	case IDENT:
		// ok
	case MUL:
		p.n()
	default:
		return false
	}

	// TypeName case IDENT:
	if p.c() != IDENT {
		return false
	}

	if !p.typeName() {
		return false
	}

	// TypeArgs case LBRACK:
	switch p.c() {
	case LBRACK:
		return false
	default:
		return true
	}
}

// InterfaceType = "interface" "{" [ InterfaceElem { ";" InterfaceElem } [ ";" ] ] "}" .
func (p *parser) interfaceType() bool {
	// InterfaceType case INTERFACE:
	if p.c() != INTERFACE {
		return false
	}

	p.n()
	if !p.must(LBRACE) {
		return false
	}

	// InterfaceElem case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
	switch p.c() {
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
		return false
	case RBRACE:
		// ok
	default:
		return false
	}

	return p.must(RBRACE)
}

// PointerType = "*" BaseType .
func (p *parser) pointerType() bool {
	// PointerType case MUL:
	if p.c() != MUL {
		return false
	}

	p.n()
	return p.baseType()
}

// BaseType = Type .
func (p *parser) baseType() bool {
	// BaseType case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
	switch p.c() {
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
		return p.type1()
	default:
		return false
	}
}

// TypeName =
// 	  QualifiedIdent
// 	| identifier .
func (p *parser) typeName() bool {
	// TypeName case IDENT:
	if p.qualifiedIdent() {
		return true
	}

	if p.c() == IDENT {
		p.n()
		return true
	}

	return false
}

// QualifiedIdent = PackageName "." identifier .
func (p *parser) qualifiedIdent() bool {
	// QualifiedIdent case IDENT:
	if p.c() != IDENT || p.peek(1).tok != PERIOD {
		return false
	}

	if !p.packageName() {
		return false
	}

	if p.c() != PERIOD {
		return false
	}

	p.n()
	if p.c() != IDENT {
		return false
	}

	p.n()
	return true
}

// IdentifierList = identifier { "," identifier } .
func (p *parser) identifierList() bool {
	// IdentifierList case IDENT:
	if p.c() != IDENT {
		return false
	}

	p.n()
	for p.c() == COMMA {
		p.n()
		if !p.must(IDENT) {
			return false
		}
	}
	return true
}

// ImportDecl = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
func (p *parser) importDecl() bool {
	if p.c() != IMPORT {
		return false
	}

	p.n()
	// ImportSpec case IDENT, PERIOD, STRING:
	switch p.c() {
	case LPAREN:
		return false
	case IDENT, PERIOD, STRING:
		return p.importSpec()
	default:
		return false
	}
}

// ImportSpec = [ "." | PackageName ] ImportPath .
func (p *parser) importSpec() bool {
	switch p.c() {
	case IDENT, PERIOD, STRING:
		p.n()
		return true
	default:
		return false
	}
}

// PackageClause = "package" PackageName .
func (p *parser) packageClause() bool {
	return p.must(PACKAGE) && p.packageName()
}

// PackageName = identifier .
func (p *parser) packageName() bool { return p.must(IDENT) }
