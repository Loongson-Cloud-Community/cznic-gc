// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

var (
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
	switch p.ch() {
	//       SourceFile
	case PACKAGE:
		r = &SourceFile{PackageClause: &PackageClause{Package: p.must(PACKAGE), PackageName: p.must(IDENTIFIER), Semicolon: p.must(';')}, ImportDecls: p.importDecls()}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
	}
	if err := p.s.errs.Err(); err != nil {
		return nil, err
	}

	if cfg.Accept != nil && !cfg.Accept(r) {
		p.err(errorf("rejected"))
		return nil, p.Err()
	}

	r.TopLevelDecls = p.topLevelDecls()
	if err := p.Err(); err != nil {
		return nil, err
	}

	switch p.ch() {
	//              eof
	case EOF:
		r.EOF = p.shift()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		return nil, p.Err()
	}

	return r, nil
}

// ImportDecl = "import" ImportSpec
// 	| "import" "(" { ImportSpec ";" } ")" .
func (p *parser) importDecls() (r []*ImportDecl) {
	for {
		switch p.ch() {
		//       ImportDecl
		case IMPORT:
			im := p.shift()
			var n *ImportDecl
			switch p.ch() {
			case '(':
				n = &ImportDecl{Import: im, LParen: p.shift(), ImportSpecs: p.importSpecs(), RParen: p.must(')')}
			//       ImportSpec
			case '.', IDENTIFIER, STRING_LIT:
				n = &ImportDecl{Import: im, ImportSpecs: []*ImportSpec{p.importSpec()}}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
			n.Semicolon = p.must(';')
			r = append(r, n)
		default:
			return r
		}
	}
}

func (p *parser) importSpecs() (r []*ImportSpec) {
	for {
		switch p.ch() {
		//       ImportSpec
		case '.', IDENTIFIER, STRING_LIT:
			n := p.importSpec()
			n.Semicolon = p.must(';')
			r = append(r, n)
		default:
			return r
		}
	}
}

// ImportSpec = "." ImportPath
// 	| PackageName ImportPath
// 	| ImportPath .
func (p *parser) importSpec() (r *ImportSpec) {
	switch p.ch() {
	case '.':
		return &ImportSpec{Qualifier: p.shift(), ImportPath: p.must(STRING_LIT)}
	//      PackageName
	case IDENTIFIER:
		return &ImportSpec{Qualifier: p.shift(), ImportPath: p.must(STRING_LIT)}
	//       ImportPath
	case STRING_LIT:
		return &ImportSpec{ImportPath: p.shift()}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// { TopLevelDecl ";" }
func (p *parser) topLevelDecls() (r []Node) {
	//     TopLevelDecl case CONST, FUNC, TYPE, VAR:
	for {
		switch p.ch() {
		case CONST:
			r = append(r, p.constDecl(true))
		case FUNC:
			fn := p.shift()
			switch p.ch() {
			case '(':
				r = append(r, p.methodDecl(fn))
			case IDENTIFIER:
				r = append(r, p.functionDecl(fn))
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case TYPE:
			r = append(r, p.typeDecl(true))
		case VAR:
			r = append(r, p.varDecl(true))
		default:
			return r
		}
	}
}

// MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
func (p *parser) methodDecl(fn Token) (r *MethodDecl) {
	r = &MethodDecl{Func: fn, Receiver: p.parameters(), MethodName: p.must(IDENTIFIER), Signature: p.signature()}
	if p.ch() == '{' {
		r.FunctionBody = &Block{LBrace: p.must('{'), StatementList: p.statementList(), RBrace: p.must('}')}
	}
	r.Semicolon = p.must(';')
	return r
}

// ConstDecl = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
func (p *parser) constDecl(semi bool) (r *ConstDecl) {
	switch p.ch() {
	//        ConstDecl
	case CONST:
		c := p.shift()
		switch p.ch() {
		case '(':
			r = &ConstDecl{Const: c, LParen: p.shift(), ConstSpecs: p.constSpecs(), RParen: p.must(')')}
		//        ConstSpec
		case IDENTIFIER:
			r = &ConstDecl{Const: c, ConstSpecs: []*ConstSpec{p.constSpec(false)}}
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
	r.Semicolon = p.semi(semi)
	return r
}

// ConstSpec = IdentifierList "=" ExpressionList
// 	| IdentifierList Type "=" ExpressionList
// 	| IdentifierList .
func (p *parser) constSpec(semi bool) (r *ConstSpec) {
	switch p.ch() {
	//        ConstSpec
	case IDENTIFIER:
		r = &ConstSpec{IdentifierList: p.identifierList(notok)}
		switch p.ch() {
		case '=':
			r.Eq = p.shift()
			r.ExpressionList = p.expressionList(notok)
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
		r.Semicolon = p.semi(semi)
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

func (p *parser) constSpecs() (r []*ConstSpec) {
	//        ConstSpec case IDENTIFIER:
	for p.ch() == IDENTIFIER {
		r = append(r, p.constSpec(true))
	}
	return r
}

func (p *parser) expressionList(id Token) (r []*ExpressionListItem) {
	for {
		var n *ExpressionListItem
		switch {
		case id.IsValid():
			n = &ExpressionListItem{Expression: p.expression(id)}
			id = notok
		default:
			switch p.ch() {
			//       Expression
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

func (p *parser) expression2(in Node) (r Node) {
	r = in
	for {
		switch p.ch() {
		case '%', '&', '*', '+', '-', '/', '<', '>', '^', '|', AND_NOT, EQ, GE, LAND, LE, LOR, NE, SHL, SHR:
			op := p.shift()
			rhs := p.expression(notok)
			switch x := r.(type) {
			case *BinaryExpression:
				panic(todo(""))
				use(x)
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			default:
				r = &BinaryExpression{A: r, Op: op, B: rhs}
			}
		default:
			return r
		}
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
			// case '(':
			// 	r = &Conversion{Type: t, LParen: p.shift(), Expression: p.expression(notok), Comma: p.opt(','), RParen: p.must(')')}
			default:
				use(t)
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
				r = &CompositeLit{LiteralType: &QualifiedIdent{Ident: id}, LiteralValue: p.literalValue2()}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		// case MAP:
		// 	r = &CompositeLit{LiteralType: p.type1(), LiteralValue: p.literalValue1()}
		case FUNC:
			f := p.shift()
			sig := p.signature()
			switch p.ch() {
			case '{':
				r = p.functionLit(f, sig)
			default:
				use(f, sig)
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
			// case '(':
			// 	r = &TypeAssertion{PrimaryExpr: r, Dot: dot, LParen: p.shift(), Type: p.type1(), RParen: p.must(')')}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		case '(':
			r = p.arguments(r)
		case ';', '}', ',', ':', ')', ']', DEFINE, '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN, '<', '>', EQ, GE, LE, NE, body, '%', '&', '*', '+', '-', '/', '^', '|', AND_NOT, LAND, LOR, SHL, SHR:
			return r
		// case '{':
		// 	r = &CompositeLit{LiteralType: &Type{Type: r}, LiteralValue: p.literalValue2()}
		case '[':
			r = &Index{PrimaryExpr: r, LBracket: p.shift(), Expression: p.expression(notok), RBracket: p.must(']')}
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
}

// FunctionLit = "func" Signature lbrace StatementList "#fixlbr" "}" .
func (p *parser) functionLit(f Token, sig *Signature) (r *FunctionLit) {
	r = &FunctionLit{Func: f, Signature: sig}
	var lbr bool
	r.FunctionBody = &Block{LBrace: p.lbrace(&lbr), StatementList: p.statementList()}
	p.fixlbr(lbr)
	r.FunctionBody.RBrace = p.must('}')
	return r
}

// (expr) or (type)
func (p *parser) parenExprOrType() (r Node) {
	lparen := p.must('(')
	switch p.ch() {
	// case '*':
	// 	switch x := p.exprOrType().(type) {
	// 	default:
	// 		p.err(errorf("TODO %T", x))
	// 		p.shift()
	// 		return r
	// 	}
	default:
		use(lparen)
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// LiteralValue2 = "{" "}"
// 	| "{" ElementList [ "," ] "}" .
func (p *parser) literalValue2() (r *LiteralValue) {
	r = &LiteralValue{LBrace: p.must('{')}
	switch p.ch() {
	//      ElementList
	case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT, body:
		r.ElementList = p.keyedElements()
	// case '}':
	// 	// ok
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

// LiteralValue1 = lbrace ElementList [ "," ] "#fixlbr" "}"
// 	| lbrace "#fixlbr" "}" .
func (p *parser) literalValue1() (r *LiteralValue) {
	var lbr bool
	r = &LiteralValue{LBrace: p.lbrace(&lbr)}
	switch p.ch() {
	//      ElementList
	case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT, body:
		r.ElementList = p.keyedElements()
	// case '}':
	// 	// ok
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

// Arguments = "(" ")"
// 	| "(" ExpressionList [ "..." ] [ "," ] ")"
// 	| "(" Type "," ExpressionList [ "..." ] [ "," ] ")"
// 	| "(" Type [ "..." ] [ "," ] ")" .
func (p *parser) arguments(primaryExpr Node) (r *Arguments) {
	switch p.ch() {
	//        Arguments
	case '(':
		//   ExpressionList case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		r = &Arguments{PrimaryExpr: primaryExpr, LParen: p.shift()}
		switch p.ch() {
		case '!', '&', '+', '-', '^', FLOAT_LIT, IMAG_LIT, INT_LIT, RUNE_LIT, STRING_LIT:
			// ExpressionList
			r.ExpressionList = p.expressionList(notok)
		// case '*', ARROW:
		// 	// ExpressionList or Type
		// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		// 	p.shift()
		// 	return r
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
		// case '(':
		// 	// ExpressionList or Type
		// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		// 	p.shift()
		// 	return r
		case '[', CHAN, FUNC, INTERFACE, MAP, STRUCT:
			// Type or a Literal
			t := p.type1()
			switch p.ch() {
			// case ',':
			// 	r.Type = t
			// 	r.Comma = p.shift()
			// 	switch p.ch() {
			// 	//   ExpressionList
			// 	case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
			// 		r.ExpressionList = p.expressionList(notok)
			// 	default:
			// 		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			// 		p.shift()
			// 		return r
			// 	}
			// case ')':
			// 	// ok
			// case '{':
			// 	expr := &CompositeLit{LiteralType: t, LiteralValue: p.literalValue1()}
			// 	switch p.ch() {
			// 	default:
			// 		use(expr)
			// 		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			// 		p.shift()
			// 		return r
			// 	}
			// case '(':
			// 	expr := &Conversion{Type: t, LParen: p.shift(), Expression: p.expression(notok), RParen: p.must(')')}
			// 	switch p.ch() {
			// 	default:
			// 		use(expr)
			// 		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			// 		p.shift()
			// 		return r
			// 	}
			default:
				use(t)
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
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

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
	// case '*':
	// 	star := p.shift()
	// 	switch p.ch() {
	// 	default:
	// 		use(star)
	// 		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	// 		p.shift()
	// 		return r
	// 	}
	// case ARROW:
	// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	// 	p.shift()
	// 	return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

func (p *parser) semi(enabled bool) (r Token) {
	if enabled {
		switch p.ch() {
		case ';':
			r = p.shift()
		case ')', '}':
			// ok
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
		}
	}
	return r
}

// VarDecl = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
func (p *parser) varDecl(semi bool) (r *VarDecl) {
	switch p.ch() {
	//          VarDecl
	case VAR:
		v := p.shift()
		switch p.ch() {
		case '(':
			r = &VarDecl{Var: v, LParen: p.shift(), VarSpecs: p.varSpecs(), RParen: p.must(')')}
		//          VarSpec
		case IDENTIFIER:
			r = &VarDecl{Var: v, VarSpecs: []*VarSpec{p.varSpec()}}
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
	r.Semicolon = p.semi(semi)
	return r
}

func (p *parser) varSpecs() (r []*VarSpec) {
	for p.ch() == IDENTIFIER {
		n := p.varSpec()
		n.Semicolon = p.semi(true)
		r = append(r, n)
	}
	return r
}

// VarSpec = IdentifierList Type [ "=" ExpressionList ]
// 	| IdentifierList "=" ExpressionList .
func (p *parser) varSpec() (r *VarSpec) {
	switch p.ch() {
	//          VarSpec
	case IDENTIFIER:
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
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

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

// TypeDecl = "type" TypeSpec
// 	| "type" "(" { TypeSpec ";" } ")" .
func (p *parser) typeDecl(semi bool) (r *TypeDecl) {
	switch p.ch() {
	//         TypeDecl
	case TYPE:
		typ := p.shift()
		switch p.ch() {
		// case '(':
		// 	r = &TypeDecl{Type: typ, LParen: p.shift(), TypeSpecs: p.typeSpecs(), RParen: p.must(')')}
		//         TypeSpec
		case IDENTIFIER:
			r = &TypeDecl{Type: typ, TypeSpecs: []Node{p.typeSpec(false)}}
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
	r.Semicolon = p.semi(semi)
	return r
}

// TypeSpec = AliasDecl | TypeDef .
// TypeDef = identifier [ TypeParameters ] Type .
// AliasDecl = identifier "=" Type .
func (p *parser) typeSpec(semi bool) (r Node) {
	switch p.ch() {
	//         TypeSpec
	case IDENTIFIER:
		id := p.shift()
		//   TypeParameters case '[':
		//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		switch p.ch() {
		case '[':
			switch x := p.typeOrTypeParamaters().(type) {
			case *TypeParameters:
				return &TypeDef{Ident: id, TypeParameters: x, Type: p.type1()}
			default:
				return &TypeDef{Ident: id, Type: x}
			}
		// case '=':
		// 	return &AliasDecl{Ident: id, Eq: p.shift(), Type: p.type1()}
		case '(', '*', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
			n := &TypeDef{Ident: id, Type: p.type1()}
			n.Semicolon = p.semi(semi)
			return n
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

// p.s.Tok.Ch == '['
func (p *parser) typeOrTypeParamaters() (r Node) {
	//   TypeParameters case '[':
	//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	switch p.ch() {
	case '[':
		lbracket := p.shift()
		// '[' .
		switch p.ch() {
		// case IDENTIFIER:
		// 	id := p.shift()
		// 	// '[' IDENTIFIER  .
		// 	// TypeParameters = "[" . TypeParamList [ "," ] "]" .
		// 	// TypeParamList = . TypeParamDecl { "," TypeParamDecl } .
		// 	// TypeParamDecl = . IdentifierList TypeConstraint .
		// 	// ArrayType = "[" . ArrayLength "]" ElementType .
		// 	switch p.ch() {
		// 	case IDENTIFIER:
		// 		// '[' IDENTIFIER  . IDENTIFIER -> TypeParameters
		// 		return p.typeParameters(lbracket, id)
		// 	default:
		// 		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		// 		p.shift()
		// 		return r
		// 	}
		// case ']':
		// 	return &SliceType{LBracket: lbracket, RBracket: p.shift(), ElementType: p.type1()}
		default:
			use(lbracket)
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	// case '(', '*', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	// 	p.shift()
	// 	return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// FunctionDecl = "func" FunctionName TypeParameters Signature [ FunctionBody ]
// 	| "func" FunctionName Signature [ FunctionBody ] .
func (p *parser) functionDecl(fn Token) (r *FunctionDecl) {
	r = &FunctionDecl{Func: fn, FunctionName: p.must(IDENTIFIER)}
	switch p.ch() {
	case '(':
		r.Signature = p.signature()
	// case '[':
	// 	r.TypeParameters = p.typeParameters(notok, notok)
	// 	r.Signature = p.signature()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	if p.ch() == '{' {
		r.FunctionBody = &Block{LBrace: p.must('{'), StatementList: p.statementList(), RBrace: p.must('}')}
	}
	r.Semicolon = p.must(';')
	return r
}

// StatementList = { [ Statement ] ";" } [ Statement ] .
func (p *parser) statementList() (r []Node) {
	//        Statement case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, BREAK, CHAN, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT_LIT, FOR, FUNC, GO, GOTO, IDENTIFIER, IF, IMAG_LIT, INTERFACE, INT_LIT, MAP, RETURN, RUNE_LIT, SELECT, STRING_LIT, STRUCT, SWITCH, TYPE, VAR:
	for {
		switch p.ch() {
		case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, BREAK, CHAN, CONTINUE, FALLTHROUGH, FLOAT_LIT, FUNC, GOTO, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, SELECT, STRING_LIT, STRUCT:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		// case CONST:
		// 	n = &Statement{Statement: p.constDecl()}
		case DEFER:
			r = append(r, &DeferStmt{Defer: p.must(DEFER), Expression: p.expression(notok), Semicolon: p.semi(true)})
		case FOR:
			r = append(r, p.forStmt())
			// case GO:
			// 	n = &Statement{Statement: &GoStmt{Go: p.shift(), Expression: p.expression(notok)}}
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			case DEFINE:
				// RecvStmt = IdentifierList ":=" RecvExpr
				// ShortVarDecl = IdentifierList ":=" ExpressionList .
				r = append(r, &ShortVarDecl{IdentifierList: p.identifierList(id), Define: p.shift(), ExpressionList: p.expressionList(notok), Semicolon: p.semi(true)})
			case '.', '(':
				// MethodExpr = ReceiverType "." MethodName .
				// QualifiedIdent = PackageName "." identifier .
				expr := p.expression(id)
				switch p.ch() {
				case ';':
					r = append(r, &ExpressionStatement{Expression: expr, Semicolon: p.shift()})
				case '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
					r = append(r, &Assignment{LExpressionList: []*ExpressionListItem{{Expression: expr}}, AssOp: p.shift(), RExpressionList: p.expressionList(notok), Semicolon: p.semi(true)})
				default:
					p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
					p.shift()
					return r
				}
			case '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
				r = append(r, &Assignment{LExpressionList: []*ExpressionListItem{{Expression: id}}, AssOp: p.shift(), RExpressionList: p.expressionList(notok), Semicolon: p.semi(true)})
			case ',', '[':
				el := p.expressionList(id)
				switch p.ch() {
				case DEFINE:
					il := p.exprList2IdList(p.s.Tok, el)
					r = append(r, &ShortVarDecl{IdentifierList: il, Define: p.shift(), ExpressionList: p.expressionList(notok), Semicolon: p.semi(true)})
				// case '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
				// 	n = &Statement{Statement: &Assignment{LExpressionList: el, AssOp: p.shift(), RExpressionList: p.expressionList(notok)}}
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
		case IF:
			r = append(r, p.ifStmt())
		case RETURN:
			r = append(r, p.returnStmt())
		case SWITCH:
			r = append(r, p.switchStmt())
		// case TYPE:
		// 	n = &Statement{Statement: p.typeDecl()}
		case VAR:
			r = append(r, p.varDecl(true))
		case '}':
			return r
		default:
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	}
}

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
	// case ';':
	// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	// 	p.shift()
	// 	return r
	// case RANGE:
	// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	// 	p.shift()
	// 	return r
	case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		el := p.expressionList(notok)
		switch p.ch() {
		case DEFINE:
			def := p.shift()
			switch p.ch() {
			case RANGE:
				r.RangeClause = &RangeClause{ExpressionList: el, Assign: def, Range: p.shift(), Expression: p.expression(notok)}
			// //       Expression
			// case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
			// 	ss := &ShortVarDecl{IdentifierList: p.exprList2IdList(def, el), Define: def, ExpressionList: p.expressionList(notok)}
			// 	r.ForClause = p.forClause(ss)
			default:
				use(def)
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		default:
			use(el)
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

// SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
// ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] body { ExprCaseClause } "}" .
// TypeSwitchStmt = "switch" [ SimpleStmt ";" ] TypeSwitchGuard body { TypeCaseClause } "}" .
func (p *parser) switchStmt() (r Node) {
	sw := p.must(SWITCH)
	switch p.ch() {
	case IDENTIFIER:
		ss := p.simpleStmt()
		switch p.ch() {
		default:
			use(ss)
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	default:
		use(sw)
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

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
	// case '!', '&', '(', '*', '+', '-', '[', '^', CHAN, FLOAT_LIT, FUNC, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
	// 	expr := p.expression(notok)
	// 	switch p.ch() {
	// 	case body:
	// 		r.Expression = expr
	// 	default:
	// 		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	// 		p.shift()
	// 		return r
	// 	}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
	r.Block = p.loopBody()
	r.Semicolon2 = p.semi(true)
	return r
}

func (p *parser) loopBody() (r *Block) {
	r = &Block{LBrace: p.must(body), StatementList: p.statementList(), RBrace: p.must('}')}
	r.LBrace.Ch = '{'
	return r
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
			use(el)
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

func (p *parser) returnStmt() (r *ReturnStmt) {
	switch p.ch() {
	//       ReturnStmt
	case RETURN:
		r = &ReturnStmt{Return: p.shift()}
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
		r.Semicolon = p.semi(true)
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

// Signature = Parameters [ Result ] .
func (p *parser) signature() (r *Signature) {
	switch p.ch() {
	//       Parameters
	case '(':
		r = &Signature{Parameters: p.parameters()}
		switch p.ch() {
		//           Result
		case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
			r.Result = p.result()
		}
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
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

// Parameters = "(" [ ParameterList [ "," ] ] ")" .
func (p *parser) parameters() (r *Parameters) {
	switch p.ch() {
	//       Parameters
	case '(':
		r = &Parameters{LParen: p.shift()}
		switch p.ch() {
		case '(', '*', '[', ARROW, CHAN, ELLIPSIS, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
			r.ParameterList = p.parameterList()
		}
		r.Comma = p.opt(',')
		r.RParen = p.must(')')
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

func (p *parser) parameterList() (r []*ParameterDecl) {
	for {
		var n *ParameterDecl
		//    ParameterList case '(', '*', '[', ARROW, CHAN, ELLIPSIS, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
		switch p.ch() {
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			// case ELLIPSIS:
			// 	n = &ParameterDecl{IdentifierList: []*IdentifierListItem{{Ident: id}}, ELLIPSIS: p.shift(), Type: p.type1()}
			case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
				n = &ParameterDecl{IdentifierList: p.identifierList(id), Type: p.type1()}
			case ',', ')':
				n = &ParameterDecl{Type: &QualifiedIdent{Ident: id}}
			// case '.':
			// 	n = &ParameterDecl{Type: &Type{Type: &QualifiedIdent{PackageName: id, Dot: p.shift(), Ident: p.must(IDENTIFIER)}}}
			default:
				p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
				p.shift()
				return r
			}
		// case ELLIPSIS:
		// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		// 	p.shift()
		// 	return r
		// case '(', '*', '[', ARROW, CHAN, FUNC, INTERFACE, MAP, STRUCT:
		// 	n = &ParameterDecl{Type: p.type1()}
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

// Type = TypeName
// 	| TypeLit
// 	| "(" Type ")" .
func (p *parser) type1() (r Node) {
	//             Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
	switch p.ch() {
	// case '(':
	// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	// 	p.shift()
	// 	return r
	case '*':
		return &PointerType{Star: p.shift(), BaseType: p.type1()}
	case '[':
		lbracket := p.shift()
		switch p.ch() {
		case ']':
			return &SliceType{LBracket: lbracket, RBracket: p.shift(), ElementType: p.type1()}
		// case ELLIPSIS:
		// 	return &Type{Type: &ArrayType{LBracket: lbracket, ArrayLength: p.shift(), RBracket: p.must(']'), ElementType: p.type1()}}
		// //       Expression
		// case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, FLOAT_LIT, FUNC, IDENTIFIER, IMAG_LIT, INTERFACE, INT_LIT, MAP, RUNE_LIT, STRING_LIT, STRUCT:
		// 	return &Type{Type: &ArrayType{LBracket: lbracket, ArrayLength: p.expression(notok), RBracket: p.must(']'), ElementType: p.type1()}}
		default:
			use(lbracket)
			p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			p.shift()
			return r
		}
	// case ARROW:
	// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
	// 	p.shift()
	// 	return r
	// case CHAN:
	// 	return &Type{Type: p.channelType(notok)}
	// case FUNC:
	// 	return &Type{Type: p.functionType()}
	case IDENTIFIER:
		return p.typeName()
	case INTERFACE:
		return p.interfaceType()
	// case MAP:
	// 	return &Type{Type: p.mapType()}
	case STRUCT:
		return p.structType()
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

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
		// case '(', '*', '[', '~', ARROW, CHAN, FUNC, INTERFACE, MAP, STRUCT:
		// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		// 	p.shift()
		// 	return r
		case IDENTIFIER:
			id := p.shift()
			switch p.ch() {
			case '(':
				n := &MethodElem{MethodName: id, Signature: p.signature(), Semicolon: p.opt(';')}
				r.InterfaceElems = append(r.InterfaceElems, n)
			default:
				use(id)
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
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
		//TODO r.TypeArgs = p.typeArgs()
	}
	return r
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

// StructType = "struct" lbrace "#fixlbr" "}"
// 	| "struct" lbrace FieldDecl { ";" FieldDecl } [ ";" ] "#fixlbr" "}" .
func (p *parser) structType() (r *StructType) {
	switch p.ch() {
	//       StructType
	case STRUCT:
		//        FieldDecl case '*', IDENTIFIER:
		var lbr bool
		r = &StructType{Struct: p.shift(), LBrace: p.lbrace(&lbr)}
		// FieldDecl = IdentifierList Type [ Tag ]
		// 	| EmbeddedField [ Tag ] .
		for {
			var n *FieldDecl
			switch p.ch() {
			// case '*':
			// 	p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
			// 	p.shift()
			// 	return r
			case IDENTIFIER:
				id := p.shift()
				switch p.ch() {
				//             Type
				case '(', '*', '[', ARROW, CHAN, FUNC, IDENTIFIER, INTERFACE, MAP, STRUCT:
					n = &FieldDecl{IdentifierList: p.identifierList(id), Type: p.type1()}
				case ',':
					n = &FieldDecl{IdentifierList: p.identifierList(id), Type: p.type1()}
				// case ';':
				// 	n = &FieldDecl{EmbeddedField: &EmbeddedField{TypeName: &QualifiedIdent{Ident: id}}}
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
			n.Semicolon = p.opt(';')
			r.FieldDecls = append(r.FieldDecls, n)
		}
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}

func (p *parser) lbrace(lbr *bool) (r Token) {
	switch p.ch() {
	case '{':
		return p.shift()
	case body:
		r = p.shift()
		r.Ch = '{'
		*lbr = true
		return r
	default:
		p.err(errorf("TODO %v", p.s.Tok.Ch.str()))
		p.shift()
		return r
	}
}
