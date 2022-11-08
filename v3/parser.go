// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/scanner"
	"go/token"
	"runtime"

	"modernc.org/mathutil"
)

const parserBudget = 1e6

var (
	noBack    bool //TODO-
	panicBack bool //TODO-
)

type Node interface {
	Position() token.Position
}

type noder struct{} //TODO-

func (*noder) Position() (r token.Position) { return r }

type tok struct { //TODO-
	f   *token.File
	pos token.Pos
	tok token.Token
	lit string
}

func (n tok) Position() token.Position { return n.f.PositionFor(n.pos, true) }

type parser struct {
	a             *analyzer
	f             *token.File
	maxBackOrigin string
	maxBackRange  [2]int
	path          string
	toks          []tok

	backs   int
	budget  int
	ix      int
	maxBack int
	maxIx   int

	closed bool
	record bool
}

func newParser(path string, src []byte, record bool) (r *parser, err error) {
	r = &parser{
		a:      newAnalyzer(),
		budget: parserBudget,
		path:   path,
		record: record,
	}
	var s scanner.Scanner
	fs := token.NewFileSet()
	f := fs.AddFile(path, -1, len(src))
	r.f = f
	s.Init(r.f, src, func(pos token.Position, msg string) { err = errorf("%v: %s", pos, msg) }, 0)
	for {
		pos, t, lit := s.Scan()
		r.toks = append(r.toks, tok{f, pos, t, lit})
		if err != nil {
			return nil, err
		}

		if t == EOF {
			return r, nil
		}
	}
}

func (p *parser) errPosition() token.Position {
	return p.toks[p.maxIx].Position()
}

func (p *parser) c() token.Token      { return p.peek(0) }
func (p *parser) pos() token.Position { return p.toks[p.ix].Position() }

func (p *parser) accept(t token.Token) bool {
	if p.c() == t {
		p.ix++
		p.budget--
		return true
	}

	return false
}

func (p *parser) peek(n int) token.Token {
	if p.budget <= 0 || p.closed || p.ix+n >= len(p.toks) {
		return p.toks[len(p.toks)-1].tok
	}

	p.maxIx = mathutil.Max(p.maxIx, p.ix+n)
	return p.toks[p.ix+n].tok
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

func (p *parser) later(ix int) {
	if p.closed || p.ix == ix {
		return
	}

	p.recordBacktrack(ix, false)
}

func (p *parser) back(ix int) {
	if p.closed {
		return
	}

	p.recordBacktrack(ix, true)
	if noBack {
		p.closed = true
	}
	if panicBack {
		panic(todo("%v: (%v:)", p.errPosition(), origin(2)))
	}
}

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

// AliasDeclNode represents the production
//
//	AliasDecl = identifier "=" Type .
type AliasDeclNode struct{ noder }

func (p *parser) aliasDecl() Node {
	// ebnf.Sequence identifier "=" Type ctx []
	{
		if p.peek(1) != ASSIGN {
			return nil
		}
		ix := p.ix
		// *ebnf.Name identifier ctx []
		if !p.accept(IDENT) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "=" ctx []
		if !p.accept(ASSIGN) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Type ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.type1() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &AliasDeclNode{}
}

// ArgumentsNode represents the production
//
//	Arguments = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
type ArgumentsNode struct{ noder }

func (p *parser) arguments() Node {
	// ebnf.Sequence "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" ctx []
	{
		ix := p.ix
		// *ebnf.Token "(" ctx []
		if !p.accept(LPAREN) {
			p.back(ix)
			return nil
		}
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
					if p.expressionList() == nil {
						p.back(ix)
						goto _0
					}
				case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 0 1
					// *ebnf.Name ExpressionList ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
					if p.expressionList() == nil {
						goto _1
					}
					break
					goto _1
				_1:
					// ebnf.Sequence Type [ "," ExpressionList ] ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
					{
						ix := p.ix
						// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
						if p.type1() == nil {
							p.back(ix)
							goto _2
						}
						// *ebnf.Option [ "," ExpressionList ] ctx []
						switch p.c() {
						case COMMA:
							// ebnf.Sequence "," ExpressionList ctx [COMMA]
							{
								ix := p.ix
								// *ebnf.Token "," ctx [COMMA]
								p.ix++
								p.budget--
								// *ebnf.Name ExpressionList ctx []
								switch p.c() {
								case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
									if p.expressionList() == nil {
										p.back(ix)
										goto _3
									}
								default:
									p.back(ix)
									goto _3
								}
							}
						}
						goto _3
					_3:
					}
					break
					goto _2
				_2:
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
					p.ix++
					p.budget--
				}
				goto _4
			_4:
				// *ebnf.Option [ "," ] ctx []
				switch p.c() {
				case COMMA:
					// *ebnf.Token "," ctx [COMMA]
					p.ix++
					p.budget--
				}
				goto _5
			_5:
			}
		}
		goto _0
	_0:
		// *ebnf.Token ")" ctx []
		if !p.accept(RPAREN) {
			p.back(ix)
			return nil
		}
	}
	return &ArgumentsNode{}
}

// ArrayLengthNode represents the production
//
//	ArrayLength = Expression | "..." .
type ArrayLengthNode struct{ noder }

func (p *parser) arrayLength() Node {
	// ebnf.Alternative Expression | "..." ctx []
	switch p.c() {
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0
		// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.expression() == nil {
			return nil
		}
	case ELLIPSIS: // 1
		// *ebnf.Token "..." ctx [ELLIPSIS]
		p.ix++
		p.budget--
	default:
		return nil
	}
	return &ArrayLengthNode{}
}

// ArrayTypeNode represents the production
//
//	ArrayType = "[" ArrayLength "]" ElementType .
type ArrayTypeNode struct{ noder }

func (p *parser) arrayType() Node {
	// ebnf.Sequence "[" ArrayLength "]" ElementType ctx []
	{
		ix := p.ix
		// *ebnf.Token "[" ctx []
		if !p.accept(LBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ArrayLength ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, ELLIPSIS, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.arrayLength() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "]" ctx []
		if !p.accept(RBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ElementType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.elementType() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ArrayTypeNode{}
}

// AssignmentNode represents the production
//
//	Assignment = ExpressionList ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" | ":=" ) ExpressionList .
type AssignmentNode struct{ noder }

func (p *parser) assignment() Node {
	// ebnf.Sequence ExpressionList ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" | ":=" ) ExpressionList ctx []
	{
		ix := p.ix
		// *ebnf.Name ExpressionList ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expressionList() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Group ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" | ":=" ) ctx []
		// ebnf.Alternative "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" | ":=" ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, DEFINE, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
		switch p.c() {
		case ASSIGN: // 0
			// *ebnf.Token "=" ctx [ASSIGN]
			p.ix++
			p.budget--
		case ADD_ASSIGN: // 1
			// *ebnf.Token "+=" ctx [ADD_ASSIGN]
			p.ix++
			p.budget--
		case SUB_ASSIGN: // 2
			// *ebnf.Token "-=" ctx [SUB_ASSIGN]
			p.ix++
			p.budget--
		case OR_ASSIGN: // 3
			// *ebnf.Token "|=" ctx [OR_ASSIGN]
			p.ix++
			p.budget--
		case XOR_ASSIGN: // 4
			// *ebnf.Token "^=" ctx [XOR_ASSIGN]
			p.ix++
			p.budget--
		case MUL_ASSIGN: // 5
			// *ebnf.Token "*=" ctx [MUL_ASSIGN]
			p.ix++
			p.budget--
		case QUO_ASSIGN: // 6
			// *ebnf.Token "/=" ctx [QUO_ASSIGN]
			p.ix++
			p.budget--
		case REM_ASSIGN: // 7
			// *ebnf.Token "%=" ctx [REM_ASSIGN]
			p.ix++
			p.budget--
		case SHL_ASSIGN: // 8
			// *ebnf.Token "<<=" ctx [SHL_ASSIGN]
			p.ix++
			p.budget--
		case SHR_ASSIGN: // 9
			// *ebnf.Token ">>=" ctx [SHR_ASSIGN]
			p.ix++
			p.budget--
		case AND_ASSIGN: // 10
			// *ebnf.Token "&=" ctx [AND_ASSIGN]
			p.ix++
			p.budget--
		case AND_NOT_ASSIGN: // 11
			// *ebnf.Token "&^=" ctx [AND_NOT_ASSIGN]
			p.ix++
			p.budget--
		case DEFINE: // 12
			// *ebnf.Token ":=" ctx [DEFINE]
			p.ix++
			p.budget--
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Name ExpressionList ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expressionList() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &AssignmentNode{}
}

// AssignmentPreBlockNode represents the production
//
//	AssignmentPreBlock = ExpressionList ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" | ":=" ) ExpressionListPreBlock .
type AssignmentPreBlockNode struct{ noder }

func (p *parser) assignmentPreBlock() Node {
	// ebnf.Sequence ExpressionList ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" | ":=" ) ExpressionListPreBlock ctx []
	{
		ix := p.ix
		// *ebnf.Name ExpressionList ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expressionList() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Group ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" | ":=" ) ctx []
		// ebnf.Alternative "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" | ":=" ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, DEFINE, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
		switch p.c() {
		case ASSIGN: // 0
			// *ebnf.Token "=" ctx [ASSIGN]
			p.ix++
			p.budget--
		case ADD_ASSIGN: // 1
			// *ebnf.Token "+=" ctx [ADD_ASSIGN]
			p.ix++
			p.budget--
		case SUB_ASSIGN: // 2
			// *ebnf.Token "-=" ctx [SUB_ASSIGN]
			p.ix++
			p.budget--
		case OR_ASSIGN: // 3
			// *ebnf.Token "|=" ctx [OR_ASSIGN]
			p.ix++
			p.budget--
		case XOR_ASSIGN: // 4
			// *ebnf.Token "^=" ctx [XOR_ASSIGN]
			p.ix++
			p.budget--
		case MUL_ASSIGN: // 5
			// *ebnf.Token "*=" ctx [MUL_ASSIGN]
			p.ix++
			p.budget--
		case QUO_ASSIGN: // 6
			// *ebnf.Token "/=" ctx [QUO_ASSIGN]
			p.ix++
			p.budget--
		case REM_ASSIGN: // 7
			// *ebnf.Token "%=" ctx [REM_ASSIGN]
			p.ix++
			p.budget--
		case SHL_ASSIGN: // 8
			// *ebnf.Token "<<=" ctx [SHL_ASSIGN]
			p.ix++
			p.budget--
		case SHR_ASSIGN: // 9
			// *ebnf.Token ">>=" ctx [SHR_ASSIGN]
			p.ix++
			p.budget--
		case AND_ASSIGN: // 10
			// *ebnf.Token "&=" ctx [AND_ASSIGN]
			p.ix++
			p.budget--
		case AND_NOT_ASSIGN: // 11
			// *ebnf.Token "&^=" ctx [AND_NOT_ASSIGN]
			p.ix++
			p.budget--
		case DEFINE: // 12
			// *ebnf.Token ":=" ctx [DEFINE]
			p.ix++
			p.budget--
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Name ExpressionListPreBlock ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expressionListPreBlock() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &AssignmentPreBlockNode{}
}

// BaseTypeNode represents the production
//
//	BaseType = Type .
type BaseTypeNode struct{ noder }

func (p *parser) baseType() Node {
	return p.type1()
}

// BasicLitNode represents the production
//
//	BasicLit = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
type BasicLitNode struct{ noder }

func (p *parser) basicLit() Node {
	// ebnf.Alternative int_lit | float_lit | imaginary_lit | rune_lit | string_lit ctx []
	switch p.c() {
	case INT: // 0
		// *ebnf.Name int_lit ctx [INT]
		if !p.accept(INT) {
			return nil
		}
	case FLOAT: // 1
		// *ebnf.Name float_lit ctx [FLOAT]
		if !p.accept(FLOAT) {
			return nil
		}
	case IMAG: // 2
		// *ebnf.Name imaginary_lit ctx [IMAG]
		if !p.accept(IMAG) {
			return nil
		}
	case CHAR: // 3
		// *ebnf.Name rune_lit ctx [CHAR]
		if !p.accept(CHAR) {
			return nil
		}
	case STRING: // 4
		// *ebnf.Name string_lit ctx [STRING]
		if !p.accept(STRING) {
			return nil
		}
	default:
		return nil
	}
	return &BasicLitNode{}
}

// BlockNode represents the production
//
//	Block = "{" StatementList "}" .
type BlockNode struct{ noder }

func (p *parser) block() Node {
	// ebnf.Sequence "{" StatementList "}" ctx []
	{
		ix := p.ix
		// *ebnf.Token "{" ctx []
		if !p.accept(LBRACE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name StatementList ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if p.statementList() == nil {
				p.back(ix)
				return nil
			}
		}
		// *ebnf.Token "}" ctx []
		if !p.accept(RBRACE) {
			p.back(ix)
			return nil
		}
	}
	return &BlockNode{}
}

// BreakStmtNode represents the production
//
//	BreakStmt = "break" [ Label ] .
type BreakStmtNode struct{ noder }

func (p *parser) breakStmt() Node {
	// ebnf.Sequence "break" [ Label ] ctx []
	{
		ix := p.ix
		// *ebnf.Token "break" ctx []
		if !p.accept(BREAK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Label ] ctx []
		switch p.c() {
		case IDENT:
			// *ebnf.Name Label ctx [IDENT]
			if p.label() == nil {
				goto _0
			}
		}
		goto _0
	_0:
	}
	return &BreakStmtNode{}
}

// ChannelNode represents the production
//
//	Channel = Expression .
type ChannelNode struct{ noder }

func (p *parser) channel() Node {
	return p.expression()
}

// ChannelTypeNode represents the production
//
//	ChannelType = "<-" "chan" ElementType | "chan" "<-" ElementType | "chan" ElementType .
type ChannelTypeNode struct{ noder }

func (p *parser) channelType() Node {
	// ebnf.Alternative "<-" "chan" ElementType | "chan" "<-" ElementType | "chan" ElementType ctx []
	switch p.c() {
	case ARROW: // 0
		// ebnf.Sequence "<-" "chan" ElementType ctx [ARROW]
		{
			if p.peek(1) != CHAN {
				return nil
			}
			ix := p.ix
			// *ebnf.Token "<-" ctx [ARROW]
			p.ix++
			p.budget--
			// *ebnf.Token "chan" ctx []
			if !p.accept(CHAN) {
				p.back(ix)
				return nil
			}
			// *ebnf.Name ElementType ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.elementType() == nil {
					p.back(ix)
					return nil
				}
			default:
				p.back(ix)
				return nil
			}
		}
	case CHAN: // 1 2
		// ebnf.Sequence "chan" "<-" ElementType ctx [CHAN]
		{
			if p.peek(1) != ARROW {
				goto _0
			}
			ix := p.ix
			// *ebnf.Token "chan" ctx [CHAN]
			p.ix++
			p.budget--
			// *ebnf.Token "<-" ctx []
			if !p.accept(ARROW) {
				p.back(ix)
				goto _0
			}
			// *ebnf.Name ElementType ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.elementType() == nil {
					p.back(ix)
					goto _0
				}
			default:
				p.back(ix)
				goto _0
			}
		}
		break
		goto _0
	_0:
		// ebnf.Sequence "chan" ElementType ctx [CHAN]
		{
			ix := p.ix
			// *ebnf.Token "chan" ctx [CHAN]
			p.ix++
			p.budget--
			// *ebnf.Name ElementType ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.elementType() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
		}
		break
		goto _1
	_1:
		return nil
	default:
		return nil
	}
	return &ChannelTypeNode{}
}

// CommCaseNode represents the production
//
//	CommCase = "case" ( SendStmt | RecvStmt ) | "default" .
type CommCaseNode struct{ noder }

func (p *parser) commCase() Node {
	// ebnf.Alternative "case" ( SendStmt | RecvStmt ) | "default" ctx []
	switch p.c() {
	case CASE: // 0
		// ebnf.Sequence "case" ( SendStmt | RecvStmt ) ctx [CASE]
		{
			ix := p.ix
			// *ebnf.Token "case" ctx [CASE]
			p.ix++
			p.budget--
			// *ebnf.Group ( SendStmt | RecvStmt ) ctx []
			// ebnf.Alternative SendStmt | RecvStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0 1
				// *ebnf.Name SendStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.sendStmt() == nil {
					goto _0
				}
				break
				goto _0
			_0:
				// *ebnf.Name RecvStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.recvStmt() == nil {
					goto _1
				}
				break
				goto _1
			_1:
				p.back(ix)
				return nil
			default:
				p.back(ix)
				return nil
			}
		}
	case DEFAULT: // 1
		// *ebnf.Token "default" ctx [DEFAULT]
		p.ix++
		p.budget--
	default:
		return nil
	}
	return &CommCaseNode{}
}

// CommClauseNode represents the production
//
//	CommClause = CommCase ":" StatementList .
type CommClauseNode struct{ noder }

func (p *parser) commClause() Node {
	// ebnf.Sequence CommCase ":" StatementList ctx []
	{
		ix := p.ix
		// *ebnf.Name CommCase ctx []
		switch p.c() {
		case CASE, DEFAULT:
			if p.commCase() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token ":" ctx []
		if !p.accept(COLON) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name StatementList ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if p.statementList() == nil {
				p.back(ix)
				return nil
			}
		}
	}
	return &CommClauseNode{}
}

// ConditionNode represents the production
//
//	Condition = Expression .
type ConditionNode struct{ noder }

func (p *parser) condition() Node {
	return p.expression()
}

// ConstDeclNode represents the production
//
//	ConstDecl = "const" ( ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ) .
type ConstDeclNode struct{ noder }

func (p *parser) constDecl() Node {
	// ebnf.Sequence "const" ( ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ) ctx []
	{
		ix := p.ix
		// *ebnf.Token "const" ctx []
		if !p.accept(CONST) {
			p.back(ix)
			return nil
		}
		// *ebnf.Group ( ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ) ctx []
		// ebnf.Alternative ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name ConstSpec ctx [IDENT]
			if p.constSpec() == nil {
				p.back(ix)
				return nil
			}
		case LPAREN: // 1
			// ebnf.Sequence "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				p.ix++
				p.budget--
				// *ebnf.Option [ ConstSpec { ";" ConstSpec } [ ";" ] ] ctx []
				switch p.c() {
				case IDENT:
					// ebnf.Sequence ConstSpec { ";" ConstSpec } [ ";" ] ctx [IDENT]
					{
						ix := p.ix
						// *ebnf.Name ConstSpec ctx [IDENT]
						if p.constSpec() == nil {
							p.back(ix)
							goto _0
						}
						// *ebnf.Repetition { ";" ConstSpec } ctx []
					_1:
						switch p.c() {
						case SEMICOLON:
							// ebnf.Sequence ";" ConstSpec ctx [SEMICOLON]
							ix := p.ix
							// *ebnf.Token ";" ctx [SEMICOLON]
							p.ix++
							p.budget--
							// *ebnf.Name ConstSpec ctx []
							switch p.c() {
							case IDENT:
								if p.constSpec() == nil {
									p.back(ix)
									goto _2
								}
							default:
								p.back(ix)
								goto _2
							}
							goto _1
						}
						goto _2
					_2:
						// *ebnf.Option [ ";" ] ctx []
						switch p.c() {
						case SEMICOLON:
							// *ebnf.Token ";" ctx [SEMICOLON]
							p.ix++
							p.budget--
						}
						goto _3
					_3:
					}
				}
				goto _0
			_0:
				// *ebnf.Token ")" ctx []
				if !p.accept(RPAREN) {
					p.back(ix)
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ConstDeclNode{}
}

// ConstSpecNode represents the production
//
//	ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
type ConstSpecNode struct{ noder }

func (p *parser) constSpec() Node {
	// ebnf.Sequence IdentifierList [ [ Type ] "=" ExpressionList ] ctx []
	{
		ix := p.ix
		// *ebnf.Name IdentifierList ctx []
		switch p.c() {
		case IDENT:
			if p.identifierList() == nil {
				p.back(ix)
				return nil
			}
		default:
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
					if p.type1() == nil {
						goto _1
					}
				}
				goto _1
			_1:
				// *ebnf.Token "=" ctx []
				if !p.accept(ASSIGN) {
					p.back(ix)
					goto _0
				}
				// *ebnf.Name ExpressionList ctx []
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if p.expressionList() == nil {
						p.back(ix)
						goto _0
					}
				default:
					p.back(ix)
					goto _0
				}
			}
		}
		goto _0
	_0:
	}
	return &ConstSpecNode{}
}

// ContinueStmtNode represents the production
//
//	ContinueStmt = "continue" [ Label ] .
type ContinueStmtNode struct{ noder }

func (p *parser) continueStmt() Node {
	// ebnf.Sequence "continue" [ Label ] ctx []
	{
		ix := p.ix
		// *ebnf.Token "continue" ctx []
		if !p.accept(CONTINUE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Label ] ctx []
		switch p.c() {
		case IDENT:
			// *ebnf.Name Label ctx [IDENT]
			if p.label() == nil {
				goto _0
			}
		}
		goto _0
	_0:
	}
	return &ContinueStmtNode{}
}

// DeclarationNode represents the production
//
//	Declaration = ConstDecl | TypeDecl | VarDecl .
type DeclarationNode struct{ noder }

func (p *parser) declaration() Node {
	// ebnf.Alternative ConstDecl | TypeDecl | VarDecl ctx []
	switch p.c() {
	case CONST: // 0
		// *ebnf.Name ConstDecl ctx [CONST]
		if p.constDecl() == nil {
			return nil
		}
	case TYPE: // 1
		// *ebnf.Name TypeDecl ctx [TYPE]
		if p.typeDecl() == nil {
			return nil
		}
	case VAR: // 2
		// *ebnf.Name VarDecl ctx [VAR]
		if p.varDecl() == nil {
			return nil
		}
	default:
		return nil
	}
	return &DeclarationNode{}
}

// DeferStmtNode represents the production
//
//	DeferStmt = "defer" Expression .
type DeferStmtNode struct{ noder }

func (p *parser) deferStmt() Node {
	// ebnf.Sequence "defer" Expression ctx []
	{
		ix := p.ix
		// *ebnf.Token "defer" ctx []
		if !p.accept(DEFER) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Expression ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expression() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &DeferStmtNode{}
}

// ElementNode represents the production
//
//	Element = Expression | LiteralValue .
type ElementNode struct{ noder }

func (p *parser) element() Node {
	// ebnf.Alternative Expression | LiteralValue ctx []
	switch p.c() {
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0
		// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.expression() == nil {
			return nil
		}
	case LBRACE: // 1
		// *ebnf.Name LiteralValue ctx [LBRACE]
		if p.literalValue() == nil {
			return nil
		}
	default:
		return nil
	}
	return &ElementNode{}
}

// ElementListNode represents the production
//
//	ElementList = KeyedElement { "," KeyedElement } .
type ElementListNode struct{ noder }

func (p *parser) elementList() Node {
	// ebnf.Sequence KeyedElement { "," KeyedElement } ctx []
	{
		ix := p.ix
		// *ebnf.Name KeyedElement ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.keyedElement() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," KeyedElement } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," KeyedElement ctx [COMMA]
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			p.ix++
			p.budget--
			// *ebnf.Name KeyedElement ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.keyedElement() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &ElementListNode{}
}

// ElementTypeNode represents the production
//
//	ElementType = Type .
type ElementTypeNode struct{ noder }

func (p *parser) elementType() Node {
	return p.type1()
}

// EmbeddedFieldNode represents the production
//
//	EmbeddedField = [ "*" ] TypeName [ TypeArgs ] .
type EmbeddedFieldNode struct{ noder }

func (p *parser) embeddedField() Node {
	// ebnf.Sequence [ "*" ] TypeName [ TypeArgs ] ctx []
	{
		ix := p.ix
		// *ebnf.Option [ "*" ] ctx []
		switch p.c() {
		case MUL:
			// *ebnf.Token "*" ctx [MUL]
			p.ix++
			p.budget--
		}
		goto _0
	_0:
		// *ebnf.Name TypeName ctx []
		switch p.c() {
		case IDENT:
			if p.typeName() == nil {
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
			if p.typeArgs() == nil {
				goto _1
			}
		}
		goto _1
	_1:
	}
	return &EmbeddedFieldNode{}
}

// EmptyStmtNode represents the production
//
//	EmptyStmt =  .
type EmptyStmtNode struct{ noder }

func (p *parser) emptyStmt() Node {
	return &EmptyStmtNode{}
}

// ExprCaseClauseNode represents the production
//
//	ExprCaseClause = ExprSwitchCase ":" StatementList .
type ExprCaseClauseNode struct{ noder }

func (p *parser) exprCaseClause() Node {
	// ebnf.Sequence ExprSwitchCase ":" StatementList ctx []
	{
		ix := p.ix
		// *ebnf.Name ExprSwitchCase ctx []
		switch p.c() {
		case CASE, DEFAULT:
			if p.exprSwitchCase() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token ":" ctx []
		if !p.accept(COLON) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name StatementList ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if p.statementList() == nil {
				p.back(ix)
				return nil
			}
		}
	}
	return &ExprCaseClauseNode{}
}

// ExprSwitchCaseNode represents the production
//
//	ExprSwitchCase = "case" ExpressionList | "default" .
type ExprSwitchCaseNode struct{ noder }

func (p *parser) exprSwitchCase() Node {
	// ebnf.Alternative "case" ExpressionList | "default" ctx []
	switch p.c() {
	case CASE: // 0
		// ebnf.Sequence "case" ExpressionList ctx [CASE]
		{
			ix := p.ix
			// *ebnf.Token "case" ctx [CASE]
			p.ix++
			p.budget--
			// *ebnf.Name ExpressionList ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expressionList() == nil {
					p.back(ix)
					return nil
				}
			default:
				p.back(ix)
				return nil
			}
		}
	case DEFAULT: // 1
		// *ebnf.Token "default" ctx [DEFAULT]
		p.ix++
		p.budget--
	default:
		return nil
	}
	return &ExprSwitchCaseNode{}
}

// ExprSwitchStmtNode represents the production
//
//	ExprSwitchStmt = "switch" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" | "switch" SimpleStmt ";" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" .
type ExprSwitchStmtNode struct{ noder }

func (p *parser) exprSwitchStmt() Node {
	// ebnf.Alternative "switch" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" | "switch" SimpleStmt ";" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" ctx []
	switch p.c() {
	case SWITCH: // 0 1
		// ebnf.Sequence "switch" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" ctx [SWITCH]
		{
			ix := p.ix
			// *ebnf.Token "switch" ctx [SWITCH]
			p.ix++
			p.budget--
			// *ebnf.Option [ ExpressionPreBlock ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name ExpressionPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.expressionPreBlock() == nil {
					goto _1
				}
			}
			goto _1
		_1:
			// *ebnf.Token "{" ctx []
			if !p.accept(LBRACE) {
				p.back(ix)
				goto _0
			}
			// *ebnf.Repetition { ExprCaseClause } ctx []
		_2:
			switch p.c() {
			case CASE, DEFAULT:
				// *ebnf.Name ExprCaseClause ctx [CASE, DEFAULT]
				if p.exprCaseClause() == nil {
					goto _3
				}
				goto _2
			}
			goto _3
		_3:
			// *ebnf.Token "}" ctx []
			if !p.accept(RBRACE) {
				p.back(ix)
				goto _0
			}
		}
		break
		goto _0
	_0:
		// ebnf.Sequence "switch" SimpleStmt ";" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" ctx [SWITCH]
		{
			ix := p.ix
			// *ebnf.Token "switch" ctx [SWITCH]
			p.ix++
			p.budget--
			// *ebnf.Name SimpleStmt ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
				if p.simpleStmt() == nil {
					p.back(ix)
					goto _4
				}
			}
			// *ebnf.Token ";" ctx []
			if !p.accept(SEMICOLON) {
				p.back(ix)
				goto _4
			}
			// *ebnf.Option [ ExpressionPreBlock ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name ExpressionPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.expressionPreBlock() == nil {
					goto _5
				}
			}
			goto _5
		_5:
			// *ebnf.Token "{" ctx []
			if !p.accept(LBRACE) {
				p.back(ix)
				goto _4
			}
			// *ebnf.Repetition { ExprCaseClause } ctx []
		_6:
			switch p.c() {
			case CASE, DEFAULT:
				// *ebnf.Name ExprCaseClause ctx [CASE, DEFAULT]
				if p.exprCaseClause() == nil {
					goto _7
				}
				goto _6
			}
			goto _7
		_7:
			// *ebnf.Token "}" ctx []
			if !p.accept(RBRACE) {
				p.back(ix)
				goto _4
			}
		}
		break
		goto _4
	_4:
		return nil
	default:
		return nil
	}
	return &ExprSwitchStmtNode{}
}

// ExpressionNode represents the production
//
//	Expression = UnaryExpr { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) Expression } .
type ExpressionNode struct{ noder }

func (p *parser) expression() Node {
	// ebnf.Sequence UnaryExpr { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) Expression } ctx []
	{
		ix := p.ix
		// *ebnf.Name UnaryExpr ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.unaryExpr() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) Expression } ctx []
	_0:
		switch p.c() {
		case ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR:
			// ebnf.Sequence ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) Expression ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			ix := p.ix
			// *ebnf.Group ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			// ebnf.Alternative "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			switch p.c() {
			case LOR: // 0
				// *ebnf.Token "||" ctx [LOR]
				p.ix++
				p.budget--
			case LAND: // 1
				// *ebnf.Token "&&" ctx [LAND]
				p.ix++
				p.budget--
			case EQL: // 2
				// *ebnf.Token "==" ctx [EQL]
				p.ix++
				p.budget--
			case NEQ: // 3
				// *ebnf.Token "!=" ctx [NEQ]
				p.ix++
				p.budget--
			case LSS: // 4
				// *ebnf.Token "<" ctx [LSS]
				p.ix++
				p.budget--
			case LEQ: // 5
				// *ebnf.Token "<=" ctx [LEQ]
				p.ix++
				p.budget--
			case GTR: // 6
				// *ebnf.Token ">" ctx [GTR]
				p.ix++
				p.budget--
			case GEQ: // 7
				// *ebnf.Token ">=" ctx [GEQ]
				p.ix++
				p.budget--
			case ADD: // 8
				// *ebnf.Token "+" ctx [ADD]
				p.ix++
				p.budget--
			case SUB: // 9
				// *ebnf.Token "-" ctx [SUB]
				p.ix++
				p.budget--
			case OR: // 10
				// *ebnf.Token "|" ctx [OR]
				p.ix++
				p.budget--
			case XOR: // 11
				// *ebnf.Token "^" ctx [XOR]
				p.ix++
				p.budget--
			case MUL: // 12
				// *ebnf.Token "*" ctx [MUL]
				p.ix++
				p.budget--
			case QUO: // 13
				// *ebnf.Token "/" ctx [QUO]
				p.ix++
				p.budget--
			case REM: // 14
				// *ebnf.Token "%" ctx [REM]
				p.ix++
				p.budget--
			case SHL: // 15
				// *ebnf.Token "<<" ctx [SHL]
				p.ix++
				p.budget--
			case SHR: // 16
				// *ebnf.Token ">>" ctx [SHR]
				p.ix++
				p.budget--
			case AND: // 17
				// *ebnf.Token "&" ctx [AND]
				p.ix++
				p.budget--
			case AND_NOT: // 18
				// *ebnf.Token "&^" ctx [AND_NOT]
				p.ix++
				p.budget--
			default:
				p.back(ix)
				goto _1
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expression() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &ExpressionNode{}
}

// ExpressionListNode represents the production
//
//	ExpressionList = Expression { "," Expression } .
type ExpressionListNode struct{ noder }

func (p *parser) expressionList() Node {
	// ebnf.Sequence Expression { "," Expression } ctx []
	{
		ix := p.ix
		// *ebnf.Name Expression ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expression() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," Expression } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," Expression ctx [COMMA]
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			p.ix++
			p.budget--
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expression() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &ExpressionListNode{}
}

// ExpressionListPreBlockNode represents the production
//
//	ExpressionListPreBlock = ExpressionPreBlock { "," ExpressionPreBlock } .
type ExpressionListPreBlockNode struct{ noder }

func (p *parser) expressionListPreBlock() Node {
	// ebnf.Sequence ExpressionPreBlock { "," ExpressionPreBlock } ctx []
	{
		ix := p.ix
		// *ebnf.Name ExpressionPreBlock ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expressionPreBlock() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," ExpressionPreBlock } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," ExpressionPreBlock ctx [COMMA]
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			p.ix++
			p.budget--
			// *ebnf.Name ExpressionPreBlock ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expressionPreBlock() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &ExpressionListPreBlockNode{}
}

// ExpressionPreBlockNode represents the production
//
//	ExpressionPreBlock = UnaryExprPreBlock { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) ExpressionPreBlock } .
type ExpressionPreBlockNode struct{ noder }

func (p *parser) expressionPreBlock() Node {
	// ebnf.Sequence UnaryExprPreBlock { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) ExpressionPreBlock } ctx []
	{
		ix := p.ix
		// *ebnf.Name UnaryExprPreBlock ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.unaryExprPreBlock() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) ExpressionPreBlock } ctx []
	_0:
		switch p.c() {
		case ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR:
			// ebnf.Sequence ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) ExpressionPreBlock ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			ix := p.ix
			// *ebnf.Group ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			// ebnf.Alternative "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			switch p.c() {
			case LOR: // 0
				// *ebnf.Token "||" ctx [LOR]
				p.ix++
				p.budget--
			case LAND: // 1
				// *ebnf.Token "&&" ctx [LAND]
				p.ix++
				p.budget--
			case EQL: // 2
				// *ebnf.Token "==" ctx [EQL]
				p.ix++
				p.budget--
			case NEQ: // 3
				// *ebnf.Token "!=" ctx [NEQ]
				p.ix++
				p.budget--
			case LSS: // 4
				// *ebnf.Token "<" ctx [LSS]
				p.ix++
				p.budget--
			case LEQ: // 5
				// *ebnf.Token "<=" ctx [LEQ]
				p.ix++
				p.budget--
			case GTR: // 6
				// *ebnf.Token ">" ctx [GTR]
				p.ix++
				p.budget--
			case GEQ: // 7
				// *ebnf.Token ">=" ctx [GEQ]
				p.ix++
				p.budget--
			case ADD: // 8
				// *ebnf.Token "+" ctx [ADD]
				p.ix++
				p.budget--
			case SUB: // 9
				// *ebnf.Token "-" ctx [SUB]
				p.ix++
				p.budget--
			case OR: // 10
				// *ebnf.Token "|" ctx [OR]
				p.ix++
				p.budget--
			case XOR: // 11
				// *ebnf.Token "^" ctx [XOR]
				p.ix++
				p.budget--
			case MUL: // 12
				// *ebnf.Token "*" ctx [MUL]
				p.ix++
				p.budget--
			case QUO: // 13
				// *ebnf.Token "/" ctx [QUO]
				p.ix++
				p.budget--
			case REM: // 14
				// *ebnf.Token "%" ctx [REM]
				p.ix++
				p.budget--
			case SHL: // 15
				// *ebnf.Token "<<" ctx [SHL]
				p.ix++
				p.budget--
			case SHR: // 16
				// *ebnf.Token ">>" ctx [SHR]
				p.ix++
				p.budget--
			case AND: // 17
				// *ebnf.Token "&" ctx [AND]
				p.ix++
				p.budget--
			case AND_NOT: // 18
				// *ebnf.Token "&^" ctx [AND_NOT]
				p.ix++
				p.budget--
			default:
				p.back(ix)
				goto _1
			}
			// *ebnf.Name ExpressionPreBlock ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expressionPreBlock() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &ExpressionPreBlockNode{}
}

// ExpressionStmtNode represents the production
//
//	ExpressionStmt = Expression .
type ExpressionStmtNode struct{ noder }

func (p *parser) expressionStmt() Node {
	return p.expression()
}

// ExpressionStmtPreBlockNode represents the production
//
//	ExpressionStmtPreBlock = ExpressionPreBlock .
type ExpressionStmtPreBlockNode struct{ noder }

func (p *parser) expressionStmtPreBlock() Node {
	return p.expressionPreBlock()
}

// FallthroughStmtNode represents the production
//
//	FallthroughStmt = "fallthrough" .
type FallthroughStmtNode struct{ noder }

func (p *parser) fallthroughStmt() Node {
	if p.c() == FALLTHROUGH {
		p.ix++
		p.budget--
		return &FallthroughStmtNode{}
	}
	return nil
}

// FieldDeclNode represents the production
//
//	FieldDecl = ( IdentifierList Type | EmbeddedField ) [ Tag ] .
type FieldDeclNode struct{ noder }

func (p *parser) fieldDecl() Node {
	// ebnf.Sequence ( IdentifierList Type | EmbeddedField ) [ Tag ] ctx []
	{
		ix := p.ix
		// *ebnf.Group ( IdentifierList Type | EmbeddedField ) ctx []
		// ebnf.Alternative IdentifierList Type | EmbeddedField ctx [IDENT, MUL]
		switch p.c() {
		case IDENT: // 0 1
			// ebnf.Sequence IdentifierList Type ctx [IDENT]
			{
				ix := p.ix
				// *ebnf.Name IdentifierList ctx [IDENT]
				if p.identifierList() == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Name Type ctx []
				switch p.c() {
				case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
					if p.type1() == nil {
						p.back(ix)
						goto _0
					}
				default:
					p.back(ix)
					goto _0
				}
			}
			break
			goto _0
		_0:
			// *ebnf.Name EmbeddedField ctx [IDENT]
			if p.embeddedField() == nil {
				goto _1
			}
			break
			goto _1
		_1:
			p.back(ix)
			return nil
		case MUL: // 1
			// *ebnf.Name EmbeddedField ctx [MUL]
			if p.embeddedField() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Tag ] ctx []
		switch p.c() {
		case STRING:
			// *ebnf.Name Tag ctx [STRING]
			if p.tag() == nil {
				goto _2
			}
		}
		goto _2
	_2:
	}
	return &FieldDeclNode{}
}

// ForClauseNode represents the production
//
//	ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
type ForClauseNode struct{ noder }

func (p *parser) forClause() Node {
	// ebnf.Sequence [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] ctx []
	{
		ix := p.ix
		// *ebnf.Option [ InitStmt ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
			// *ebnf.Name InitStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
			if p.initStmt() == nil {
				goto _0
			}
		}
		goto _0
	_0:
		// *ebnf.Token ";" ctx []
		if !p.accept(SEMICOLON) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Condition ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// *ebnf.Name Condition ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if p.condition() == nil {
				goto _1
			}
		}
		goto _1
	_1:
		// *ebnf.Token ";" ctx []
		if !p.accept(SEMICOLON) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ PostStmt ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
			// *ebnf.Name PostStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
			if p.postStmt() == nil {
				goto _2
			}
		}
		goto _2
	_2:
	}
	return &ForClauseNode{}
}

// ForStmtNode represents the production
//
//	ForStmt = "for" ExpressionPreBlock Block | "for" ForClause Block | "for" RangeClause Block | "for" Block .
type ForStmtNode struct{ noder }

func (p *parser) forStmt() Node {
	// ebnf.Alternative "for" ExpressionPreBlock Block | "for" ForClause Block | "for" RangeClause Block | "for" Block ctx []
	switch p.c() {
	case FOR: // 0 1 2 3
		// ebnf.Sequence "for" ExpressionPreBlock Block ctx [FOR]
		{
			ix := p.ix
			// *ebnf.Token "for" ctx [FOR]
			p.ix++
			p.budget--
			// *ebnf.Name ExpressionPreBlock ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expressionPreBlock() == nil {
					p.back(ix)
					goto _0
				}
			default:
				p.back(ix)
				goto _0
			}
			// *ebnf.Name Block ctx []
			switch p.c() {
			case LBRACE:
				if p.block() == nil {
					p.back(ix)
					goto _0
				}
			default:
				p.back(ix)
				goto _0
			}
		}
		break
		goto _0
	_0:
		// ebnf.Sequence "for" ForClause Block ctx [FOR]
		{
			ix := p.ix
			// *ebnf.Token "for" ctx [FOR]
			p.ix++
			p.budget--
			// *ebnf.Name ForClause ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR:
				if p.forClause() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			// *ebnf.Name Block ctx []
			switch p.c() {
			case LBRACE:
				if p.block() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
		}
		break
		goto _1
	_1:
		// ebnf.Sequence "for" RangeClause Block ctx [FOR]
		{
			ix := p.ix
			// *ebnf.Token "for" ctx [FOR]
			p.ix++
			p.budget--
			// *ebnf.Name RangeClause ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, STRING, STRUCT, SUB, XOR:
				if p.rangeClause() == nil {
					p.back(ix)
					goto _2
				}
			default:
				p.back(ix)
				goto _2
			}
			// *ebnf.Name Block ctx []
			switch p.c() {
			case LBRACE:
				if p.block() == nil {
					p.back(ix)
					goto _2
				}
			default:
				p.back(ix)
				goto _2
			}
		}
		break
		goto _2
	_2:
		// ebnf.Sequence "for" Block ctx [FOR]
		{
			ix := p.ix
			// *ebnf.Token "for" ctx [FOR]
			p.ix++
			p.budget--
			// *ebnf.Name Block ctx []
			switch p.c() {
			case LBRACE:
				if p.block() == nil {
					p.back(ix)
					goto _3
				}
			default:
				p.back(ix)
				goto _3
			}
		}
		break
		goto _3
	_3:
		return nil
	default:
		return nil
	}
	return &ForStmtNode{}
}

// FunctionBodyNode represents the production
//
//	FunctionBody = Block .
type FunctionBodyNode struct{ noder }

func (p *parser) functionBody() Node {
	return p.block()
}

// FunctionDeclNode represents the production
//
//	FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
type FunctionDeclNode struct{ noder }

func (p *parser) functionDecl() Node {
	// ebnf.Sequence "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] ctx []
	{
		if p.peek(1) != IDENT {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "func" ctx []
		if !p.accept(FUNC) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name FunctionName ctx []
		switch p.c() {
		case IDENT:
			if p.functionName() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ TypeParameters ] ctx []
		switch p.c() {
		case LBRACK:
			// *ebnf.Name TypeParameters ctx [LBRACK]
			if p.typeParameters() == nil {
				goto _0
			}
		}
		goto _0
	_0:
		// *ebnf.Name Signature ctx []
		switch p.c() {
		case LPAREN:
			if p.signature() == nil {
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
			if p.functionBody() == nil {
				goto _1
			}
		}
		goto _1
	_1:
	}
	return &FunctionDeclNode{}
}

// FunctionLitNode represents the production
//
//	FunctionLit = "func" Signature FunctionBody .
type FunctionLitNode struct{ noder }

func (p *parser) functionLit() Node {
	// ebnf.Sequence "func" Signature FunctionBody ctx []
	{
		ix := p.ix
		// *ebnf.Token "func" ctx []
		if !p.accept(FUNC) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Signature ctx []
		switch p.c() {
		case LPAREN:
			if p.signature() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Name FunctionBody ctx []
		switch p.c() {
		case LBRACE:
			if p.functionBody() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &FunctionLitNode{}
}

// FunctionNameNode represents the production
//
//	FunctionName = identifier .
type FunctionNameNode struct{ noder }

func (p *parser) functionName() Node {
	if p.c() == IDENT {
		p.ix++
		p.budget--
		return &FunctionNameNode{}
	}
	return nil
}

// FunctionTypeNode represents the production
//
//	FunctionType = "func" Signature .
type FunctionTypeNode struct{ noder }

func (p *parser) functionType() Node {
	// ebnf.Sequence "func" Signature ctx []
	{
		ix := p.ix
		// *ebnf.Token "func" ctx []
		if !p.accept(FUNC) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Signature ctx []
		switch p.c() {
		case LPAREN:
			if p.signature() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &FunctionTypeNode{}
}

// GoStmtNode represents the production
//
//	GoStmt = "go" Expression .
type GoStmtNode struct{ noder }

func (p *parser) goStmt() Node {
	// ebnf.Sequence "go" Expression ctx []
	{
		ix := p.ix
		// *ebnf.Token "go" ctx []
		if !p.accept(GO) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Expression ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expression() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &GoStmtNode{}
}

// GotoStmtNode represents the production
//
//	GotoStmt = "goto" Label .
type GotoStmtNode struct{ noder }

func (p *parser) gotoStmt() Node {
	// ebnf.Sequence "goto" Label ctx []
	{
		if p.peek(1) != IDENT {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "goto" ctx []
		if !p.accept(GOTO) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Label ctx []
		switch p.c() {
		case IDENT:
			if p.label() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &GotoStmtNode{}
}

// IdentifierListNode represents the production
//
//	IdentifierList = identifier { "," identifier } .
type IdentifierListNode struct{ noder }

func (p *parser) identifierList() Node {
	// ebnf.Sequence identifier { "," identifier } ctx []
	{
		ix := p.ix
		// *ebnf.Name identifier ctx []
		if !p.accept(IDENT) {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," identifier } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," identifier ctx [COMMA]
			if p.peek(1) != IDENT {
				goto _1
			}
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			p.ix++
			p.budget--
			// *ebnf.Name identifier ctx []
			if !p.accept(IDENT) {
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &IdentifierListNode{}
}

// IfStmtNode represents the production
//
//	IfStmt = "if" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] | "if" SimpleStmt ";" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] .
type IfStmtNode struct{ noder }

func (p *parser) ifStmt() Node {
	// ebnf.Alternative "if" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] | "if" SimpleStmt ";" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] ctx []
	switch p.c() {
	case IF: // 0 1
		// ebnf.Sequence "if" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] ctx [IF]
		{
			ix := p.ix
			// *ebnf.Token "if" ctx [IF]
			p.ix++
			p.budget--
			// *ebnf.Name ExpressionPreBlock ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expressionPreBlock() == nil {
					p.back(ix)
					goto _0
				}
			default:
				p.back(ix)
				goto _0
			}
			// *ebnf.Name Block ctx []
			switch p.c() {
			case LBRACE:
				if p.block() == nil {
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
					ix := p.ix
					// *ebnf.Token "else" ctx [ELSE]
					p.ix++
					p.budget--
					// *ebnf.Group ( IfStmt | Block ) ctx []
					// ebnf.Alternative IfStmt | Block ctx [IF, LBRACE]
					switch p.c() {
					case IF: // 0
						// *ebnf.Name IfStmt ctx [IF]
						if p.ifStmt() == nil {
							p.back(ix)
							goto _1
						}
					case LBRACE: // 1
						// *ebnf.Name Block ctx [LBRACE]
						if p.block() == nil {
							p.back(ix)
							goto _1
						}
					default:
						p.back(ix)
						goto _1
					}
				}
			}
			goto _1
		_1:
		}
		break
		goto _0
	_0:
		// ebnf.Sequence "if" SimpleStmt ";" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] ctx [IF]
		{
			ix := p.ix
			// *ebnf.Token "if" ctx [IF]
			p.ix++
			p.budget--
			// *ebnf.Name SimpleStmt ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
				if p.simpleStmt() == nil {
					p.back(ix)
					goto _2
				}
			}
			// *ebnf.Token ";" ctx []
			if !p.accept(SEMICOLON) {
				p.back(ix)
				goto _2
			}
			// *ebnf.Name ExpressionPreBlock ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expressionPreBlock() == nil {
					p.back(ix)
					goto _2
				}
			default:
				p.back(ix)
				goto _2
			}
			// *ebnf.Name Block ctx []
			switch p.c() {
			case LBRACE:
				if p.block() == nil {
					p.back(ix)
					goto _2
				}
			default:
				p.back(ix)
				goto _2
			}
			// *ebnf.Option [ "else" ( IfStmt | Block ) ] ctx []
			switch p.c() {
			case ELSE:
				// ebnf.Sequence "else" ( IfStmt | Block ) ctx [ELSE]
				{
					ix := p.ix
					// *ebnf.Token "else" ctx [ELSE]
					p.ix++
					p.budget--
					// *ebnf.Group ( IfStmt | Block ) ctx []
					// ebnf.Alternative IfStmt | Block ctx [IF, LBRACE]
					switch p.c() {
					case IF: // 0
						// *ebnf.Name IfStmt ctx [IF]
						if p.ifStmt() == nil {
							p.back(ix)
							goto _3
						}
					case LBRACE: // 1
						// *ebnf.Name Block ctx [LBRACE]
						if p.block() == nil {
							p.back(ix)
							goto _3
						}
					default:
						p.back(ix)
						goto _3
					}
				}
			}
			goto _3
		_3:
		}
		break
		goto _2
	_2:
		return nil
	default:
		return nil
	}
	return &IfStmtNode{}
}

// ImportDeclNode represents the production
//
//	ImportDecl = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
type ImportDeclNode struct{ noder }

func (p *parser) importDecl() Node {
	// ebnf.Sequence "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) ctx []
	{
		ix := p.ix
		// *ebnf.Token "import" ctx []
		if !p.accept(IMPORT) {
			p.back(ix)
			return nil
		}
		// *ebnf.Group ( ImportSpec | "(" { ImportSpec ";" } ")" ) ctx []
		// ebnf.Alternative ImportSpec | "(" { ImportSpec ";" } ")" ctx [IDENT, LPAREN, PERIOD, STRING]
		switch p.c() {
		case IDENT, PERIOD, STRING: // 0
			// *ebnf.Name ImportSpec ctx [IDENT, PERIOD, STRING]
			if p.importSpec() == nil {
				p.back(ix)
				return nil
			}
		case LPAREN: // 1
			// ebnf.Sequence "(" { ImportSpec ";" } ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				p.ix++
				p.budget--
				// *ebnf.Repetition { ImportSpec ";" } ctx []
			_0:
				switch p.c() {
				case IDENT, PERIOD, STRING:
					// ebnf.Sequence ImportSpec ";" ctx [IDENT, PERIOD, STRING]
					ix := p.ix
					// *ebnf.Name ImportSpec ctx [IDENT, PERIOD, STRING]
					if p.importSpec() == nil {
						p.back(ix)
						goto _1
					}
					// *ebnf.Token ";" ctx []
					if !p.accept(SEMICOLON) {
						p.back(ix)
						goto _1
					}
					goto _0
				}
				goto _1
			_1:
				// *ebnf.Token ")" ctx []
				if !p.accept(RPAREN) {
					p.back(ix)
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ImportDeclNode{}
}

// ImportPathNode represents the production
//
//	ImportPath = string_lit .
type ImportPathNode struct{ noder }

func (p *parser) importPath() Node {
	if p.c() == STRING {
		p.ix++
		p.budget--
		return &ImportPathNode{}
	}
	return nil
}

// ImportSpecNode represents the production
//
//	ImportSpec = [ "." | PackageName ] ImportPath .
type ImportSpecNode struct{ noder }

func (p *parser) importSpec() Node {
	// ebnf.Sequence [ "." | PackageName ] ImportPath ctx []
	{
		ix := p.ix
		// *ebnf.Option [ "." | PackageName ] ctx []
		switch p.c() {
		case IDENT, PERIOD:
			// ebnf.Alternative "." | PackageName ctx [IDENT, PERIOD]
			switch p.c() {
			case PERIOD: // 0
				// *ebnf.Token "." ctx [PERIOD]
				p.ix++
				p.budget--
			case IDENT: // 1
				// *ebnf.Name PackageName ctx [IDENT]
				if p.packageName() == nil {
					goto _0
				}
			default:
				goto _0
			}
		}
		goto _0
	_0:
		// *ebnf.Name ImportPath ctx []
		switch p.c() {
		case STRING:
			if p.importPath() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ImportSpecNode{}
}

// IncDecStmtNode represents the production
//
//	IncDecStmt = Expression ( "++" | "--" ) .
type IncDecStmtNode struct{ noder }

func (p *parser) incDecStmt() Node {
	// ebnf.Sequence Expression ( "++" | "--" ) ctx []
	{
		ix := p.ix
		// *ebnf.Name Expression ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expression() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Group ( "++" | "--" ) ctx []
		// ebnf.Alternative "++" | "--" ctx [DEC, INC]
		switch p.c() {
		case INC: // 0
			// *ebnf.Token "++" ctx [INC]
			p.ix++
			p.budget--
		case DEC: // 1
			// *ebnf.Token "--" ctx [DEC]
			p.ix++
			p.budget--
		default:
			p.back(ix)
			return nil
		}
	}
	return &IncDecStmtNode{}
}

// IndexNode represents the production
//
//	Index = "[" Expression "]" .
type IndexNode struct{ noder }

func (p *parser) index() Node {
	// ebnf.Sequence "[" Expression "]" ctx []
	{
		ix := p.ix
		// *ebnf.Token "[" ctx []
		if !p.accept(LBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Expression ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expression() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "]" ctx []
		if !p.accept(RBRACK) {
			p.back(ix)
			return nil
		}
	}
	return &IndexNode{}
}

// InitStmtNode represents the production
//
//	InitStmt = SimpleStmt .
type InitStmtNode struct{ noder }

func (p *parser) initStmt() Node {
	return p.simpleStmt()
}

// InterfaceElemNode represents the production
//
//	InterfaceElem = MethodElem | TypeElem .
type InterfaceElemNode struct{ noder }

func (p *parser) interfaceElem() Node {
	// ebnf.Alternative MethodElem | TypeElem ctx []
	switch p.c() {
	case IDENT: // 0 1
		// *ebnf.Name MethodElem ctx [IDENT]
		if p.methodElem() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name TypeElem ctx [IDENT]
		if p.typeElem() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		return nil
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE: // 1
		// *ebnf.Name TypeElem ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
		if p.typeElem() == nil {
			return nil
		}
	default:
		return nil
	}
	return &InterfaceElemNode{}
}

// InterfaceTypeNode represents the production
//
//	InterfaceType = "interface" "{" [ InterfaceElem { ";" InterfaceElem } [ ";" ] ] "}" .
type InterfaceTypeNode struct{ noder }

func (p *parser) interfaceType() Node {
	// ebnf.Sequence "interface" "{" [ InterfaceElem { ";" InterfaceElem } [ ";" ] ] "}" ctx []
	{
		if p.peek(1) != LBRACE {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "interface" ctx []
		if !p.accept(INTERFACE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "{" ctx []
		if !p.accept(LBRACE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ InterfaceElem { ";" InterfaceElem } [ ";" ] ] ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
			// ebnf.Sequence InterfaceElem { ";" InterfaceElem } [ ";" ] ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
			{
				ix := p.ix
				// *ebnf.Name InterfaceElem ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
				if p.interfaceElem() == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Repetition { ";" InterfaceElem } ctx []
			_1:
				switch p.c() {
				case SEMICOLON:
					// ebnf.Sequence ";" InterfaceElem ctx [SEMICOLON]
					ix := p.ix
					// *ebnf.Token ";" ctx [SEMICOLON]
					p.ix++
					p.budget--
					// *ebnf.Name InterfaceElem ctx []
					switch p.c() {
					case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
						if p.interfaceElem() == nil {
							p.back(ix)
							goto _2
						}
					default:
						p.back(ix)
						goto _2
					}
					goto _1
				}
				goto _2
			_2:
				// *ebnf.Option [ ";" ] ctx []
				switch p.c() {
				case SEMICOLON:
					// *ebnf.Token ";" ctx [SEMICOLON]
					p.ix++
					p.budget--
				}
				goto _3
			_3:
			}
		}
		goto _0
	_0:
		// *ebnf.Token "}" ctx []
		if !p.accept(RBRACE) {
			p.back(ix)
			return nil
		}
	}
	return &InterfaceTypeNode{}
}

// KeyTypeNode represents the production
//
//	KeyType = Type .
type KeyTypeNode struct{ noder }

func (p *parser) keyType() Node {
	return p.type1()
}

// KeyedElementNode represents the production
//
//	KeyedElement = Element [ ":" Element ] .
type KeyedElementNode struct{ noder }

func (p *parser) keyedElement() Node {
	// ebnf.Sequence Element [ ":" Element ] ctx []
	{
		ix := p.ix
		// *ebnf.Name Element ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.element() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ ":" Element ] ctx []
		switch p.c() {
		case COLON:
			// ebnf.Sequence ":" Element ctx [COLON]
			{
				ix := p.ix
				// *ebnf.Token ":" ctx [COLON]
				p.ix++
				p.budget--
				// *ebnf.Name Element ctx []
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if p.element() == nil {
						p.back(ix)
						goto _0
					}
				default:
					p.back(ix)
					goto _0
				}
			}
		}
		goto _0
	_0:
	}
	return &KeyedElementNode{}
}

// LabelNode represents the production
//
//	Label = identifier .
type LabelNode struct{ noder }

func (p *parser) label() Node {
	if p.c() == IDENT {
		p.ix++
		p.budget--
		return &LabelNode{}
	}
	return nil
}

// LabeledStmtNode represents the production
//
//	LabeledStmt = Label ":" Statement .
type LabeledStmtNode struct{ noder }

func (p *parser) labeledStmt() Node {
	// ebnf.Sequence Label ":" Statement ctx []
	{
		if p.peek(1) != COLON {
			return nil
		}
		ix := p.ix
		// *ebnf.Name Label ctx []
		switch p.c() {
		case IDENT:
			if p.label() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token ":" ctx []
		if !p.accept(COLON) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Statement ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if p.statement() == nil {
				p.back(ix)
				return nil
			}
		}
	}
	return &LabeledStmtNode{}
}

// LiteralNode represents the production
//
//	Literal = BasicLit | FunctionLit .
type LiteralNode struct{ noder }

func (p *parser) literal() Node {
	// ebnf.Alternative BasicLit | FunctionLit ctx []
	switch p.c() {
	case CHAR, FLOAT, IMAG, INT, STRING: // 0
		// *ebnf.Name BasicLit ctx [CHAR, FLOAT, IMAG, INT, STRING]
		if p.basicLit() == nil {
			return nil
		}
	case FUNC: // 1
		// *ebnf.Name FunctionLit ctx [FUNC]
		if p.functionLit() == nil {
			return nil
		}
	default:
		return nil
	}
	return &LiteralNode{}
}

// LiteralValueNode represents the production
//
//	LiteralValue = "{" [ ElementList [ "," ] ] "}" .
type LiteralValueNode struct{ noder }

func (p *parser) literalValue() Node {
	// ebnf.Sequence "{" [ ElementList [ "," ] ] "}" ctx []
	{
		ix := p.ix
		// *ebnf.Token "{" ctx []
		if !p.accept(LBRACE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ ElementList [ "," ] ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// ebnf.Sequence ElementList [ "," ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name ElementList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.elementList() == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Option [ "," ] ctx []
				switch p.c() {
				case COMMA:
					// *ebnf.Token "," ctx [COMMA]
					p.ix++
					p.budget--
				}
				goto _1
			_1:
			}
		}
		goto _0
	_0:
		// *ebnf.Token "}" ctx []
		if !p.accept(RBRACE) {
			p.back(ix)
			return nil
		}
	}
	return &LiteralValueNode{}
}

// MapTypeNode represents the production
//
//	MapType = "map" "[" KeyType "]" ElementType .
type MapTypeNode struct{ noder }

func (p *parser) mapType() Node {
	// ebnf.Sequence "map" "[" KeyType "]" ElementType ctx []
	{
		if p.peek(1) != LBRACK {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "map" ctx []
		if !p.accept(MAP) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "[" ctx []
		if !p.accept(LBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name KeyType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.keyType() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "]" ctx []
		if !p.accept(RBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ElementType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.elementType() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &MapTypeNode{}
}

// MethodDeclNode represents the production
//
//	MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
type MethodDeclNode struct{ noder }

func (p *parser) methodDecl() Node {
	// ebnf.Sequence "func" Receiver MethodName Signature [ FunctionBody ] ctx []
	{
		ix := p.ix
		// *ebnf.Token "func" ctx []
		if !p.accept(FUNC) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Receiver ctx []
		switch p.c() {
		case LPAREN:
			if p.receiver() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Name MethodName ctx []
		switch p.c() {
		case IDENT:
			if p.methodName() == nil {
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
			if p.signature() == nil {
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
			if p.functionBody() == nil {
				goto _0
			}
		}
		goto _0
	_0:
	}
	return &MethodDeclNode{}
}

// MethodElemNode represents the production
//
//	MethodElem = MethodName Signature .
type MethodElemNode struct{ noder }

func (p *parser) methodElem() Node {
	// ebnf.Sequence MethodName Signature ctx []
	{
		ix := p.ix
		// *ebnf.Name MethodName ctx []
		switch p.c() {
		case IDENT:
			if p.methodName() == nil {
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
			if p.signature() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &MethodElemNode{}
}

// MethodNameNode represents the production
//
//	MethodName = identifier .
type MethodNameNode struct{ noder }

func (p *parser) methodName() Node {
	if p.c() == IDENT {
		p.ix++
		p.budget--
		return &MethodNameNode{}
	}
	return nil
}

// OperandNode represents the production
//
//	Operand = Literal | Type [ LiteralValue ] | "(" Expression ")" .
type OperandNode struct{ noder }

func (p *parser) operand() Node {
	// ebnf.Alternative Literal | Type [ LiteralValue ] | "(" Expression ")" ctx []
	switch p.c() {
	case CHAR, FLOAT, IMAG, INT, STRING: // 0
		// *ebnf.Name Literal ctx [CHAR, FLOAT, IMAG, INT, STRING]
		if p.literal() == nil {
			return nil
		}
	case FUNC: // 0 1
		// *ebnf.Name Literal ctx [FUNC]
		if p.literal() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// ebnf.Sequence Type [ LiteralValue ] ctx [FUNC]
		{
			ix := p.ix
			// *ebnf.Name Type ctx [FUNC]
			if p.type1() == nil {
				p.back(ix)
				goto _1
			}
			// *ebnf.Option [ LiteralValue ] ctx []
			switch p.c() {
			case LBRACE:
				// *ebnf.Name LiteralValue ctx [LBRACE]
				if p.literalValue() == nil {
					goto _2
				}
			}
			goto _2
		_2:
		}
		break
		goto _1
	_1:
		return nil
	case ARROW, CHAN, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 1
		// ebnf.Sequence Type [ LiteralValue ] ctx [ARROW, CHAN, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		{
			ix := p.ix
			// *ebnf.Name Type ctx [ARROW, CHAN, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT]
			if p.type1() == nil {
				p.back(ix)
				return nil
			}
			// *ebnf.Option [ LiteralValue ] ctx []
			switch p.c() {
			case LBRACE:
				// *ebnf.Name LiteralValue ctx [LBRACE]
				if p.literalValue() == nil {
					goto _3
				}
			}
			goto _3
		_3:
		}
	case LPAREN: // 1 2
		// ebnf.Sequence Type [ LiteralValue ] ctx [LPAREN]
		{
			ix := p.ix
			// *ebnf.Name Type ctx [LPAREN]
			if p.type1() == nil {
				p.back(ix)
				goto _4
			}
			// *ebnf.Option [ LiteralValue ] ctx []
			switch p.c() {
			case LBRACE:
				// *ebnf.Name LiteralValue ctx [LBRACE]
				if p.literalValue() == nil {
					goto _5
				}
			}
			goto _5
		_5:
		}
		break
		goto _4
	_4:
		// ebnf.Sequence "(" Expression ")" ctx [LPAREN]
		{
			ix := p.ix
			// *ebnf.Token "(" ctx [LPAREN]
			p.ix++
			p.budget--
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expression() == nil {
					p.back(ix)
					goto _6
				}
			default:
				p.back(ix)
				goto _6
			}
			// *ebnf.Token ")" ctx []
			if !p.accept(RPAREN) {
				p.back(ix)
				goto _6
			}
		}
		break
		goto _6
	_6:
		return nil
	default:
		return nil
	}
	return &OperandNode{}
}

// OperandPreBlockNode represents the production
//
//	OperandPreBlock = Literal | QualifiedIdent [ TypeArgs ] | TypePreBlock [ LiteralValue ] | "(" Expression ")" .
type OperandPreBlockNode struct{ noder }

func (p *parser) operandPreBlock() Node {
	// ebnf.Alternative Literal | QualifiedIdent [ TypeArgs ] | TypePreBlock [ LiteralValue ] | "(" Expression ")" ctx []
	switch p.c() {
	case CHAR, FLOAT, IMAG, INT, STRING: // 0
		// *ebnf.Name Literal ctx [CHAR, FLOAT, IMAG, INT, STRING]
		if p.literal() == nil {
			return nil
		}
	case FUNC: // 0 2
		// *ebnf.Name Literal ctx [FUNC]
		if p.literal() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// ebnf.Sequence TypePreBlock [ LiteralValue ] ctx [FUNC]
		{
			ix := p.ix
			// *ebnf.Name TypePreBlock ctx [FUNC]
			if p.typePreBlock() == nil {
				p.back(ix)
				goto _1
			}
			// *ebnf.Option [ LiteralValue ] ctx []
			switch p.c() {
			case LBRACE:
				// *ebnf.Name LiteralValue ctx [LBRACE]
				if p.literalValue() == nil {
					goto _2
				}
			}
			goto _2
		_2:
		}
		break
		goto _1
	_1:
		return nil
	case IDENT: // 1
		// ebnf.Sequence QualifiedIdent [ TypeArgs ] ctx [IDENT]
		{
			ix := p.ix
			// *ebnf.Name QualifiedIdent ctx [IDENT]
			if p.qualifiedIdent() == nil {
				p.back(ix)
				return nil
			}
			// *ebnf.Option [ TypeArgs ] ctx []
			switch p.c() {
			case LBRACK:
				// *ebnf.Name TypeArgs ctx [LBRACK]
				if p.typeArgs() == nil {
					goto _3
				}
			}
			goto _3
		_3:
		}
	case ARROW, CHAN, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 2
		// ebnf.Sequence TypePreBlock [ LiteralValue ] ctx [ARROW, CHAN, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		{
			ix := p.ix
			// *ebnf.Name TypePreBlock ctx [ARROW, CHAN, INTERFACE, LBRACK, MAP, MUL, STRUCT]
			if p.typePreBlock() == nil {
				p.back(ix)
				return nil
			}
			// *ebnf.Option [ LiteralValue ] ctx []
			switch p.c() {
			case LBRACE:
				// *ebnf.Name LiteralValue ctx [LBRACE]
				if p.literalValue() == nil {
					goto _4
				}
			}
			goto _4
		_4:
		}
	case LPAREN: // 2 3
		// ebnf.Sequence TypePreBlock [ LiteralValue ] ctx [LPAREN]
		{
			ix := p.ix
			// *ebnf.Name TypePreBlock ctx [LPAREN]
			if p.typePreBlock() == nil {
				p.back(ix)
				goto _5
			}
			// *ebnf.Option [ LiteralValue ] ctx []
			switch p.c() {
			case LBRACE:
				// *ebnf.Name LiteralValue ctx [LBRACE]
				if p.literalValue() == nil {
					goto _6
				}
			}
			goto _6
		_6:
		}
		break
		goto _5
	_5:
		// ebnf.Sequence "(" Expression ")" ctx [LPAREN]
		{
			ix := p.ix
			// *ebnf.Token "(" ctx [LPAREN]
			p.ix++
			p.budget--
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expression() == nil {
					p.back(ix)
					goto _7
				}
			default:
				p.back(ix)
				goto _7
			}
			// *ebnf.Token ")" ctx []
			if !p.accept(RPAREN) {
				p.back(ix)
				goto _7
			}
		}
		break
		goto _7
	_7:
		return nil
	default:
		return nil
	}
	return &OperandPreBlockNode{}
}

// PackageClauseNode represents the production
//
//	PackageClause = "package" PackageName .
type PackageClauseNode struct{ noder }

func (p *parser) packageClause() Node {
	// ebnf.Sequence "package" PackageName ctx []
	{
		if p.peek(1) != IDENT {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "package" ctx []
		if !p.accept(PACKAGE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name PackageName ctx []
		switch p.c() {
		case IDENT:
			if p.packageName() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &PackageClauseNode{}
}

// PackageNameNode represents the production
//
//	PackageName = identifier .
type PackageNameNode struct{ noder }

func (p *parser) packageName() Node {
	if p.c() == IDENT {
		p.ix++
		p.budget--
		return &PackageNameNode{}
	}
	return nil
}

// ParameterDeclNode represents the production
//
//	ParameterDecl = identifier "..." Type | identifier Type | "..." Type | Type .
type ParameterDeclNode struct{ noder }

func (p *parser) parameterDecl() Node {
	// ebnf.Alternative identifier "..." Type | identifier Type | "..." Type | Type ctx []
	switch p.c() {
	case IDENT: // 0 1 3
		// ebnf.Sequence identifier "..." Type ctx [IDENT]
		{
			if p.peek(1) != ELLIPSIS {
				goto _0
			}
			ix := p.ix
			// *ebnf.Name identifier ctx [IDENT]
			if !p.accept(IDENT) {
				p.back(ix)
				goto _0
			}
			// *ebnf.Token "..." ctx []
			if !p.accept(ELLIPSIS) {
				p.back(ix)
				goto _0
			}
			// *ebnf.Name Type ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.type1() == nil {
					p.back(ix)
					goto _0
				}
			default:
				p.back(ix)
				goto _0
			}
		}
		break
		goto _0
	_0:
		// ebnf.Sequence identifier Type ctx [IDENT]
		{
			ix := p.ix
			// *ebnf.Name identifier ctx [IDENT]
			if !p.accept(IDENT) {
				p.back(ix)
				goto _1
			}
			// *ebnf.Name Type ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.type1() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
		}
		break
		goto _1
	_1:
		// *ebnf.Name Type ctx [IDENT]
		if p.type1() == nil {
			goto _2
		}
		break
		goto _2
	_2:
		return nil
	case ELLIPSIS: // 2
		// ebnf.Sequence "..." Type ctx [ELLIPSIS]
		{
			ix := p.ix
			// *ebnf.Token "..." ctx [ELLIPSIS]
			p.ix++
			p.budget--
			// *ebnf.Name Type ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.type1() == nil {
					p.back(ix)
					return nil
				}
			default:
				p.back(ix)
				return nil
			}
		}
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 3
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if p.type1() == nil {
			return nil
		}
	default:
		return nil
	}
	return &ParameterDeclNode{}
}

// ParameterListNode represents the production
//
//	ParameterList = ParameterDecl { "," ParameterDecl } .
type ParameterListNode struct{ noder }

func (p *parser) parameterList() Node {
	// ebnf.Sequence ParameterDecl { "," ParameterDecl } ctx []
	{
		ix := p.ix
		// *ebnf.Name ParameterDecl ctx []
		switch p.c() {
		case ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.parameterDecl() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," ParameterDecl } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," ParameterDecl ctx [COMMA]
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			p.ix++
			p.budget--
			// *ebnf.Name ParameterDecl ctx []
			switch p.c() {
			case ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.parameterDecl() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &ParameterListNode{}
}

// ParametersNode represents the production
//
//	Parameters = "(" [ ParameterList [ "," ] ] ")" .
type ParametersNode struct{ noder }

func (p *parser) parameters() Node {
	// ebnf.Sequence "(" [ ParameterList [ "," ] ] ")" ctx []
	{
		ix := p.ix
		// *ebnf.Token "(" ctx []
		if !p.accept(LPAREN) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ ParameterList [ "," ] ] ctx []
		switch p.c() {
		case ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			// ebnf.Sequence ParameterList [ "," ] ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			{
				ix := p.ix
				// *ebnf.Name ParameterList ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
				if p.parameterList() == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Option [ "," ] ctx []
				switch p.c() {
				case COMMA:
					// *ebnf.Token "," ctx [COMMA]
					p.ix++
					p.budget--
				}
				goto _1
			_1:
			}
		}
		goto _0
	_0:
		// *ebnf.Token ")" ctx []
		if !p.accept(RPAREN) {
			p.back(ix)
			return nil
		}
	}
	return &ParametersNode{}
}

// PointerTypeNode represents the production
//
//	PointerType = "*" BaseType .
type PointerTypeNode struct{ noder }

func (p *parser) pointerType() Node {
	// ebnf.Sequence "*" BaseType ctx []
	{
		ix := p.ix
		// *ebnf.Token "*" ctx []
		if !p.accept(MUL) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name BaseType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.baseType() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &PointerTypeNode{}
}

// PostStmtNode represents the production
//
//	PostStmt = SimpleStmtPreBlock .
type PostStmtNode struct{ noder }

func (p *parser) postStmt() Node {
	return p.simpleStmtPreBlock()
}

// PrimaryExprNode represents the production
//
//	PrimaryExpr = Operand { Selector | Index | Slice | TypeAssertion | Arguments } .
type PrimaryExprNode struct{ noder }

func (p *parser) primaryExpr() Node {
	// ebnf.Sequence Operand { Selector | Index | Slice | TypeAssertion | Arguments } ctx []
	{
		ix := p.ix
		// *ebnf.Name Operand ctx []
		switch p.c() {
		case ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT:
			if p.operand() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { Selector | Index | Slice | TypeAssertion | Arguments } ctx []
	_0:
		switch p.c() {
		case LBRACK, LPAREN, PERIOD:
			// ebnf.Alternative Selector | Index | Slice | TypeAssertion | Arguments ctx [LBRACK, LPAREN, PERIOD]
			switch p.c() {
			case PERIOD: // 0 3
				// *ebnf.Name Selector ctx [PERIOD]
				if p.selector() == nil {
					goto _2
				}
				break
				goto _2
			_2:
				// *ebnf.Name TypeAssertion ctx [PERIOD]
				if p.typeAssertion() == nil {
					goto _3
				}
				break
				goto _3
			_3:
				goto _1
			case LBRACK: // 1 2
				// *ebnf.Name Index ctx [LBRACK]
				if p.index() == nil {
					goto _4
				}
				break
				goto _4
			_4:
				// *ebnf.Name Slice ctx [LBRACK]
				if p.slice() == nil {
					goto _5
				}
				break
				goto _5
			_5:
				goto _1
			case LPAREN: // 4
				// *ebnf.Name Arguments ctx [LPAREN]
				if p.arguments() == nil {
					goto _1
				}
			default:
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &PrimaryExprNode{}
}

// PrimaryExprPreBlockNode represents the production
//
//	PrimaryExprPreBlock = OperandPreBlock { Selector | Index | Slice | TypeAssertion | Arguments } .
type PrimaryExprPreBlockNode struct{ noder }

func (p *parser) primaryExprPreBlock() Node {
	// ebnf.Sequence OperandPreBlock { Selector | Index | Slice | TypeAssertion | Arguments } ctx []
	{
		ix := p.ix
		// *ebnf.Name OperandPreBlock ctx []
		switch p.c() {
		case ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT:
			if p.operandPreBlock() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { Selector | Index | Slice | TypeAssertion | Arguments } ctx []
	_0:
		switch p.c() {
		case LBRACK, LPAREN, PERIOD:
			// ebnf.Alternative Selector | Index | Slice | TypeAssertion | Arguments ctx [LBRACK, LPAREN, PERIOD]
			switch p.c() {
			case PERIOD: // 0 3
				// *ebnf.Name Selector ctx [PERIOD]
				if p.selector() == nil {
					goto _2
				}
				break
				goto _2
			_2:
				// *ebnf.Name TypeAssertion ctx [PERIOD]
				if p.typeAssertion() == nil {
					goto _3
				}
				break
				goto _3
			_3:
				goto _1
			case LBRACK: // 1 2
				// *ebnf.Name Index ctx [LBRACK]
				if p.index() == nil {
					goto _4
				}
				break
				goto _4
			_4:
				// *ebnf.Name Slice ctx [LBRACK]
				if p.slice() == nil {
					goto _5
				}
				break
				goto _5
			_5:
				goto _1
			case LPAREN: // 4
				// *ebnf.Name Arguments ctx [LPAREN]
				if p.arguments() == nil {
					goto _1
				}
			default:
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &PrimaryExprPreBlockNode{}
}

// QualifiedIdentNode represents the production
//
//	QualifiedIdent = PackageName [ "." identifier ] .
type QualifiedIdentNode struct{ noder }

func (p *parser) qualifiedIdent() Node {
	// ebnf.Sequence PackageName [ "." identifier ] ctx []
	{
		ix := p.ix
		// *ebnf.Name PackageName ctx []
		switch p.c() {
		case IDENT:
			if p.packageName() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ "." identifier ] ctx []
		switch p.c() {
		case PERIOD:
			// ebnf.Sequence "." identifier ctx [PERIOD]
			{
				if p.peek(1) != IDENT {
					goto _0
				}
				ix := p.ix
				// *ebnf.Token "." ctx [PERIOD]
				p.ix++
				p.budget--
				// *ebnf.Name identifier ctx []
				if !p.accept(IDENT) {
					p.back(ix)
					goto _0
				}
			}
		}
		goto _0
	_0:
	}
	return &QualifiedIdentNode{}
}

// RangeClauseNode represents the production
//
//	RangeClause = [ ExpressionList ( "=" | ":=" ) ] "range" ExpressionPreBlock .
type RangeClauseNode struct{ noder }

func (p *parser) rangeClause() Node {
	// ebnf.Sequence [ ExpressionList ( "=" | ":=" ) ] "range" ExpressionPreBlock ctx []
	{
		ix := p.ix
		// *ebnf.Option [ ExpressionList ( "=" | ":=" ) ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// ebnf.Sequence ExpressionList ( "=" | ":=" ) ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.expressionList() == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Group ( "=" | ":=" ) ctx []
				// ebnf.Alternative "=" | ":=" ctx [ASSIGN, DEFINE]
				switch p.c() {
				case ASSIGN: // 0
					// *ebnf.Token "=" ctx [ASSIGN]
					p.ix++
					p.budget--
				case DEFINE: // 1
					// *ebnf.Token ":=" ctx [DEFINE]
					p.ix++
					p.budget--
				default:
					p.back(ix)
					goto _0
				}
			}
		}
		goto _0
	_0:
		// *ebnf.Token "range" ctx []
		if !p.accept(RANGE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ExpressionPreBlock ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expressionPreBlock() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &RangeClauseNode{}
}

// ReceiverNode represents the production
//
//	Receiver = Parameters .
type ReceiverNode struct{ noder }

func (p *parser) receiver() Node {
	return p.parameters()
}

// RecvExprNode represents the production
//
//	RecvExpr = Expression .
type RecvExprNode struct{ noder }

func (p *parser) recvExpr() Node {
	return p.expression()
}

// RecvStmtNode represents the production
//
//	RecvStmt = [ ExpressionList ( "=" | ":=" ) ] RecvExpr .
type RecvStmtNode struct{ noder }

func (p *parser) recvStmt() Node {
	// ebnf.Sequence [ ExpressionList ( "=" | ":=" ) ] RecvExpr ctx []
	{
		ix := p.ix
		// *ebnf.Option [ ExpressionList ( "=" | ":=" ) ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// ebnf.Sequence ExpressionList ( "=" | ":=" ) ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.expressionList() == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Group ( "=" | ":=" ) ctx []
				// ebnf.Alternative "=" | ":=" ctx [ASSIGN, DEFINE]
				switch p.c() {
				case ASSIGN: // 0
					// *ebnf.Token "=" ctx [ASSIGN]
					p.ix++
					p.budget--
				case DEFINE: // 1
					// *ebnf.Token ":=" ctx [DEFINE]
					p.ix++
					p.budget--
				default:
					p.back(ix)
					goto _0
				}
			}
		}
		goto _0
	_0:
		// *ebnf.Name RecvExpr ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.recvExpr() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &RecvStmtNode{}
}

// ResultNode represents the production
//
//	Result = Parameters | Type .
type ResultNode struct{ noder }

func (p *parser) result() Node {
	// ebnf.Alternative Parameters | Type ctx []
	switch p.c() {
	case LPAREN: // 0 1
		// *ebnf.Name Parameters ctx [LPAREN]
		if p.parameters() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name Type ctx [LPAREN]
		if p.type1() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		return nil
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 1
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		if p.type1() == nil {
			return nil
		}
	default:
		return nil
	}
	return &ResultNode{}
}

// ReturnStmtNode represents the production
//
//	ReturnStmt = "return" [ ExpressionList ] .
type ReturnStmtNode struct{ noder }

func (p *parser) returnStmt() Node {
	// ebnf.Sequence "return" [ ExpressionList ] ctx []
	{
		ix := p.ix
		// *ebnf.Token "return" ctx []
		if !p.accept(RETURN) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ ExpressionList ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if p.expressionList() == nil {
				goto _0
			}
		}
		goto _0
	_0:
	}
	return &ReturnStmtNode{}
}

// SelectStmtNode represents the production
//
//	SelectStmt = "select" "{" { CommClause } "}" .
type SelectStmtNode struct{ noder }

func (p *parser) selectStmt() Node {
	// ebnf.Sequence "select" "{" { CommClause } "}" ctx []
	{
		if p.peek(1) != LBRACE {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "select" ctx []
		if !p.accept(SELECT) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "{" ctx []
		if !p.accept(LBRACE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { CommClause } ctx []
	_0:
		switch p.c() {
		case CASE, DEFAULT:
			// *ebnf.Name CommClause ctx [CASE, DEFAULT]
			if p.commClause() == nil {
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
		// *ebnf.Token "}" ctx []
		if !p.accept(RBRACE) {
			p.back(ix)
			return nil
		}
	}
	return &SelectStmtNode{}
}

// SelectorNode represents the production
//
//	Selector = "." identifier .
type SelectorNode struct{ noder }

func (p *parser) selector() Node {
	// ebnf.Sequence "." identifier ctx []
	{
		if p.peek(1) != IDENT {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "." ctx []
		if !p.accept(PERIOD) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name identifier ctx []
		if !p.accept(IDENT) {
			p.back(ix)
			return nil
		}
	}
	return &SelectorNode{}
}

// SendStmtNode represents the production
//
//	SendStmt = Channel "<-" Expression .
type SendStmtNode struct{ noder }

func (p *parser) sendStmt() Node {
	// ebnf.Sequence Channel "<-" Expression ctx []
	{
		ix := p.ix
		// *ebnf.Name Channel ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.channel() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "<-" ctx []
		if !p.accept(ARROW) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Expression ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expression() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &SendStmtNode{}
}

// SendStmtPreBlockNode represents the production
//
//	SendStmtPreBlock = Channel "<-" ExpressionPreBlock .
type SendStmtPreBlockNode struct{ noder }

func (p *parser) sendStmtPreBlock() Node {
	// ebnf.Sequence Channel "<-" ExpressionPreBlock ctx []
	{
		ix := p.ix
		// *ebnf.Name Channel ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.channel() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "<-" ctx []
		if !p.accept(ARROW) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ExpressionPreBlock ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if p.expressionPreBlock() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &SendStmtPreBlockNode{}
}

// SignatureNode represents the production
//
//	Signature = Parameters [ Result ] .
type SignatureNode struct{ noder }

func (p *parser) signature() Node {
	// ebnf.Sequence Parameters [ Result ] ctx []
	{
		ix := p.ix
		// *ebnf.Name Parameters ctx []
		switch p.c() {
		case LPAREN:
			if p.parameters() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Result ] ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			// *ebnf.Name Result ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if p.result() == nil {
				goto _0
			}
		}
		goto _0
	_0:
	}
	return &SignatureNode{}
}

// SimpleStmtNode represents the production
//
//	SimpleStmt = Assignment | IncDecStmt | SendStmt | ExpressionStmt | EmptyStmt .
type SimpleStmtNode struct{ noder }

func (p *parser) simpleStmt() Node {
	// ebnf.Alternative Assignment | IncDecStmt | SendStmt | ExpressionStmt | EmptyStmt ctx []
	switch p.c() {
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0 1 2 3
		// *ebnf.Name Assignment ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.assignment() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name IncDecStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.incDecStmt() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		// *ebnf.Name SendStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.sendStmt() == nil {
			goto _2
		}
		break
		goto _2
	_2:
		// *ebnf.Name ExpressionStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.expressionStmt() == nil {
			goto _3
		}
		break
		goto _3
	_3:
		return nil
	default: //  /* ε */ 4
		// *ebnf.Name EmptyStmt ctx [ /* ε */]
		if p.emptyStmt() == nil {
			return nil
		}
	}
	return &SimpleStmtNode{}
}

// SimpleStmtPreBlockNode represents the production
//
//	SimpleStmtPreBlock = AssignmentPreBlock | IncDecStmt | SendStmtPreBlock | ExpressionStmtPreBlock | EmptyStmt .
type SimpleStmtPreBlockNode struct{ noder }

func (p *parser) simpleStmtPreBlock() Node {
	// ebnf.Alternative AssignmentPreBlock | IncDecStmt | SendStmtPreBlock | ExpressionStmtPreBlock | EmptyStmt ctx []
	switch p.c() {
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0 1 2 3
		// *ebnf.Name AssignmentPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.assignmentPreBlock() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name IncDecStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.incDecStmt() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		// *ebnf.Name SendStmtPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.sendStmtPreBlock() == nil {
			goto _2
		}
		break
		goto _2
	_2:
		// *ebnf.Name ExpressionStmtPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if p.expressionStmtPreBlock() == nil {
			goto _3
		}
		break
		goto _3
	_3:
		return nil
	default: //  /* ε */ 4
		// *ebnf.Name EmptyStmt ctx [ /* ε */]
		if p.emptyStmt() == nil {
			return nil
		}
	}
	return &SimpleStmtPreBlockNode{}
}

// SliceNode represents the production
//
//	Slice = "[" [ Expression ] ":" [ Expression ] "]" | "[" [ Expression ] ":" Expression ":" Expression "]" .
type SliceNode struct{ noder }

func (p *parser) slice() Node {
	// ebnf.Alternative "[" [ Expression ] ":" [ Expression ] "]" | "[" [ Expression ] ":" Expression ":" Expression "]" ctx []
	switch p.c() {
	case LBRACK: // 0 1
		// ebnf.Sequence "[" [ Expression ] ":" [ Expression ] "]" ctx [LBRACK]
		{
			ix := p.ix
			// *ebnf.Token "[" ctx [LBRACK]
			p.ix++
			p.budget--
			// *ebnf.Option [ Expression ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.expression() == nil {
					goto _1
				}
			}
			goto _1
		_1:
			// *ebnf.Token ":" ctx []
			if !p.accept(COLON) {
				p.back(ix)
				goto _0
			}
			// *ebnf.Option [ Expression ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.expression() == nil {
					goto _2
				}
			}
			goto _2
		_2:
			// *ebnf.Token "]" ctx []
			if !p.accept(RBRACK) {
				p.back(ix)
				goto _0
			}
		}
		break
		goto _0
	_0:
		// ebnf.Sequence "[" [ Expression ] ":" Expression ":" Expression "]" ctx [LBRACK]
		{
			ix := p.ix
			// *ebnf.Token "[" ctx [LBRACK]
			p.ix++
			p.budget--
			// *ebnf.Option [ Expression ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if p.expression() == nil {
					goto _4
				}
			}
			goto _4
		_4:
			// *ebnf.Token ":" ctx []
			if !p.accept(COLON) {
				p.back(ix)
				goto _3
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expression() == nil {
					p.back(ix)
					goto _3
				}
			default:
				p.back(ix)
				goto _3
			}
			// *ebnf.Token ":" ctx []
			if !p.accept(COLON) {
				p.back(ix)
				goto _3
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.expression() == nil {
					p.back(ix)
					goto _3
				}
			default:
				p.back(ix)
				goto _3
			}
			// *ebnf.Token "]" ctx []
			if !p.accept(RBRACK) {
				p.back(ix)
				goto _3
			}
		}
		break
		goto _3
	_3:
		return nil
	default:
		return nil
	}
	return &SliceNode{}
}

// SliceTypeNode represents the production
//
//	SliceType = "[" "]" ElementType .
type SliceTypeNode struct{ noder }

func (p *parser) sliceType() Node {
	// ebnf.Sequence "[" "]" ElementType ctx []
	{
		if p.peek(1) != RBRACK {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "[" ctx []
		if !p.accept(LBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "]" ctx []
		if !p.accept(RBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ElementType ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.elementType() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &SliceTypeNode{}
}

// SourceFileNode represents the production
//
//	SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
type SourceFileNode struct{ noder }

func (p *parser) sourceFile() Node {
	// ebnf.Sequence PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } ctx []
	{
		ix := p.ix
		// *ebnf.Name PackageClause ctx []
		switch p.c() {
		case PACKAGE:
			if p.packageClause() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token ";" ctx []
		if !p.accept(SEMICOLON) {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { ImportDecl ";" } ctx []
	_0:
		switch p.c() {
		case IMPORT:
			// ebnf.Sequence ImportDecl ";" ctx [IMPORT]
			ix := p.ix
			// *ebnf.Name ImportDecl ctx [IMPORT]
			if p.importDecl() == nil {
				p.back(ix)
				goto _1
			}
			// *ebnf.Token ";" ctx []
			if !p.accept(SEMICOLON) {
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
		// *ebnf.Repetition { TopLevelDecl ";" } ctx []
	_2:
		switch p.c() {
		case CONST, FUNC, TYPE, VAR:
			// ebnf.Sequence TopLevelDecl ";" ctx [CONST, FUNC, TYPE, VAR]
			ix := p.ix
			// *ebnf.Name TopLevelDecl ctx [CONST, FUNC, TYPE, VAR]
			if p.topLevelDecl() == nil {
				p.back(ix)
				goto _3
			}
			// *ebnf.Token ";" ctx []
			if !p.accept(SEMICOLON) {
				p.back(ix)
				goto _3
			}
			goto _2
		}
		goto _3
	_3:
	}
	return &SourceFileNode{}
}

// StatementNode represents the production
//
//	Statement = Declaration | LabeledStmt | GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt | DeferStmt | SimpleStmt .
type StatementNode struct{ noder }

func (p *parser) statement() Node {
	// ebnf.Alternative Declaration | LabeledStmt | GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt | DeferStmt | SimpleStmt ctx []
	switch p.c() {
	case CONST, TYPE, VAR: // 0
		// *ebnf.Name Declaration ctx [CONST, TYPE, VAR]
		if p.declaration() == nil {
			return nil
		}
	case IDENT: // 1 14
		// *ebnf.Name LabeledStmt ctx [IDENT]
		if p.labeledStmt() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name SimpleStmt ctx [IDENT]
		if p.simpleStmt() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		return nil
	case GO: // 2
		// *ebnf.Name GoStmt ctx [GO]
		if p.goStmt() == nil {
			return nil
		}
	case RETURN: // 3
		// *ebnf.Name ReturnStmt ctx [RETURN]
		if p.returnStmt() == nil {
			return nil
		}
	case BREAK: // 4
		// *ebnf.Name BreakStmt ctx [BREAK]
		if p.breakStmt() == nil {
			return nil
		}
	case CONTINUE: // 5
		// *ebnf.Name ContinueStmt ctx [CONTINUE]
		if p.continueStmt() == nil {
			return nil
		}
	case GOTO: // 6
		// *ebnf.Name GotoStmt ctx [GOTO]
		if p.gotoStmt() == nil {
			return nil
		}
	case FALLTHROUGH: // 7
		// *ebnf.Name FallthroughStmt ctx [FALLTHROUGH]
		if p.fallthroughStmt() == nil {
			return nil
		}
	case LBRACE: // 8
		// *ebnf.Name Block ctx [LBRACE]
		if p.block() == nil {
			return nil
		}
	case IF: // 9
		// *ebnf.Name IfStmt ctx [IF]
		if p.ifStmt() == nil {
			return nil
		}
	case SWITCH: // 10
		// *ebnf.Name SwitchStmt ctx [SWITCH]
		if p.switchStmt() == nil {
			return nil
		}
	case SELECT: // 11
		// *ebnf.Name SelectStmt ctx [SELECT]
		if p.selectStmt() == nil {
			return nil
		}
	case FOR: // 12
		// *ebnf.Name ForStmt ctx [FOR]
		if p.forStmt() == nil {
			return nil
		}
	case DEFER: // 13
		// *ebnf.Name DeferStmt ctx [DEFER]
		if p.deferStmt() == nil {
			return nil
		}
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ : // 14
		// *ebnf.Name SimpleStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
		if p.simpleStmt() == nil {
			return nil
		}
	}
	return &StatementNode{}
}

// StatementListNode represents the production
//
//	StatementList = [ Statement { ";" Statement } [ ";" ] ] .
type StatementListNode struct{ noder }

func (p *parser) statementList() Node {
	// *ebnf.Option [ Statement { ";" Statement } [ ";" ] ] ctx []
	switch p.c() {
	case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
		// ebnf.Sequence Statement { ";" Statement } [ ";" ] ctx [ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */]
		{
			ix := p.ix
			// *ebnf.Name Statement ctx [ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */]
			switch p.c() {
			case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
				if p.statement() == nil {
					p.back(ix)
					goto _0
				}
			}
			// *ebnf.Repetition { ";" Statement } ctx []
		_1:
			switch p.c() {
			case SEMICOLON:
				// ebnf.Sequence ";" Statement ctx [SEMICOLON]
				ix := p.ix
				// *ebnf.Token ";" ctx [SEMICOLON]
				p.ix++
				p.budget--
				// *ebnf.Name Statement ctx []
				switch p.c() {
				case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
					if p.statement() == nil {
						p.back(ix)
						goto _2
					}
				}
				goto _1
			}
			goto _2
		_2:
			// *ebnf.Option [ ";" ] ctx []
			switch p.c() {
			case SEMICOLON:
				// *ebnf.Token ";" ctx [SEMICOLON]
				p.ix++
				p.budget--
			}
			goto _3
		_3:
		}
	}
	goto _0
_0:
	return &StatementListNode{}
}

// StructTypeNode represents the production
//
//	StructType = "struct" "{" [ FieldDecl { ";" FieldDecl } [ ";" ] ] "}" .
type StructTypeNode struct{ noder }

func (p *parser) structType() Node {
	// ebnf.Sequence "struct" "{" [ FieldDecl { ";" FieldDecl } [ ";" ] ] "}" ctx []
	{
		if p.peek(1) != LBRACE {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "struct" ctx []
		if !p.accept(STRUCT) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "{" ctx []
		if !p.accept(LBRACE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ FieldDecl { ";" FieldDecl } [ ";" ] ] ctx []
		switch p.c() {
		case IDENT, MUL:
			// ebnf.Sequence FieldDecl { ";" FieldDecl } [ ";" ] ctx [IDENT, MUL]
			{
				ix := p.ix
				// *ebnf.Name FieldDecl ctx [IDENT, MUL]
				if p.fieldDecl() == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Repetition { ";" FieldDecl } ctx []
			_1:
				switch p.c() {
				case SEMICOLON:
					// ebnf.Sequence ";" FieldDecl ctx [SEMICOLON]
					ix := p.ix
					// *ebnf.Token ";" ctx [SEMICOLON]
					p.ix++
					p.budget--
					// *ebnf.Name FieldDecl ctx []
					switch p.c() {
					case IDENT, MUL:
						if p.fieldDecl() == nil {
							p.back(ix)
							goto _2
						}
					default:
						p.back(ix)
						goto _2
					}
					goto _1
				}
				goto _2
			_2:
				// *ebnf.Option [ ";" ] ctx []
				switch p.c() {
				case SEMICOLON:
					// *ebnf.Token ";" ctx [SEMICOLON]
					p.ix++
					p.budget--
				}
				goto _3
			_3:
			}
		}
		goto _0
	_0:
		// *ebnf.Token "}" ctx []
		if !p.accept(RBRACE) {
			p.back(ix)
			return nil
		}
	}
	return &StructTypeNode{}
}

// SwitchStmtNode represents the production
//
//	SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
type SwitchStmtNode struct{ noder }

func (p *parser) switchStmt() Node {
	// ebnf.Alternative ExprSwitchStmt | TypeSwitchStmt ctx []
	switch p.c() {
	case SWITCH: // 0 1
		// *ebnf.Name ExprSwitchStmt ctx [SWITCH]
		if p.exprSwitchStmt() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name TypeSwitchStmt ctx [SWITCH]
		if p.typeSwitchStmt() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		return nil
	default:
		return nil
	}
	return &SwitchStmtNode{}
}

// TagNode represents the production
//
//	Tag = string_lit .
type TagNode struct{ noder }

func (p *parser) tag() Node {
	if p.c() == STRING {
		p.ix++
		p.budget--
		return &TagNode{}
	}
	return nil
}

// TopLevelDeclNode represents the production
//
//	TopLevelDecl = Declaration | FunctionDecl | MethodDecl .
type TopLevelDeclNode struct{ noder }

func (p *parser) topLevelDecl() Node {
	// ebnf.Alternative Declaration | FunctionDecl | MethodDecl ctx []
	switch p.c() {
	case CONST, TYPE, VAR: // 0
		// *ebnf.Name Declaration ctx [CONST, TYPE, VAR]
		if p.declaration() == nil {
			return nil
		}
	case FUNC: // 1 2
		// *ebnf.Name FunctionDecl ctx [FUNC]
		if p.functionDecl() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name MethodDecl ctx [FUNC]
		if p.methodDecl() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		return nil
	default:
		return nil
	}
	return &TopLevelDeclNode{}
}

// TypeNode represents the production
//
//	Type = TypeName [ TypeArgs ] | TypeLit | "(" Type ")" .
type TypeNode struct{ noder }

func (p *parser) type1() Node {
	// ebnf.Alternative TypeName [ TypeArgs ] | TypeLit | "(" Type ")" ctx []
	switch p.c() {
	case IDENT: // 0
		// ebnf.Sequence TypeName [ TypeArgs ] ctx [IDENT]
		{
			ix := p.ix
			// *ebnf.Name TypeName ctx [IDENT]
			if p.typeName() == nil {
				p.back(ix)
				return nil
			}
			// *ebnf.Option [ TypeArgs ] ctx []
			switch p.c() {
			case LBRACK:
				// *ebnf.Name TypeArgs ctx [LBRACK]
				if p.typeArgs() == nil {
					goto _0
				}
			}
			goto _0
		_0:
		}
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 1
		// *ebnf.Name TypeLit ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		if p.typeLit() == nil {
			return nil
		}
	case LPAREN: // 2
		// ebnf.Sequence "(" Type ")" ctx [LPAREN]
		{
			ix := p.ix
			// *ebnf.Token "(" ctx [LPAREN]
			p.ix++
			p.budget--
			// *ebnf.Name Type ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.type1() == nil {
					p.back(ix)
					return nil
				}
			default:
				p.back(ix)
				return nil
			}
			// *ebnf.Token ")" ctx []
			if !p.accept(RPAREN) {
				p.back(ix)
				return nil
			}
		}
	default:
		return nil
	}
	return &TypeNode{}
}

// TypeArgsNode represents the production
//
//	TypeArgs = "[" TypeList [ "," ] "]" .
type TypeArgsNode struct{ noder }

func (p *parser) typeArgs() Node {
	// ebnf.Sequence "[" TypeList [ "," ] "]" ctx []
	{
		ix := p.ix
		// *ebnf.Token "[" ctx []
		if !p.accept(LBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name TypeList ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.typeList() == nil {
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
			p.ix++
			p.budget--
		}
		goto _0
	_0:
		// *ebnf.Token "]" ctx []
		if !p.accept(RBRACK) {
			p.back(ix)
			return nil
		}
	}
	return &TypeArgsNode{}
}

// TypeAssertionNode represents the production
//
//	TypeAssertion = "." "(" Type ")" .
type TypeAssertionNode struct{ noder }

func (p *parser) typeAssertion() Node {
	// ebnf.Sequence "." "(" Type ")" ctx []
	{
		if p.peek(1) != LPAREN {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "." ctx []
		if !p.accept(PERIOD) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "(" ctx []
		if !p.accept(LPAREN) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Type ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.type1() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token ")" ctx []
		if !p.accept(RPAREN) {
			p.back(ix)
			return nil
		}
	}
	return &TypeAssertionNode{}
}

// TypeCaseClauseNode represents the production
//
//	TypeCaseClause = TypeSwitchCase ":" StatementList .
type TypeCaseClauseNode struct{ noder }

func (p *parser) typeCaseClause() Node {
	// ebnf.Sequence TypeSwitchCase ":" StatementList ctx []
	{
		ix := p.ix
		// *ebnf.Name TypeSwitchCase ctx []
		switch p.c() {
		case CASE, DEFAULT:
			if p.typeSwitchCase() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token ":" ctx []
		if !p.accept(COLON) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name StatementList ctx []
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if p.statementList() == nil {
				p.back(ix)
				return nil
			}
		}
	}
	return &TypeCaseClauseNode{}
}

// TypeConstraintNode represents the production
//
//	TypeConstraint = TypeElem .
type TypeConstraintNode struct{ noder }

func (p *parser) typeConstraint() Node {
	return p.typeElem()
}

// TypeDeclNode represents the production
//
//	TypeDecl = "type" ( TypeSpec | "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ) .
type TypeDeclNode struct{ noder }

func (p *parser) typeDecl() Node {
	// ebnf.Sequence "type" ( TypeSpec | "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ) ctx []
	{
		ix := p.ix
		// *ebnf.Token "type" ctx []
		if !p.accept(TYPE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Group ( TypeSpec | "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ) ctx []
		// ebnf.Alternative TypeSpec | "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name TypeSpec ctx [IDENT]
			if p.typeSpec() == nil {
				p.back(ix)
				return nil
			}
		case LPAREN: // 1
			// ebnf.Sequence "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				p.ix++
				p.budget--
				// *ebnf.Option [ TypeSpec { ";" TypeSpec } [ ";" ] ] ctx []
				switch p.c() {
				case IDENT:
					// ebnf.Sequence TypeSpec { ";" TypeSpec } [ ";" ] ctx [IDENT]
					{
						ix := p.ix
						// *ebnf.Name TypeSpec ctx [IDENT]
						if p.typeSpec() == nil {
							p.back(ix)
							goto _0
						}
						// *ebnf.Repetition { ";" TypeSpec } ctx []
					_1:
						switch p.c() {
						case SEMICOLON:
							// ebnf.Sequence ";" TypeSpec ctx [SEMICOLON]
							ix := p.ix
							// *ebnf.Token ";" ctx [SEMICOLON]
							p.ix++
							p.budget--
							// *ebnf.Name TypeSpec ctx []
							switch p.c() {
							case IDENT:
								if p.typeSpec() == nil {
									p.back(ix)
									goto _2
								}
							default:
								p.back(ix)
								goto _2
							}
							goto _1
						}
						goto _2
					_2:
						// *ebnf.Option [ ";" ] ctx []
						switch p.c() {
						case SEMICOLON:
							// *ebnf.Token ";" ctx [SEMICOLON]
							p.ix++
							p.budget--
						}
						goto _3
					_3:
					}
				}
				goto _0
			_0:
				// *ebnf.Token ")" ctx []
				if !p.accept(RPAREN) {
					p.back(ix)
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &TypeDeclNode{}
}

// TypeDefNode represents the production
//
//	TypeDef = identifier [ TypeParameters ] Type .
type TypeDefNode struct{ noder }

func (p *parser) typeDef() Node {
	// ebnf.Sequence identifier [ TypeParameters ] Type ctx []
	{
		ix := p.ix
		// *ebnf.Name identifier ctx []
		if !p.accept(IDENT) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ TypeParameters ] ctx []
		switch p.c() {
		case LBRACK:
			// *ebnf.Name TypeParameters ctx [LBRACK]
			if p.typeParameters() == nil {
				goto _0
			}
		}
		goto _0
	_0:
		// *ebnf.Name Type ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.type1() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &TypeDefNode{}
}

// TypeElemNode represents the production
//
//	TypeElem = TypeTerm { "|" TypeTerm } .
type TypeElemNode struct{ noder }

func (p *parser) typeElem() Node {
	// ebnf.Sequence TypeTerm { "|" TypeTerm } ctx []
	{
		ix := p.ix
		// *ebnf.Name TypeTerm ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
			if p.typeTerm() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "|" TypeTerm } ctx []
	_0:
		switch p.c() {
		case OR:
			// ebnf.Sequence "|" TypeTerm ctx [OR]
			ix := p.ix
			// *ebnf.Token "|" ctx [OR]
			p.ix++
			p.budget--
			// *ebnf.Name TypeTerm ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
				if p.typeTerm() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &TypeElemNode{}
}

// TypeListNode represents the production
//
//	TypeList = Type { "," Type } .
type TypeListNode struct{ noder }

func (p *parser) typeList() Node {
	// ebnf.Sequence Type { "," Type } ctx []
	{
		ix := p.ix
		// *ebnf.Name Type ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.type1() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," Type } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," Type ctx [COMMA]
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			p.ix++
			p.budget--
			// *ebnf.Name Type ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.type1() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &TypeListNode{}
}

// TypeLitNode represents the production
//
//	TypeLit = ArrayType | StructType | PointerType | FunctionType | InterfaceType | SliceType | MapType | ChannelType .
type TypeLitNode struct{ noder }

func (p *parser) typeLit() Node {
	// ebnf.Alternative ArrayType | StructType | PointerType | FunctionType | InterfaceType | SliceType | MapType | ChannelType ctx []
	switch p.c() {
	case LBRACK: // 0 5
		// *ebnf.Name ArrayType ctx [LBRACK]
		if p.arrayType() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name SliceType ctx [LBRACK]
		if p.sliceType() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		return nil
	case STRUCT: // 1
		// *ebnf.Name StructType ctx [STRUCT]
		if p.structType() == nil {
			return nil
		}
	case MUL: // 2
		// *ebnf.Name PointerType ctx [MUL]
		if p.pointerType() == nil {
			return nil
		}
	case FUNC: // 3
		// *ebnf.Name FunctionType ctx [FUNC]
		if p.functionType() == nil {
			return nil
		}
	case INTERFACE: // 4
		// *ebnf.Name InterfaceType ctx [INTERFACE]
		if p.interfaceType() == nil {
			return nil
		}
	case MAP: // 6
		// *ebnf.Name MapType ctx [MAP]
		if p.mapType() == nil {
			return nil
		}
	case ARROW, CHAN: // 7
		// *ebnf.Name ChannelType ctx [ARROW, CHAN]
		if p.channelType() == nil {
			return nil
		}
	default:
		return nil
	}
	return &TypeLitNode{}
}

// TypeNameNode represents the production
//
//	TypeName = QualifiedIdent .
type TypeNameNode struct{ noder }

func (p *parser) typeName() Node {
	return p.qualifiedIdent()
}

// TypeParamDeclNode represents the production
//
//	TypeParamDecl = IdentifierList TypeConstraint .
type TypeParamDeclNode struct{ noder }

func (p *parser) typeParamDecl() Node {
	// ebnf.Sequence IdentifierList TypeConstraint ctx []
	{
		ix := p.ix
		// *ebnf.Name IdentifierList ctx []
		switch p.c() {
		case IDENT:
			if p.identifierList() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Name TypeConstraint ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
			if p.typeConstraint() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &TypeParamDeclNode{}
}

// TypeParamListNode represents the production
//
//	TypeParamList = TypeParamDecl { "," TypeParamDecl } .
type TypeParamListNode struct{ noder }

func (p *parser) typeParamList() Node {
	// ebnf.Sequence TypeParamDecl { "," TypeParamDecl } ctx []
	{
		ix := p.ix
		// *ebnf.Name TypeParamDecl ctx []
		switch p.c() {
		case IDENT:
			if p.typeParamDecl() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," TypeParamDecl } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," TypeParamDecl ctx [COMMA]
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			p.ix++
			p.budget--
			// *ebnf.Name TypeParamDecl ctx []
			switch p.c() {
			case IDENT:
				if p.typeParamDecl() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			goto _0
		}
		goto _1
	_1:
	}
	return &TypeParamListNode{}
}

// TypeParametersNode represents the production
//
//	TypeParameters = "[" TypeParamList [ "," ] "]" .
type TypeParametersNode struct{ noder }

func (p *parser) typeParameters() Node {
	// ebnf.Sequence "[" TypeParamList [ "," ] "]" ctx []
	{
		ix := p.ix
		// *ebnf.Token "[" ctx []
		if !p.accept(LBRACK) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name TypeParamList ctx []
		switch p.c() {
		case IDENT:
			if p.typeParamList() == nil {
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
			p.ix++
			p.budget--
		}
		goto _0
	_0:
		// *ebnf.Token "]" ctx []
		if !p.accept(RBRACK) {
			p.back(ix)
			return nil
		}
	}
	return &TypeParametersNode{}
}

// TypePreBlockNode represents the production
//
//	TypePreBlock = TypeLit | "(" Type ")" .
type TypePreBlockNode struct{ noder }

func (p *parser) typePreBlock() Node {
	// ebnf.Alternative TypeLit | "(" Type ")" ctx []
	switch p.c() {
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 0
		// *ebnf.Name TypeLit ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		if p.typeLit() == nil {
			return nil
		}
	case LPAREN: // 1
		// ebnf.Sequence "(" Type ")" ctx [LPAREN]
		{
			ix := p.ix
			// *ebnf.Token "(" ctx [LPAREN]
			p.ix++
			p.budget--
			// *ebnf.Name Type ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.type1() == nil {
					p.back(ix)
					return nil
				}
			default:
				p.back(ix)
				return nil
			}
			// *ebnf.Token ")" ctx []
			if !p.accept(RPAREN) {
				p.back(ix)
				return nil
			}
		}
	default:
		return nil
	}
	return &TypePreBlockNode{}
}

// TypeSpecNode represents the production
//
//	TypeSpec = AliasDecl | TypeDef .
type TypeSpecNode struct{ noder }

func (p *parser) typeSpec() Node {
	// ebnf.Alternative AliasDecl | TypeDef ctx []
	switch p.c() {
	case IDENT: // 0 1
		// *ebnf.Name AliasDecl ctx [IDENT]
		if p.aliasDecl() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// *ebnf.Name TypeDef ctx [IDENT]
		if p.typeDef() == nil {
			goto _1
		}
		break
		goto _1
	_1:
		return nil
	default:
		return nil
	}
	return &TypeSpecNode{}
}

// TypeSwitchCaseNode represents the production
//
//	TypeSwitchCase = "case" TypeList | "default" .
type TypeSwitchCaseNode struct{ noder }

func (p *parser) typeSwitchCase() Node {
	// ebnf.Alternative "case" TypeList | "default" ctx []
	switch p.c() {
	case CASE: // 0
		// ebnf.Sequence "case" TypeList ctx [CASE]
		{
			ix := p.ix
			// *ebnf.Token "case" ctx [CASE]
			p.ix++
			p.budget--
			// *ebnf.Name TypeList ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if p.typeList() == nil {
					p.back(ix)
					return nil
				}
			default:
				p.back(ix)
				return nil
			}
		}
	case DEFAULT: // 1
		// *ebnf.Token "default" ctx [DEFAULT]
		p.ix++
		p.budget--
	default:
		return nil
	}
	return &TypeSwitchCaseNode{}
}

// TypeSwitchGuardNode represents the production
//
//	TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" .
type TypeSwitchGuardNode struct{ noder }

func (p *parser) typeSwitchGuard() Node {
	// ebnf.Sequence [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" ctx []
	{
		ix := p.ix
		// *ebnf.Option [ identifier ":=" ] ctx []
		switch p.c() {
		case IDENT:
			// ebnf.Sequence identifier ":=" ctx [IDENT]
			{
				if p.peek(1) != DEFINE {
					goto _0
				}
				ix := p.ix
				// *ebnf.Name identifier ctx [IDENT]
				if !p.accept(IDENT) {
					p.back(ix)
					goto _0
				}
				// *ebnf.Token ":=" ctx []
				if !p.accept(DEFINE) {
					p.back(ix)
					goto _0
				}
			}
		}
		goto _0
	_0:
		// *ebnf.Name PrimaryExpr ctx []
		switch p.c() {
		case ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT:
			if p.primaryExpr() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "." ctx []
		if !p.accept(PERIOD) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "(" ctx []
		if !p.accept(LPAREN) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token "type" ctx []
		if !p.accept(TYPE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Token ")" ctx []
		if !p.accept(RPAREN) {
			p.back(ix)
			return nil
		}
	}
	return &TypeSwitchGuardNode{}
}

// TypeSwitchStmtNode represents the production
//
//	TypeSwitchStmt = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
type TypeSwitchStmtNode struct{ noder }

func (p *parser) typeSwitchStmt() Node {
	// ebnf.Sequence "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" ctx []
	{
		ix := p.ix
		// *ebnf.Token "switch" ctx []
		if !p.accept(SWITCH) {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ SimpleStmt ";" ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR:
			// ebnf.Sequence SimpleStmt ";" ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name SimpleStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR]
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if p.simpleStmt() == nil {
						p.back(ix)
						goto _0
					}
				default:
					p.back(ix)
					goto _0
				}
				// *ebnf.Token ";" ctx []
				if !p.accept(SEMICOLON) {
					p.back(ix)
					goto _0
				}
			}
		}
		goto _0
	_0:
		// *ebnf.Name TypeSwitchGuard ctx []
		switch p.c() {
		case ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT:
			if p.typeSwitchGuard() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Token "{" ctx []
		if !p.accept(LBRACE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { TypeCaseClause } ctx []
	_1:
		switch p.c() {
		case CASE, DEFAULT:
			// *ebnf.Name TypeCaseClause ctx [CASE, DEFAULT]
			if p.typeCaseClause() == nil {
				goto _2
			}
			goto _1
		}
		goto _2
	_2:
		// *ebnf.Token "}" ctx []
		if !p.accept(RBRACE) {
			p.back(ix)
			return nil
		}
	}
	return &TypeSwitchStmtNode{}
}

// TypeTermNode represents the production
//
//	TypeTerm = Type | UnderlyingType .
type TypeTermNode struct{ noder }

func (p *parser) typeTerm() Node {
	// ebnf.Alternative Type | UnderlyingType ctx []
	switch p.c() {
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 0
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if p.type1() == nil {
			return nil
		}
	case TILDE: // 1
		// *ebnf.Name UnderlyingType ctx [TILDE]
		if p.underlyingType() == nil {
			return nil
		}
	default:
		return nil
	}
	return &TypeTermNode{}
}

// UnaryExprNode represents the production
//
//	UnaryExpr = PrimaryExpr | ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr .
type UnaryExprNode struct{ noder }

func (p *parser) unaryExpr() Node {
	// ebnf.Alternative PrimaryExpr | ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx []
	switch p.c() {
	case CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT: // 0
		// *ebnf.Name PrimaryExpr ctx [CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT]
		if p.primaryExpr() == nil {
			return nil
		}
	case ARROW, MUL: // 0 1
		// *ebnf.Name PrimaryExpr ctx [ARROW, MUL]
		if p.primaryExpr() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// ebnf.Sequence ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx [ARROW, MUL]
		{
			ix := p.ix
			// *ebnf.Group ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) ctx [ARROW, MUL]
			// ebnf.Alternative "+" | "-" | "!" | "^" | "*" | "&" | "<-" ctx [ARROW, MUL]
			switch p.c() {
			case MUL: // 4
				// *ebnf.Token "*" ctx [MUL]
				p.ix++
				p.budget--
			case ARROW: // 6
				// *ebnf.Token "<-" ctx [ARROW]
				p.ix++
				p.budget--
			default:
				p.back(ix)
				goto _1
			}
			// *ebnf.Name UnaryExpr ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.unaryExpr() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
		}
		break
		goto _1
	_1:
		return nil
	case ADD, AND, NOT, SUB, XOR: // 1
		// ebnf.Sequence ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx [ADD, AND, NOT, SUB, XOR]
		{
			ix := p.ix
			// *ebnf.Group ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) ctx [ADD, AND, NOT, SUB, XOR]
			// ebnf.Alternative "+" | "-" | "!" | "^" | "*" | "&" | "<-" ctx [ADD, AND, NOT, SUB, XOR]
			switch p.c() {
			case ADD: // 0
				// *ebnf.Token "+" ctx [ADD]
				p.ix++
				p.budget--
			case SUB: // 1
				// *ebnf.Token "-" ctx [SUB]
				p.ix++
				p.budget--
			case NOT: // 2
				// *ebnf.Token "!" ctx [NOT]
				p.ix++
				p.budget--
			case XOR: // 3
				// *ebnf.Token "^" ctx [XOR]
				p.ix++
				p.budget--
			case AND: // 5
				// *ebnf.Token "&" ctx [AND]
				p.ix++
				p.budget--
			default:
				p.back(ix)
				return nil
			}
			// *ebnf.Name UnaryExpr ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.unaryExpr() == nil {
					p.back(ix)
					return nil
				}
			default:
				p.back(ix)
				return nil
			}
		}
	default:
		return nil
	}
	return &UnaryExprNode{}
}

// UnaryExprPreBlockNode represents the production
//
//	UnaryExprPreBlock = PrimaryExprPreBlock | ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExprPreBlock .
type UnaryExprPreBlockNode struct{ noder }

func (p *parser) unaryExprPreBlock() Node {
	// ebnf.Alternative PrimaryExprPreBlock | ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExprPreBlock ctx []
	switch p.c() {
	case CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT: // 0
		// *ebnf.Name PrimaryExprPreBlock ctx [CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT]
		if p.primaryExprPreBlock() == nil {
			return nil
		}
	case ARROW, MUL: // 0 1
		// *ebnf.Name PrimaryExprPreBlock ctx [ARROW, MUL]
		if p.primaryExprPreBlock() == nil {
			goto _0
		}
		break
		goto _0
	_0:
		// ebnf.Sequence ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExprPreBlock ctx [ARROW, MUL]
		{
			ix := p.ix
			// *ebnf.Group ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) ctx [ARROW, MUL]
			// ebnf.Alternative "+" | "-" | "!" | "^" | "*" | "&" | "<-" ctx [ARROW, MUL]
			switch p.c() {
			case MUL: // 4
				// *ebnf.Token "*" ctx [MUL]
				p.ix++
				p.budget--
			case ARROW: // 6
				// *ebnf.Token "<-" ctx [ARROW]
				p.ix++
				p.budget--
			default:
				p.back(ix)
				goto _1
			}
			// *ebnf.Name UnaryExprPreBlock ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.unaryExprPreBlock() == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
		}
		break
		goto _1
	_1:
		return nil
	case ADD, AND, NOT, SUB, XOR: // 1
		// ebnf.Sequence ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExprPreBlock ctx [ADD, AND, NOT, SUB, XOR]
		{
			ix := p.ix
			// *ebnf.Group ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) ctx [ADD, AND, NOT, SUB, XOR]
			// ebnf.Alternative "+" | "-" | "!" | "^" | "*" | "&" | "<-" ctx [ADD, AND, NOT, SUB, XOR]
			switch p.c() {
			case ADD: // 0
				// *ebnf.Token "+" ctx [ADD]
				p.ix++
				p.budget--
			case SUB: // 1
				// *ebnf.Token "-" ctx [SUB]
				p.ix++
				p.budget--
			case NOT: // 2
				// *ebnf.Token "!" ctx [NOT]
				p.ix++
				p.budget--
			case XOR: // 3
				// *ebnf.Token "^" ctx [XOR]
				p.ix++
				p.budget--
			case AND: // 5
				// *ebnf.Token "&" ctx [AND]
				p.ix++
				p.budget--
			default:
				p.back(ix)
				return nil
			}
			// *ebnf.Name UnaryExprPreBlock ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if p.unaryExprPreBlock() == nil {
					p.back(ix)
					return nil
				}
			default:
				p.back(ix)
				return nil
			}
		}
	default:
		return nil
	}
	return &UnaryExprPreBlockNode{}
}

// UnderlyingTypeNode represents the production
//
//	UnderlyingType = "~" Type .
type UnderlyingTypeNode struct{ noder }

func (p *parser) underlyingType() Node {
	// ebnf.Sequence "~" Type ctx []
	{
		ix := p.ix
		// *ebnf.Token "~" ctx []
		if !p.accept(TILDE) {
			p.back(ix)
			return nil
		}
		// *ebnf.Name Type ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			if p.type1() == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &UnderlyingTypeNode{}
}

// VarDeclNode represents the production
//
//	VarDecl = "var" ( VarSpec | "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ) .
type VarDeclNode struct{ noder }

func (p *parser) varDecl() Node {
	// ebnf.Sequence "var" ( VarSpec | "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ) ctx []
	{
		ix := p.ix
		// *ebnf.Token "var" ctx []
		if !p.accept(VAR) {
			p.back(ix)
			return nil
		}
		// *ebnf.Group ( VarSpec | "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ) ctx []
		// ebnf.Alternative VarSpec | "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name VarSpec ctx [IDENT]
			if p.varSpec() == nil {
				p.back(ix)
				return nil
			}
		case LPAREN: // 1
			// ebnf.Sequence "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				p.ix++
				p.budget--
				// *ebnf.Option [ VarSpec { ";" VarSpec } [ ";" ] ] ctx []
				switch p.c() {
				case IDENT:
					// ebnf.Sequence VarSpec { ";" VarSpec } [ ";" ] ctx [IDENT]
					{
						ix := p.ix
						// *ebnf.Name VarSpec ctx [IDENT]
						if p.varSpec() == nil {
							p.back(ix)
							goto _0
						}
						// *ebnf.Repetition { ";" VarSpec } ctx []
					_1:
						switch p.c() {
						case SEMICOLON:
							// ebnf.Sequence ";" VarSpec ctx [SEMICOLON]
							ix := p.ix
							// *ebnf.Token ";" ctx [SEMICOLON]
							p.ix++
							p.budget--
							// *ebnf.Name VarSpec ctx []
							switch p.c() {
							case IDENT:
								if p.varSpec() == nil {
									p.back(ix)
									goto _2
								}
							default:
								p.back(ix)
								goto _2
							}
							goto _1
						}
						goto _2
					_2:
						// *ebnf.Option [ ";" ] ctx []
						switch p.c() {
						case SEMICOLON:
							// *ebnf.Token ";" ctx [SEMICOLON]
							p.ix++
							p.budget--
						}
						goto _3
					_3:
					}
				}
				goto _0
			_0:
				// *ebnf.Token ")" ctx []
				if !p.accept(RPAREN) {
					p.back(ix)
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &VarDeclNode{}
}

// VarSpecNode represents the production
//
//	VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
type VarSpecNode struct{ noder }

func (p *parser) varSpec() Node {
	// ebnf.Sequence IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) ctx []
	{
		ix := p.ix
		// *ebnf.Name IdentifierList ctx []
		switch p.c() {
		case IDENT:
			if p.identifierList() == nil {
				p.back(ix)
				return nil
			}
		default:
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
				if p.type1() == nil {
					p.back(ix)
					p.back(ix)
					return nil
				}
				// *ebnf.Option [ "=" ExpressionList ] ctx []
				switch p.c() {
				case ASSIGN:
					// ebnf.Sequence "=" ExpressionList ctx [ASSIGN]
					{
						ix := p.ix
						// *ebnf.Token "=" ctx [ASSIGN]
						p.ix++
						p.budget--
						// *ebnf.Name ExpressionList ctx []
						switch p.c() {
						case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
							if p.expressionList() == nil {
								p.back(ix)
								goto _0
							}
						default:
							p.back(ix)
							goto _0
						}
					}
				}
				goto _0
			_0:
			}
		case ASSIGN: // 1
			// ebnf.Sequence "=" ExpressionList ctx [ASSIGN]
			{
				ix := p.ix
				// *ebnf.Token "=" ctx [ASSIGN]
				p.ix++
				p.budget--
				// *ebnf.Name ExpressionList ctx []
				switch p.c() {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
					if p.expressionList() == nil {
						p.back(ix)
						p.back(ix)
						return nil
					}
				default:
					p.back(ix)
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &VarSpecNode{}
}
