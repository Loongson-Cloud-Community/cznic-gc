// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/token"
	"modernc.org/mathutil"
	"runtime"
)

const parserBudget = 1e7

var (
	noBack    bool
	panicBack bool
)

type parser struct {
	a             *analyzer
	maxBackOrigin string
	maxBackRange  [2]int
	path          string
	s             *scanner

	backs   int
	budget  int
	ix      int
	maxBack int
	maxIx   int

	isClosed bool
	record   bool
}

func newParser(path string, src []byte, record bool) *parser {
	return &parser{
		a:      newAnalyzer(),
		budget: parserBudget,
		path:   path,
		record: record,
		s:      newScanner(path, src),
	}
}

func (p *parser) c() token.Token              { return p.peek(0) }
func (p *parser) errPosition() token.Position { return p.s.toks[p.maxIx].position(p.s.source) }
func (p *parser) pos() token.Position         { return p.s.toks[p.ix].position(p.s.source) }

func (p *parser) consume() (r Token) {
	r = p.s.token()
	p.ix++
	p.budget--
	return r
}

func (p *parser) accept(t token.Token) (r Token, _ bool) {
	if p.c() == t {
		r = p.s.token()
		p.ix++
		p.budget--
		return r, true
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
	p.maxIx = mathutil.Max(p.maxIx, p.ix+n)
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
	if p.isClosed {
		return
	}

	p.recordBacktrack(ix, true)
	if noBack {
		p.isClosed = true
	}
	if panicBack {
		panic(todo("%v: (%v:)", p.errPosition(), origin(2)))
	}
}

func (p *parser) parse() (err error) {
	if p.c() != PACKAGE {
		return errorf("%s: syntax error", p.errPosition())
	}

	ast := p.sourceFile()
	if p.budget <= 0 {
		return errorf("%s: resources exhausted", p.path)
	}

	if ast == nil || p.ix != len(p.s.toks)-1 || p.s.toks[p.ix].ch != int32(EOF) {
		return errorf("%s: syntax error", p.errPosition())
	}

	return nil
}

// AliasDeclNode represents the production
//
//	AliasDecl = identifier "=" Type .
type AliasDeclNode struct {
	IDENT  Token
	ASSIGN Token
	Type   *TypeNode
}

// Position implements Node.
func (n *AliasDeclNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) aliasDecl() *AliasDeclNode {
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
	return &AliasDeclNode{
		IDENT:  identTok,
		ASSIGN: assignTok,
		Type:   typeNode,
	}
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
						p.back(ix)
						goto _0
					}
				case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 0 1
					// *ebnf.Name ExpressionList ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
					if expressionList = p.expressionList(false); expressionList == nil {
						goto _1
					}
					break
				_1:
					// ebnf.Sequence Type [ "," ExpressionList ] ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
					{
						ix := p.ix
						// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
						if typeNode = p.type1(); typeNode == nil {
							p.back(ix)
							goto _2
						}
						// *ebnf.Option [ "," ExpressionList ] ctx []
						switch p.c() {
						case COMMA:
							// ebnf.Sequence "," ExpressionList ctx [COMMA]
							{
								switch p.peek(1) {
								case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
								default:
									goto _3
								}
								ix := p.ix
								// *ebnf.Token "," ctx [COMMA]
								commaTok = p.expect(COMMA)
								// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
								if expressionList2 = p.expressionList(false); expressionList2 == nil {
									p.back(ix)
									goto _3
								}
							}
						}
					_3:
					}
					break
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
	_0:
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
	Expression *ExpressionNode
}

// Position implements Node.
func (n *ArrayLengthNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) arrayLength() *ArrayLengthNode {
	var (
		expression *ExpressionNode
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
//	Assignment = ExpressionList ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" ) ExpressionList .
type AssignmentNode struct {
	ExpressionList  *ExpressionListNode
	Op              Token
	ExpressionList2 *ExpressionListNode
}

// Position implements Node.
func (n *AssignmentNode) Position() token.Position { return n.ExpressionList.Position() }

func (p *parser) assignment(expressionList *ExpressionListNode, preBlock bool) *AssignmentNode {
	var (
		tok             Token
		expressionList2 *ExpressionListNode
	)
	// ebnf.Sequence ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" ) ExpressionList ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
	{
		ix := p.ix
		// *ebnf.Group ( "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" ) ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
		// ebnf.Alternative "=" | "+=" | "-=" | "|=" | "^=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "&^=" ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
		tok = p.consume()
		// *ebnf.Name ExpressionList ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if expressionList2 = p.expressionList(preBlock); expressionList2 == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &AssignmentNode{
		ExpressionList:  expressionList,
		Op:              tok,
		ExpressionList2: expressionList2,
	}
}

// BaseTypeNode represents the production
//
//	BaseType = Type .
type BaseTypeNode struct {
	Type *TypeNode
}

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

// BlockNode represents the production
//
//	Block = "{" StatementList "}" .
type BlockNode struct {
	LBRACE        Token
	StatementList *StatementListNode
	RBRACE        Token
}

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
	_0:
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
	Expression *ExpressionNode
}

// Position implements Node.
func (n *ChannelNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) channel() *ChannelNode {
	var (
		expression *ExpressionNode
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
//	ChannelType = "<-" "chan" ElementType | "chan" "<-" ElementType | "chan" ElementType .
type ChannelTypeNode struct {
	ARROW       Token
	CHAN        Token
	ElementType *ElementTypeNode
}

// Position implements Node.
func (n *ChannelTypeNode) Position() token.Position {
	if n.ARROW.IsValid() {
		return n.ARROW.Position()
	}

	return n.CHAN.Position()
}

func (p *parser) channelType() *ChannelTypeNode {
	var (
		arrowTok    Token
		chanTok     Token
		elementType *ElementTypeNode
	)
	// ebnf.Alternative "<-" "chan" ElementType | "chan" "<-" ElementType | "chan" ElementType ctx [ARROW, CHAN]
	switch p.c() {
	case ARROW: // 0
		// ebnf.Sequence "<-" "chan" ElementType ctx [ARROW]
		{
			if p.peek(1) != CHAN {
				return nil
			}
			ix := p.ix
			// *ebnf.Token "<-" ctx [ARROW]
			arrowTok = p.expect(ARROW)
			// *ebnf.Token "chan" ctx [CHAN]
			chanTok = p.expect(CHAN)
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
	case CHAN: // 1 2
		// ebnf.Sequence "chan" "<-" ElementType ctx [CHAN]
		{
			if p.peek(1) != ARROW {
				goto _0
			}
			ix := p.ix
			// *ebnf.Token "chan" ctx [CHAN]
			chanTok = p.expect(CHAN)
			// *ebnf.Token "<-" ctx [ARROW]
			arrowTok = p.expect(ARROW)
			// *ebnf.Name ElementType ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if elementType = p.elementType(); elementType == nil {
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
		// ebnf.Sequence "chan" ElementType ctx [CHAN]
		{
			switch p.peek(1) {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			default:
				goto _1
			}
			ix := p.ix
			// *ebnf.Token "chan" ctx [CHAN]
			chanTok = p.expect(CHAN)
			// *ebnf.Name ElementType ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if elementType = p.elementType(); elementType == nil {
				p.back(ix)
				goto _1
			}
		}
		break
	_1:
		return nil
	default:
		return nil
	}
	return &ChannelTypeNode{
		ARROW:       arrowTok,
		CHAN:        chanTok,
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

// Position implements Node.
func (n *CommCaseNode) Position() token.Position { return n.CASE.Position() }

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
				return nil
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
					goto _0
				}
				break
			_0:
				// *ebnf.Name RecvStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if recvStmt = p.recvStmt(); recvStmt == nil {
					goto _1
				}
				break
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
	Expression *ExpressionNode
}

// Position implements Node.
func (n *ConditionNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) condition() *ConditionNode {
	var (
		expression *ExpressionNode
	)
	// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	if expression = p.expression(false); expression == nil {
		return nil
	}
	return &ConditionNode{
		Expression: expression,
	}
}

// ConstDeclNode represents the production
//
//	ConstDecl = "const" ( ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ) .
type ConstDeclNode struct {
	CONST      Token
	ConstSpec  *ConstSpecNode
	LPAREN     Token
	ConstSpec2 *ConstSpecNode
	List       []struct {
		SEMICOLON Token
		ConstSpec *ConstSpecNode
	}
	SEMICOLON2 Token
	RPAREN     Token
}

// Position implements Node.
func (n *ConstDeclNode) Position() token.Position { return n.CONST.Position() }

func (p *parser) constDecl() *ConstDeclNode {
	var (
		ok         bool
		constTok   Token
		constSpec  *ConstSpecNode
		lparenTok  Token
		constSpec2 *ConstSpecNode
		list       []struct {
			SEMICOLON Token
			ConstSpec *ConstSpecNode
		}
		semicolonTok  Token
		constSpec3    *ConstSpecNode
		semicolon2Tok Token
		rparenTok     Token
	)
	// ebnf.Sequence "const" ( ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ) ctx [CONST]
	{
		switch p.peek(1) {
		case IDENT, LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "const" ctx [CONST]
		constTok = p.expect(CONST)
		// *ebnf.Group ( ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ) ctx [IDENT, LPAREN]
		// ebnf.Alternative ConstSpec | "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name ConstSpec ctx [IDENT]
			if constSpec = p.constSpec(); constSpec == nil {
				p.back(ix)
				return nil
			}
		case LPAREN: // 1
			// ebnf.Sequence "(" [ ConstSpec { ";" ConstSpec } [ ";" ] ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				lparenTok = p.expect(LPAREN)
				// *ebnf.Option [ ConstSpec { ";" ConstSpec } [ ";" ] ] ctx []
				switch p.c() {
				case IDENT:
					// ebnf.Sequence ConstSpec { ";" ConstSpec } [ ";" ] ctx [IDENT]
					{
						ix := p.ix
						// *ebnf.Name ConstSpec ctx [IDENT]
						if constSpec2 = p.constSpec(); constSpec2 == nil {
							p.back(ix)
							goto _0
						}
						// *ebnf.Repetition { ";" ConstSpec } ctx []
					_1:
						switch p.c() {
						case SEMICOLON:
							// ebnf.Sequence ";" ConstSpec ctx [SEMICOLON]
							switch p.peek(1) {
							case IDENT:
							default:
								goto _2
							}
							ix := p.ix
							// *ebnf.Token ";" ctx [SEMICOLON]
							semicolonTok = p.expect(SEMICOLON)
							// *ebnf.Name ConstSpec ctx [IDENT]
							if constSpec3 = p.constSpec(); constSpec3 == nil {
								p.back(ix)
								goto _2
							}
							list = append(list, struct {
								SEMICOLON Token
								ConstSpec *ConstSpecNode
							}{semicolonTok, constSpec3})
							goto _1
						}
					_2:
						// *ebnf.Option [ ";" ] ctx []
						switch p.c() {
						case SEMICOLON:
							// *ebnf.Token ";" ctx [SEMICOLON]
							semicolon2Tok = p.expect(SEMICOLON)
						}
					}
				}
			_0:
				// *ebnf.Token ")" ctx []
				if rparenTok, ok = p.accept(RPAREN); !ok {
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ConstDeclNode{
		CONST:      constTok,
		ConstSpec:  constSpec,
		LPAREN:     lparenTok,
		ConstSpec2: constSpec2,
		List:       list,
		SEMICOLON2: semicolon2Tok,
		RPAREN:     rparenTok,
	}
}

// ConstSpecNode represents the production
//
//	ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
type ConstSpecNode struct {
	IdentifierList *IdentifierListNode
	Type           *TypeNode
	ASSIGN         Token
	ExpressionList *ExpressionListNode
}

// Position implements Node.
func (n *ConstSpecNode) Position() token.Position { return n.IdentifierList.Position() }

func (p *parser) constSpec() *ConstSpecNode {
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
						goto _1
					}
				}
			_1:
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
	_0:
	}
	return &ConstSpecNode{
		IdentifierList: identifierList,
		Type:           typeNode,
		ASSIGN:         assignTok,
		ExpressionList: expressionList,
	}
}

// ContinueStmtNode represents the production
//
//	ContinueStmt = "continue" [ Label ] .
type ContinueStmtNode struct {
	CONTINUE Token
	Label    *LabelNode
}

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
	_0:
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
	Expression *ExpressionNode
	COMMA      Token
	RPAREN     Token
}

// Position implements Node.
func (n *ConversionNode) Position() token.Position { return n.Type.Position() }

func (p *parser) conversion() *ConversionNode {
	var (
		ok         bool
		typeNode   *TypeNode
		lparenTok  Token
		expression *ExpressionNode
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

// Position implements Node.
func (n *DeclarationNode) Position() token.Position { return n.ConstDecl.Position() }

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
			return nil
		}
	case TYPE: // 1
		// *ebnf.Name TypeDecl ctx [TYPE]
		if typeDecl = p.typeDecl(); typeDecl == nil {
			return nil
		}
	case VAR: // 2
		// *ebnf.Name VarDecl ctx [VAR]
		if varDecl = p.varDecl(); varDecl == nil {
			return nil
		}
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
	Expression *ExpressionNode
}

// Position implements Node.
func (n *DeferStmtNode) Position() token.Position { return n.DEFER.Position() }

func (p *parser) deferStmt() *DeferStmtNode {
	var (
		deferTok   Token
		expression *ExpressionNode
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
	Expression   *ExpressionNode
	LiteralValue *LiteralValueNode
}

// Position implements Node.
func (n *ElementNode) Position() token.Position {
	if n.Expression != nil {
		return n.Expression.Position()
	}

	return n.LiteralValue.Position()
}

func (p *parser) element() *ElementNode {
	var (
		expression   *ExpressionNode
		literalValue *LiteralValueNode
	)
	// ebnf.Alternative Expression | LiteralValue ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	switch p.c() {
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 0
		// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if expression = p.expression(false); expression == nil {
			return nil
		}
	case LBRACE: // 1
		// *ebnf.Name LiteralValue ctx [LBRACE]
		if literalValue = p.literalValue(); literalValue == nil {
			return nil
		}
	default:
		return nil
	}
	return &ElementNode{
		Expression:   expression,
		LiteralValue: literalValue,
	}
}

// ElementListNode represents the production
//
//	ElementList = KeyedElement { "," KeyedElement } .
type ElementListNode struct {
	KeyedElement *KeyedElementNode
	List         []struct {
		COMMA        Token
		KeyedElement *KeyedElementNode
	}
}

// Position implements Node.
func (n *ElementListNode) Position() token.Position { return n.KeyedElement.Position() }

func (p *parser) elementList() *ElementListNode {
	var (
		keyedElement *KeyedElementNode
		list         []struct {
			COMMA        Token
			KeyedElement *KeyedElementNode
		}
		commaTok      Token
		keyedElement2 *KeyedElementNode
	)
	// ebnf.Sequence KeyedElement { "," KeyedElement } ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name KeyedElement ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if keyedElement = p.keyedElement(); keyedElement == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," KeyedElement } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," KeyedElement ctx [COMMA]
			switch p.peek(1) {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			default:
				goto _1
			}
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			commaTok = p.expect(COMMA)
			// *ebnf.Name KeyedElement ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if keyedElement2 = p.keyedElement(); keyedElement2 == nil {
				p.back(ix)
				goto _1
			}
			list = append(list, struct {
				COMMA        Token
				KeyedElement *KeyedElementNode
			}{commaTok, keyedElement2})
			goto _0
		}
	_1:
	}
	return &ElementListNode{
		KeyedElement: keyedElement,
		List:         list,
	}
}

// ElementTypeNode represents the production
//
//	ElementType = Type .
type ElementTypeNode struct {
	Type *TypeNode
}

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

// Position implements Node.
func (n *EmbeddedFieldNode) Position() token.Position {
	if n.MUL.IsValid() {
		return n.MUL.Position()
	}

	return n.TypeName.Position()
}

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
				goto _1
			}
		}
	_1:
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

// Position implements Node.
func (n *EmptyStmtNode) Position() (r token.Position) { return r }

func (p *parser) emptyStmt() *EmptyStmtNode {
	var ()
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

// Position implements Node.
func (n *ExprSwitchCaseNode) Position() token.Position { return n.CASE.Position() }

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
				return nil
			}
			ix := p.ix
			// *ebnf.Token "case" ctx [CASE]
			caseTok = p.expect(CASE)
			// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expressionList = p.expressionList(false); expressionList == nil {
				p.back(ix)
				return nil
			}
		}
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
//	ExprSwitchStmt = "switch" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" | "switch" SimpleStmt ";" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" .
type ExprSwitchStmtNode struct {
	SWITCH             Token
	ExpressionPreBlock *ExpressionNode
	LBRACE             Token
	List               []*ExprCaseClauseNode
	RBRACE             Token
	SimpleStmt         *SimpleStmtNode
	SEMICOLON          Token
}

// Position implements Node.
func (n *ExprSwitchStmtNode) Position() token.Position { return n.SWITCH.Position() }

func (p *parser) exprSwitchStmt() *ExprSwitchStmtNode {
	var (
		ok                 bool
		switchTok          Token
		expressionPreBlock *ExpressionNode
		lbraceTok          Token
		list               []*ExprCaseClauseNode
		exprCaseClause     *ExprCaseClauseNode
		rbraceTok          Token
		simpleStmt         *SimpleStmtNode
		semicolonTok       Token
	)
	// ebnf.Alternative "switch" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" | "switch" SimpleStmt ";" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" ctx [SWITCH]
	switch p.c() {
	case SWITCH: // 0 1
		// ebnf.Sequence "switch" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" ctx [SWITCH]
		{
			ix := p.ix
			// *ebnf.Token "switch" ctx [SWITCH]
			switchTok = p.expect(SWITCH)
			// *ebnf.Option [ ExpressionPreBlock ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name ExpressionPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expressionPreBlock = p.expression(true); expressionPreBlock == nil {
					goto _1
				}
			}
		_1:
			// *ebnf.Token "{" ctx []
			if lbraceTok, ok = p.accept(LBRACE); !ok {
				p.back(ix)
				goto _0
			}
			// *ebnf.Repetition { ExprCaseClause } ctx []
		_2:
			switch p.c() {
			case CASE, DEFAULT:
				// *ebnf.Name ExprCaseClause ctx [CASE, DEFAULT]
				if exprCaseClause = p.exprCaseClause(); exprCaseClause == nil {
					goto _3
				}
				list = append(list, exprCaseClause)
				goto _2
			}
		_3:
			// *ebnf.Token "}" ctx []
			if rbraceTok, ok = p.accept(RBRACE); !ok {
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
		// ebnf.Sequence "switch" SimpleStmt ";" [ ExpressionPreBlock ] "{" { ExprCaseClause } "}" ctx [SWITCH]
		{
			ix := p.ix
			// *ebnf.Token "switch" ctx [SWITCH]
			switchTok = p.expect(SWITCH)
			// *ebnf.Name SimpleStmt ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
				if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
					p.back(ix)
					goto _4
				}
			}
			// *ebnf.Token ";" ctx []
			if semicolonTok, ok = p.accept(SEMICOLON); !ok {
				p.back(ix)
				goto _4
			}
			// *ebnf.Option [ ExpressionPreBlock ] ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				// *ebnf.Name ExpressionPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expressionPreBlock = p.expression(true); expressionPreBlock == nil {
					goto _5
				}
			}
		_5:
			// *ebnf.Token "{" ctx []
			if lbraceTok, ok = p.accept(LBRACE); !ok {
				p.back(ix)
				goto _4
			}
			// *ebnf.Repetition { ExprCaseClause } ctx []
		_6:
			switch p.c() {
			case CASE, DEFAULT:
				// *ebnf.Name ExprCaseClause ctx [CASE, DEFAULT]
				if exprCaseClause = p.exprCaseClause(); exprCaseClause == nil {
					goto _7
				}
				goto _6
			}
		_7:
			// *ebnf.Token "}" ctx []
			if rbraceTok, ok = p.accept(RBRACE); !ok {
				p.back(ix)
				goto _4
			}
		}
		break
	_4:
		return nil
	default:
		return nil
	}
	return &ExprSwitchStmtNode{
		SWITCH:             switchTok,
		ExpressionPreBlock: expressionPreBlock,
		LBRACE:             lbraceTok,
		List:               list,
		RBRACE:             rbraceTok,
		SimpleStmt:         simpleStmt,
		SEMICOLON:          semicolonTok,
	}
}

// ExpressionNode represents the production
//
//	Expression = UnaryExpr { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) UnaryExpr } .
type ExpressionNode struct {
	UnaryExpr *UnaryExprNode
	List      []struct {
		Op        Token
		UnaryExpr *UnaryExprNode
	}
}

// Position implements Node.
func (n *ExpressionNode) Position() token.Position { return n.UnaryExpr.Position() }

func (p *parser) expression(preBlock bool) *ExpressionNode {
	var (
		unaryExpr *UnaryExprNode
		list      []struct {
			Op        Token
			UnaryExpr *UnaryExprNode
		}
		tok        Token
		unaryExpr2 *UnaryExprNode
	)
	// ebnf.Sequence UnaryExpr { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) UnaryExpr } ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Name UnaryExpr ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		if unaryExpr = p.unaryExpr(preBlock); unaryExpr == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) UnaryExpr } ctx []
	_0:
		switch p.c() {
		case ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR:
			// ebnf.Sequence ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) UnaryExpr ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			// *ebnf.Group ( "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			// ebnf.Alternative "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "|" | "^" | "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ctx [ADD, AND, AND_NOT, EQL, GEQ, GTR, LAND, LEQ, LOR, LSS, MUL, NEQ, OR, QUO, REM, SHL, SHR, SUB, XOR]
			tok = p.consume()
			// *ebnf.Name UnaryExpr ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if unaryExpr2 = p.unaryExpr(preBlock); unaryExpr2 == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
			list = append(list, struct {
				Op        Token
				UnaryExpr *UnaryExprNode
			}{tok, unaryExpr2})
			goto _0
		}
	_1:
	}
	return &ExpressionNode{
		UnaryExpr: unaryExpr,
		List:      list,
	}
}

// ExpressionListNode represents the production
//
//	ExpressionList = Expression { "," Expression } .
type ExpressionListNode struct {
	Expression *ExpressionNode
	List       []struct {
		COMMA      Token
		Expression *ExpressionNode
	}
}

// Position implements Node.
func (n *ExpressionListNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) expressionList(preBlock bool) *ExpressionListNode {
	var (
		expression *ExpressionNode
		list       []struct {
			COMMA      Token
			Expression *ExpressionNode
		}
		commaTok    Token
		expression2 *ExpressionNode
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
			if expression2 = p.expression(preBlock); expression2 == nil {
				p.back(ix)
				goto _1
			}
			list = append(list, struct {
				COMMA      Token
				Expression *ExpressionNode
			}{commaTok, expression2})
			goto _0
		}
	_1:
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

// Position implements Node.
func (n *FallthroughStmtNode) Position() token.Position { return n.FALLTHROUGH.Position() }

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

// Position implements Node.
func (n *FieldDeclNode) Position() token.Position {
	if n.IdentifierList != nil {
		return n.IdentifierList.Position()
	}

	return n.EmbeddedField.Position()
}

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
			// *ebnf.Name EmbeddedField ctx [IDENT]
			if embeddedField = p.embeddedField(); embeddedField == nil {
				goto _1
			}
			break
		_1:
			p.back(ix)
			return nil
		case MUL: // 1
			// *ebnf.Name EmbeddedField ctx [MUL]
			if embeddedField = p.embeddedField(); embeddedField == nil {
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
			if tag = p.tag(); tag == nil {
				goto _2
			}
		}
	_2:
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

// Position implements Node.
func (n *ForClauseNode) Position() token.Position {
	if n.InitStmt != nil {
		return n.InitStmt.Position()
	}

	return n.SEMICOLON.Position()
}

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
	_0:
		// *ebnf.Token ";" ctx []
		if semicolonTok, ok = p.accept(SEMICOLON); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Option [ Condition ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// *ebnf.Name Condition ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if condition = p.condition(); condition == nil {
				goto _1
			}
		}
	_1:
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
				goto _2
			}
		}
	_2:
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
//	ForStmt = "for" ( Block | ExpressionPreBlock Block | ForClause Block | RangeClause Block ) .
type ForStmtNode struct {
	FOR                Token
	Block              *BlockNode
	ExpressionPreBlock *ExpressionNode
	Block2             *BlockNode
	ForClause          *ForClauseNode
	Block3             *BlockNode
	RangeClause        *RangeClauseNode
	Block4             *BlockNode
}

// Position implements Node.
func (n *ForStmtNode) Position() token.Position { return n.FOR.Position() }

func (p *parser) forStmt() *ForStmtNode {
	var (
		forTok             Token
		block              *BlockNode
		expressionPreBlock *ExpressionNode
		block2             *BlockNode
		forClause          *ForClauseNode
		block3             *BlockNode
		rangeClause        *RangeClauseNode
		block4             *BlockNode
	)
	// ebnf.Sequence "for" ( Block | ExpressionPreBlock Block | ForClause Block | RangeClause Block ) ctx [FOR]
	{
		switch p.peek(1) {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, SEMICOLON, STRING, STRUCT, SUB, XOR:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "for" ctx [FOR]
		forTok = p.expect(FOR)
		// *ebnf.Group ( Block | ExpressionPreBlock Block | ForClause Block | RangeClause Block ) ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, SEMICOLON, STRING, STRUCT, SUB, XOR]
		// ebnf.Alternative Block | ExpressionPreBlock Block | ForClause Block | RangeClause Block ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, SEMICOLON, STRING, STRUCT, SUB, XOR]
		switch p.c() {
		case LBRACE: // 0
			// *ebnf.Name Block ctx [LBRACE]
			if block = p.block(); block == nil {
				p.back(ix)
				return nil
			}
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR: // 1 2 3
			// ebnf.Sequence ExpressionPreBlock Block ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name ExpressionPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expressionPreBlock = p.expression(true); expressionPreBlock == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Name Block ctx []
				switch p.c() {
				case LBRACE:
					if block2 = p.block(); block2 == nil {
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
			// ebnf.Sequence ForClause Block ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name ForClause ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if forClause = p.forClause(); forClause == nil {
					p.back(ix)
					goto _1
				}
				// *ebnf.Name Block ctx []
				switch p.c() {
				case LBRACE:
					if block3 = p.block(); block3 == nil {
						p.back(ix)
						goto _1
					}
				default:
					p.back(ix)
					goto _1
				}
			}
			break
		_1:
			// ebnf.Sequence RangeClause Block ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name RangeClause ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if rangeClause = p.rangeClause(); rangeClause == nil {
					p.back(ix)
					goto _2
				}
				// *ebnf.Name Block ctx []
				switch p.c() {
				case LBRACE:
					if block4 = p.block(); block4 == nil {
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
			p.back(ix)
			return nil
		case SEMICOLON: // 2
			// ebnf.Sequence ForClause Block ctx [SEMICOLON]
			{
				ix := p.ix
				// *ebnf.Name ForClause ctx [SEMICOLON]
				if forClause = p.forClause(); forClause == nil {
					p.back(ix)
					return nil
				}
				// *ebnf.Name Block ctx []
				switch p.c() {
				case LBRACE:
					if block3 = p.block(); block3 == nil {
						p.back(ix)
						return nil
					}
				default:
					p.back(ix)
					return nil
				}
			}
		case RANGE: // 3
			// ebnf.Sequence RangeClause Block ctx [RANGE]
			{
				ix := p.ix
				// *ebnf.Name RangeClause ctx [RANGE]
				if rangeClause = p.rangeClause(); rangeClause == nil {
					p.back(ix)
					return nil
				}
				// *ebnf.Name Block ctx []
				switch p.c() {
				case LBRACE:
					if block4 = p.block(); block4 == nil {
						p.back(ix)
						return nil
					}
				default:
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ForStmtNode{
		FOR:                forTok,
		Block:              block,
		ExpressionPreBlock: expressionPreBlock,
		Block2:             block2,
		ForClause:          forClause,
		Block3:             block3,
		RangeClause:        rangeClause,
		Block4:             block4,
	}
}

// FunctionBodyNode represents the production
//
//	FunctionBody = Block .
type FunctionBodyNode struct {
	Block *BlockNode
}

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
	FUNC           Token
	FunctionName   *FunctionNameNode
	TypeParameters *TypeParametersNode
	Signature      *SignatureNode
	FunctionBody   *FunctionBodyNode
}

// Position implements Node.
func (n *FunctionDeclNode) Position() token.Position { return n.FUNC.Position() }

func (p *parser) functionDecl() *FunctionDeclNode {
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
	_0:
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
				goto _1
			}
		}
	_1:
	}
	return &FunctionDeclNode{
		FUNC:           funcTok,
		FunctionName:   functionName,
		TypeParameters: typeParameters,
		Signature:      signature,
		FunctionBody:   functionBody,
	}
}

// FunctionLitNode represents the production
//
//	FunctionLit = "func" Signature FunctionBody .
type FunctionLitNode struct {
	FUNC         Token
	Signature    *SignatureNode
	FunctionBody *FunctionBodyNode
}

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
	Expression *ExpressionNode
}

// Position implements Node.
func (n *GoStmtNode) Position() token.Position { return n.GO.Position() }

func (p *parser) goStmt() *GoStmtNode {
	var (
		goTok      Token
		expression *ExpressionNode
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

// Position implements Node.
func (n *IdentifierListNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) identifierList() *IdentifierListNode {
	var (
		identTok Token
		list     []struct {
			COMMA Token
			IDENT Token
		}
		commaTok  Token
		ident2Tok Token
	)
	// ebnf.Sequence identifier { "," identifier } ctx [IDENT]
	{
		// *ebnf.Name identifier ctx [IDENT]
		identTok = p.expect(IDENT)
		// *ebnf.Repetition { "," identifier } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," identifier ctx [COMMA]
			if p.peek(1) != IDENT {
				goto _1
			}
			// *ebnf.Token "," ctx [COMMA]
			commaTok = p.expect(COMMA)
			// *ebnf.Name identifier ctx [IDENT]
			ident2Tok = p.expect(IDENT)
			list = append(list, struct {
				COMMA Token
				IDENT Token
			}{commaTok, ident2Tok})
			goto _0
		}
	_1:
	}
	return &IdentifierListNode{
		IDENT: identTok,
		List:  list,
	}
}

// IfStmtNode represents the production
//
//	IfStmt = "if" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] | "if" SimpleStmt ";" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] .
type IfStmtNode struct {
	IF                 Token
	ExpressionPreBlock *ExpressionNode
	Block              *BlockNode
	ELSE               Token
	IfStmt             *IfStmtNode
	Block2             *BlockNode
	SimpleStmt         *SimpleStmtNode
	SEMICOLON          Token
}

// Position implements Node.
func (n *IfStmtNode) Position() token.Position { return n.IF.Position() }

func (p *parser) ifStmt() *IfStmtNode {
	var (
		ok                 bool
		ifTok              Token
		expressionPreBlock *ExpressionNode
		block              *BlockNode
		elseTok            Token
		ifStmt             *IfStmtNode
		block2             *BlockNode
		simpleStmt         *SimpleStmtNode
		semicolonTok       Token
	)
	// ebnf.Alternative "if" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] | "if" SimpleStmt ";" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] ctx [IF]
	switch p.c() {
	case IF: // 0 1
		// ebnf.Sequence "if" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] ctx [IF]
		{
			switch p.peek(1) {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			default:
				goto _0
			}
			ix := p.ix
			// *ebnf.Token "if" ctx [IF]
			ifTok = p.expect(IF)
			// *ebnf.Name ExpressionPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expressionPreBlock = p.expression(true); expressionPreBlock == nil {
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
							p.back(ix)
							goto _1
						}
					case LBRACE: // 1
						// *ebnf.Name Block ctx [LBRACE]
						if block2 = p.block(); block2 == nil {
							p.back(ix)
							goto _1
						}
					default:
						p.back(ix)
						goto _1
					}
				}
			}
		_1:
		}
		break
	_0:
		// ebnf.Sequence "if" SimpleStmt ";" ExpressionPreBlock Block [ "else" ( IfStmt | Block ) ] ctx [IF]
		{
			ix := p.ix
			// *ebnf.Token "if" ctx [IF]
			ifTok = p.expect(IF)
			// *ebnf.Name SimpleStmt ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ :
				if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
					p.back(ix)
					goto _2
				}
			}
			// *ebnf.Token ";" ctx []
			if semicolonTok, ok = p.accept(SEMICOLON); !ok {
				p.back(ix)
				goto _2
			}
			// *ebnf.Name ExpressionPreBlock ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expressionPreBlock = p.expression(true); expressionPreBlock == nil {
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
				if block = p.block(); block == nil {
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
					switch p.peek(1) {
					case IF, LBRACE:
					default:
						goto _3
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
							p.back(ix)
							goto _3
						}
					case LBRACE: // 1
						// *ebnf.Name Block ctx [LBRACE]
						if block2 = p.block(); block2 == nil {
							p.back(ix)
							goto _3
						}
					default:
						p.back(ix)
						goto _3
					}
				}
			}
		_3:
		}
		break
	_2:
		return nil
	default:
		return nil
	}
	return &IfStmtNode{
		IF:                 ifTok,
		ExpressionPreBlock: expressionPreBlock,
		Block:              block,
		ELSE:               elseTok,
		IfStmt:             ifStmt,
		Block2:             block2,
		SimpleStmt:         simpleStmt,
		SEMICOLON:          semicolonTok,
	}
}

// ImportDeclNode represents the production
//
//	ImportDecl = "import" ( ImportSpec | "(" [ ImportSpec { ";" ImportSpec } [ ";" ] ] ")" ) .
type ImportDeclNode struct {
	IMPORT      Token
	ImportSpec  *ImportSpecNode
	LPAREN      Token
	ImportSpec2 *ImportSpecNode
	List        []struct {
		SEMICOLON  Token
		ImportSpec *ImportSpecNode
	}
	SEMICOLON2 Token
	RPAREN     Token
}

// Position implements Node.
func (n *ImportDeclNode) Position() token.Position { return n.IMPORT.Position() }

func (p *parser) importDecl() *ImportDeclNode {
	var (
		ok          bool
		importTok   Token
		importSpec  *ImportSpecNode
		lparenTok   Token
		importSpec2 *ImportSpecNode
		list        []struct {
			SEMICOLON  Token
			ImportSpec *ImportSpecNode
		}
		semicolonTok  Token
		importSpec3   *ImportSpecNode
		semicolon2Tok Token
		rparenTok     Token
	)
	// ebnf.Sequence "import" ( ImportSpec | "(" [ ImportSpec { ";" ImportSpec } [ ";" ] ] ")" ) ctx [IMPORT]
	{
		switch p.peek(1) {
		case IDENT, LPAREN, PERIOD, STRING:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "import" ctx [IMPORT]
		importTok = p.expect(IMPORT)
		// *ebnf.Group ( ImportSpec | "(" [ ImportSpec { ";" ImportSpec } [ ";" ] ] ")" ) ctx [IDENT, LPAREN, PERIOD, STRING]
		// ebnf.Alternative ImportSpec | "(" [ ImportSpec { ";" ImportSpec } [ ";" ] ] ")" ctx [IDENT, LPAREN, PERIOD, STRING]
		switch p.c() {
		case IDENT, PERIOD, STRING: // 0
			// *ebnf.Name ImportSpec ctx [IDENT, PERIOD, STRING]
			if importSpec = p.importSpec(); importSpec == nil {
				p.back(ix)
				return nil
			}
		case LPAREN: // 1
			// ebnf.Sequence "(" [ ImportSpec { ";" ImportSpec } [ ";" ] ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				lparenTok = p.expect(LPAREN)
				// *ebnf.Option [ ImportSpec { ";" ImportSpec } [ ";" ] ] ctx []
				switch p.c() {
				case IDENT, PERIOD, STRING:
					// ebnf.Sequence ImportSpec { ";" ImportSpec } [ ";" ] ctx [IDENT, PERIOD, STRING]
					{
						ix := p.ix
						// *ebnf.Name ImportSpec ctx [IDENT, PERIOD, STRING]
						if importSpec2 = p.importSpec(); importSpec2 == nil {
							p.back(ix)
							goto _0
						}
						// *ebnf.Repetition { ";" ImportSpec } ctx []
					_1:
						switch p.c() {
						case SEMICOLON:
							// ebnf.Sequence ";" ImportSpec ctx [SEMICOLON]
							switch p.peek(1) {
							case IDENT, PERIOD, STRING:
							default:
								goto _2
							}
							ix := p.ix
							// *ebnf.Token ";" ctx [SEMICOLON]
							semicolonTok = p.expect(SEMICOLON)
							// *ebnf.Name ImportSpec ctx [IDENT, PERIOD, STRING]
							if importSpec3 = p.importSpec(); importSpec3 == nil {
								p.back(ix)
								goto _2
							}
							list = append(list, struct {
								SEMICOLON  Token
								ImportSpec *ImportSpecNode
							}{semicolonTok, importSpec3})
							goto _1
						}
					_2:
						// *ebnf.Option [ ";" ] ctx []
						switch p.c() {
						case SEMICOLON:
							// *ebnf.Token ";" ctx [SEMICOLON]
							semicolon2Tok = p.expect(SEMICOLON)
						}
					}
				}
			_0:
				// *ebnf.Token ")" ctx []
				if rparenTok, ok = p.accept(RPAREN); !ok {
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &ImportDeclNode{
		IMPORT:      importTok,
		ImportSpec:  importSpec,
		LPAREN:      lparenTok,
		ImportSpec2: importSpec2,
		List:        list,
		SEMICOLON2:  semicolon2Tok,
		RPAREN:      rparenTok,
	}
}

// ImportPathNode represents the production
//
//	ImportPath = string_lit .
type ImportPathNode struct { //TODO-
	STRING Token
}

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

// Position implements Node.
func (n *ImportSpecNode) Position() token.Position {
	if n.PERIOD.IsValid() {
		return n.PERIOD.Position()
	}

	if n.PackageName != nil {
		return n.PackageName.Position()
	}

	return n.ImportPath.Position()
}

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
					goto _0
				}
			default:
				goto _0
			}
		}
	_0:
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
	Expression *ExpressionNode
	RBRACK     Token
}

// Position implements Node.
func (n *IndexNode) Position() token.Position { return n.LBRACK.Position() }

func (p *parser) index() *IndexNode {
	var (
		ok         bool
		lbrackTok  Token
		expression *ExpressionNode
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

// Position implements Node.
func (n *InitStmtNode) Position() token.Position { return n.SimpleStmt.Position() }

func (p *parser) initStmt() *InitStmtNode {
	var (
		simpleStmt *SimpleStmtNode
	)
	// *ebnf.Name SimpleStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
	if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
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

// Position implements Node.
func (n *InterfaceElemNode) Position() token.Position {
	if n.MethodElem != nil {
		return n.MethodElem.Position()
	}

	return n.TypeElem.Position()
}

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
		// *ebnf.Name TypeElem ctx [IDENT]
		if typeElem = p.typeElem(); typeElem == nil {
			goto _1
		}
		break
	_1:
		return nil
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE: // 1
		// *ebnf.Name TypeElem ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
		if typeElem = p.typeElem(); typeElem == nil {
			return nil
		}
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
//	InterfaceType = "interface" "{" [ InterfaceElem { ";" InterfaceElem } [ ";" ] ] "}" .
type InterfaceTypeNode struct {
	INTERFACE     Token
	LBRACE        Token
	InterfaceElem *InterfaceElemNode
	List          []struct {
		SEMICOLON     Token
		InterfaceElem *InterfaceElemNode
	}
	SEMICOLON2 Token
	RBRACE     Token
}

// Position implements Node.
func (n *InterfaceTypeNode) Position() token.Position { return n.INTERFACE.Position() }

func (p *parser) interfaceType() *InterfaceTypeNode {
	var (
		ok            bool
		interfaceTok  Token
		lbraceTok     Token
		interfaceElem *InterfaceElemNode
		list          []struct {
			SEMICOLON     Token
			InterfaceElem *InterfaceElemNode
		}
		semicolonTok   Token
		interfaceElem2 *InterfaceElemNode
		semicolon2Tok  Token
		rbraceTok      Token
	)
	// ebnf.Sequence "interface" "{" [ InterfaceElem { ";" InterfaceElem } [ ";" ] ] "}" ctx [INTERFACE]
	{
		if p.peek(1) != LBRACE {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "interface" ctx [INTERFACE]
		interfaceTok = p.expect(INTERFACE)
		// *ebnf.Token "{" ctx [LBRACE]
		lbraceTok = p.expect(LBRACE)
		// *ebnf.Option [ InterfaceElem { ";" InterfaceElem } [ ";" ] ] ctx []
		switch p.c() {
		case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
			// ebnf.Sequence InterfaceElem { ";" InterfaceElem } [ ";" ] ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
			{
				ix := p.ix
				// *ebnf.Name InterfaceElem ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
				if interfaceElem = p.interfaceElem(); interfaceElem == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Repetition { ";" InterfaceElem } ctx []
			_1:
				switch p.c() {
				case SEMICOLON:
					// ebnf.Sequence ";" InterfaceElem ctx [SEMICOLON]
					switch p.peek(1) {
					case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE:
					default:
						goto _2
					}
					ix := p.ix
					// *ebnf.Token ";" ctx [SEMICOLON]
					semicolonTok = p.expect(SEMICOLON)
					// *ebnf.Name InterfaceElem ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE]
					if interfaceElem2 = p.interfaceElem(); interfaceElem2 == nil {
						p.back(ix)
						goto _2
					}
					list = append(list, struct {
						SEMICOLON     Token
						InterfaceElem *InterfaceElemNode
					}{semicolonTok, interfaceElem2})
					goto _1
				}
			_2:
				// *ebnf.Option [ ";" ] ctx []
				switch p.c() {
				case SEMICOLON:
					// *ebnf.Token ";" ctx [SEMICOLON]
					semicolon2Tok = p.expect(SEMICOLON)
				}
			}
		}
	_0:
		// *ebnf.Token "}" ctx []
		if rbraceTok, ok = p.accept(RBRACE); !ok {
			p.back(ix)
			return nil
		}
	}
	return &InterfaceTypeNode{
		INTERFACE:     interfaceTok,
		LBRACE:        lbraceTok,
		InterfaceElem: interfaceElem,
		List:          list,
		SEMICOLON2:    semicolon2Tok,
		RBRACE:        rbraceTok,
	}
}

// KeyTypeNode represents the production
//
//	KeyType = Type .
type KeyTypeNode struct {
	Type *TypeNode
}

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
	_0:
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
	Statement *StatementNode
}

// Position implements Node.
func (n *LabeledStmtNode) Position() token.Position { return n.Label.Position() }

func (p *parser) labeledStmt() *LabeledStmtNode {
	var (
		label     *LabelNode
		colonTok  Token
		statement *StatementNode
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

// LiteralNode represents the production
//
//	Literal = BasicLit | CompositeLit | FunctionLit .
type LiteralNode struct { //TODO-
	BasicLit     Token
	CompositeLit *CompositeLitNode
	FunctionLit  *FunctionLitNode
}

// Position implements Node.
func (n *LiteralNode) Position() token.Position {
	if n.BasicLit.IsValid() {
		return n.BasicLit.Position()
	}

	if n.CompositeLit != nil {
		return n.CompositeLit.Position()
	}

	return n.FunctionLit.Position()
}

func (p *parser) literal() *LiteralNode {
	var (
		basicLit     Token
		compositeLit *CompositeLitNode
		functionLit  *FunctionLitNode
	)
	// ebnf.Alternative BasicLit | CompositeLit | FunctionLit ctx [CHAR, FLOAT, FUNC, IMAG, INT, LBRACK, MAP, STRING, STRUCT]
	switch p.c() {
	case CHAR, FLOAT, IMAG, INT, STRING: // 0
		// *ebnf.Name BasicLit ctx [CHAR, FLOAT, IMAG, INT, STRING]
		basicLit = p.consume()
	case LBRACK, MAP, STRUCT: // 1
		// *ebnf.Name CompositeLit ctx [LBRACK, MAP, STRUCT]
		if compositeLit = p.compositeLit(); compositeLit == nil {
			return nil
		}
	case FUNC: // 2
		// *ebnf.Name FunctionLit ctx [FUNC]
		if functionLit = p.functionLit(); functionLit == nil {
			return nil
		}
	default:
		return nil
	}
	return &LiteralNode{
		BasicLit:     basicLit,
		CompositeLit: compositeLit,
		FunctionLit:  functionLit,
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

// Position implements Node.
func (n *LiteralTypeNode) Position() token.Position {
	if n.StructType != nil {
		return n.StructType.Position()
	}

	if n.ArrayType != nil {
		return n.ArrayType.Position()
	}

	if n.LBRACK.IsValid() {
		return n.LBRACK.Position()
	}

	if n.SliceType != nil {
		return n.SliceType.Position()
	}

	return n.MapType.Position()
}

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
			return nil
		}
	case LBRACK: // 1 2 3
		// *ebnf.Name ArrayType ctx [LBRACK]
		if arrayType = p.arrayType(); arrayType == nil {
			goto _0
		}
		break
	_0:
		// ebnf.Sequence "[" "..." "]" ElementType ctx [LBRACK]
		{
			if p.peek(1) != ELLIPSIS {
				goto _1
			}
			ix := p.ix
			// *ebnf.Token "[" ctx [LBRACK]
			lbrackTok = p.expect(LBRACK)
			// *ebnf.Token "..." ctx [ELLIPSIS]
			ellipsisTok = p.expect(ELLIPSIS)
			// *ebnf.Token "]" ctx []
			if rbrackTok, ok = p.accept(RBRACK); !ok {
				p.back(ix)
				goto _1
			}
			// *ebnf.Name ElementType ctx []
			switch p.c() {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
				if elementType = p.elementType(); elementType == nil {
					p.back(ix)
					goto _1
				}
			default:
				p.back(ix)
				goto _1
			}
		}
		break
	_1:
		// *ebnf.Name SliceType ctx [LBRACK]
		if sliceType = p.sliceType(); sliceType == nil {
			goto _2
		}
		break
	_2:
		return nil
	case MAP: // 4
		// *ebnf.Name MapType ctx [MAP]
		if mapType = p.mapType(); mapType == nil {
			return nil
		}
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
//	LiteralValue = "{" [ ElementList [ "," ] ] "}" .
type LiteralValueNode struct {
	LBRACE      Token
	ElementList *ElementListNode
	COMMA       Token
	RBRACE      Token
}

// Position implements Node.
func (n *LiteralValueNode) Position() token.Position { return n.LBRACE.Position() }

func (p *parser) literalValue() *LiteralValueNode {
	var (
		ok          bool
		lbraceTok   Token
		elementList *ElementListNode
		commaTok    Token
		rbraceTok   Token
	)
	// ebnf.Sequence "{" [ ElementList [ "," ] ] "}" ctx [LBRACE]
	{
		ix := p.ix
		// *ebnf.Token "{" ctx [LBRACE]
		lbraceTok = p.expect(LBRACE)
		// *ebnf.Option [ ElementList [ "," ] ] ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// ebnf.Sequence ElementList [ "," ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name ElementList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if elementList = p.elementList(); elementList == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Option [ "," ] ctx []
				switch p.c() {
				case COMMA:
					// *ebnf.Token "," ctx [COMMA]
					commaTok = p.expect(COMMA)
				}
			}
		}
	_0:
		// *ebnf.Token "}" ctx []
		if rbraceTok, ok = p.accept(RBRACE); !ok {
			p.back(ix)
			return nil
		}
	}
	return &LiteralValueNode{
		LBRACE:      lbraceTok,
		ElementList: elementList,
		COMMA:       commaTok,
		RBRACE:      rbraceTok,
	}
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
	_0:
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

// OperandNode represents the production
//
//	Operand = Literal | OperandName [ TypeArgs ] [ LiteralValue ] | "(" Expression ")" .
type OperandNode struct {
	Literal      *LiteralNode
	OperandName  *OperandNameNode
	TypeArgs     *TypeArgsNode
	LiteralValue *LiteralValueNode
	LPAREN       Token
	Expression   *ExpressionNode
	RPAREN       Token
}

// Position implements Node.
func (n *OperandNode) Position() token.Position {
	if n.Literal != nil {
		return n.Literal.Position()
	}

	if n.OperandName != nil {
		return n.OperandName.Position()
	}

	return n.LPAREN.Position()
}

func (p *parser) operand(preBlock bool) *OperandNode {
	var (
		ok           bool
		literal      *LiteralNode
		operandName  *OperandNameNode
		typeArgs     *TypeArgsNode
		literalValue *LiteralValueNode
		lparenTok    Token
		expression   *ExpressionNode
		rparenTok    Token
	)
	// ebnf.Alternative Literal | OperandName [ TypeArgs ] [ LiteralValue ] | "(" Expression ")" ctx [CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, LPAREN, MAP, STRING, STRUCT]
	switch p.c() {
	case CHAR, FLOAT, FUNC, IMAG, INT, LBRACK, MAP, STRING, STRUCT: // 0
		// *ebnf.Name Literal ctx [CHAR, FLOAT, FUNC, IMAG, INT, LBRACK, MAP, STRING, STRUCT]
		if literal = p.literal(); literal == nil {
			return nil
		}
	case IDENT: // 1
		// ebnf.Sequence OperandName [ TypeArgs ] [ LiteralValue ] ctx [IDENT]
		{
			ix := p.ix
			// *ebnf.Name OperandName ctx [IDENT]
			if operandName = p.operandName(); operandName == nil {
				p.back(ix)
				return nil
			}
			// *ebnf.Option [ TypeArgs ] ctx []
			switch p.c() {
			case LBRACK:
				// *ebnf.Name TypeArgs ctx [LBRACK]
				if typeArgs = p.typeArgs(); typeArgs == nil {
					goto _0
				}
			}
		_0:
			if !preBlock {
				// *ebnf.Option [ LiteralValue ] ctx []
				switch p.c() {
				case LBRACE:
					// *ebnf.Name LiteralValue ctx [LBRACE]
					if literalValue = p.literalValue(); literalValue == nil {
						goto _1
					}
				}
			}
		_1:
		}
	case LPAREN: // 2
		// ebnf.Sequence "(" Expression ")" ctx [LPAREN]
		{
			switch p.peek(1) {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			default:
				return nil
			}
			ix := p.ix
			// *ebnf.Token "(" ctx [LPAREN]
			lparenTok = p.expect(LPAREN)
			// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expression = p.expression(false); expression == nil {
				p.back(ix)
				return nil
			}
			// *ebnf.Token ")" ctx []
			if rparenTok, ok = p.accept(RPAREN); !ok {
				p.back(ix)
				return nil
			}
		}
	default:
		return nil
	}
	return &OperandNode{
		Literal:      literal,
		OperandName:  operandName,
		TypeArgs:     typeArgs,
		LiteralValue: literalValue,
		LPAREN:       lparenTok,
		Expression:   expression,
		RPAREN:       rparenTok,
	}
}

// OperandNameNode represents the production
//
//	OperandName = QualifiedIdent .
type OperandNameNode struct {
	QualifiedIdent *QualifiedIdentNode
}

// Position implements Node.
func (n *OperandNameNode) Position() token.Position { return n.QualifiedIdent.Position() }

func (p *parser) operandName() *OperandNameNode {
	var (
		qualifiedIdent *QualifiedIdentNode
	)
	// *ebnf.Name QualifiedIdent ctx [IDENT]
	if qualifiedIdent = p.qualifiedIdent(); qualifiedIdent == nil {
		return nil
	}
	return &OperandNameNode{
		QualifiedIdent: qualifiedIdent,
	}
}

// PackageClauseNode represents the production
//
//	PackageClause = "package" PackageName .
type PackageClauseNode struct {
	PACKAGE     Token
	PackageName *PackageNameNode
}

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
type PackageNameNode struct { //TODO-
	IDENT Token
}

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

// Position implements Node.
func (n *ParameterDeclNode) Position() token.Position {
	if n.IDENT.IsValid() {
		return n.IDENT.Position()
	}

	if n.ELLIPSIS.IsValid() {
		return n.ELLIPSIS.Position()
	}

	return n.Type.Position()
}

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
		// *ebnf.Name Type ctx [IDENT]
		if typeNode = p.type1(); typeNode == nil {
			goto _2
		}
		break
	_2:
		return nil
	case ELLIPSIS: // 2
		// ebnf.Sequence "..." Type ctx [ELLIPSIS]
		{
			switch p.peek(1) {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			default:
				return nil
			}
			ix := p.ix
			// *ebnf.Token "..." ctx [ELLIPSIS]
			ellipsisTok = p.expect(ELLIPSIS)
			// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if typeNode = p.type1(); typeNode == nil {
				p.back(ix)
				return nil
			}
		}
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT: // 3
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if typeNode = p.type1(); typeNode == nil {
			return nil
		}
	default:
		return nil
	}
	return &ParameterDeclNode{
		IDENT:    identTok,
		ELLIPSIS: ellipsisTok,
		Type:     typeNode,
	}
}

// ParameterListNode represents the production
//
//	ParameterList = ParameterDecl { "," ParameterDecl } .
type ParameterListNode struct {
	ParameterDecl *ParameterDeclNode
	List          []struct {
		COMMA         Token
		ParameterDecl *ParameterDeclNode
	}
}

// Position implements Node.
func (n *ParameterListNode) Position() token.Position { return n.ParameterDecl.Position() }

func (p *parser) parameterList() *ParameterListNode {
	var (
		parameterDecl *ParameterDeclNode
		list          []struct {
			COMMA         Token
			ParameterDecl *ParameterDeclNode
		}
		commaTok       Token
		parameterDecl2 *ParameterDeclNode
	)
	// ebnf.Sequence ParameterDecl { "," ParameterDecl } ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
	{
		ix := p.ix
		// *ebnf.Name ParameterDecl ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
		if parameterDecl = p.parameterDecl(); parameterDecl == nil {
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { "," ParameterDecl } ctx []
	_0:
		switch p.c() {
		case COMMA:
			// ebnf.Sequence "," ParameterDecl ctx [COMMA]
			switch p.peek(1) {
			case ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			default:
				goto _1
			}
			ix := p.ix
			// *ebnf.Token "," ctx [COMMA]
			commaTok = p.expect(COMMA)
			// *ebnf.Name ParameterDecl ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if parameterDecl2 = p.parameterDecl(); parameterDecl2 == nil {
				p.back(ix)
				goto _1
			}
			list = append(list, struct {
				COMMA         Token
				ParameterDecl *ParameterDeclNode
			}{commaTok, parameterDecl2})
			goto _0
		}
	_1:
	}
	return &ParameterListNode{
		ParameterDecl: parameterDecl,
		List:          list,
	}
}

// ParametersNode represents the production
//
//	Parameters = "(" [ ParameterList [ "," ] ] ")" .
type ParametersNode struct {
	LPAREN        Token
	ParameterList *ParameterListNode
	COMMA         Token
	RPAREN        Token
}

// Position implements Node.
func (n *ParametersNode) Position() token.Position { return n.LPAREN.Position() }

func (p *parser) parameters() *ParametersNode {
	var (
		ok            bool
		lparenTok     Token
		parameterList *ParameterListNode
		commaTok      Token
		rparenTok     Token
	)
	// ebnf.Sequence "(" [ ParameterList [ "," ] ] ")" ctx [LPAREN]
	{
		ix := p.ix
		// *ebnf.Token "(" ctx [LPAREN]
		lparenTok = p.expect(LPAREN)
		// *ebnf.Option [ ParameterList [ "," ] ] ctx []
		switch p.c() {
		case ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			// ebnf.Sequence ParameterList [ "," ] ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			{
				ix := p.ix
				// *ebnf.Name ParameterList ctx [ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
				if parameterList = p.parameterList(); parameterList == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Option [ "," ] ctx []
				switch p.c() {
				case COMMA:
					// *ebnf.Token "," ctx [COMMA]
					commaTok = p.expect(COMMA)
				}
			}
		}
	_0:
		// *ebnf.Token ")" ctx []
		if rparenTok, ok = p.accept(RPAREN); !ok {
			p.back(ix)
			return nil
		}
	}
	return &ParametersNode{
		LPAREN:        lparenTok,
		ParameterList: parameterList,
		COMMA:         commaTok,
		RPAREN:        rparenTok,
	}
}

// PointerTypeNode represents the production
//
//	PointerType = "*" BaseType .
type PointerTypeNode struct {
	MUL      Token
	BaseType *BaseTypeNode
}

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
//	PostStmt = SimpleStmtPreBlock .
type PostStmtNode struct {
	SimpleStmtPreBlock *SimpleStmtNode
}

// Position implements Node.
func (n *PostStmtNode) Position() token.Position { return n.SimpleStmtPreBlock.Position() }

func (p *parser) postStmt() *PostStmtNode {
	var (
		simpleStmtPreBlock *SimpleStmtNode
	)
	// *ebnf.Name SimpleStmtPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
	if simpleStmtPreBlock = p.simpleStmt(true); simpleStmtPreBlock == nil {
		return nil
	}
	return &PostStmtNode{
		SimpleStmtPreBlock: simpleStmtPreBlock,
	}
}

// PrimaryExprNode represents the production
//
//	PrimaryExpr = ( Operand | Conversion | MethodExpr ) { Selector | Index | Slice | TypeAssertion | Arguments } .
type PrimaryExprNode struct {
	Operand    *OperandNode
	Conversion *ConversionNode
	MethodExpr *MethodExprNode
	List       []struct {
		Selector      *SelectorNode
		Index         *IndexNode
		Slice         *SliceNode
		TypeAssertion *TypeAssertionNode
		Arguments     *ArgumentsNode
	}
}

// Position implements Node.
func (n *PrimaryExprNode) Position() token.Position {
	if n.Operand != nil {
		return n.Operand.Position()
	}

	if n.Conversion != nil {
		return n.Conversion.Position()
	}

	return n.MethodExpr.Position()
}

func (p *parser) primaryExpr(preBlock bool) *PrimaryExprNode {
	var (
		operand    *OperandNode
		conversion *ConversionNode
		methodExpr *MethodExprNode
		list       []struct {
			Selector      *SelectorNode
			Index         *IndexNode
			Slice         *SliceNode
			TypeAssertion *TypeAssertionNode
			Arguments     *ArgumentsNode
		}
		selector      *SelectorNode
		index         *IndexNode
		slice         *SliceNode
		typeAssertion *TypeAssertionNode
		arguments     *ArgumentsNode
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
				p.back(ix)
				return nil
			}
		case FUNC, IDENT, LBRACK, LPAREN, MAP, STRUCT: // 0 1 2
			// *ebnf.Name Operand ctx [FUNC, IDENT, LBRACK, LPAREN, MAP, STRUCT]
			if operand = p.operand(preBlock); operand == nil {
				goto _0
			}
			break
		_0:
			// *ebnf.Name Conversion ctx [FUNC, IDENT, LBRACK, LPAREN, MAP, STRUCT]
			if conversion = p.conversion(); conversion == nil {
				goto _1
			}
			break
		_1:
			// *ebnf.Name MethodExpr ctx [FUNC, IDENT, LBRACK, LPAREN, MAP, STRUCT]
			if methodExpr = p.methodExpr(); methodExpr == nil {
				goto _2
			}
			break
		_2:
			p.back(ix)
			return nil
		case ARROW, CHAN, INTERFACE, MUL: // 1 2
			// *ebnf.Name Conversion ctx [ARROW, CHAN, INTERFACE, MUL]
			if conversion = p.conversion(); conversion == nil {
				goto _3
			}
			break
		_3:
			// *ebnf.Name MethodExpr ctx [ARROW, CHAN, INTERFACE, MUL]
			if methodExpr = p.methodExpr(); methodExpr == nil {
				goto _4
			}
			break
		_4:
			p.back(ix)
			return nil
		default:
			p.back(ix)
			return nil
		}
		// *ebnf.Repetition { Selector | Index | Slice | TypeAssertion | Arguments } ctx []
	_5:
		switch p.c() {
		case LBRACK, LPAREN, PERIOD:
			// ebnf.Alternative Selector | Index | Slice | TypeAssertion | Arguments ctx [LBRACK, LPAREN, PERIOD]
			switch p.c() {
			case PERIOD: // 0 3
				// *ebnf.Name Selector ctx [PERIOD]
				if selector = p.selector(); selector == nil {
					goto _7
				}
				break
			_7:
				// *ebnf.Name TypeAssertion ctx [PERIOD]
				if typeAssertion = p.typeAssertion(); typeAssertion == nil {
					goto _8
				}
				break
			_8:
				goto _6
			case LBRACK: // 1 2
				// *ebnf.Name Index ctx [LBRACK]
				if index = p.index(); index == nil {
					goto _9
				}
				break
			_9:
				// *ebnf.Name Slice ctx [LBRACK]
				if slice = p.slice(); slice == nil {
					goto _10
				}
				break
			_10:
				goto _6
			case LPAREN: // 4
				// *ebnf.Name Arguments ctx [LPAREN]
				if arguments = p.arguments(); arguments == nil {
					goto _6
				}
			default:
				goto _6
			}
			list = append(list, struct {
				Selector      *SelectorNode
				Index         *IndexNode
				Slice         *SliceNode
				TypeAssertion *TypeAssertionNode
				Arguments     *ArgumentsNode
			}{selector, index, slice, typeAssertion, arguments},
			)
			goto _5
		}
	_6:
	}
	return &PrimaryExprNode{
		Operand:    operand,
		Conversion: conversion,
		MethodExpr: methodExpr,
		List:       list,
	}
}

// QualifiedIdentNode represents the production
//
//	QualifiedIdent = PackageName [ "." identifier ] .
type QualifiedIdentNode struct {
	PackageName *PackageNameNode
	PERIOD      Token
	IDENT       Token
}

// Position implements Node.
func (n *QualifiedIdentNode) Position() token.Position { return n.PackageName.Position() }

func (p *parser) qualifiedIdent() *QualifiedIdentNode {
	var (
		packageName *PackageNameNode
		periodTok   Token
		identTok    Token
	)
	// ebnf.Sequence PackageName [ "." identifier ] ctx [IDENT]
	{
		ix := p.ix
		// *ebnf.Name PackageName ctx [IDENT]
		if packageName = p.packageName(); packageName == nil {
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
				// *ebnf.Token "." ctx [PERIOD]
				periodTok = p.expect(PERIOD)
				// *ebnf.Name identifier ctx [IDENT]
				identTok = p.expect(IDENT)
			}
		}
	_0:
	}
	return &QualifiedIdentNode{
		PackageName: packageName,
		PERIOD:      periodTok,
		IDENT:       identTok,
	}
}

// RangeClauseNode represents the production
//
//	RangeClause = [ ExpressionList ( "=" | ":=" ) ] "range" ExpressionPreBlock .
type RangeClauseNode struct {
	ExpressionList     *ExpressionListNode
	ASSIGN             Token
	DEFINE             Token
	RANGE              Token
	ExpressionPreBlock *ExpressionNode
}

// Position implements Node.
func (n *RangeClauseNode) Position() token.Position {
	if n.ExpressionList != nil {
		return n.ExpressionList.Position()
	}

	return n.RANGE.Position()
}

func (p *parser) rangeClause() *RangeClauseNode {
	var (
		ok                 bool
		expressionList     *ExpressionListNode
		assignTok          Token
		defineTok          Token
		rangeTok           Token
		expressionPreBlock *ExpressionNode
	)
	// ebnf.Sequence [ ExpressionList ( "=" | ":=" ) ] "range" ExpressionPreBlock ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Option [ ExpressionList ( "=" | ":=" ) ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, STRING, STRUCT, SUB, XOR]
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			// ebnf.Sequence ExpressionList ( "=" | ":=" ) ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			{
				ix := p.ix
				// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expressionList = p.expressionList(false); expressionList == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Group ( "=" | ":=" ) ctx []
				// ebnf.Alternative "=" | ":=" ctx [ASSIGN, DEFINE]
				switch p.c() {
				case ASSIGN: // 0
					// *ebnf.Token "=" ctx [ASSIGN]
					assignTok = p.expect(ASSIGN)
				case DEFINE: // 1
					// *ebnf.Token ":=" ctx [DEFINE]
					defineTok = p.expect(DEFINE)
				default:
					p.back(ix)
					goto _0
				}
			}
		}
	_0:
		// *ebnf.Token "range" ctx []
		if rangeTok, ok = p.accept(RANGE); !ok {
			p.back(ix)
			return nil
		}
		// *ebnf.Name ExpressionPreBlock ctx []
		switch p.c() {
		case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
			if expressionPreBlock = p.expression(true); expressionPreBlock == nil {
				p.back(ix)
				return nil
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &RangeClauseNode{
		ExpressionList:     expressionList,
		ASSIGN:             assignTok,
		DEFINE:             defineTok,
		RANGE:              rangeTok,
		ExpressionPreBlock: expressionPreBlock,
	}
}

// ReceiverNode represents the production
//
//	Receiver = Parameters .
type ReceiverNode struct {
	Parameters *ParametersNode
}

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
	Expression *ExpressionNode
}

// Position implements Node.
func (n *RecvExprNode) Position() token.Position { return n.Expression.Position() }

func (p *parser) recvExpr() *RecvExprNode {
	var (
		expression *ExpressionNode
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
//	RecvStmt = [ ExpressionList ( "=" | ":=" ) ] RecvExpr .
type RecvStmtNode struct {
	ExpressionList *ExpressionListNode
	ASSIGN         Token
	DEFINE         Token
	RecvExpr       *RecvExprNode
}

// Position implements Node.
func (n *RecvStmtNode) Position() token.Position {
	if n.ExpressionList != nil {
		return n.ExpressionList.Position()
	}

	return n.RecvExpr.Position()
}

func (p *parser) recvStmt() *RecvStmtNode {
	var (
		expressionList *ExpressionListNode
		assignTok      Token
		defineTok      Token
		recvExpr       *RecvExprNode
	)
	// ebnf.Sequence [ ExpressionList ( "=" | ":=" ) ] RecvExpr ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	{
		ix := p.ix
		// *ebnf.Option [ ExpressionList ( "=" | ":=" ) ] ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		// ebnf.Sequence ExpressionList ( "=" | ":=" ) ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
		{
			ix := p.ix
			// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
			if expressionList = p.expressionList(false); expressionList == nil {
				p.back(ix)
			}
			// *ebnf.Group ( "=" | ":=" ) ctx []
			// ebnf.Alternative "=" | ":=" ctx [ASSIGN, DEFINE]
			switch p.c() {
			case ASSIGN: // 0
				// *ebnf.Token "=" ctx [ASSIGN]
				assignTok = p.expect(ASSIGN)
			case DEFINE: // 1
				// *ebnf.Token ":=" ctx [DEFINE]
				defineTok = p.expect(DEFINE)
			default:
				p.back(ix)
			}
		}
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
		DEFINE:         defineTok,
		RecvExpr:       recvExpr,
	}
}

// ResultNode represents the production
//
//	Result = Parameters | Type .
type ResultNode struct {
	Parameters *ParametersNode
	Type       *TypeNode
}

// Position implements Node.
func (n *ResultNode) Position() token.Position {
	if n.Parameters != nil {
		return n.Parameters.Position()
	}

	return n.Type.Position()
}

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
		// *ebnf.Name Type ctx [LPAREN]
		if typeNode = p.type1(); typeNode == nil {
			goto _1
		}
		break
	_1:
		return nil
	case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 1
		// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		if typeNode = p.type1(); typeNode == nil {
			return nil
		}
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
	_0:
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
	List   []*CommClauseNode
	RBRACE Token
}

// Position implements Node.
func (n *SelectStmtNode) Position() token.Position { return n.SELECT.Position() }

func (p *parser) selectStmt() *SelectStmtNode {
	var (
		ok         bool
		selectTok  Token
		lbraceTok  Token
		list       []*CommClauseNode
		commClause *CommClauseNode
		rbraceTok  Token
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
		switch p.c() {
		case CASE, DEFAULT:
			// *ebnf.Name CommClause ctx [CASE, DEFAULT]
			if commClause = p.commClause(); commClause == nil {
				goto _1
			}
			list = append(list, commClause)
			goto _0
		}
	_1:
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
	Expression *ExpressionNode
}

// Position implements Node.
func (n *SendStmtNode) Position() token.Position { return n.Channel.Position() }

func (p *parser) sendStmt() *SendStmtNode {
	var (
		ok         bool
		channel    *ChannelNode
		arrowTok   Token
		expression *ExpressionNode
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
//	ShortVarDecl = ":=" ExpressionList .
type ShortVarDeclNode struct {
	DEFINE         Token
	ExpressionList *ExpressionListNode
}

// Position implements Node.
func (n *ShortVarDeclNode) Position() token.Position { return n.DEFINE.Position() }

func (p *parser) shortVarDecl(preBlock bool) *ShortVarDeclNode {
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
	return &ShortVarDeclNode{
		DEFINE:         defineTok,
		ExpressionList: expressionList,
	}
}

// SignatureNode represents the production
//
//	Signature = Parameters [ Result ] .
type SignatureNode struct {
	Parameters *ParametersNode
	Result     *ResultNode
}

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
	_0:
	}
	return &SignatureNode{
		Parameters: parameters,
		Result:     result,
	}
}

// SimpleStmtNode represents the production
//
//	SimpleStmt = Assignment | ExpressionList [ ShortVarDecl | "<-" Expression | "++" | "--" ] | EmptyStmt .
type SimpleStmtNode struct {
	ExpressionList *ExpressionListNode
	Assignment     *AssignmentNode
	ShortVarDecl   *ShortVarDeclNode
	ARROW          Token
	Expression     *ExpressionNode
	INC            Token
	DEC            Token
	EmptyStmt      *EmptyStmtNode
}

// Position implements Node.
func (n *SimpleStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) simpleStmt(preBlock bool) *SimpleStmtNode {
	var (
		expressionList *ExpressionListNode
		assignment     *AssignmentNode
		shortVarDecl   *ShortVarDeclNode
		arrowTok       Token
		expression     *ExpressionNode
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
				return nil
			}
			// *ebnf.Option [ Assignment | ShortVarDecl | "<-" Expression | "++" | "--" ] ctx []
			switch p.c() {
			case ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ARROW, ASSIGN, DEC, DEFINE, INC, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
				// ebnf.Alternative Assignment | ShortVarDecl | "<-" Expression | "++" | "--" ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ARROW, ASSIGN, DEC, DEFINE, INC, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
				switch p.c() {
				case ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN: // 0
					// *ebnf.Name Assignment ctx [ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN]
					if assignment = p.assignment(expressionList, preBlock); assignment == nil {
						goto _0
					}
				case DEFINE: // 1
					// *ebnf.Name ShortVarDecl ctx [DEFINE]
					if shortVarDecl = p.shortVarDecl(preBlock); shortVarDecl == nil {
						goto _0
					}
				case ARROW: // 2
					// ebnf.Sequence "<-" Expression ctx [ARROW]
					{
						switch p.peek(1) {
						case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
						default:
							goto _0
						}
						ix := p.ix
						// *ebnf.Token "<-" ctx [ARROW]
						arrowTok = p.expect(ARROW)
						// *ebnf.Name Expression ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
						if expression = p.expression(preBlock); expression == nil {
							p.back(ix)
							goto _0
						}
					}
				case INC: // 3
					// *ebnf.Token "++" ctx [INC]
					incTok = p.expect(INC)
				case DEC: // 4
					// *ebnf.Token "--" ctx [DEC]
					decTok = p.expect(DEC)
				default:
					goto _0
				}
			}
		_0:
		}
	default: //  /* ε */ 1
		// *ebnf.Name EmptyStmt ctx [ /* ε */]
		if emptyStmt = p.emptyStmt(); emptyStmt == nil {
			return nil
		}
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
	Expression  *ExpressionNode
	COLON       Token
	Expression2 *ExpressionNode
	RBRACK      Token
	COLON2      Token
	Expression3 *ExpressionNode
}

// Position implements Node.
func (n *SliceNode) Position() token.Position { panic("TODO") }

func (p *parser) slice() *SliceNode {
	var (
		ok          bool
		lbrackTok   Token
		expression  *ExpressionNode
		colonTok    Token
		expression2 *ExpressionNode
		rbrackTok   Token
		colon2Tok   Token
		expression3 *ExpressionNode
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
		_1:
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
					goto _2
				}
			}
		_2:
			// *ebnf.Token "]" ctx []
			if rbrackTok, ok = p.accept(RBRACK); !ok {
				p.back(ix)
				goto _0
			}
		}
		break
	_0:
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
					goto _4
				}
			}
		_4:
			// *ebnf.Token ":" ctx []
			if colonTok, ok = p.accept(COLON); !ok {
				p.back(ix)
				goto _3
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expression2 = p.expression(false); expression2 == nil {
					p.back(ix)
					goto _3
				}
			default:
				p.back(ix)
				goto _3
			}
			// *ebnf.Token ":" ctx []
			if colon2Tok, ok = p.accept(COLON); !ok {
				p.back(ix)
				goto _3
			}
			// *ebnf.Name Expression ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if expression3 = p.expression(false); expression3 == nil {
					p.back(ix)
					goto _3
				}
			default:
				p.back(ix)
				goto _3
			}
			// *ebnf.Token "]" ctx []
			if rbrackTok, ok = p.accept(RBRACK); !ok {
				p.back(ix)
				goto _3
			}
		}
		break
	_3:
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
		importDecl    *ImportDeclNode
		semicolon2Tok Token
		list2         []struct {
			TopLevelDecl *TopLevelDeclNode
			SEMICOLON    Token
		}
		topLevelDecl  *TopLevelDeclNode
		semicolon3Tok Token
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
			if semicolon2Tok, ok = p.accept(SEMICOLON); !ok {
				p.back(ix)
				goto _1
			}
			list = append(list, struct {
				ImportDecl *ImportDeclNode
				SEMICOLON  Token
			}{importDecl, semicolon2Tok})
			goto _0
		}
	_1:
		// *ebnf.Repetition { TopLevelDecl ";" } ctx []
	_2:
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
			if semicolon3Tok, ok = p.accept(SEMICOLON); !ok {
				p.back(ix)
				goto _3
			}
			list2 = append(list2, struct {
				TopLevelDecl *TopLevelDeclNode
				SEMICOLON    Token
			}{topLevelDecl, semicolon3Tok})
			goto _2
		}
	_3:
	}
	return &SourceFileNode{
		PackageClause: packageClause,
		SEMICOLON:     semicolonTok,
		List:          list,
		List2:         list2,
	}
}

// StatementNode represents the production
//
//	Statement = Declaration | LabeledStmt | GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt | DeferStmt | SimpleStmt .
type StatementNode struct {
	Declaration     *DeclarationNode
	LabeledStmt     *LabeledStmtNode
	GoStmt          *GoStmtNode
	ReturnStmt      *ReturnStmtNode
	BreakStmt       *BreakStmtNode
	ContinueStmt    *ContinueStmtNode
	GotoStmt        *GotoStmtNode
	FallthroughStmt *FallthroughStmtNode
	Block           *BlockNode
	IfStmt          *IfStmtNode
	SwitchStmt      *SwitchStmtNode
	SelectStmt      *SelectStmtNode
	ForStmt         *ForStmtNode
	DeferStmt       *DeferStmtNode
	SimpleStmt      *SimpleStmtNode
}

// Position implements Node.
func (n *StatementNode) Position() token.Position { panic("TODO") }

func (p *parser) statement() *StatementNode {
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
	case IDENT: // 1 14
		// *ebnf.Name LabeledStmt ctx [IDENT]
		if labeledStmt = p.labeledStmt(); labeledStmt == nil {
			goto _0
		}
		break
	_0:
		// *ebnf.Name SimpleStmt ctx [IDENT]
		if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
			goto _1
		}
		break
	_1:
		return nil
	case GO: // 2
		// *ebnf.Name GoStmt ctx [GO]
		if goStmt = p.goStmt(); goStmt == nil {
			return nil
		}
	case RETURN: // 3
		// *ebnf.Name ReturnStmt ctx [RETURN]
		if returnStmt = p.returnStmt(); returnStmt == nil {
			return nil
		}
	case BREAK: // 4
		// *ebnf.Name BreakStmt ctx [BREAK]
		if breakStmt = p.breakStmt(); breakStmt == nil {
			return nil
		}
	case CONTINUE: // 5
		// *ebnf.Name ContinueStmt ctx [CONTINUE]
		if continueStmt = p.continueStmt(); continueStmt == nil {
			return nil
		}
	case GOTO: // 6
		// *ebnf.Name GotoStmt ctx [GOTO]
		if gotoStmt = p.gotoStmt(); gotoStmt == nil {
			return nil
		}
	case FALLTHROUGH: // 7
		// *ebnf.Name FallthroughStmt ctx [FALLTHROUGH]
		if fallthroughStmt = p.fallthroughStmt(); fallthroughStmt == nil {
			return nil
		}
	case LBRACE: // 8
		// *ebnf.Name Block ctx [LBRACE]
		if block = p.block(); block == nil {
			return nil
		}
	case IF: // 9
		// *ebnf.Name IfStmt ctx [IF]
		if ifStmt = p.ifStmt(); ifStmt == nil {
			return nil
		}
	case SWITCH: // 10
		// *ebnf.Name SwitchStmt ctx [SWITCH]
		if switchStmt = p.switchStmt(); switchStmt == nil {
			return nil
		}
	case SELECT: // 11
		// *ebnf.Name SelectStmt ctx [SELECT]
		if selectStmt = p.selectStmt(); selectStmt == nil {
			return nil
		}
	case FOR: // 12
		// *ebnf.Name ForStmt ctx [FOR]
		if forStmt = p.forStmt(); forStmt == nil {
			return nil
		}
	case DEFER: // 13
		// *ebnf.Name DeferStmt ctx [DEFER]
		if deferStmt = p.deferStmt(); deferStmt == nil {
			return nil
		}
	case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */ : // 14
		// *ebnf.Name SimpleStmt ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */]
		if simpleStmt = p.simpleStmt(false); simpleStmt == nil {
			return nil
		}
	}
	return &StatementNode{
		Declaration:     declaration,
		LabeledStmt:     labeledStmt,
		GoStmt:          goStmt,
		ReturnStmt:      returnStmt,
		BreakStmt:       breakStmt,
		ContinueStmt:    continueStmt,
		GotoStmt:        gotoStmt,
		FallthroughStmt: fallthroughStmt,
		Block:           block,
		IfStmt:          ifStmt,
		SwitchStmt:      switchStmt,
		SelectStmt:      selectStmt,
		ForStmt:         forStmt,
		DeferStmt:       deferStmt,
		SimpleStmt:      simpleStmt,
	}
}

// StatementListNode represents the production
//
//	StatementList = [ Statement { ";" Statement } [ ";" ] ] .
type StatementListNode struct {
	Statement *StatementNode
	List      []struct {
		SEMICOLON Token
		Statement *StatementNode
	}
	SEMICOLON2 Token
}

// Position implements Node.
func (n *StatementListNode) Position() token.Position { panic("TODO") }

func (p *parser) statementList() *StatementListNode {
	var (
		statement *StatementNode
		list      []struct {
			SEMICOLON Token
			Statement *StatementNode
		}
		semicolonTok  Token
		statement2    *StatementNode
		semicolon2Tok Token
	)
	// *ebnf.Option [ Statement { ";" Statement } [ ";" ] ] ctx [ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */]
	// ebnf.Sequence Statement { ";" Statement } [ ";" ] ctx [ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */]
	{
		ix := p.ix
		// *ebnf.Name Statement ctx [ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */]
		switch p.c() {
		case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
			if statement = p.statement(); statement == nil {
				p.back(ix)
			}
		}
		// *ebnf.Repetition { ";" Statement } ctx []
	_0:
		switch p.c() {
		case SEMICOLON:
			// ebnf.Sequence ";" Statement ctx [SEMICOLON]
			ix := p.ix
			// *ebnf.Token ";" ctx [SEMICOLON]
			semicolonTok = p.expect(SEMICOLON)
			// *ebnf.Name Statement ctx []
			switch p.c() {
			case ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */ :
				if statement2 = p.statement(); statement2 == nil {
					p.back(ix)
					goto _1
				}
			}
			list = append(list, struct {
				SEMICOLON Token
				Statement *StatementNode
			}{semicolonTok, statement2})
			goto _0
		}
	_1:
		// *ebnf.Option [ ";" ] ctx []
		switch p.c() {
		case SEMICOLON:
			// *ebnf.Token ";" ctx [SEMICOLON]
			semicolon2Tok = p.expect(SEMICOLON)
		}
	}
	return &StatementListNode{
		Statement:  statement,
		List:       list,
		SEMICOLON2: semicolon2Tok,
	}
}

// StructTypeNode represents the production
//
//	StructType = "struct" "{" [ FieldDecl { ";" FieldDecl } [ ";" ] ] "}" .
type StructTypeNode struct {
	STRUCT    Token
	LBRACE    Token
	FieldDecl *FieldDeclNode
	List      []struct {
		SEMICOLON Token
		FieldDecl *FieldDeclNode
	}
	SEMICOLON2 Token
	RBRACE     Token
}

// Position implements Node.
func (n *StructTypeNode) Position() token.Position { return n.STRUCT.Position() }

func (p *parser) structType() *StructTypeNode {
	var (
		ok        bool
		structTok Token
		lbraceTok Token
		fieldDecl *FieldDeclNode
		list      []struct {
			SEMICOLON Token
			FieldDecl *FieldDeclNode
		}
		semicolonTok  Token
		fieldDecl2    *FieldDeclNode
		semicolon2Tok Token
		rbraceTok     Token
	)
	// ebnf.Sequence "struct" "{" [ FieldDecl { ";" FieldDecl } [ ";" ] ] "}" ctx [STRUCT]
	{
		if p.peek(1) != LBRACE {
			return nil
		}
		ix := p.ix
		// *ebnf.Token "struct" ctx [STRUCT]
		structTok = p.expect(STRUCT)
		// *ebnf.Token "{" ctx [LBRACE]
		lbraceTok = p.expect(LBRACE)
		// *ebnf.Option [ FieldDecl { ";" FieldDecl } [ ";" ] ] ctx []
		switch p.c() {
		case IDENT, MUL:
			// ebnf.Sequence FieldDecl { ";" FieldDecl } [ ";" ] ctx [IDENT, MUL]
			{
				ix := p.ix
				// *ebnf.Name FieldDecl ctx [IDENT, MUL]
				if fieldDecl = p.fieldDecl(); fieldDecl == nil {
					p.back(ix)
					goto _0
				}
				// *ebnf.Repetition { ";" FieldDecl } ctx []
			_1:
				switch p.c() {
				case SEMICOLON:
					// ebnf.Sequence ";" FieldDecl ctx [SEMICOLON]
					switch p.peek(1) {
					case IDENT, MUL:
					default:
						goto _2
					}
					ix := p.ix
					// *ebnf.Token ";" ctx [SEMICOLON]
					semicolonTok = p.expect(SEMICOLON)
					// *ebnf.Name FieldDecl ctx [IDENT, MUL]
					if fieldDecl2 = p.fieldDecl(); fieldDecl2 == nil {
						p.back(ix)
						goto _2
					}
					list = append(list, struct {
						SEMICOLON Token
						FieldDecl *FieldDeclNode
					}{semicolonTok, fieldDecl2})
					goto _1
				}
			_2:
				// *ebnf.Option [ ";" ] ctx []
				switch p.c() {
				case SEMICOLON:
					// *ebnf.Token ";" ctx [SEMICOLON]
					semicolon2Tok = p.expect(SEMICOLON)
				}
			}
		}
	_0:
		// *ebnf.Token "}" ctx []
		if rbraceTok, ok = p.accept(RBRACE); !ok {
			p.back(ix)
			return nil
		}
	}
	return &StructTypeNode{
		STRUCT:     structTok,
		LBRACE:     lbraceTok,
		FieldDecl:  fieldDecl,
		List:       list,
		SEMICOLON2: semicolon2Tok,
		RBRACE:     rbraceTok,
	}
}

// SwitchStmtNode represents the production
//
//	SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
type SwitchStmtNode struct {
	ExprSwitchStmt *ExprSwitchStmtNode
	TypeSwitchStmt *TypeSwitchStmtNode
}

// Position implements Node.
func (n *SwitchStmtNode) Position() token.Position { panic("TODO") }

func (p *parser) switchStmt() *SwitchStmtNode {
	var (
		exprSwitchStmt *ExprSwitchStmtNode
		typeSwitchStmt *TypeSwitchStmtNode
	)
	// ebnf.Alternative ExprSwitchStmt | TypeSwitchStmt ctx [SWITCH]
	switch p.c() {
	case SWITCH: // 0 1
		// *ebnf.Name ExprSwitchStmt ctx [SWITCH]
		if exprSwitchStmt = p.exprSwitchStmt(); exprSwitchStmt == nil {
			goto _0
		}
		break
	_0:
		// *ebnf.Name TypeSwitchStmt ctx [SWITCH]
		if typeSwitchStmt = p.typeSwitchStmt(); typeSwitchStmt == nil {
			goto _1
		}
		break
	_1:
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
			return nil
		}
	case FUNC: // 1 2
		// *ebnf.Name FunctionDecl ctx [FUNC]
		if functionDecl = p.functionDecl(); functionDecl == nil {
			goto _0
		}
		break
	_0:
		// *ebnf.Name MethodDecl ctx [FUNC]
		if methodDecl = p.methodDecl(); methodDecl == nil {
			goto _1
		}
		break
	_1:
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
				return nil
			}
			// *ebnf.Option [ TypeArgs ] ctx []
			switch p.c() {
			case LBRACK:
				// *ebnf.Name TypeArgs ctx [LBRACK]
				if typeArgs = p.typeArgs(); typeArgs == nil {
					goto _0
				}
			}
		_0:
		}
	case ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT: // 1
		// *ebnf.Name TypeLit ctx [ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT]
		if typeLit = p.typeLit(); typeLit == nil {
			return nil
		}
	case LPAREN: // 2
		// ebnf.Sequence "(" Type ")" ctx [LPAREN]
		{
			switch p.peek(1) {
			case ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT:
			default:
				return nil
			}
			ix := p.ix
			// *ebnf.Token "(" ctx [LPAREN]
			lparenTok = p.expect(LPAREN)
			// *ebnf.Name Type ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if typeNode = p.type1(); typeNode == nil {
				p.back(ix)
				return nil
			}
			// *ebnf.Token ")" ctx []
			if rparenTok, ok = p.accept(RPAREN); !ok {
				p.back(ix)
				return nil
			}
		}
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
//	TypeDecl = "type" ( TypeSpec | "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ) .
type TypeDeclNode struct {
	TYPE      Token
	TypeSpec  *TypeSpecNode
	LPAREN    Token
	TypeSpec2 *TypeSpecNode
	List      []struct {
		SEMICOLON Token
		TypeSpec  *TypeSpecNode
	}
	SEMICOLON2 Token
	RPAREN     Token
}

// Position implements Node.
func (n *TypeDeclNode) Position() token.Position { return n.TYPE.Position() }

func (p *parser) typeDecl() *TypeDeclNode {
	var (
		ok        bool
		typeTok   Token
		typeSpec  *TypeSpecNode
		lparenTok Token
		typeSpec2 *TypeSpecNode
		list      []struct {
			SEMICOLON Token
			TypeSpec  *TypeSpecNode
		}
		semicolonTok  Token
		typeSpec3     *TypeSpecNode
		semicolon2Tok Token
		rparenTok     Token
	)
	// ebnf.Sequence "type" ( TypeSpec | "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ) ctx [TYPE]
	{
		switch p.peek(1) {
		case IDENT, LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "type" ctx [TYPE]
		typeTok = p.expect(TYPE)
		// *ebnf.Group ( TypeSpec | "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ) ctx [IDENT, LPAREN]
		// ebnf.Alternative TypeSpec | "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name TypeSpec ctx [IDENT]
			if typeSpec = p.typeSpec(); typeSpec == nil {
				p.back(ix)
				return nil
			}
		case LPAREN: // 1
			// ebnf.Sequence "(" [ TypeSpec { ";" TypeSpec } [ ";" ] ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				lparenTok = p.expect(LPAREN)
				// *ebnf.Option [ TypeSpec { ";" TypeSpec } [ ";" ] ] ctx []
				switch p.c() {
				case IDENT:
					// ebnf.Sequence TypeSpec { ";" TypeSpec } [ ";" ] ctx [IDENT]
					{
						ix := p.ix
						// *ebnf.Name TypeSpec ctx [IDENT]
						if typeSpec2 = p.typeSpec(); typeSpec2 == nil {
							p.back(ix)
							goto _0
						}
						// *ebnf.Repetition { ";" TypeSpec } ctx []
					_1:
						switch p.c() {
						case SEMICOLON:
							// ebnf.Sequence ";" TypeSpec ctx [SEMICOLON]
							switch p.peek(1) {
							case IDENT:
							default:
								goto _2
							}
							ix := p.ix
							// *ebnf.Token ";" ctx [SEMICOLON]
							semicolonTok = p.expect(SEMICOLON)
							// *ebnf.Name TypeSpec ctx [IDENT]
							if typeSpec3 = p.typeSpec(); typeSpec3 == nil {
								p.back(ix)
								goto _2
							}
							list = append(list, struct {
								SEMICOLON Token
								TypeSpec  *TypeSpecNode
							}{semicolonTok, typeSpec3})
							goto _1
						}
					_2:
						// *ebnf.Option [ ";" ] ctx []
						switch p.c() {
						case SEMICOLON:
							// *ebnf.Token ";" ctx [SEMICOLON]
							semicolon2Tok = p.expect(SEMICOLON)
						}
					}
				}
			_0:
				// *ebnf.Token ")" ctx []
				if rparenTok, ok = p.accept(RPAREN); !ok {
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &TypeDeclNode{
		TYPE:       typeTok,
		TypeSpec:   typeSpec,
		LPAREN:     lparenTok,
		TypeSpec2:  typeSpec2,
		List:       list,
		SEMICOLON2: semicolon2Tok,
		RPAREN:     rparenTok,
	}
}

// TypeDefNode represents the production
//
//	TypeDef = identifier [ TypeParameters ] Type .
type TypeDefNode struct {
	IDENT          Token
	TypeParameters *TypeParametersNode
	Type           *TypeNode
}

// Position implements Node.
func (n *TypeDefNode) Position() token.Position { return n.IDENT.Position() }

func (p *parser) typeDef() *TypeDefNode {
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
	_0:
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
	return &TypeDefNode{
		IDENT:          identTok,
		TypeParameters: typeParameters,
		Type:           typeNode,
	}
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

// Position implements Node.
func (n *TypeElemNode) Position() token.Position { return n.TypeTerm.Position() }

func (p *parser) typeElem() *TypeElemNode {
	var (
		typeTerm *TypeTermNode
		list     []struct {
			OR       Token
			TypeTerm *TypeTermNode
		}
		orTok     Token
		typeTerm2 *TypeTermNode
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
			if typeTerm2 = p.typeTerm(); typeTerm2 == nil {
				p.back(ix)
				goto _1
			}
			list = append(list, struct {
				OR       Token
				TypeTerm *TypeTermNode
			}{orTok, typeTerm2})
			goto _0
		}
	_1:
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

// Position implements Node.
func (n *TypeListNode) Position() token.Position { return n.Type.Position() }

func (p *parser) typeList() *TypeListNode {
	var (
		typeNode *TypeNode
		list     []struct {
			COMMA Token
			Type  *TypeNode
		}
		commaTok Token
		type2    *TypeNode
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
			if type2 = p.type1(); type2 == nil {
				p.back(ix)
				goto _1
			}
			list = append(list, struct {
				COMMA Token
				Type  *TypeNode
			}{commaTok, type2})
			goto _0
		}
	_1:
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
		// *ebnf.Name SliceType ctx [LBRACK]
		if sliceType = p.sliceType(); sliceType == nil {
			goto _1
		}
		break
	_1:
		return nil
	case STRUCT: // 1
		// *ebnf.Name StructType ctx [STRUCT]
		if structType = p.structType(); structType == nil {
			return nil
		}
	case MUL: // 2
		// *ebnf.Name PointerType ctx [MUL]
		if pointerType = p.pointerType(); pointerType == nil {
			return nil
		}
	case FUNC: // 3
		// *ebnf.Name FunctionType ctx [FUNC]
		if functionType = p.functionType(); functionType == nil {
			return nil
		}
	case INTERFACE: // 4
		// *ebnf.Name InterfaceType ctx [INTERFACE]
		if interfaceType = p.interfaceType(); interfaceType == nil {
			return nil
		}
	case MAP: // 6
		// *ebnf.Name MapType ctx [MAP]
		if mapType = p.mapType(); mapType == nil {
			return nil
		}
	case ARROW, CHAN: // 7
		// *ebnf.Name ChannelType ctx [ARROW, CHAN]
		if channelType = p.channelType(); channelType == nil {
			return nil
		}
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
//	TypeName = QualifiedIdent .
type TypeNameNode struct {
	QualifiedIdent *QualifiedIdentNode
}

// Position implements Node.
func (n *TypeNameNode) Position() token.Position { return n.QualifiedIdent.Position() }

func (p *parser) typeName() *TypeNameNode {
	var (
		qualifiedIdent *QualifiedIdentNode
	)
	// *ebnf.Name QualifiedIdent ctx [IDENT]
	if qualifiedIdent = p.qualifiedIdent(); qualifiedIdent == nil {
		return nil
	}
	return &TypeNameNode{
		QualifiedIdent: qualifiedIdent,
	}
}

// TypeParamDeclNode represents the production
//
//	TypeParamDecl = IdentifierList TypeConstraint .
type TypeParamDeclNode struct {
	IdentifierList *IdentifierListNode
	TypeConstraint *TypeConstraintNode
}

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

// Position implements Node.
func (n *TypeParamListNode) Position() token.Position { return n.TypeParamDecl.Position() }

func (p *parser) typeParamList() *TypeParamListNode {
	var (
		typeParamDecl *TypeParamDeclNode
		list          []struct {
			COMMA         Token
			TypeParamDecl *TypeParamDeclNode
		}
		commaTok       Token
		typeParamDecl2 *TypeParamDeclNode
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
			if typeParamDecl2 = p.typeParamDecl(); typeParamDecl2 == nil {
				p.back(ix)
				goto _1
			}
			list = append(list, struct {
				COMMA         Token
				TypeParamDecl *TypeParamDeclNode
			}{commaTok, typeParamDecl2})
			goto _0
		}
	_1:
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

// TypeSpecNode represents the production
//
//	TypeSpec = AliasDecl | TypeDef .
type TypeSpecNode struct {
	AliasDecl *AliasDeclNode
	TypeDef   *TypeDefNode
}

// Position implements Node.
func (n *TypeSpecNode) Position() token.Position { panic("TODO") }

func (p *parser) typeSpec() *TypeSpecNode {
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
		break
	_0:
		// *ebnf.Name TypeDef ctx [IDENT]
		if typeDef = p.typeDef(); typeDef == nil {
			goto _1
		}
		break
	_1:
		return nil
	default:
		return nil
	}
	return &TypeSpecNode{
		AliasDecl: aliasDecl,
		TypeDef:   typeDef,
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
				return nil
			}
			ix := p.ix
			// *ebnf.Token "case" ctx [CASE]
			caseTok = p.expect(CASE)
			// *ebnf.Name TypeList ctx [ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT]
			if typeList = p.typeList(); typeList == nil {
				p.back(ix)
				return nil
			}
		}
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
	PrimaryExpr *PrimaryExprNode
	PERIOD      Token
	LPAREN      Token
	TYPE        Token
	RPAREN      Token
}

// Position implements Node.
func (n *TypeSwitchGuardNode) Position() token.Position { panic("TODO") }

func (p *parser) typeSwitchGuard() *TypeSwitchGuardNode {
	var (
		ok          bool
		identTok    Token
		defineTok   Token
		primaryExpr *PrimaryExprNode
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
	_0:
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
	List            []*TypeCaseClauseNode
	RBRACE          Token
}

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
		list            []*TypeCaseClauseNode
		typeCaseClause  *TypeCaseClauseNode
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
	_0:
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
	_1:
		switch p.c() {
		case CASE, DEFAULT:
			// *ebnf.Name TypeCaseClause ctx [CASE, DEFAULT]
			if typeCaseClause = p.typeCaseClause(); typeCaseClause == nil {
				goto _2
			}
			list = append(list, typeCaseClause)
			goto _1
		}
	_2:
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
			return nil
		}
	case TILDE: // 1
		// *ebnf.Name UnderlyingType ctx [TILDE]
		if underlyingType = p.underlyingType(); underlyingType == nil {
			return nil
		}
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
	PrimaryExpr *PrimaryExprNode
	Op          Token
	UnaryExpr   *UnaryExprNode
}

// Position implements Node.
func (n *UnaryExprNode) Position() token.Position { panic("TODO") }

func (p *parser) unaryExpr(preBlock bool) *UnaryExprNode {
	var (
		primaryExpr *PrimaryExprNode
		tok         Token
		unaryExpr   *UnaryExprNode
	)
	// ebnf.Alternative PrimaryExpr | ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
	switch p.c() {
	case CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT: // 0
		// *ebnf.Name PrimaryExpr ctx [CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, STRING, STRUCT]
		if primaryExpr = p.primaryExpr(preBlock); primaryExpr == nil {
			return nil
		}
	case ARROW, MUL: // 0 1
		// *ebnf.Name PrimaryExpr ctx [ARROW, MUL]
		if primaryExpr = p.primaryExpr(preBlock); primaryExpr == nil {
			goto _0
		}
		break
	_0:
		// ebnf.Sequence ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx [ARROW, MUL]
		{
			ix := p.ix
			// *ebnf.Group ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) ctx [ARROW, MUL]
			// ebnf.Alternative "+" | "-" | "!" | "^" | "*" | "&" | "<-" ctx [ARROW, MUL]
			tok = p.consume()
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
		}
		break
	_1:
		return nil
	case ADD, AND, NOT, SUB, XOR: // 1
		// ebnf.Sequence ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) UnaryExpr ctx [ADD, AND, NOT, SUB, XOR]
		{
			ix := p.ix
			// *ebnf.Group ( "+" | "-" | "!" | "^" | "*" | "&" | "<-" ) ctx [ADD, AND, NOT, SUB, XOR]
			// ebnf.Alternative "+" | "-" | "!" | "^" | "*" | "&" | "<-" ctx [ADD, AND, NOT, SUB, XOR]
			tok = p.consume()
			// *ebnf.Name UnaryExpr ctx []
			switch p.c() {
			case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				if unaryExpr = p.unaryExpr(preBlock); unaryExpr == nil {
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
	return &UnaryExprNode{
		PrimaryExpr: primaryExpr,
		Op:          tok,
		UnaryExpr:   unaryExpr,
	}
}

// UnderlyingTypeNode represents the production
//
//	UnderlyingType = "~" Type .
type UnderlyingTypeNode struct {
	TILDE Token
	Type  *TypeNode
}

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
//	VarDecl = "var" ( VarSpec | "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ) .
type VarDeclNode struct {
	VAR      Token
	VarSpec  *VarSpecNode
	LPAREN   Token
	VarSpec2 *VarSpecNode
	List     []struct {
		SEMICOLON Token
		VarSpec   *VarSpecNode
	}
	SEMICOLON2 Token
	RPAREN     Token
}

// Position implements Node.
func (n *VarDeclNode) Position() token.Position { return n.VAR.Position() }

func (p *parser) varDecl() *VarDeclNode {
	var (
		ok        bool
		varTok    Token
		varSpec   *VarSpecNode
		lparenTok Token
		varSpec2  *VarSpecNode
		list      []struct {
			SEMICOLON Token
			VarSpec   *VarSpecNode
		}
		semicolonTok  Token
		varSpec3      *VarSpecNode
		semicolon2Tok Token
		rparenTok     Token
	)
	// ebnf.Sequence "var" ( VarSpec | "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ) ctx [VAR]
	{
		switch p.peek(1) {
		case IDENT, LPAREN:
		default:
			return nil
		}
		ix := p.ix
		// *ebnf.Token "var" ctx [VAR]
		varTok = p.expect(VAR)
		// *ebnf.Group ( VarSpec | "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ) ctx [IDENT, LPAREN]
		// ebnf.Alternative VarSpec | "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ctx [IDENT, LPAREN]
		switch p.c() {
		case IDENT: // 0
			// *ebnf.Name VarSpec ctx [IDENT]
			if varSpec = p.varSpec(); varSpec == nil {
				p.back(ix)
				return nil
			}
		case LPAREN: // 1
			// ebnf.Sequence "(" [ VarSpec { ";" VarSpec } [ ";" ] ] ")" ctx [LPAREN]
			{
				ix := p.ix
				// *ebnf.Token "(" ctx [LPAREN]
				lparenTok = p.expect(LPAREN)
				// *ebnf.Option [ VarSpec { ";" VarSpec } [ ";" ] ] ctx []
				switch p.c() {
				case IDENT:
					// ebnf.Sequence VarSpec { ";" VarSpec } [ ";" ] ctx [IDENT]
					{
						ix := p.ix
						// *ebnf.Name VarSpec ctx [IDENT]
						if varSpec2 = p.varSpec(); varSpec2 == nil {
							p.back(ix)
							goto _0
						}
						// *ebnf.Repetition { ";" VarSpec } ctx []
					_1:
						switch p.c() {
						case SEMICOLON:
							// ebnf.Sequence ";" VarSpec ctx [SEMICOLON]
							switch p.peek(1) {
							case IDENT:
							default:
								goto _2
							}
							ix := p.ix
							// *ebnf.Token ";" ctx [SEMICOLON]
							semicolonTok = p.expect(SEMICOLON)
							// *ebnf.Name VarSpec ctx [IDENT]
							if varSpec3 = p.varSpec(); varSpec3 == nil {
								p.back(ix)
								goto _2
							}
							list = append(list, struct {
								SEMICOLON Token
								VarSpec   *VarSpecNode
							}{semicolonTok, varSpec3})
							goto _1
						}
					_2:
						// *ebnf.Option [ ";" ] ctx []
						switch p.c() {
						case SEMICOLON:
							// *ebnf.Token ";" ctx [SEMICOLON]
							semicolon2Tok = p.expect(SEMICOLON)
						}
					}
				}
			_0:
				// *ebnf.Token ")" ctx []
				if rparenTok, ok = p.accept(RPAREN); !ok {
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &VarDeclNode{
		VAR:        varTok,
		VarSpec:    varSpec,
		LPAREN:     lparenTok,
		VarSpec2:   varSpec2,
		List:       list,
		SEMICOLON2: semicolon2Tok,
		RPAREN:     rparenTok,
	}
}

// VarSpecNode represents the production
//
//	VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
type VarSpecNode struct {
	IdentifierList  *IdentifierListNode
	Type            *TypeNode
	ASSIGN          Token
	ExpressionList  *ExpressionListNode
	ASSIGN2         Token
	ExpressionList2 *ExpressionListNode
}

// Position implements Node.
func (n *VarSpecNode) Position() token.Position { return n.IdentifierList.Position() }

func (p *parser) varSpec() *VarSpecNode {
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
					return nil
				}
				// *ebnf.Option [ "=" ExpressionList ] ctx []
				switch p.c() {
				case ASSIGN:
					// ebnf.Sequence "=" ExpressionList ctx [ASSIGN]
					{
						switch p.peek(1) {
						case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
						default:
							goto _0
						}
						ix := p.ix
						// *ebnf.Token "=" ctx [ASSIGN]
						assignTok = p.expect(ASSIGN)
						// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
						if expressionList = p.expressionList(false); expressionList == nil {
							p.back(ix)
							goto _0
						}
					}
				}
			_0:
			}
		case ASSIGN: // 1
			// ebnf.Sequence "=" ExpressionList ctx [ASSIGN]
			{
				switch p.peek(1) {
				case ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR:
				default:
					p.back(ix)
					return nil
				}
				ix := p.ix
				// *ebnf.Token "=" ctx [ASSIGN]
				assign2Tok = p.expect(ASSIGN)
				// *ebnf.Name ExpressionList ctx [ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR]
				if expressionList2 = p.expressionList(false); expressionList2 == nil {
					p.back(ix)
					return nil
				}
			}
		default:
			p.back(ix)
			return nil
		}
	}
	return &VarSpecNode{
		IdentifierList:  identifierList,
		Type:            typeNode,
		ASSIGN:          assignTok,
		ExpressionList:  expressionList,
		ASSIGN2:         assign2Tok,
		ExpressionList2: expressionList2,
	}
}
