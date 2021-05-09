// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"fmt"
	"go/token"
	"strings"
	"unicode"
	"unicode/utf8"

	mtoken "modernc.org/token"
)

var (
	_ Node = (*Token)(nil)
)

type errItem struct {
	off int32
	err error
}

func (n errItem) position(s *source) token.Position {
	return token.Position(s.file.PositionFor(mtoken.Pos(n.off+1), true))
}

type errList []errItem

func (e errList) Err(s *source) error {
	if len(e) == 0 {
		return nil
	}

	w := 0
	prev := errItem{off: -1}
	for _, v := range e {
		if v.off != prev.off || v.err.Error() != prev.err.Error() {
			e[w] = v
			w++
			prev = v
		}
	}

	var a []string
	for _, v := range e {
		a = append(a, fmt.Sprintf("%v: %v", token.Position(s.file.PositionFor(mtoken.Pos(v.off+1), true)), v.err))
	}
	return fmt.Errorf("%s", strings.Join(a, "\n"))
}

func (e *errList) err(off int32, skip int, msg string, args ...interface{}) {
	errs := *e
	msg = fmt.Sprintf(msg, args...)
	*e = append(errs, errItem{off, fmt.Errorf("%s (%v:)", msg, origin(skip+2))})
}

// Ch represents the lexical value of a Token.
type Ch rune

// Token translates, if possible, c to the lexeme value defined in go/token or
// token.ILLEGAL otherwise.
func (c Ch) Token() token.Token {
	if r, ok := xlat[c]; ok {
		return r
	}

	return token.ILLEGAL
}

// Node is an item of the CST tree.
type Node interface {
	Position() token.Position
}

// Token is the product of Scanner.Scan and a terminal node of the complete
// syntax tree.
type Token struct { // 24 bytes on 64 bit arch
	source *source

	Ch
	next   int32
	off    int32
	sepOff int32
}

func (n *Token) sepPosition() (r token.Position) {
	if n == nil {
		return r
	}

	return token.Position(n.source.file.PositionFor(mtoken.Pos(n.sepOff+1), true))
}

// Position implements Node.
func (n Token) Position() (r token.Position) {
	if n.IsValid() {
		return token.Position(n.source.file.PositionFor(mtoken.Pos(n.off+1), true))
	}

	return r
}

func (n Token) pos() pos { return pos{n.source, n.off} }

type pos struct {
	source *source
	off    int32
}

func (n pos) Position() (r token.Position) {
	if n.source != nil {
		return token.Position(n.source.file.PositionFor(mtoken.Pos(n.off+1), true))
	}

	return r
}

// Offset reports the starting offset of n, in bytes, within the source buffer.
func (n *Token) Offset() int { return int(n.off) }

// SepOffset reports the starting offset of n's preceding white space, if any,
// in bytes, within the source buffer.
func (n *Token) SepOffset() int { return int(n.sepOff) }

// String pretty formats n.
func (n *Token) String() string {
	if n.Ch <= beforeTokens || n.Ch >= afterTokens { //TODO
		return fmt.Sprintf("%v: %q %#U", n.Position(), n.Src(), rune(n.Ch))
	}

	return fmt.Sprintf("%v: %q %s", n.Position(), n.Src(), n.Ch)
}

// IsValid reports the validity of n. Tokens not present in some nodes will
// report false.
func (n *Token) IsValid() bool { return n.source != nil }

// Sep returns the whitespace preceding n, if any. The result is read only.
func (n *Token) Sep() []byte { return n.source.buf[n.sepOff:n.off] }

// Src returns the original textual form of n. The result is read only.
func (n *Token) Src() []byte { return n.source.buf[n.off:n.next] }

type source struct {
	buf  []byte
	file *mtoken.File
}

// Scanner provides lexical analysis of its buffer.
type Scanner struct {
	*source
	// Tok is the current token. It is valid after first call to Scan. The
	// value is read only.
	Tok  Token
	errs errList

	cnt  int32
	last Ch
	off  int32 // Index into source.buf.

	c byte // Lookahead byte.

	allErrros bool
	isClosed  bool
}

// NewScanner returns a newly created scanner that will tokenize buf. Positions
// are reported as if buf is coming from a file named name. The buffer becomes
// owned by the scanner and must not be modified after calling NewScanner.
//
// The scanner normally stops scanning after some number of errors. Passing
// allErrros == true overides that.
func NewScanner(buf []byte, name string, allErrros bool) (*Scanner, error) {
	r := &Scanner{
		source:    &source{buf: buf, file: mtoken.NewFile(name, len(buf))},
		allErrros: allErrros,
	}
	if len(buf) != 0 {
		r.c = buf[0]
		if r.c == '\n' {
			r.file.AddLine(int(r.off) + 1)
		}
	}
	return r, nil
}

func (s *Scanner) position() token.Position {
	return token.Position(s.source.file.PositionFor(mtoken.Pos(s.off+1), true))
}

// Err reports any errors the scanner encountered during .Scan() invocations.
// For typical use please see the .Scan() documentation.
func (s *Scanner) Err() error { return s.errs.Err(s.source) }

func (s *Scanner) err(off int32, skip int, msg string, args ...interface{}) {
	if len(s.errs) == 10 && !s.allErrros {
		s.close()
		return
	}

	s.errs.err(off, skip+1, msg, args...)
}

func (s *Scanner) close() {
	if s.isClosed {
		return
	}

	if s.cnt == 1 {
		s.err(s.off, 1, "empty input")
	}
	s.Tok.Ch = EOF
	s.Tok.next = s.off
	s.Tok.off = s.off
	s.Tok.source = s.source
	s.isClosed = true
}

func isIDFirst(c byte) bool {
	return c >= 'a' && c <= 'z' ||
		c >= 'A' && c <= 'Z' ||
		c == '_'
}

func isBinaryDigit(c byte) bool { return c == '0' || c == '1' }
func isDigit(c byte) bool       { return c >= '0' && c <= '9' }
func isHexDigit(c byte) bool    { return isDigit(c) || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F' }
func isIDNext(c byte) bool      { return isIDFirst(c) || isDigit(c) }
func isOctalDigit(c byte) bool  { return c >= '0' && c <= '7' }

func (s *Scanner) next() {
	if int(s.off) == len(s.buf)-1 {
		s.c = 0
		return
	}

	s.off++
	s.Tok.next = s.off
	s.c = s.buf[s.off]
	if s.c == '\n' {
		s.file.AddLine(int(s.off) + 1)
	}
}

func (s *Scanner) nextN(n int) {
	if int(s.off) == len(s.buf)-n {
		s.c = 0
		return
	}

	s.off += int32(n)
	s.Tok.next = s.off
	s.c = s.buf[s.off]
	if s.c == '\n' {
		s.file.AddLine(int(s.off) + 1)
	}
}

// Scan moves to the next token and returns true if not at end of file. Usage
// example:
//
//	s, _ = NewScanner(buf, name, false)
//	for s.Scan() {
//		...
//	}
//	if err := s.Err() {
//		...
//	}
func (s *Scanner) Scan() bool {
	if s.isClosed {
		return false
	}

	s.cnt++
	s.last = s.Tok.Ch
	s.Tok.sepOff = s.off
	s.Tok.source = s.source
again:
	s.Tok.off = s.off
	s.Tok.next = s.off
	switch s.c {
	case ' ', '\t', '\r', '\n':
		// White space, formed from spaces (U+0020), horizontal tabs (U+0009), carriage
		// returns (U+000D), and newlines (U+000A), is ignored except as it separates
		// tokens that would otherwise combine into a single token.
		if s.c == '\n' && s.injectSemi() {
			return true
		}

		s.next()
		goto again
	case '/':
		s.next()
		switch s.c {
		case '=':
			s.next()
			s.Tok.Ch = QUO_ASSIGN
		case '/':
			// Line comments start with the character sequence // and stop at the end of
			// the line.
			s.next()
			if s.lineComment() {
				return true
			}

			goto again
		case '*':
			// General comments start with the character sequence /* and stop with the
			// first subsequent character sequence */.
			s.next()
			if s.generalComment() {
				return true
			}

			goto again
		default:
			s.Tok.Ch = '/'
		}
	case '(', ')', '[', ']', '{', '}', ',', ';':
		s.Tok.Ch = Ch(s.c)
		s.next()
	case '"':
		s.next()
		s.stringLiteral()
	case '\'':
		s.next()
		s.runeLiteral()
	case '`':
		s.next()
		for {
			switch s.c {
			case '`':
				s.next()
				s.Tok.Ch = STRING_LIT
				return true
			case 0:
				panic(todo("%v: %#U", s.position(), s.c))
			default:
				s.next()
			}
		}
	case '.':
		s.next()
		if isDigit(s.c) {
			s.dot()
			return true
		}

		if s.c != '.' {
			s.Tok.Ch = '.'
			return true
		}

		s.next()
		switch {
		case s.c == '.':
			s.next()
		default:
			s.err(s.off, 0, "expected '.'")
		}
		s.Tok.Ch = ELLIPSIS
	case '%':
		s.next()
		switch s.c {
		case '=':
			s.next()
			s.Tok.Ch = REM_ASSIGN
		default:
			s.Tok.Ch = '%'
		}
	case '*':
		s.next()
		switch s.c {
		case '=':
			s.next()
			s.Tok.Ch = MUL_ASSIGN
		default:
			s.Tok.Ch = '*'
		}
	case '^':
		s.next()
		switch s.c {
		case '=':
			s.next()
			s.Tok.Ch = XOR_ASSIGN
		default:
			s.Tok.Ch = '^'
		}
	case '+':
		s.next()
		switch s.c {
		case '+':
			s.next()
			s.Tok.Ch = INC
		case '=':
			s.next()
			s.Tok.Ch = ADD_ASSIGN
		default:
			s.Tok.Ch = '+'
		}
	case '-':
		s.next()
		switch s.c {
		case '-':
			s.next()
			s.Tok.Ch = DEC
		case '=':
			s.next()
			s.Tok.Ch = SUB_ASSIGN
		default:
			s.Tok.Ch = '-'
		}
	case ':':
		s.next()
		switch {
		case s.c == '=':
			s.next()
			s.Tok.Ch = DEFINE
		default:
			s.Tok.Ch = ':'
		}
	case '=':
		s.next()
		switch {
		case s.c == '=':
			s.next()
			s.Tok.Ch = EQ
		default:
			s.Tok.Ch = '='
		}
	case '!':
		s.next()
		switch {
		case s.c == '=':
			s.next()
			s.Tok.Ch = NE
		default:
			s.Tok.Ch = '!'
		}
	case '>':
		s.next()
		switch s.c {
		case '=':
			s.next()
			s.Tok.Ch = GE
		case '>':
			s.next()
			switch s.c {
			case '=':
				s.next()
				s.Tok.Ch = SHR_ASSIGN
			default:
				s.Tok.Ch = SHR
			}
		default:
			s.Tok.Ch = '>'
		}
	case '<':
		s.next()
		switch s.c {
		case '=':
			s.next()
			s.Tok.Ch = LE
		case '<':
			s.next()
			switch s.c {
			case '=':
				s.next()
				s.Tok.Ch = SHL_ASSIGN
			default:
				s.Tok.Ch = SHL
			}
		case '-':
			s.next()
			s.Tok.Ch = ARROW
		default:
			s.Tok.Ch = '<'
		}
	case '|':
		s.next()
		switch s.c {
		case '|':
			s.next()
			s.Tok.Ch = LOR
		case '=':
			s.next()
			s.Tok.Ch = OR_ASSIGN
		default:
			s.Tok.Ch = '|'
		}
	case '&':
		s.next()
		switch s.c {
		case '&':
			s.next()
			s.Tok.Ch = LAND
		case '^':
			s.next()
			switch s.c {
			case '=':
				s.next()
				s.Tok.Ch = AND_NOT_ASSIGN
			default:
				s.Tok.Ch = AND_NOT
			}
		case '=':
			s.next()
			s.Tok.Ch = AND_ASSIGN
		default:
			s.Tok.Ch = '&'
		}
	case 0:
		if s.injectSemi() {
			return true
		}

		s.close()
		return false
	default:
		switch {
		case isIDFirst(s.c):
			s.next()
			s.identifierOrKeyword()
		case isDigit(s.c):
			s.numericLiteral()
		case s.c >= 0x80:
			switch r := s.rune(); {
			case unicode.IsLetter(r):
				s.identifierOrKeyword()
			default:
				panic(todo("%v: %#U", s.position(), r))
			}
		default:
			s.err(s.off, 0, "unexpected %#U", s.c)
			s.next()
		}
	}
	return true
}

// When the input is broken into tokens, a semicolon is automatically inserted into the token stream immediately after a line's final token if that token is
//
//	an identifier
//	an integer, floating-point, imaginary, rune, or string literal
//	one of the keywords break, continue, fallthrough, or return
//	one of the operators and punctuation ++, --, ), ], or }
func (s *Scanner) injectSemi() bool {
	switch s.last {
	case
		IDENTIFIER, INT_LIT, FLOAT_LIT, IMAG, RUNE_LIT, STRING_LIT,
		BREAK, CONTINUE, FALLTHROUGH, RETURN,
		INC, DEC, ')', ']', '}':

		s.Tok.Ch = ';'
		s.last = 0
		if s.c == '\n' {
			s.next()
		}
		return true
	}

	s.last = 0
	return false
}

func (s *Scanner) numericLiteral() {
	// Leading decimal digit not consumed.
more:
	switch s.c {
	case '0':
		s.next()
		switch s.c {
		case '.':
			// nop
		case 'b', 'B':
			s.next()
			s.binaryLiteral()
			s.Tok.Ch = INT_LIT
			return
		case 'e', 'E', 'p', 'P':
			s.exponent()
			s.Tok.Ch = FLOAT_LIT
			return
		case 'o', 'O':
			panic(todo("%v: %#U", s.position(), s.c))
		case 'x', 'X':
			s.next()
			if !isHexDigit(s.c) {
				panic(todo("%v: %#U", s.position(), s.c))
			}

			for isHexDigit(s.c) || s.c == '_' {
				s.next()
			}
			if !isHexDigit(s.source.buf[s.off-1]) {
				panic(todo("%v: %#U", s.position(), s.c))
			}
			s.Tok.Ch = INT_LIT
		case 'i':
			s.next()
			s.Tok.Ch = IMAG
			return
		default:
			for isOctalDigit(s.c) {
				s.next()
			}
			switch {
			case s.c == '.':
				break more
			case isDigit(s.c):
				break more
			}
			s.Tok.Ch = INT_LIT
			return
		}
	default:
		s.decimals()
	}
	switch s.c {
	case '.':
		s.next()
		s.dot()
	case 'e', 'E', 'p', 'P':
		s.exponent()
		if s.c == 'i' {
			s.next()
			s.Tok.Ch = IMAG
			return
		}

		s.Tok.Ch = FLOAT_LIT
	case 'i':
		s.next()
		s.Tok.Ch = IMAG
	default:
		s.Tok.Ch = INT_LIT
	}
}

func (s *Scanner) dot() {
	s.decimals()
	switch s.c {
	case 'e', 'E', 'p', 'P':
		s.exponent()
		if s.c == 'i' {
			s.next()
			s.Tok.Ch = IMAG
			return
		}

		s.Tok.Ch = FLOAT_LIT
	case 'i':
		s.next()
		s.Tok.Ch = IMAG
	default:
		s.Tok.Ch = FLOAT_LIT
	}
}

func (s *Scanner) binaryLiteral() {
	// Leading 0b consumed.
	ok := false
	for {
		if s.c == '_' {
			s.next()
		}
		switch s.c {
		case '0', '1':
			s.next()
			ok = true
		default:
			if !ok {
				panic(todo("%v: %#U", s.position(), s.c))
			}
			return
		}
	}
}

func (s *Scanner) exponent() {
	// Leanding e or E not consumed.
	s.next()
	switch s.c {
	case '+', '-':
		s.next()
	}
	if !isDigit(s.c) {
		panic(todo("%v: %#U", s.position(), s.c))
	}

	s.decimals()
}

func (s *Scanner) decimals() {
	for isDigit(s.c) {
		s.next()
	}
}

func (s *Scanner) runeLiteral() {
	// Leading ' consumed.
	switch s.c {
	case '\\':
		s.next()
		switch s.c {
		case '\'', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v':
			s.next()
		case 'x', 'X':
			s.next()
			if !isHexDigit(s.c) {
				panic(todo("%v: %#U", s.position(), s.c))
			}

			s.next()
			if !isHexDigit(s.c) {
				panic(todo("%v: %#U", s.position(), s.c))
			}

			s.next()
		case 'u':
			s.u4()
		case 'U':
			s.u8()
		default:
			switch {
			case isOctalDigit(s.c):
				s.next()
				if isOctalDigit(s.c) {
					s.next()
				}
				if isOctalDigit(s.c) {
					s.next()
				}
			default:
				panic(todo("%v: %#U", s.position(), s.c))
			}
		}
		goto last
	}

	switch {
	case s.c == 0:
		panic(todo("%v: %#U", s.position(), s.c))
	case s.c == '\t':
		s.next()
	case s.c < ' ':
		s.err(s.off, 0, "non-printable character: %#U", s.c)
		s.next()
	case s.c >= 0x80:
		s.rune()
	default:
		s.next()
	}

last:
	switch s.c {
	case '\'':
		s.next()
	case 0:
		panic(todo("%v: %#U", s.position(), s.c))
	default:
		panic(todo("%v: %#U", s.position(), s.c))
	}
	s.Tok.Ch = RUNE_LIT
	return
}

func (s *Scanner) rune() rune {
	switch r, sz := utf8.DecodeRune(s.buf[s.off:]); {
	case r == utf8.RuneError && sz == 0:
		panic(todo("%v: %#U", s.position(), s.c))
	case r == utf8.RuneError && sz == 1:
		panic(todo("%v: %#U", s.position(), s.c))
	default:
		s.nextN(sz)
		return r
	}
}

func (s *Scanner) stringLiteral() {
	// Leadind " consumed.
	for {
		switch s.c {
		case '"':
			s.next()
			s.Tok.Ch = STRING_LIT
			return
		case '\\':
			s.next()
			switch s.c {
			case '"', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v':
				s.next()
				continue
			case 'x', 'X':
				s.next()
				if !isHexDigit(s.c) {
					panic(todo("%v: %#U", s.position(), s.c))
				}

				s.next()
				if !isHexDigit(s.c) {
					panic(todo("%v: %#U", s.position(), s.c))
				}

				s.next()
				continue
			case 'u':
				s.u4()
				continue
			case 'U':
				s.u8()
				continue
			default:
				switch {
				case isOctalDigit(s.c):
					s.next()
					if isOctalDigit(s.c) {
						s.next()
					}
					if isOctalDigit(s.c) {
						s.next()
					}
					continue
				default:
					panic(todo("%v: %#U", s.position(), s.c))
				}
			}
		case 0:
			panic(todo("%v: %#U", s.position(), s.c))
		}

		switch {
		case s.c == '\t':
			// ok
		case s.c < ' ':
			s.err(s.off, 0, "non-printable character: %#U", s.c)
			s.next()
		}
		s.next()
	}
}

func (s *Scanner) u4() {
	// Leading u not consumed.
	s.next()
	for i := 0; i < 4; i++ {
		if !isHexDigit(s.c) {
			panic(todo("%v: %#U", s.position(), s.c))
		}

		s.next()
	}
}

func (s *Scanner) u8() {
	// Leading U not consumed.
	s.next()
	for i := 0; i < 8; i++ {
		if !isHexDigit(s.c) {
			panic(todo("%v: %#U", s.position(), s.c))
		}

		s.next()
	}
}

func (s *Scanner) identifierOrKeyword() {
out:
	for {
		switch {
		case isIDNext(s.c):
			s.next()
		case s.c >= 0x80:
			switch r := s.rune(); {
			case unicode.IsLetter(r) || unicode.IsDigit(r):
				// already consumed
			default:
				panic(todo("%v: %#U", s.position(), r))
			}
		case s.c == 0:
			break out
		default:
			break out
		}
	}
	if s.Tok.Ch = Keywords[string(s.Tok.Src())]; s.Tok.Ch == 0 {
		s.Tok.Ch = IDENTIFIER
	}
	return
}

func (s *Scanner) generalComment() bool {
	// Leading /* consumed
	var nl bool
	for {
		switch s.c {
		case '*':
			s.next()
			switch s.c {
			case '/':
				s.next()
				if nl {
					return s.injectSemi()
				}

				return false
			}
		case '\n':
			nl = true
			s.next()
		case 0:
			panic(todo("%v: %#U", s.position(), s.c))
		default:
			s.next()
		}
	}
}

func (s *Scanner) lineComment() bool {
	// Leading // consumed
	for {
		switch s.c {
		case '\n':
			if s.injectSemi() {
				return true
			}

			s.next()
			return false
		case 0:
			return false
		default:
			s.next()
		}
	}
}

// Named values of Ch.
const (
	beforeTokens Ch = iota + 0xe000

	ADD_ASSIGN     // +=
	AND_ASSIGN     // &=
	AND_NOT        // &^
	AND_NOT_ASSIGN // &^=
	ARROW          // <-
	BREAK          // break
	CASE           // case
	CHAN           // chan
	CONST          // const
	CONTINUE       // continue
	DEC            // --
	DEFAULT        // default
	DEFER          // defer
	DEFINE         // :=
	ELLIPSIS       // ...
	ELSE           // else
	EOF            // end of file
	EQ             // ==
	FALLTHROUGH    // fallthrough
	FLOAT_LIT      // floating point literal
	FOR            // for
	FUNC           // func
	GE             // >=
	GO             // go
	GOTO           // goto
	IDENTIFIER     // identifier
	IF             // if
	IMAG           // 123.45i
	IMPORT         // import
	INC            // ++
	INTERFACE      // interface
	INT_LIT        // integer literal
	LAND           // &&
	LE             // <=
	LOR            // ||
	MAP            // map
	MUL_ASSIGN     // *=
	NE             // !=
	OR_ASSIGN      // |=
	PACKAGE        // package
	QUO_ASSIGN     // /=
	RANGE          // range
	REM_ASSIGN     // %=
	RETURN         // return
	RUNE_LIT       // rune literal
	SELECT         // select
	SHL            // <<
	SHL_ASSIGN     // <<=
	SHR            // >>
	SHR_ASSIGN     // >>=
	STRING_LIT     // string literal
	STRUCT         // struct
	SUB_ASSIGN     // -=
	SWITCH         // switch
	TYPE           // type
	VAR            // var
	XOR_ASSIGN     // ^=

	afterTokens
)

var xlat = map[Ch]token.Token{
	'!':            token.NOT,
	'%':            token.REM,
	'&':            token.AND,
	'(':            token.LPAREN,
	')':            token.RPAREN,
	'*':            token.MUL,
	'+':            token.ADD,
	',':            token.COMMA,
	'-':            token.SUB,
	'.':            token.PERIOD,
	'/':            token.QUO,
	':':            token.COLON,
	';':            token.SEMICOLON,
	'<':            token.LSS,
	'=':            token.ASSIGN,
	'>':            token.GTR,
	'[':            token.LBRACK,
	']':            token.RBRACK,
	'^':            token.XOR,
	'{':            token.LBRACE,
	'|':            token.OR,
	'}':            token.RBRACE,
	ADD_ASSIGN:     token.ADD_ASSIGN,
	AND_ASSIGN:     token.AND_ASSIGN,
	AND_NOT:        token.AND_NOT,
	AND_NOT_ASSIGN: token.AND_NOT_ASSIGN,
	ARROW:          token.ARROW,
	BREAK:          token.BREAK,
	CASE:           token.CASE,
	CHAN:           token.CHAN,
	CONST:          token.CONST,
	CONTINUE:       token.CONTINUE,
	DEC:            token.DEC,
	DEFAULT:        token.DEFAULT,
	DEFER:          token.DEFER,
	DEFINE:         token.DEFINE,
	ELLIPSIS:       token.ELLIPSIS,
	ELSE:           token.ELSE,
	EOF:            token.EOF,
	EQ:             token.EQL,
	FALLTHROUGH:    token.FALLTHROUGH,
	FLOAT_LIT:      token.FLOAT,
	FOR:            token.FOR,
	FUNC:           token.FUNC,
	GE:             token.GEQ,
	GO:             token.GO,
	GOTO:           token.GOTO,
	IDENTIFIER:     token.IDENT,
	IF:             token.IF,
	IMAG:           token.IMAG,
	IMPORT:         token.IMPORT,
	INC:            token.INC,
	INTERFACE:      token.INTERFACE,
	INT_LIT:        token.INT,
	LAND:           token.LAND,
	LE:             token.LEQ,
	LOR:            token.LOR,
	MAP:            token.MAP,
	MUL_ASSIGN:     token.MUL_ASSIGN,
	NE:             token.NEQ,
	OR_ASSIGN:      token.OR_ASSIGN,
	PACKAGE:        token.PACKAGE,
	QUO_ASSIGN:     token.QUO_ASSIGN,
	RANGE:          token.RANGE,
	REM_ASSIGN:     token.REM_ASSIGN,
	RETURN:         token.RETURN,
	RUNE_LIT:       token.CHAR,
	SELECT:         token.SELECT,
	SHL:            token.SHL,
	SHL_ASSIGN:     token.SHL_ASSIGN,
	SHR:            token.SHR,
	SHR_ASSIGN:     token.SHR_ASSIGN,
	STRING_LIT:     token.STRING,
	STRUCT:         token.STRUCT,
	SUB_ASSIGN:     token.SUB_ASSIGN,
	SWITCH:         token.SWITCH,
	TYPE:           token.TYPE,
	VAR:            token.VAR,
	XOR_ASSIGN:     token.XOR_ASSIGN,
}

// Keywords represents the mapping of identifiers to Go reserved names.
var Keywords = map[string]Ch{
	"break":       BREAK,
	"case":        CASE,
	"chan":        CHAN,
	"const":       CONST,
	"continue":    CONTINUE,
	"default":     DEFAULT,
	"defer":       DEFER,
	"else":        ELSE,
	"fallthrough": FALLTHROUGH,
	"for":         FOR,
	"func":        FUNC,
	"go":          GO,
	"goto":        GOTO,
	"if":          IF,
	"import":      IMPORT,
	"interface":   INTERFACE,
	"map":         MAP,
	"package":     PACKAGE,
	"range":       RANGE,
	"return":      RETURN,
	"select":      SELECT,
	"struct":      STRUCT,
	"switch":      SWITCH,
	"type":        TYPE,
	"var":         VAR,
}

func (c Ch) str() string {
	if c < beforeTokens || c > afterTokens { //TODO
		return fmt.Sprintf("%#U", c)
	}

	return c.String()
}
