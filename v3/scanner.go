// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v3"

import (
	"go/token"

	mtoken "modernc.org/token"
)

var (
	_ Node = (*Token)(nil)
)

// Node is an item of the CST tree.
type Node interface {
	Position() token.Position
}

// Token represents a lexeme, its position and its semantic value.
type Token struct { // 16 bytes on 64 bit arch
	source *source

	ch    int32
	index int32
}

// Ch returns which token t represents
func (t *Token) Ch() token.Token { return token.Token(t.ch) }

// Positions implements Node.
func (t *Token) Position() (r token.Position) {
	s := t.source
	return token.Position(s.file.PositionFor(mtoken.Pos(s.base+s.toks[t.index].src), true))
}

func (t *Token) tok(delta int) tok {
	index := int(t.index) + delta
	if index >= len(t.source.toks) {
		index = len(t.source.toks)
	}
	return t.source.toks[index]
}

// Prev returns the token preceding t or a zero value if no such token exists.
func (t *Token) Prev() (r Token) {
	if t.index > 0 {
		tok := t.tok(-1)
		return Token{source: t.source, ch: tok.ch, index: t.index - 1}
	}

	return r
}

// Next returns the token following t or a zero value if no such token exists.
func (t *Token) Next() (r Token) {
	if int(t.index) < len(t.source.toks) {
		tok := t.tok(1)
		return Token{source: t.source, ch: tok.ch, index: t.index + 1}
	}

	return r
}

// Sep returns any separators, combined, preceding t.
func (t *Token) Sep() string {
	s := t.source
	if p, ok := s.sepPatches[t.index]; ok {
		return p
	}

	tok := t.tok(0)
	return string(s.buf[tok.sep:tok.src])
}

// SetSep sets t's separator.
func (t *Token) SetSep(s string) {
	src := t.source
	if src.sepPatches == nil {
		src.sepPatches = map[int32]string{}
	}
	src.sepPatches[t.index] = s
}

// Src returns t's source form.
func (t *Token) Src() string {
	s := t.source
	if p, ok := s.srcPatches[t.index]; ok {
		return p
	}

	return string(s.buf[t.tok(0).src:t.tok(1).sep])
}

// SetSrc sets t's source form.
func (t *Token) SetSrc(s string) {
	src := t.source
	if src.srcPatches == nil {
		src.srcPatches = map[int32]string{}
	}
	src.srcPatches[t.index] = s
}

type tok struct { // 12 bytes
	ch  int32
	sep int32
	src int32
}

// source represents a single Go source file, editor text buffer etc.
type source struct {
	buf        []byte
	file       *mtoken.File
	name       string
	sepPatches map[int32]string
	srcPatches map[int32]string
	toks       []tok

	base int32
}

// 'buf' becomes owned by the result and must not be modified afterwards.
func newSource(name string, buf []byte) (r *source, err error) {
	file := mtoken.NewFile(name, len(buf))
	r = &source{
		buf:  buf,
		file: file,
		name: name,
		base: int32(file.Base()),
	}
	panic(todo(""))
}
