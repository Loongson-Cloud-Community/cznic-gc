// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/token"
)

var (
	Invalid = &InvalidType{}
)

var (
	_ Type = (*ArrayTypeNode)(nil)
	_ Type = (*ChannelTypeNode)(nil)
	_ Type = (*FunctionTypeNode)(nil)
	_ Type = (*InterfaceTypeNode)(nil)
	_ Type = (*InvalidType)(nil)
	_ Type = (*MapTypeNode)(nil)
	_ Type = (*ParenthesizedTypeNode)(nil)
	_ Type = (*PointerTypeNode)(nil)
	_ Type = (*SliceTypeNode)(nil)
	_ Type = (*StructTypeNode)(nil)
	_ Type = (*TypeDefNode)(nil)
	_ Type = (*TypeNameNode)(nil)
	_ Type = (*TypeNode)(nil)

	typeCheckLoop = &InvalidType{}
)

// A Kind represents the specific kind of type that a Type represents. The zero
// Kind is not a valid kind.
type Kind byte

// Values of type Kind
const (
	InvalidKind Kind = iota // <invalid type>

	Array          // array
	Bool           // bool
	Chan           // chan
	Complex128     // complex128
	Complex64      // complex64
	Float32        // float32
	Float64        // float64
	Function       // function
	Int            // int
	Int16          // int16
	Int32          // int32
	Int64          // int64
	Int8           // int8
	Interface      // interface
	Map            // map
	Pointer        // pointer
	Slice          // slice
	String         // string
	Struct         // struct
	Tuple          // tuple
	Uint           // uint
	Uint16         // uint16
	Uint32         // uint32
	Uint64         // uint64
	Uint8          // uint8
	Uintptr        // uintptr
	UnsafePointer  // unsafe.Pointer
	UntypedBool    // untyped bool
	UntypedComplex // untyped complex
	UntypedFloat   // untyped float
	UntypedInt     // untyped int
	UntypedNil     // untyped nil
	UntypedRune    // untyped rune
	UntypedString  // untyped string
)

type typeCache struct {
	t Type
}

func (n *typeCache) Type() Type {
	if n.t != nil {
		return n.t
	}

	return Invalid
}

func (n *typeCache) set(t Type) Type {
	n.t = t
	return t
}

func (n *typeCache) enter(c *ctx, nd Node) bool {
	switch {
	case n.t == nil:
		n.t = typeCheckLoop
		return true
	case n.t == typeCheckLoop:
		n.t = Invalid
		c.err(nd, "type checking loop")
		return false
	default:
		return false
	}
}

type typer interface {
	Type() Type
}

type Type interface {
	Node

	// Align returns the alignment in bytes of a value of this type when allocated
	// in memory.
	Align() int

	// FieldAlign returns the alignment in bytes of a value of this type when used
	// as a field in a struct.
	FieldAlign() int

	// Kind returns the specific kind of this type.
	Kind() Kind

	// Size returns the number of bytes needed to store a value of the given type;
	// it is analogous to unsafe.Sizeof.
	Size() uintptr

	// String returns a string representation of the type.  The string
	// representation is not guaranteed to be unique among types. To test for type
	// identity, compare the Types directly.
	String() string

	check(c *ctx) Type
}

type InvalidType struct{}

func (n *InvalidType) Align() int                   { return 1 }
func (n *InvalidType) FieldAlign() int              { return 1 }
func (n *InvalidType) Kind() Kind                   { return InvalidKind }
func (n *InvalidType) Position() (r token.Position) { return r }
func (n *InvalidType) Size() uintptr                { return 1 }
func (n *InvalidType) Source(full bool) string      { return "<invalid type>" }
func (n *InvalidType) String() string               { return "<invalid type>" }
func (n *InvalidType) check(c *ctx) Type            { return n }

func (n *TypeNode) Align() int        { panic(todo("")) }
func (n *TypeNode) FieldAlign() int   { panic(todo("")) }
func (n *TypeNode) Kind() Kind        { panic(todo("")) }
func (n *TypeNode) Size() uintptr     { panic(todo("")) }
func (n *TypeNode) String() string    { panic(todo("")) }
func (n *TypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *TypeNameNode) Align() int        { panic(todo("")) }
func (n *TypeNameNode) FieldAlign() int   { panic(todo("")) }
func (n *TypeNameNode) Kind() Kind        { panic(todo("")) }
func (n *TypeNameNode) Size() uintptr     { panic(todo("")) }
func (n *TypeNameNode) String() string    { panic(todo("")) }
func (n *TypeNameNode) check(c *ctx) Type { panic(todo("")) }

func (n *ParenthesizedTypeNode) Align() int        { panic(todo("")) }
func (n *ParenthesizedTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *ParenthesizedTypeNode) Kind() Kind        { panic(todo("")) }
func (n *ParenthesizedTypeNode) Size() uintptr     { panic(todo("")) }
func (n *ParenthesizedTypeNode) String() string    { panic(todo("")) }
func (n *ParenthesizedTypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *SliceTypeNode) Align() int        { panic(todo("")) }
func (n *SliceTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *SliceTypeNode) Kind() Kind        { panic(todo("")) }
func (n *SliceTypeNode) Size() uintptr     { panic(todo("")) }
func (n *SliceTypeNode) String() string    { panic(todo("")) }
func (n *SliceTypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *ArrayTypeNode) Align() int        { panic(todo("")) }
func (n *ArrayTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *ArrayTypeNode) Kind() Kind        { panic(todo("")) }
func (n *ArrayTypeNode) Size() uintptr     { panic(todo("")) }
func (n *ArrayTypeNode) String() string    { panic(todo("")) }
func (n *ArrayTypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *StructTypeNode) Align() int        { panic(todo("")) }
func (n *StructTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *StructTypeNode) Kind() Kind        { panic(todo("")) }
func (n *StructTypeNode) Size() uintptr     { panic(todo("")) }
func (n *StructTypeNode) String() string    { panic(todo("")) }
func (n *StructTypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *PointerTypeNode) Align() int        { panic(todo("")) }
func (n *PointerTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *PointerTypeNode) Kind() Kind        { panic(todo("")) }
func (n *PointerTypeNode) Size() uintptr     { panic(todo("")) }
func (n *PointerTypeNode) String() string    { panic(todo("")) }
func (n *PointerTypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *FunctionTypeNode) Align() int        { panic(todo("")) }
func (n *FunctionTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *FunctionTypeNode) Kind() Kind        { panic(todo("")) }
func (n *FunctionTypeNode) Size() uintptr     { panic(todo("")) }
func (n *FunctionTypeNode) String() string    { panic(todo("")) }
func (n *FunctionTypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *InterfaceTypeNode) Align() int        { panic(todo("")) }
func (n *InterfaceTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *InterfaceTypeNode) Kind() Kind        { panic(todo("")) }
func (n *InterfaceTypeNode) Size() uintptr     { panic(todo("")) }
func (n *InterfaceTypeNode) String() string    { panic(todo("")) }
func (n *InterfaceTypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *MapTypeNode) Align() int        { panic(todo("")) }
func (n *MapTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *MapTypeNode) Kind() Kind        { panic(todo("")) }
func (n *MapTypeNode) Size() uintptr     { panic(todo("")) }
func (n *MapTypeNode) String() string    { panic(todo("")) }
func (n *MapTypeNode) check(c *ctx) Type { panic(todo("")) }

// ChanDir represents a channel direction.
type ChanDir int

// Values of type ChanDir.
const (
	SendRecv ChanDir = iota
	SendOnly
	RecvOnly
)

func (n *ChannelTypeNode) Align() int        { panic(todo("")) }
func (n *ChannelTypeNode) FieldAlign() int   { panic(todo("")) }
func (n *ChannelTypeNode) Kind() Kind        { panic(todo("")) }
func (n *ChannelTypeNode) Size() uintptr     { panic(todo("")) }
func (n *ChannelTypeNode) String() string    { panic(todo("")) }
func (n *ChannelTypeNode) check(c *ctx) Type { panic(todo("")) }

func (n *TypeDefNode) Align() int        { panic(todo("")) }
func (n *TypeDefNode) FieldAlign() int   { panic(todo("")) }
func (n *TypeDefNode) Kind() Kind        { panic(todo("")) }
func (n *TypeDefNode) Size() uintptr     { panic(todo("")) }
func (n *TypeDefNode) String() string    { panic(todo("")) }
func (n *TypeDefNode) check(c *ctx) Type { panic(todo("")) }
