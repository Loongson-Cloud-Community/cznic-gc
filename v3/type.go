// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

var (
	Invalid = &InvalidType{}
)

var (
	_ Type = (*InvalidType)(nil)
	//_ Type = (*TypeDefNode)(nil)

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

func (n *InvalidType) Align() int        { return 1 }
func (n *InvalidType) FieldAlign() int   { return 1 }
func (n *InvalidType) Kind() Kind        { return InvalidKind }
func (n *InvalidType) Size() uintptr     { return 1 }
func (n *InvalidType) String() string    { return "<invalid type>" }
func (n *InvalidType) check(c *ctx) Type { return n }

// ChanDir represents a channel direction.
type ChanDir int

// Values of type ChanDir.
const (
	SendRecv ChanDir = iota
	SendOnly
	RecvOnly
)
