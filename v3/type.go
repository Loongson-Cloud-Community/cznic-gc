// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

var (
	_ Type = (*ChannelType)(nil)
	_ Type = (*InterfaceType)(nil)
	_ Type = (*InvalidType)(nil)
	_ Type = (*MapType)(nil)
	_ Type = (*SliceType)(nil)
	_ Type = (*TupleType)(nil)
	_ Type = (*TypeDefNode)(nil)
	_ Type = PredefinedType(0)
)

// Singleton instances of some types.
var (
	Invalid    Type = &InvalidType{}
	any             = &InterfaceType{}
	comparable      = &InterfaceType{}
)

const (
	unchecked guard = iota
	checking
	checked
)

type guard byte

func (g *guard) enter(c *ctx, n Node) bool {
	if n == nil || g == nil {
		return false
	}

	switch *g {
	case unchecked:
		*g = checking
		return true
	case checking:
		c.err(n, "type checking loop")
		return false
	case checked:
		return false
	default:
		panic(todo(""))
	}
}

func (g *guard) exit() { *g = checked }

type typer struct {
	guard
	typ Type
}

func newTyper(t Type) typer { return typer{typ: t} }

func (t *typer) setType(typ Type) Type { t.typ = t; return t }

// Kind implements Type.
func (t *typer) Kind() Kind { return t.Type().Kind() }

// Type returns the type of a node or Invalid if the type is
// unknown/undetermined.
func (t *typer) Type() Type {
	if t.typ != nil {
		return t.typ
	}

	switch t.guard {
	case unchecked:
		panic(todo("missed type check"))
	case checking:
		panic(todo("internal error: guard == %s", t.guard))
	default:
		return Invalid
	}
}

type typeNode interface {
	Node
	typ
}

type typ interface {
	Type() Type
	check(*ctx) Type
	setType(Type) Type
}

// Type is the representation of a Go type.
//
// The dynamic type of a Type is one of
//
//	PredefinedType
//	TODO ...
type Type interface {
	// Kind returns the specific kind of a type.
	Kind() Kind
}

// A Kind represents the specific kind of type that a Type represents. The zero
// Kind is not a valid kind.
type Kind int

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
	UntypedBool    // untyped bool
	UntypedComplex // untyped complex
	UntypedFloat   // untyped float
	UntypedInt     // untyped int
	UntypedNil     // untyped nil
	UntypedString  // untyped string
)

// InvalidType represents an invalid type.
type InvalidType struct{}

// Kind implements Type.
func (t *InvalidType) Kind() Kind { return InvalidKind }

// PredefinedType represents a predefined type.
type PredefinedType Kind

// Kind implements Type.
func (t PredefinedType) Kind() Kind { return Kind(t) }

// InterfaceType represents an interface type.
type InterfaceType struct {
	Elems []Node //TODO
}

// Kind implements Type.
func (t *InterfaceType) Kind() Kind { return Interface }

// TupleType represents an ordered list of types.
type TupleType struct {
	Types []Type
}

func newTupleType(types []Type) *TupleType { return &TupleType{types} }

// Kind implements Type.
func (t *TupleType) Kind() Kind { return Tuple }

// SliceType represents a slice type.
type SliceType struct {
	Elem Type
}

// Kind implements Type.
func (t *SliceType) Kind() Kind { return Slice }

// MapType represents a map type.
type MapType struct {
	Elem Type
	Key  Type
}

// Kind implements Type.
func (t *MapType) Kind() Kind { return Map }

// ChanDir represents a channel direction.
type ChanDir int

// Values of type ChanDir.
const (
	SendRecv ChanDir = iota
	SendOnly
	RecvOnly
)

// ChannelType represents a channel type.
type ChannelType struct {
	Dir  ChanDir
	Elem Type
}

// Kind implements Type.
func (t *ChannelType) Kind() Kind { return Chan }
