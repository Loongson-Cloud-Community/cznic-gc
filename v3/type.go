// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

var (
	_ Type = (*InvalidType)(nil)
	_ Type = (*TypeDefNode)(nil)
	_ Type = PredefinedType(0)
)

// Singleton instances of some compile-time only pseudo types.
var (
	Invalid Type = &InvalidType{}
)

const (
	unchecked guard = iota
	checking
	checked
)

type guard byte

func (g *guard) enter(c *ctx, n Node) bool {
	if n == nil {
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

func (t typer) setType(typ Type) { t.typ = t }

// Type returns the type of a node or Invalid if the type is
// unknown/undetermined.
func (t typer) Type() Type {
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

type typ interface {
	Type() Type
	setType(Type)
}

// Type is the representation of a Go type.
//
// The dynamic type of a Type is one of
//
//	PredefinedType
//	TODO ...
type Type interface {
	// Kind returns the specific kind of a type.
	//TODO Kind() Kind
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
