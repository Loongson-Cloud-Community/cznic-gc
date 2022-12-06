// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"fmt"
	"strings"
)

var (
	_ Type = (*ChannelType)(nil)
	_ Type = (*InterfaceType)(nil)
	_ Type = (*InvalidType)(nil)
	_ Type = (*MapType)(nil)
	_ Type = (*PointerType)(nil)
	_ Type = (*SliceType)(nil)
	_ Type = (*StructType)(nil)
	_ Type = (*TupleType)(nil)
	_ Type = (*TypeDefNode)(nil)
	_ Type = PredefinedType(0)
)

// Singleton instances of some types.
var (
	Invalid Type = &InvalidType{}

	any            Type = &InterfaceType{}
	comparable     Type = &InterfaceType{}
	untypedBool    Type = PredefinedType(UntypedBool)
	untypedComplex Type = PredefinedType(UntypedComplex)
	untypedFloat   Type = PredefinedType(UntypedFloat)
	untypedInt     Type = PredefinedType(UntypedInt)
	untypedRune    Type = PredefinedType(UntypedRune)
	untypedString  Type = PredefinedType(UntypedString)
)

const (
	typeGuardUnchecked guard = iota
	typeGuardChecking
	typeGuardChecked
)

type guard byte

func (g *guard) enter(c *ctx, n Node) bool {
	if n == nil || g == nil {
		return false
	}

	switch *g {
	case typeGuardUnchecked:
		*g = typeGuardChecking
		return true
	case typeGuardChecking:
		c.err(n, "type checking loop")
		return false
	case typeGuardChecked:
		return false
	default:
		panic(todo(""))
	}
}

func (g *guard) exit() { *g = typeGuardChecked }

type typer struct {
	guard
	typ Type
}

func newTyper(t Type) typer { return typer{typ: t} }

func (t *typer) setType(typ Type) Type {
	t.typ = typ
	return typ
}

// Kind implements Type.
func (t *typer) Kind() Kind { return t.Type().Kind() }

// Type returns the type of a node or Invalid if the type is
// unknown/undetermined.
func (t *typer) Type() Type {
	if t.typ != nil {
		return t.typ
	}

	switch t.guard {
	case typeGuardUnchecked:
		panic(todo("missed type check"))
	case typeGuardChecking:
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
	check(*ctx, *Expression) Type
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
	String() string
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
	UntypedRune    // untyped rune
	UntypedString  // untyped string
)

// InvalidType represents an invalid type.
type InvalidType struct{}

// Kind implements Type.
func (t *InvalidType) Kind() Kind { return InvalidKind }

// String implements Type.
func (t *InvalidType) String() string { return "<invalid type>" }

// PredefinedType represents a predefined type.
type PredefinedType Kind

// Kind implements Type.
func (t PredefinedType) Kind() Kind { return Kind(t) }

// String implements Type.
func (t PredefinedType) String() string { return t.Kind().String() }

// InterfaceType represents an interface type.
type InterfaceType struct {
	Elems []Node //TODO
}

// Kind implements Type.
func (t *InterfaceType) Kind() Kind { return Interface }

// String implements Type.
func (t *InterfaceType) String() string {
	if t == nil {
		return ""
	}

	var b strings.Builder
	b.WriteString("interface{")
	if len(t.Elems) != 0 {
		b.WriteRune(' ')
	}
	for i, v := range t.Elems {
		if i != 0 {
			b.WriteString("; ")
		}
		b.WriteString(strings.TrimSpace(v.Source(false)))
	}
	if len(t.Elems) != 0 {
		b.WriteRune(' ')
	}
	b.WriteByte('}')
	return b.String()
}

// TupleType represents an ordered list of types.
type TupleType struct {
	Types []Type
}

func newTupleType(types []Type) *TupleType { return &TupleType{types} }

// Kind implements Type.
func (t *TupleType) Kind() Kind { return Tuple }

// String implements Type.
func (t *TupleType) String() string {
	if t == nil {
		return ""
	}

	var b strings.Builder
	b.WriteString("(")
	for i, v := range t.Types {
		if i != 0 {
			b.WriteString(", ")
		}
		fmt.Fprintf(&b, "%s", v)
	}
	b.WriteByte(')')
	return b.String()
}

// SliceType represents a slice type.
type SliceType struct {
	Elem Type
}

// Kind implements Type.
func (t *SliceType) Kind() Kind { return Slice }

// String implements Type.
func (t *SliceType) String() string {
	if t == nil {
		return ""
	}

	return fmt.Sprintf("[]%s", t.Elem)
}

// MapType represents a map type.
type MapType struct {
	Elem Type
	Key  Type
}

// Kind implements Type.
func (t *MapType) Kind() Kind { return Map }

// String implements Type.
func (t *MapType) String() string {
	if t == nil {
		return ""
	}

	return fmt.Sprintf("map[%s]%s", t.Key, t.Elem)
}

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

// String implements Type.
func (t *ChannelType) String() string {
	if t == nil {
		return ""
	}

	var s string
	switch t.Dir {
	case SendRecv:
		s = "chan"
	case SendOnly:
		s = "chan<-"
	case RecvOnly:
		s = "<-chan"
	default:
		panic(todo("internal error: %s", t.Dir))
	}
	return fmt.Sprintf("%s %s", s, t.Elem)
}

// PointerType represents a pointer type.
type PointerType struct {
	Elem Type
}

func newPointer(pkg *Package, t Type) *PointerType {
	return &PointerType{Elem: t}
}

// Kind implements Type.
func (t *PointerType) Kind() Kind { return Pointer }

// String implements Type.
func (t *PointerType) String() string {
	if t == nil {
		return ""
	}

	return fmt.Sprintf("*%s", t.Elem)
}

// StructType represents a struct type.
type StructType struct {
	Fields []*FieldDeclNode
}

// Kind implements Type.
func (t *StructType) Kind() Kind { return Struct }

// String implements Type.
func (t *StructType) String() string {
	if t == nil {
		return ""
	}

	var b strings.Builder
	b.WriteString("struct{")
	if len(t.Fields) != 0 {
		b.WriteRune(' ')
	}
	for i, v := range t.Fields {
		if i != 0 {
			b.WriteString("; ")
		}
		switch {
		case v.EmbeddedField != nil:
			fmt.Fprintf(&b, "%s", strings.TrimSpace(v.EmbeddedField.Source(false)))
		default:
			fmt.Fprintf(&b, "%s %s", v.IdentifierList.IDENT.Src(), v.TypeNode.Type())
		}
	}
	if len(t.Fields) != 0 {
		b.WriteRune(' ')
	}
	b.WriteByte('}')
	return b.String()
}
