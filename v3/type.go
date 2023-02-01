// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"fmt"
	"go/token"
	"strings"
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
	_ Type = (*PredeclaredType)(nil)
	_ Type = (*SliceTypeNode)(nil)
	_ Type = (*StructTypeNode)(nil)
	_ Type = (*TupleType)(nil)
	_ Type = (*TypeDefNode)(nil)
	_ Type = (*TypeNameNode)(nil)
	_ Type = (*TypeNode)(nil)

	invalidRecursiveType = &InvalidType{}
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

func (n *typeCache) setType(t Type) Type {
	n.t = t
	return t
}

func (n *typeCache) enter(c *ctx, nd Node) bool {
	switch {
	case n.t == nil:
		n.t = invalidRecursiveType
		return true
	case n.t == invalidRecursiveType:
		n.t = Invalid
		c.err(nd, "invalid recursive type")
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
	Size() int64

	// String returns a string representation of the type.  The string
	// representation is not guaranteed to be unique among types.
	String() string

	check(c *ctx) Type
}

func (n *ArrayTypeNode) Align() int      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *ArrayTypeNode) FieldAlign() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *ArrayTypeNode) Kind() Kind      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *ArrayTypeNode) Size() int64     { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *ArrayTypeNode) String() string  { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *ArrayTypeNode) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

// ChanDir represents a channel direction.
type ChanDir int

// Values of type ChanDir.
const (
	SendRecv ChanDir = iota
	SendOnly
	RecvOnly
)

func (n *ChannelTypeNode) Align() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *ChannelTypeNode) FieldAlign() int {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ChannelTypeNode) Kind() Kind     { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *ChannelTypeNode) Size() int64    { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *ChannelTypeNode) String() string { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *ChannelTypeNode) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *FunctionTypeNode) Align() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *FunctionTypeNode) FieldAlign() int {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *FunctionTypeNode) Kind() Kind  { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *FunctionTypeNode) Size() int64 { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *FunctionTypeNode) String() string {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}
func (n *FunctionTypeNode) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *InterfaceTypeNode) Align() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *InterfaceTypeNode) FieldAlign() int {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *InterfaceTypeNode) Kind() Kind  { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *InterfaceTypeNode) Size() int64 { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *InterfaceTypeNode) String() string {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}
func (n *InterfaceTypeNode) check(c *ctx) Type {
	if !n.enter(c, n) {
		if n.guard == guardChecking {
			return Invalid
		}

		return n
	}

	defer func() { n.guard = guardChecked }()

	n.InterfaceElemList.check(c, n)
	return n
}

func (n *InterfaceElemListNode) check(c *ctx, t *InterfaceTypeNode) {
	if n == nil {
		return
	}

	for l := n; l != nil; l = l.List {
		l.InterfaceElem.check(c, t)
	}
}

func (n *InterfaceElemNode) check(c *ctx, t *InterfaceTypeNode) {
	if n == nil {
		return
	}

	n.MethodElem.check(c, t)
	n.TypeElem.check(c)
}

func (n *MethodElemNode) check(c *ctx, t *InterfaceTypeNode) {
	if n == nil {
		return
	}

	nm := n.MethodName.Src()
	if ex := t.methods[nm]; ex != nil {
		panic(todo(""))
	}

	if t.methods == nil {
		t.methods = map[string]*MethodElemNode{}
	}
	t.methods[nm] = n
	n.typ = n.Signature.check(c)
}

func (n *TypeElemListNode) check(c *ctx) {
	if n == nil {
		return
	}

	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

type InvalidType struct{}

func (n *InvalidType) Align() int                   { return 1 }
func (n *InvalidType) FieldAlign() int              { return 1 }
func (n *InvalidType) Kind() Kind                   { return InvalidKind }
func (n *InvalidType) Position() (r token.Position) { return r }
func (n *InvalidType) Size() int64                  { return 1 }
func (n *InvalidType) Source(full bool) string      { return "<invalid type>" }
func (n *InvalidType) String() string               { return "<invalid type>" }
func (n *InvalidType) check(c *ctx) Type            { return n }

func (n *MapTypeNode) Align() int        { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *MapTypeNode) FieldAlign() int   { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *MapTypeNode) Kind() Kind        { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *MapTypeNode) Size() int64       { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *MapTypeNode) String() string    { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *MapTypeNode) check(c *ctx) Type { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *ParenthesizedTypeNode) Align() int {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedTypeNode) FieldAlign() int {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedTypeNode) Kind() Kind {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedTypeNode) Size() int64 {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedTypeNode) String() string {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedTypeNode) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PointerTypeNode) Align() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *PointerTypeNode) FieldAlign() int {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PointerTypeNode) Kind() Kind     { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *PointerTypeNode) Size() int64    { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *PointerTypeNode) String() string { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *PointerTypeNode) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

type PredeclaredType struct {
	Node
	kind Kind
	t    ABIType
}

func (c *ctx) newPredeclaredType(n Node, kind Kind) *PredeclaredType {
	t, ok := c.cfg.abi.Types[kind]
	if !ok && !isAnyUntypedKind(kind) {
		panic(todo("%v: internal error %s: %s", n.Position(), n.Source(false), kind))
	}

	return &PredeclaredType{
		Node: n,
		kind: kind,
		t:    t,
	}
}

func (n *PredeclaredType) Align() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *PredeclaredType) FieldAlign() int {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PredeclaredType) Kind() Kind  { return n.kind }
func (n *PredeclaredType) Size() int64 { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *PredeclaredType) String() string {
	switch n.Kind() {
	case String:
		return n.Kind().String()
	default:
		panic(todo("%v: %s %s", n.Position(), n.Kind(), n.Source(false)))
	}
}

func (n *PredeclaredType) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *SliceTypeNode) Align() int      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *SliceTypeNode) FieldAlign() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *SliceTypeNode) Kind() Kind      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *SliceTypeNode) Size() int64     { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *SliceTypeNode) String() string  { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *SliceTypeNode) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *StructTypeNode) Align() int      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *StructTypeNode) FieldAlign() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *StructTypeNode) Kind() Kind      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *StructTypeNode) Size() int64     { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *StructTypeNode) String() string  { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *StructTypeNode) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

type TupleType struct {
	Node
	Types []Type
}

func newTupleType(n Node, types []Type) *TupleType { return &TupleType{n, types} }

func (n *TupleType) Align() int      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TupleType) FieldAlign() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TupleType) Kind() Kind      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *TupleType) Size() int64 { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *TupleType) Source(full bool) (r string) {
	if n.Node != nil {
		r = n.Node.Source(full)
	}
	return r
}

func (n *TupleType) String() string {
	var a []string
	for _, v := range n.Types {
		a = append(a, v.String())
	}
	return fmt.Sprintf("(%s)", strings.Join(a, ", "))
}

func (n *TupleType) check(c *ctx) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *TypeDefNode) Align() int      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeDefNode) FieldAlign() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeDefNode) Kind() Kind      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeDefNode) Size() int64     { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeDefNode) String() string  { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *TypeDefNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if n.pkg != nil {
		return n
	}

	n.pkg = c.pkg
	if n.TypeParameters != nil {
		panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
	}

	switch x := n.TypeNode.check(c).(type) {
	case *PredeclaredType:
		n.TypeNode = x
	}
	return n
}

func (n *TypeNameNode) Align() int      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNameNode) FieldAlign() int { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNameNode) Kind() Kind      { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNameNode) Size() int64     { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNameNode) String() string  { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func (n *TypeNameNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	switch x := n.Name.(type) {
	case Token:
		nm := x.Src()
		if c.isBuiltin() {
			switch nm {
			case "bool":
				return c.newPredeclaredType(n, Bool)
			case "uint8":
				return c.newPredeclaredType(n, Uint8)
			case "uint16":
				return c.newPredeclaredType(n, Uint16)
			case "uint32":
				return c.newPredeclaredType(n, Uint32)
			case "uint64":
				return c.newPredeclaredType(n, Uint64)
			case "int8":
				return c.newPredeclaredType(n, Int8)
			case "int16":
				return c.newPredeclaredType(n, Int16)
			case "int32":
				return c.newPredeclaredType(n, Int32)
			case "int64":
				return c.newPredeclaredType(n, Int64)
			case "float32":
				return c.newPredeclaredType(n, Float32)
			case "float64":
				return c.newPredeclaredType(n, Float64)
			case "complex64":
				return c.newPredeclaredType(n, Complex64)
			case "complex128":
				return c.newPredeclaredType(n, Complex128)
			case "string":
				return c.newPredeclaredType(n, String)
			case "int":
				return c.newPredeclaredType(n, Int)
			case "uint":
				return c.newPredeclaredType(n, Uint)
			case "uintptr":
				return c.newPredeclaredType(n, Uintptr)
			case "Type":
				// ok
			default:
				panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
			}
		}

		pkg, _, nmd := c.lookup(n.LexicalScope(), x)
		switch y := nmd.n.(type) {
		case *TypeDefNode:
			if pkg != c.pkg {
				return y
			}

			return y.check(c)
		case nil:
			panic(todo("%v: %T %s", x.Position(), y, x.Source(false)))
		default:
			panic(todo("%v: %T %s", y.Position(), y, y.Source(false)))
		}
	default:
		panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
	}
}

func (n *TypeNode) Align() int        { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNode) FieldAlign() int   { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNode) Kind() Kind        { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNode) Size() int64       { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNode) String() string    { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }
func (n *TypeNode) check(c *ctx) Type { panic(todo("%v: %T %s", n.Position(), n, n.Source(false))) }

func isAnyUntypedType(t Type) bool { return isAnyUntypedKind(t.Kind()) }

func isAnyUntypedKind(k Kind) bool {
	switch k {
	case UntypedBool, UntypedComplex, UntypedFloat, UntypedInt, UntypedNil, UntypedRune, UntypedString:
		return true
	}

	return false
}

const (
	guardUnchecked guard = iota
	guardChecking
	guardChecked
)

type guard byte

func (n *guard) enter(c *ctx, nd Node) bool {
	switch *n {
	case guardUnchecked:
		*n = guardChecking
		return true
	case guardChecking:
		c.err(nd, "invalid recursive type")
		return false
	default:
		return false
	}
}
