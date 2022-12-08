// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"fmt"
	"strings"

	"modernc.org/mathutil"
)

var (
	_ Type = (*ArrayType)(nil)
	_ Type = (*ChannelType)(nil)
	_ Type = (*InterfaceType)(nil)
	_ Type = (*InvalidType)(nil)
	_ Type = (*MapType)(nil)
	_ Type = (*PointerType)(nil)
	_ Type = (*PredefinedType)(nil)
	_ Type = (*SliceType)(nil)
	_ Type = (*StructType)(nil)
	_ Type = (*TupleType)(nil)
	_ Type = (*TypeDefNode)(nil)
)

// Singleton instances of some types.
var (
	Invalid Type = &InvalidType{}

	comparable = &InterfaceType{}

	untypedBool    Type = &PredefinedType{kind: UntypedBool}
	untypedComplex Type = &PredefinedType{kind: UntypedComplex}
	untypedFloat   Type = &PredefinedType{kind: UntypedFloat}
	untypedInt     Type = &PredefinedType{kind: UntypedInt}
	untypedRune    Type = &PredefinedType{kind: UntypedRune}
	untypedString  Type = &PredefinedType{kind: UntypedString}
)

// Type is the representation of a Go type.
//
// The dynamic type of a Type is one of
//
//	*ArrayType
//	*ChannelType
//	*InterfaceType
//	*InvalidType
//	*MapType
//	*PointerType
//	*PredefinedType
//	*SliceType
//	*StructType
//	*TupleType
//	*TypeDefNode
type Type interface {
	// Kind returns the specific kind of a type.
	Kind() Kind
	// Align returns the alignment in bytes of a value of this type when allocated
	// in memory.  A negative value is reported when the alignment was not
	// determined.
	Align() int64
	// FieldAlign returns the alignment in bytes of a value of this type when used
	// as a field in a struct.  A negative value is reported when the field
	// alignment was not determined.
	FieldAlign() int64
	// Sizeof reports the size of a type in bytes. A negative value is reported
	// when the size was not determined.
	// Size returns the number of bytes needed to store a value of the given type.
	// A negative value is reported when the size was not determined.
	Size() int64
	String() string
}

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

type typeNode interface {
	Node
	typ
}

type typ interface {
	Type() Type
	check(*ctx, *Expression) Type
	setType(Type, *ctx, Node) Type
}

type sizer interface{ size(*ctx, Node) }

const (
	guardUnchecked guard = iota
	guardChecking
	guardChecked
)

const (
	guardMask = ^sizeMask
	sizeMask  = uint64(1)<<sizeBits - 1
	sizeBits  = 56
)

type guard byte

func (g *guard) enter(c *ctx, n Node) bool {
	if g == nil {
		c.err(n, "enter: internal error")
		return false
	}

	switch *g {
	case guardUnchecked:
		*g = guardChecking
		return true
	case guardChecking:
		c.err(n, "type checking loop")
		return false
	case guardChecked:
		return false
	default:
		panic(todo("", *g))
	}
}

func (g *guard) exit() { *g = guardChecked }

type typer struct {
	guard
	typ Type
}

func newTyper(t Type) typer { return typer{typ: t} }

func (t *typer) setType(typ Type, c *ctx, n Node) Type {
	t.typ = typ
	if x, ok := typ.(sizer); ok {
		x.size(c, n)
	}
	return typ
}

// Kind implements Type.
func (t *typer) Kind() Kind { return t.Type().Kind() }

type exiter interface{ exit() }

func setChecked(e Expression) {
	if x, ok := e.(exiter); ok {
		x.exit()
	}
}

// Align implements Type.
func (t *typer) Align() int64 {
	if t.guard == guardChecked && t.typ != nil {
		return t.Type().Align()
	}

	return -1
}

// FieldAlign implements Type.
func (t *typer) FieldAlign() int64 {
	if t.guard == guardChecked && t.typ != nil {
		return t.Type().Align()
	}

	return -1
}

// Size implements Type.
func (t *typer) Size() int64 {
	if t.guard != guardChecked {
		return -1
	}

	return t.Type().Size()
}

// Type returns the type of a node or Invalid if the type is
// unknown/undetermined.
func (t *typer) Type() Type {
	if t.typ != nil {
		return t.typ
	}

	switch t.guard {
	case guardUnchecked:
		panic(todo("missed type check"))
	case guardChecking:
		panic(todo("internal error: guard == %s", t.guard))
	default:
		return Invalid
	}
}

// InvalidType represents an invalid type.
type InvalidType struct{}

// Align implements Type.
func (t *InvalidType) Align() int64 { return -1 }

// FieldAlign implements Type.
func (t *InvalidType) FieldAlign() int64 { return -1 }

// Size implements Type.
func (t *InvalidType) Size() int64 { return -1 }

// Kind implements Type.
func (t *InvalidType) Kind() Kind { return InvalidKind }

// String implements Type.
func (t *InvalidType) String() string { return "<invalid type>" }

// PredefinedType represents a predefined type.
type PredefinedType struct {
	kind Kind
	cfg  *Config
}

// Align implements Type.
func (t *PredefinedType) Align() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Align
	}

	return -1
}

// FieldAlign implements Type.
func (t *PredefinedType) FieldAlign() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].FieldAlign
	}

	return -1
}

// Size implements Type.
func (t *PredefinedType) Size() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Size
	}

	return -1
}

// Kind implements Type.
func (t *PredefinedType) Kind() Kind { return t.kind }

// String implements Type.
func (t *PredefinedType) String() string { return t.Kind().String() }

// InterfaceType represents an interface type.
type InterfaceType struct {
	Elems []Node //TODO
	cfg   *Config
}

// Align implements Type.
func (t *InterfaceType) Align() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Align
	}

	return -1
}

// FieldAlign implements Type.
func (t *InterfaceType) FieldAlign() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].FieldAlign
	}

	return -1
}

// Size implements Type.
func (t *InterfaceType) Size() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Size
	}

	return -1
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

// Align implements Type.
func (t *TupleType) Align() int64 { return -1 }

// FieldAlign implements Type.
func (t *TupleType) FieldAlign() int64 { return -1 }

// Size implements Type.
func (t *TupleType) Size() (r int64) { return -1 }

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
	cfg  *Config
}

// Align implements Type.
func (t *SliceType) Align() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Align
	}

	return -1
}

// FieldAlign implements Type.
func (t *SliceType) FieldAlign() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].FieldAlign
	}

	return -1
}

// Size implements Type.
func (t *SliceType) Size() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Size
	}

	return -1
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
	cfg  *Config
}

// Align implements Type.
func (t *MapType) Align() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Align
	}

	return -1
}

// FieldAlign implements Type.
func (t *MapType) FieldAlign() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].FieldAlign
	}

	return -1
}

// Size implements Type.
func (t *MapType) Size() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Size
	}

	return -1
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
	cfg  *Config
}

// Align implements Type.
func (t *ChannelType) Align() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Align
	}

	return -1
}

// FieldAlign implements Type.
func (t *ChannelType) FieldAlign() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].FieldAlign
	}

	return -1
}

// Size implements Type.
func (t *ChannelType) Size() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Size
	}

	return -1
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
	cfg  *Config
}

func newPointer(pkg *Package, t Type) *PointerType {
	return &PointerType{Elem: t, cfg: pkg.cfg}
}

// Align implements Type.
func (t *PointerType) Align() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Align
	}

	return -1
}

// FieldAlign implements Type.
func (t *PointerType) FieldAlign() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].FieldAlign
	}

	return -1
}

// Size implements Type.
func (t *PointerType) Size() int64 {
	if t.cfg != nil {
		return t.cfg.abi.Types[t.Kind()].Size
	}

	return -1
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

// ArrayType represents a pointer type.
type ArrayType struct {
	Elems int64
	Elem  Type

	g guard
}

// size implements sizer.
func (t *ArrayType) size(c *ctx, n Node) {
	switch t.g {
	case guardUnchecked:
		t.g = guardChecking

		defer func() { t.g = guardChecked }()
	case guardChecking:
		c.err(n, "type checking loop")
		return
	case guardChecked:
		return
	default:
		c.err(n, "%T.size: internal error: %s", t, t.g)
		return
	}

	if x, ok := t.Elem.(sizer); ok {
		x.size(c, n)
	}
}

// Align implements Type.
func (t *ArrayType) Align() int64 {
	if t.Elem != nil {
		return t.Elem.Align()
	}

	return -1
}

// FieldAlign implements Type.
func (t *ArrayType) FieldAlign() int64 {
	if t.Elem != nil {
		return t.Elem.FieldAlign()
	}

	return -1
}

// Size implements Type.
func (t *ArrayType) Size() int64 {
	if sz := t.Elem.Size(); sz >= 0 && t.Elems >= 0 {
		return sz * t.Elems
	}

	return -1
}

// Kind implements Type.
func (t *ArrayType) Kind() Kind { return Array }

// String implements Type.
func (t *ArrayType) String() string {
	if t == nil {
		return ""
	}

	return fmt.Sprintf("[%d]%s", t.Elems, t.Elem)
}

type Field struct {
	n   *FieldDeclNode
	off int64
}

func (f *Field) Name() string {
	panic(todo(""))
}

func (f *Field) Type() Type {
	return f.n.TypeNode.Type()
}

func (f *Field) Tag() string {
	panic(todo(""))
}

func (f *Field) Offset() int64 {
	return f.off
}

func (f *Field) IsEmbedded() bool {
	panic(todo(""))
}

// StructType represents a struct type.
type StructType struct {
	Fields []Field

	sz int64

	align      int8
	fieldAlign int8

	g guard
}

// Align implements Type.
func (t *StructType) Align() int64 {
	if t.g != guardChecked {
		return -1
	}

	return int64(t.align)
}

// FieldAlign implements Type.
func (t *StructType) FieldAlign() int64 {
	if t.g != guardChecked {
		return -1
	}

	return int64(t.fieldAlign)
}

// size implements sizer.
func (t *StructType) size(c *ctx, n Node) {
	switch t.g {
	case guardUnchecked:
		t.g = guardChecking

		defer func() { t.g = guardChecked }()
	case guardChecking:
		c.err(n, "type checking loop")
		return
	case guardChecked:
		return
	default:
		c.err(n, "%T.size: internal error: %s", t, t.g)
		return
	}

	var off int64
	var f Field
	t.sz = -1
	t.align = -1
	t.fieldAlign = -1
	for _, f = range t.Fields {
		ft := f.Type()
		if x, ok := ft.(sizer); ok {
			x.size(c, n)
		}
		sz := f.Type().Size()
		al := f.Type().FieldAlign()
		if al < 0 || sz < 0 {
			t.align = -1
			t.fieldAlign = -1
			return
		}

		t.align = int8(mathutil.MaxInt8(t.align, int8(al)))
		off = roundup(off, al)
		f.off = off
		off += sz
	}
	if f.Type().Size() == 0 {
		off++
	}
	off = roundup(off, int64(t.align))
	t.sz = off
}

// Size implements Type.
func (t *StructType) Size() int64 {
	panic(todo(""))
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
		v := v.n
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
