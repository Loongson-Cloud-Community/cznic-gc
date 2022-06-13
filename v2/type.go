// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"go/token"
)

var (
	// Invalid is a singleton representing an invalid/undetermined type.  Invalid
	// is comparable.
	Invalid Type = &InvalidType{}

	untypedBool    = PredefinedType(UntypedBool)
	untypedComplex = PredefinedType(UntypedComplex)
	untypedFloat   = PredefinedType(UntypedFloat)
	untypedInt     = PredefinedType(UntypedInt)
	untypedString  = PredefinedType(UntypedString)
	void           = PredefinedType(Void)
)

var (
	_ Type = (*AliasType)(nil)
	_ Type = (*ArrayType)(nil)
	_ Type = (*ChannelType)(nil)
	_ Type = (*FunctionType)(nil)
	_ Type = (*InterfaceType)(nil)
	_ Type = (*InvalidType)(nil)
	_ Type = (*MapType)(nil)
	_ Type = (*PointerType)(nil)
	_ Type = (*SliceType)(nil)
	_ Type = (*StructType)(nil)
	_ Type = (*TypeName)(nil)
	_ Type = PredefinedType(0)
)

// A Kind represents the specific kind of type that a Type represents. The zero
// Kind is not a valid kind.
type Kind int

// Values of type Kind
const (
	InvalidKind Kind = iota // <invalid type>

	Alias          // alias
	Array          // array
	Bool           // bool
	Chan           // chan
	Complex128     // complex128
	Complex64      // complex64
	DefinedType    // typename
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
	UntypedString  // untyped string
	Void           // void
)

type typer struct{ typ Type }

func newTyper(t Type) typer { return typer{typ: t} }

// Type returns the type of a node or an *Invalid type value, if the type is
// unknown/undetermined.
func (t typer) Type() Type {
	if t.typ != nil {
		return t.typ
	}

	return Invalid
}

// Type is the representation of a Go type.
//
// The dynamic type of a Type is one of
//
//	*AliasType
//	*ArrayType
//	*ChannelType
//	*FunctionType
//	*InterfaceType
//	*InvalidType
//	*MapType
//	*PointerType
//	*PredefinedType
//	*SliceType
//	*StructType
//	*TypeName
//	InvalidType
type Type interface {
	Node
	Kind() Kind
}

// InvalidType represents an invalid type.
type InvalidType struct{}

// Position implements Node. Position returns a zero value.
func (t InvalidType) Position() (r token.Position) { return r }

// Source implements Node. It return nil.
func (t InvalidType) Source(full bool) []byte { return nil }

// Kind implements Type.
func (t InvalidType) Kind() Kind { return InvalidKind }

// PredefinedType represents a predefined type.
type PredefinedType Kind

// Position implements Node. Position returns a zero value.
func (t PredefinedType) Position() (r token.Position) { return r }

// Source implements Node. It return nil.
func (t PredefinedType) Source(full bool) []byte { return nil }

// Kind implements Type.
func (t PredefinedType) Kind() Kind { return Kind(t) }

// ArrayType represents an array type.
type ArrayType struct {
	node *ArrayTypeNode
	Elem Type
	Len  int64
}

// Kind implements Type.
func (t ArrayType) Kind() Kind { return Array }

// Position implements Node.
func (t ArrayType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t ArrayType) Source(full bool) []byte { return t.node.Source(full) }

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
	node *ChannelTypeNode
	Dir  ChanDir
	Elem Type
}

// Kind implements Type.
func (t ChannelType) Kind() Kind { return Chan }

// Position implements Node.
func (t ChannelType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t ChannelType) Source(full bool) []byte { return t.node.Source(full) }

// FunctionType represents a channel type.
type FunctionType struct {
	node *FunctionTypeNode
	//TODO
}

// Kind implements Type.
func (t FunctionType) Kind() Kind { return Function }

// Position implements Node.
func (t FunctionType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t FunctionType) Source(full bool) []byte { return t.node.Source(full) }

// InterfaceType represents an interface type.
type InterfaceType struct {
	node *InterfaceTypeNode
	//TODO
}

// Kind implements Type.
func (t InterfaceType) Kind() Kind { return Interface }

// Position implements Node.
func (t InterfaceType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t InterfaceType) Source(full bool) []byte { return t.node.Source(full) }

// MapType represents a map type.
type MapType struct {
	node *MapTypeNode
	Elem Type
	Key  Type
}

// Kind implements Type.
func (t MapType) Kind() Kind { return Map }

// Position implements Node.
func (t MapType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t MapType) Source(full bool) []byte { return t.node.Source(full) }

// PointerType represents a pointer type.
type PointerType struct {
	node *PointerTypeNode
	Elem Type
}

// Kind implements Type.
func (t PointerType) Kind() Kind { return Pointer }

// Position implements Node.
func (t PointerType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t PointerType) Source(full bool) []byte { return t.node.Source(full) }

// SliceType represents a slice type.
type SliceType struct {
	node *SliceTypeNode
	Elem Type
}

// Kind implements Type.
func (t SliceType) Kind() Kind { return Slice }

// Position implements Node.
func (t SliceType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t SliceType) Source(full bool) []byte { return t.node.Source(full) }

// StructType represents a struct type.
type StructType struct {
	node *StructTypeNode
}

// Kind implements Type.
func (t StructType) Kind() Kind { return Struct }

// Position implements Node.
func (t StructType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t StructType) Source(full bool) []byte { return t.node.Source(full) }

// AliasType represents an alias type.
type AliasType struct {
	typer
	node *AliasDecl
}

// Kind implements Type.
func (t AliasType) Kind() Kind { return Alias }

// Position implements Node.
func (t AliasType) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t AliasType) Source(full bool) []byte { return t.node.Source(full) }

// TypeName represents a defined type.
type TypeName struct {
	typer
	node *TypeDef
}

// Kind implements Type.
func (t TypeName) Kind() Kind { return DefinedType }

// Position implements Node.
func (t TypeName) Position() (r token.Position) { return t.node.Position() }

// Source implements Node.
func (t TypeName) Source(full bool) []byte { return t.node.Source(full) }
