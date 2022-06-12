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
)

var (
	_ Type = (*InvalidType)(nil)
	_ Type = (*PredefinedType)(nil)
)

// A Kind represents the specific kind of type that a Type represents. The zero
// Kind is not a valid kind.
type Kind int

// Values of type Kind
const (
	InvalidKind Kind = iota // <invalid type>

	Bool       // bool
	Chan       // chan
	Complex128 // complex128
	Complex64  // complex64
	Float32    // float32
	Float64    // float64
	Int        // int
	Int16      // int16
	Int32      // int32
	Int64      // int64
	Int8       // int8
	Interface  // interface
	Map        // map
	Pointer    // pointer
	Slice      // slice
	String     // string
	Struct     // struct
	Uint       // uint
	Uint16     // uint16
	Uint32     // uint32
	Uint64     // uint64
	Uint8      // uint8
	Uintptr    // uintptr
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
//  *PredefinedType
//  TODO more type classes
//TODO
type Type interface {
	Node
	Kind() Kind
}

// InvalidType represents an invalid type.
type InvalidType struct{}

// Kind implements Type.
func (t InvalidType) Kind() Kind { return InvalidKind }

// Position implements Node. Position returns a zero value.
func (t InvalidType) Position() (r token.Position) { return r }

// Source implements Node. Source returns a zero value.
func (t InvalidType) Source(full bool) []byte { return nil }

// PredefinedType represents a predefined type.
type PredefinedType Kind

// Kind implements Type.
func (t PredefinedType) Kind() Kind { return Kind(t) }

// Position implements Node. Position returns a zero value.
func (t PredefinedType) Position() (r token.Position) { return r }

// Source implements Node. Source returns a zero value.
func (t PredefinedType) Source(full bool) []byte { return nil }
