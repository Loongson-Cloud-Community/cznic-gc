// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"go/constant"
	"go/token"
	"math"
)

var (
	_ = []Expression{
		(*Arguments)(nil),
		(*BasicLit)(nil),
		(*BinaryExpression)(nil),
		(*CompositeLit)(nil),
		(*Constant)(nil),
		(*Conversion)(nil),
		(*FunctionLit)(nil),
		(*GenericOperand)(nil),
		(*Ident)(nil),
		(*Index)(nil),
		(*MethodExpr)(nil),
		(*ParenExpr)(nil),
		(*QualifiedIdent)(nil),
		(*Selector)(nil),
		(*SliceExpr)(nil),
		(*TypeAssertion)(nil),
		(*TypeSwitchGuard)(nil),
		(*UnaryExpr)(nil),
		(*invalidExprType)(nil),
	}
)

var (
	invalidExpr = &invalidExprType{typer: newTyper(Invalid)}
	unknown     = constant.MakeUnknown()
)

// Expression represents a computation.
type Expression interface {
	Node
	Type() Type
	Value() constant.Value
	checker
}

type valuer struct{ v constant.Value }

func newValuer(v constant.Value) valuer { return valuer{v} }

func (v valuer) Value() constant.Value {
	if v.v == nil {
		return unknown
	}

	return v.v
}

type invalidExprType struct {
	guard
	typer
	valuer
}

func (n *invalidExprType) Kind() Kind                   { return InvalidKind }
func (n *invalidExprType) check(c *ctx)                 {}
func (n *invalidExprType) Position() (r token.Position) { return r }
func (n *invalidExprType) Source(full bool) []byte      { return []byte("<invalid expression>") }

func (c *ctx) convertValue(n Node, v constant.Value, to Type) (r constant.Value) {
	if v.Kind() == constant.Unknown {
		return unknown
	}

	switch to.Kind() {
	case Int:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		i64, ok := constant.Int64Val(w)
		if !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		switch c.checker.GOARCH() {
		case "386", "arm":
			if i64 < math.MinInt32 || i64 > math.MaxInt32 {
				c.err(n, "value %s overflows %s", v, to)
				return unknown
			}
		}
		return w
	case Int8:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		i64, ok := constant.Int64Val(w)
		if !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		if i64 < math.MinInt8 || i64 > math.MaxInt8 {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		return w
	case Int16:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		i64, ok := constant.Int64Val(w)
		if !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		if i64 < math.MinInt16 || i64 > math.MaxInt16 {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		return w
	case Int32:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		i64, ok := constant.Int64Val(w)
		if !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		if i64 < math.MinInt32 || i64 > math.MaxInt32 {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		return w
	case Int64:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		if _, ok := constant.Int64Val(w); !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		return w
	case Uint, Uintptr:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		u64, ok := constant.Uint64Val(w)
		if !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		switch c.checker.GOARCH() {
		case "386", "arm":
			if u64 > math.MaxUint32 {
				c.err(n, "value %s overflows %s", v, to)
				return unknown
			}
		}
		return w
	case Uint8:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		u64, ok := constant.Uint64Val(w)
		if !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		if u64 > math.MaxUint8 {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		return w
	case Uint16:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		u64, ok := constant.Uint64Val(w)
		if !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		if u64 > math.MaxUint16 {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		return w
	case Uint32:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		u64, ok := constant.Uint64Val(w)
		if !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		if u64 > math.MaxUint32 {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		return w
	case Uint64:
		w := constant.ToInt(v)
		if w.Kind() == constant.Unknown {
			c.err(n, "cannot convert %s to %s", v, to)
			return unknown
		}

		if _, ok := constant.Uint64Val(w); !ok {
			c.err(n, "value %s overflows %s", v, to)
			return unknown
		}

		return w
	case Float32, Float64:
		return constant.ToFloat(v)
	default:
		c.err(n, errorf("TODO %v %v -> %v", v, v.Kind(), to.Kind()))
		return unknown
	}
	panic(todo(""))
}

func (c *ctx) convertType(n Node, from, to Type) {
	switch from.Kind() {
	case UnsafePointer:
		switch to.Kind() {
		case Pointer, UnsafePointer, Uintptr:
			// ok
		default:
			c.err(n, errorf("cannot convert %s to %s", from, to))
		}
	default:
		c.err(n, errorf("TODO %v -> %v", from.Kind(), to.Kind()))
	}
}
