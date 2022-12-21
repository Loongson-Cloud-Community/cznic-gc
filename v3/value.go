// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/constant"
	"math"
)

var (
	falseVal   = constant.MakeBool(false)
	trueVal    = constant.MakeBool(true)
	unknown    = constant.MakeUnknown()
	zeroIntVal = constant.MakeInt64(0)
)

type valuer struct{ val constant.Value }

func newValuer(v constant.Value) valuer { return valuer{v} }

// Value implements Expression
func (v valuer) Value() constant.Value {
	if v.val == nil {
		return unknown
	}

	return v.val
}

func (v *valuer) setValue(val constant.Value) { v.val = val }

type Value interface {
	Value() constant.Value
	setValue(constant.Value)
}

func convertValue(c *ctx, n Node, v constant.Value, to Type) constant.Value {
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

		if c.arch32bit {
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

		if c.arch32bit {
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
	case Interface:
		to := to.(*InterfaceType)
		if len(to.Elems) == 0 {
			return unknown
		}
	case Bool:
		if v.Kind() == constant.Bool {
			return v
		}
	}
	panic(todo("%v %v -> %v", v, v.Kind(), to.Kind()))
}
