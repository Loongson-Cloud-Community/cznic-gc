// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/constant"
)

var (
	falseVal = constant.MakeBool(false)
	trueVal  = constant.MakeBool(true)
	unknown  = constant.MakeUnknown()
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
