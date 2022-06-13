// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"go/constant"
	"go/token"
)

var (
	_ Expression = (*Arguments)(nil)
	_ Expression = (*BasicLit)(nil)
	_ Expression = (*BinaryExpression)(nil)
	_ Expression = (*CompositeLit)(nil)
	_ Expression = (*Constant)(nil)
	_ Expression = (*Conversion)(nil)
	_ Expression = (*FunctionLit)(nil)
	_ Expression = (*Ident)(nil)
	_ Expression = (*Index)(nil)
	_ Expression = (*ParenExpr)(nil)
	_ Expression = (*QualifiedIdent)(nil)
	_ Expression = (*Selector)(nil)
	_ Expression = (*SliceExpr)(nil)
	_ Expression = (*TypeAssertion)(nil)
	_ Expression = (*UnaryExpr)(nil)
	_ Expression = (*invalidExprType)(nil)
)

var (
	invalidExpr = &invalidExprType{typer: newTyper(Invalid)}
	unknown     = constant.MakeUnknown()
)

type valuer struct{ v constant.Value }

func newValuer(v constant.Value) valuer { return valuer{v} }

func (v valuer) Value() constant.Value {
	if v.v == nil {
		return unknown
	}

	return v.v
}

type invalidExprType struct {
	typer
	valuer
}

func (n *invalidExprType) Position() (r token.Position) { return r }
func (n *invalidExprType) Source(full bool) []byte      { return []byte("<invalid expression>") }

// Expression represents a computation.
type Expression interface {
	Node
	Type() Type
	Value() constant.Value
	// eval(c *ctx)
}
