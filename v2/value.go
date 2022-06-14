// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"go/constant"
	"go/token"
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
