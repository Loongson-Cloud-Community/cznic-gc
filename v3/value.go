// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/constant"
)

var (
	_ Expression = (*BasicLitNode)(nil)
	_ Expression = (*BinaryExpressionNode)(nil)
	_ Expression = (*CompositeLitNode)(nil)
	_ Expression = (*ConversionNode)(nil)
	_ Expression = (*FunctionLitNode)(nil)
	_ Expression = (*KeyedElementNode)(nil)
	_ Expression = (*LiteralValueNode)(nil)
	_ Expression = (*MethodExprNode)(nil)
	_ Expression = (*OperandNameNode)(nil)
	_ Expression = (*OperandNode)(nil)
	_ Expression = (*OperandQualifiedNameNode)(nil)
	_ Expression = (*ParenthesizedExpressionNode)(nil)
	_ Expression = (*PrimaryExprNode)(nil)
	_ Expression = (*UnaryExprNode)(nil)

	unknown = constant.MakeUnknown()
)

type valueCache struct {
	v constant.Value
}

func (n *valueCache) Value() constant.Value {
	if n.v != nil {
		return n.v
	}

	return unknown
}

func (n *valueCache) set(v constant.Value) constant.Value {
	n.v = v
	return v
}

type valuer interface {
	Value() constant.Value
}

type Expression interface {
	Node
	typer
	valuer
	checkExpr(c *ctx) Expression
}
