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
	checkExpr(c *ctx) Expression
	clone() Expression
	typer
	valuer
}

func (n *BasicLitNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *BasicLitNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *BasicLitNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *BasicLitNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNameNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNameNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNameNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNameNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedExpressionNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedExpressionNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedExpressionNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ParenthesizedExpressionNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *LiteralValueNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *LiteralValueNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *LiteralValueNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *LiteralValueNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *KeyedElementNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *KeyedElementNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *KeyedElementNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *KeyedElementNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *CompositeLitNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *CompositeLitNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *CompositeLitNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *CompositeLitNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *FunctionLitNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *FunctionLitNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *FunctionLitNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *FunctionLitNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandQualifiedNameNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandQualifiedNameNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandQualifiedNameNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandQualifiedNameNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ConversionNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ConversionNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ConversionNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ConversionNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *MethodExprNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *MethodExprNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *MethodExprNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *MethodExprNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PrimaryExprNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PrimaryExprNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PrimaryExprNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PrimaryExprNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *BinaryExpressionNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *BinaryExpressionNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *UnaryExprNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *UnaryExprNode) clone() Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}
