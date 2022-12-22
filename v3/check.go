// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/constant"
)

type ctx struct {
	cfg *Config
}

func newCtx(cfg *Config) *ctx {
	return &ctx{
		cfg: cfg,
	}
}

func (c *ctx) err(n Node, msg string, args ...interface{}) {
	panic(todo(""))
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

func (n *OperandNameNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNameNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNameNode) checkExpr(c *ctx) Expression {
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

func (n *LiteralValueNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *LiteralValueNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *LiteralValueNode) checkExpr(c *ctx) Expression {
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

func (n *CompositeLitNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *CompositeLitNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *CompositeLitNode) checkExpr(c *ctx) Expression {
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

func (n *OperandNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *OperandNode) checkExpr(c *ctx) Expression {
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

func (n *ConversionNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ConversionNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *ConversionNode) checkExpr(c *ctx) Expression {
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

func (n *PrimaryExprNode) Type() Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PrimaryExprNode) Value() constant.Value {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *PrimaryExprNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *BinaryExpressionNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *UnaryExprNode) checkExpr(c *ctx) Expression {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}
