// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/constant"
	"go/token"
	"path/filepath"
	"sync"
)

type ctx struct {
	ast  *AST
	cfg  *Config
	errs errList
	pkg  *Package
}

func newCtx(cfg *Config) *ctx {
	return &ctx{
		cfg: cfg,
	}
}

func (c *ctx) err(n Node, msg string, args ...interface{}) {
	var pos token.Position
	if n != nil {
		pos = n.Position()
	}
	c.errs.err(pos, msg, args...)
}

func (c *ctx) lookup(sc *Scope, id Token) (pkg *Package, in *Scope, r named) {
	pkg = c.pkg
	switch in, r = sc.lookup(id); x := r.n.(type) {
	default:
		panic(todo("%v: %q %T", id.Position(), id.Src(), x))
	}
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

func (n *Package) check(c *ctx) (err error) {
	if n == nil {
		return nil
	}

	c.pkg = n
	trc("PKG %q", n.ImportPath)
	defer func() { trc("PKG %q -> %s", n.ImportPath, err) }()
	for _, v := range n.GoFiles {
		path := filepath.Join(n.FSPath, v.Name())
		n.AST[path].check(c)
	}
	return c.errs.Err()
}

func (n *AST) check(c *ctx) {
	if n == nil {
		return
	}

	c.ast = n
	n.SourceFile.check(c)
}

func (n *SourceFileNode) check(c *ctx) {
	if n == nil {
		return
	}

	n.PackageClause.check(c)
	for l := n.ImportDeclList; l != nil; l = l.List {
		l.ImportDecl.check(c)
	}
	for l := n.TopLevelDeclList; l != nil; l = l.List {
		switch x := l.TopLevelDecl.(type) {
		case *TypeDeclNode:
			x.check(c)
		// 	case *ConstDeclNode:
		// 		x.check(c)
		// 	case *VarDeclNode:
		// 		x.check(c)
		// 	case *FunctionDeclNode:
		// 		x.check(c)
		// 	case *MethodDeclNode:
		// 		x.check(c)
		default:
			panic(todo("%v: %T %s", x.Position(), x, x.Source(false)))
		}
	}
	panic(todo("%v: %T %v", n.Position(), n, n.Source(false)))
}

func (n *TypeDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	for l := n.TypeSpecList; l != nil; l = l.List {
		switch x := l.TypeSpec.(type) {
		// case *TypeDefNode:
		// 	x.check(c, nil)
		// case *AliasDeclNode:
		// 	x.check(c)
		default:
			panic(todo("%v: %T %s", x.Position(), x, x.Source(false)))
		}
	}
}

// func (n *TypeDefNode) Align() int        { return 1 }
// func (n *TypeDefNode) FieldAlign() int   { return 1 }
// func (n *TypeDefNode) Kind() Kind        { return InvalidKind }
// func (n *TypeDefNode) Size() uintptr     { return 1 }
// func (n *InvalidType) String() string    { return "<invalid type>" }
// func (n *TypeDefNode) check(c *ctx) Type { return n }

func (n *ImportDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	type result struct {
		spec *ImportSpecNode
		pkg  *Package
		err  error
	}
	var a []*result
	var wg sync.WaitGroup
	for l := n.ImportSpecList; l != nil; l = l.List {
		r := &result{}
		a = append(a, r)
		wg.Add(1)
		go func(isln *ImportSpecListNode, r *result) {

			defer wg.Done()

			r.spec = isln.ImportSpec
			r.pkg, r.err = r.spec.check(c)
			r.spec.pkg = r.pkg
		}(l, r)
	}
	wg.Wait()
	fileScope := c.ast.FileScope
	pkgScope := c.pkg.Scope
	for _, v := range a {
		switch x := v.err.(type) {
		case nil:
			// ok
		default:
			panic(todo("%v: %T: %s", v.spec.Position(), x, x))
		}
		switch ex := fileScope.declare(v.pkg.Name, v.spec, 0, nil, true); {
		case ex.declTok.IsValid():
			c.err(n, "%s redeclared, previous declaration at %v:", v.pkg.Name.Src(), ex.declTok.Position())
			continue
		}

		switch ex := pkgScope.declare(v.pkg.Name, v.spec, 0, nil, true); {
		case ex.declTok.IsValid():
			c.err(n, "%s redeclared, previous declaration at %v:", v.pkg.Name.Src(), ex.declTok.Position())
			continue
		}
	}
}

func (n *ImportSpecNode) check(c *ctx) (*Package, error) {
	if n == nil {
		return nil, nil
	}

	switch {
	case n.PERIOD.IsValid():
		panic(todo("", n.Position(), n.Source(false)))
	case n.PackageName.IsValid():
		panic(todo("", n.Position(), n.Source(false)))
	default:
		//TODO version
		check := c.pkg.typeCheck
		switch check {
		case TypeCheckAll:
			// nop
		default:
			panic(todo("", check))
		}
		return c.cfg.newPackage(c.pkg.FSPath, constant.StringVal(n.ImportPath.Value()), "", nil, false, check, c.pkg.guard)
	}
}

func (n *PackageClauseNode) check(c *ctx) {
	nm := n.PackageName.Src()
	if ex := c.pkg.Name; ex.IsValid() && ex.Src() != nm {
		c.err(n.PackageName, "found different packages %q and %q", ex.Src(), nm)
		return
	}

	c.pkg.Name = n.PackageName
}
