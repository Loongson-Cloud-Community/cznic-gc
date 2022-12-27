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

func (c *ctx) isBuiltin() bool { return c.pkg.Scope.kind == UniverseScope }

func (c *ctx) lookup(sc *Scope, id Token) (pkg *Package, in *Scope, r named) {
	sc0 := sc
	pkg = c.pkg
	for {
		switch in, nm := sc.lookup(id); x := nm.n.(type) {
		case *TypeDefNode:
			if sc.kind == UniverseScope {
				if sc0.kind != UniverseScope && token.IsExported(id.Src()) {
					// trc("%v: %q %v %v", id.Position(), id.Src(), sc0.kind, sc.kind)
					return nil, nil, r
				}
			}

			return pkg, in, nm
		default:
			panic(todo("%v: %q %T", id.Position(), id.Src(), x))
		}
	}
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
		case *ConstDeclNode:
			x.check(c)
		case *VarDeclNode:
			x.check(c)
		case *FunctionDeclNode:
			x.check(c)
		// 	case *MethodDeclNode:
		// 		x.check(c)
		default:
			panic(todo("%v: %T %s", x.Position(), x, x.Source(false)))
		}
	}
	panic(todo("%v: %T %v", n.Position(), n, n.Source(false)))
}

func (n *FunctionDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	n.Signature.check(c, nil)
}

func (n *SignatureNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	// in := n.Parameters.check(c, nil)
	// out := n.Result.check(c)
	// return n.setType(newTupleType([]Type{in, out}), c, n)
	panic(todo("%v: %T %v", n.Position(), n, n.Source(false)))
}

func (n *VarDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	switch x := n.VarSpec.(type) {
	case *VarSpecNode:
		x.check(c)
	default:
		panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
	}
	// for l := n.VarSpecList; l != nil; l = l.List {
	// 	l.check(c)
	// }
}

func (n *VarSpecNode) check(c *ctx) {
	switch {
	case n.TypeNode == nil:
		panic(todo("%v: %T %v", n.Position(), n, n.Source(false)))
	default:
		n.TypeNode.check(c)
		if n.ExpressionList != nil {
			panic(todo("%v: %T %v", n.Position(), n, n.Source(false)))
		}
	}
}

func (n *ConstDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	switch x := n.ConstSpec.(type) {
	case *ConstSpecListNode:
		var prev Node
		for l := x; l != nil; l = l.List {
			switch y := l.ConstSpec.(type) {
			case *ConstSpecNode:
				if y.Expression != nil || y.TypeNode != nil {
					prev = y
				}
				y.check(c, prev)
			default:
				panic(todo("%v: %T %s", n.Position(), y, n.Source(false)))
			}
		}
	case *ConstSpecNode:
		x.check(c, nil)
	default:
		panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
	}

}

func (n *ConstSpecNode) check(c *ctx, prev Node) {
	if c.isBuiltin() {
		switch n.IDENT.Src() {
		case "true":
			switch x := n.Expression.(type) {
			case *BinaryExpressionNode:
				x.setValue(trueVal)
				x.setType(c.newPredefinedType(x, UntypedBool))
			default:
				panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
			}
		case "false":
			switch x := n.Expression.(type) {
			case *BinaryExpressionNode:
				x.setValue(falseVal)
				x.setType(c.newPredefinedType(x, UntypedBool))
			default:
				panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
			}
		case "iota":
			switch x := n.Expression.(type) {
			case *BasicLitNode:
				// ok
			default:
				panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
			}
		default:
			panic(todo("", n.Position(), n.Source(false)))
		}
		return
	}

	// switch {
	// case n.TypeNode != nil:
	// 	n.t = typeNodeCheck(n.TypeNode, c)
	// default:
	// 	switch x := prev.(type) {
	// 	case *ConstSpecNode:
	// 		n.t = x.t
	// 	case nil:
	// 		n.t = nil
	// 	default:
	// 		panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
	// 	}
	// }

	// save := c.iotaval
	// c.iotaval = iotaval{val: n.iota, valid: true}

	// defer func() { c.iotaval = save }()

	// var e Expression
	// var pe *Expression
	// switch {
	// case n.Expression != nil:
	// 	e = n.Expression
	// 	pe = &n.Expression
	// default:
	// 	switch x := prev.(type) {
	// 	case *ConstSpecNode:
	// 		e = x.Expression.clone()
	// 		pe = &e
	// 	default:
	// 		panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
	// 	}
	// }
	// ev, et := e.checkExpr(c, pe)
	// e = *pe
	// if ev.Kind() == constant.Unknown {
	// 	c.err(e, "%s is not a constant", e.Source(false))
	// 	n.t = Invalid
	// 	n.setValue(unknown)
	// 	return Invalid
	// }
	// switch {
	// case n.t == nil:
	// 	n.t = et
	// default:
	// 	if !c.isAssignable(e, e, et) {
	// 		c.err(n.Expression, "cannot assign %v (type %v) to type %v", ev, et, n.Type())
	// 		return Invalid
	// 	} else {
	// 		n.setValue(convertValue(c, e, ev, n.Type()))
	// 	}
	// }
	// return n.Type()
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *TypeDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	for l := n.TypeSpecList; l != nil; l = l.List {
		switch x := l.TypeSpec.(type) {
		case *TypeDefNode:
			if c.isBuiltin() {
				switch x.IDENT.Src() {
				case "comparable":
					switch y := x.TypeNode.(type) {
					case *InterfaceTypeNode:
						y.guard = guardChecked
						continue
					default:
						panic(todo("%v: %T %s", y.Position(), y, y.Source(false)))
					}
				}
			}

			x.check(c)
		case *AliasDeclNode:
			x.check(c)
		default:
			panic(todo("%v: %T %s", x.Position(), x, x.Source(false)))
		}
	}
}

func (n *AliasDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	n.TypeNode.check(c)
}

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
