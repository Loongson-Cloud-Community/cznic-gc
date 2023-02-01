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
	iota int64
	pkg  *Package
}

func newCtx(cfg *Config) *ctx {
	return &ctx{
		cfg:  cfg,
		iota: -1, // -> Invalid
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
		// 		// 	case *MethodDeclNode:
		// 		// 		x.check(c)
		default:
			panic(todo("%v: %T %s", x.Position(), x, x.Source(false)))
		}
	}
}

func (n *FunctionDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	if c.isBuiltin() {
		switch nm := n.FunctionName.IDENT.Src(); nm {
		case
			"append", "cap", "close", "complex", "copy",
			"delete", "imag", "len", "make", "new",
			"panic", "print", "println", "real", "recover":

			n.Signature.t = c.newPredeclaredType(n, Function)
		default:
			panic(todo("%v: %q %s", n.Position(), nm, n.Source(false)))
		}
		return
	}

	t := n.Signature.check(c)
	trc("", t)
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *SignatureNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	in := n.Parameters.check(c)
	out := n.Result.check(c)
	return n.setType(newTupleType(n.Parameters, []Type{in, out}))
}

func (n *ResultNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	switch {
	case n.Parameters != nil:
		panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
	case n.TypeNode != nil:
		return n.TypeNode.check(c)
	default:
		panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
	}
}

func (n *ParametersNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	r := newTupleType(n, nil)
	for l := n.ParameterDeclList; l != nil; l = l.List {
		r.Types = append(r.Types, l.ParameterDecl.check(c)...)
	}
	return r
}

func (n *ParameterDeclNode) check(c *ctx) []Type {
	if n == nil {
		return nil
	}

	t := n.TypeNode.check(c)
	panic(todo("%v: %T %v", n.Position(), t, t))
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
}

func (n *VarSpecNode) check(c *ctx) {
	if n == nil {
		return
	}

	if c.isBuiltin() {
		switch nm := n.IDENT.Src(); nm {
		case "nil":
			n.TypeNode = c.newPredeclaredType(n, UntypedNil)
		default:
			panic(todo("%v: %q", n.IDENT.Position(), nm))
		}
		return
	}

	panic(todo("", c.isBuiltin()))
	// 	switch {
	// 	case n.TypeNode == nil:
	// 		panic(todo("%v: %T %v", n.Position(), n, n.Source(false)))
	// 	default:
	// 		n.TypeNode.check(c)
	// 		if n.ExpressionList != nil {
	// 			panic(todo("%v: %T %v", n.Position(), n, n.Source(false)))
	// 		}
	// 	}
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
				y.check(c, prev)
				if y.Expression != nil || y.TypeNode != nil {
					prev = y
				}
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
	if n == nil {
		return
	}

	if c.isBuiltin() {
		switch n.IDENT.Src() {
		case "true":
			switch x := n.Expression.(type) {
			case *BinaryExpressionNode:
				x.setValue(trueVal)
				x.setType(c.newPredeclaredType(x, UntypedBool))
			default:
				panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
			}
		case "false":
			switch x := n.Expression.(type) {
			case *BinaryExpressionNode:
				x.setValue(falseVal)
				x.setType(c.newPredeclaredType(x, UntypedBool))
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

	// var t Type
	switch {
	case n.TypeNode != nil:
		// 	n.t = typeNodeCheck(n.TypeNode, c)
		panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
	default:
		switch x := prev.(type) {
		// 	case *ConstSpecNode:
		// 		n.t = x.t
		case nil:
			// ok
		default:
			panic(todo("%v: %T %s", prev.Position(), x, prev.Source(false)))
		}
	}

	save := c.iota
	c.iota = n.iota

	defer func() { c.iota = save }()

	// 	// var e Expression
	// 	// var pe *Expression
	// 	// switch {
	// 	// case n.Expression != nil:
	// 	// 	e = n.Expression
	// 	// 	pe = &n.Expression
	// 	// default:
	// 	// 	switch x := prev.(type) {
	// 	// 	case *ConstSpecNode:
	// 	// 		e = x.Expression.clone()
	// 	// 		pe = &e
	// 	// 	default:
	// 	// 		panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
	// 	// 	}
	// 	// }
	// 	// ev, et := e.checkExpr(c, pe)
	// 	// e = *pe
	// 	// if ev.Kind() == constant.Unknown {
	// 	// 	c.err(e, "%s is not a constant", e.Source(false))
	// 	// 	n.t = Invalid
	// 	// 	n.setValue(unknown)
	// 	// 	return Invalid
	// 	// }
	// 	// switch {
	// 	// case n.t == nil:
	// 	// 	n.t = et
	// 	// default:
	// 	// 	if !c.isAssignable(e, e, et) {
	// 	// 		c.err(n.Expression, "cannot assign %v (type %v) to type %v", ev, et, n.Type())
	// 	// 		return Invalid
	// 	// 	} else {
	// 	// 		n.setValue(convertValue(c, e, ev, n.Type()))
	// 	// 	}
	// 	// }
	// 	// return n.Type()
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
				switch nm := x.IDENT.Src(); nm {
				case "bool":
					x.TypeNode = c.newPredeclaredType(x, Bool)
				case "int":
					x.TypeNode = c.newPredeclaredType(x, Int)
				case "int8":
					x.TypeNode = c.newPredeclaredType(x, Int8)
				case "int16":
					x.TypeNode = c.newPredeclaredType(x, Int16)
				case "int32":
					x.TypeNode = c.newPredeclaredType(x, Int32)
				case "int64":
					x.TypeNode = c.newPredeclaredType(x, Int64)
				case "uint":
					x.TypeNode = c.newPredeclaredType(x, Uint)
				case "uint8":
					x.TypeNode = c.newPredeclaredType(x, Uint8)
				case "uint16":
					x.TypeNode = c.newPredeclaredType(x, Uint16)
				case "uint32":
					x.TypeNode = c.newPredeclaredType(x, Uint32)
				case "uint64":
					x.TypeNode = c.newPredeclaredType(x, Uint64)
				case "uintptr":
					x.TypeNode = c.newPredeclaredType(x, Uintptr)
				case "string":
					x.TypeNode = c.newPredeclaredType(x, String)
				case "float32":
					x.TypeNode = c.newPredeclaredType(x, Float32)
				case "float64":
					x.TypeNode = c.newPredeclaredType(x, Float64)
				case "complex64":
					x.TypeNode = c.newPredeclaredType(x, Complex64)
				case "complex128":
					x.TypeNode = c.newPredeclaredType(x, Complex128)
				case "comparable":
					x.TypeNode = c.newPredeclaredType(x, Interface)
				case "error":
					x.check(c)
				default:
					if token.IsExported(nm) {
						delete(c.pkg.Scope.nodes, nm)
						return
					}

					panic(todo("%v: %T %s", x.Position(), x, x.Source(false)))
				}
				return
			}
			//
			// 			x.check(c)
			panic(todo(""))
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
	if n == nil {
		return
	}

	nm := n.PackageName.Src()
	if ex := c.pkg.Name; ex.IsValid() && ex.Src() != nm {
		c.err(n.PackageName, "found different packages %q and %q", ex.Src(), nm)
		return
	}

	c.pkg.Name = n.PackageName
}
