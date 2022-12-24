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
