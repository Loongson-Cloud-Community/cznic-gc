// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/token"
	"path/filepath"
)

type ctx struct {
	ast  *AST
	cfg  *Config
	errs errList
	pkg  *Package
}

func (c *ctx) err(n Node, msg string, args ...interface{}) {
	var pos token.Position
	if n != nil {
		pos = n.Position()
	}
	c.errs.err(pos, msg, args...)
}

func newCtx(cfg *Config) *ctx { return &ctx{cfg: cfg} }

func (p *Package) check(c *ctx) error {
	if p.isChecked {
		return nil
	}

	defer func() { p.isChecked = true }()

	c.pkg = p
	switch {
	case p.isBuiltin:
		p.Scope = newScope(nil, scUniverse)
	default:
		p.Scope = newScope(c.cfg.builtin.Scope, scPackage)
	}
	for _, v := range p.GoFiles {
		path := filepath.Join(p.FSPath, v.Name())
		p.AST[path].check(c)
	}
	return c.errs.Err()
}

func (n *AST) check(c *ctx) {
	c.ast = n
	n.FileScope.Parent = c.pkg.Scope
	n.SourceFile.check(c)
}

func (n *SourceFileNode) check(c *ctx) {
	for _, v := range n.ImportDeclList {
		v.ImportDecl.check(c)
	}
	return //TODO-
	for _, v := range n.TopLevelDeclList {
		switch x := v.TopLevelDecl.(type) {
		case *TypeDeclNode:
			x.check(c)
		case *ConstDeclNode:
			x.check(c)
		default:
			panic(todo("%T", x))
		}
	}
}

func (n *ImportDeclNode) check(c *ctx) {
	for _, v := range n.ImportSpecList {
		v.ImportSpec.check(c)
	}
}

func (n *ImportSpecNode) check(c *ctx) {
	switch {
	case n.PERIOD.IsValid():
		panic(todo(""))
	case n.PackageName != nil:
		panic(todo(""))
	default:
		panic(todo("", n.ImportPath.Source(false)))
	}
}

func (n *TypeDeclNode) check(c *ctx) {
	for _, v := range n.TypeSpecList {
		switch x := v.TypeSpec.(type) {
		case *TypeDefNode:
			x.check(c)
		default:
			panic(todo("%T", x))
		}
	}
}

func (n *TypeDefNode) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	if c.pkg.isBuiltin {
		switch nm := n.IDENT.Src(); nm {
		case "bool":
			n.typ = PredefinedType(Bool)
		default:
			panic(todo("", nm))
		}
		return
	}

	panic(todo("", n.Source(false)))
}

func (n *ConstDeclNode) check(c *ctx) {
	panic(todo(""))
}
