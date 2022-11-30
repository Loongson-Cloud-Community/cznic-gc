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
	for n := n.ImportDeclList; n != nil; n = n.List {
		n.ImportDecl.check(c)
	}
	return //TODO-
	for n := n.TopLevelDeclList; n != nil; n = n.List {
		switch x := n.TopLevelDecl.(type) {
		case *TypeDeclNode:
			x.check(c)
		case *ConstDeclNode:
			x.check(c)
		case *VarDeclNode:
			x.check(c)
		default:
			panic(todo("%v: %T %s", n.Position(), x, x.Source(false)))
		}
	}
}

func (n *ImportDeclNode) check(c *ctx) {
	for n := n.ImportSpecList; n != nil; n = n.List {
		n.ImportSpec.check(c)
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
	for n := n.TypeSpecList; n != nil; n = n.List {
		switch x := n.TypeSpec.(type) {
		case *TypeDefNode:
			x.check(c)
		case *AliasDeclNode:
			x.check(c)
		default:
			panic(todo("%T", x))
		}
	}
}

func (n *AliasDeclNode) check(c *ctx) {
	var builtin string
	if c.pkg.isBuiltin {
		builtin = n.IDENT.Src()
	}
	n.TypeNode.check(c, builtin)
}

func (n *TypeDefNode) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	var builtin string
	if c.pkg.isBuiltin {
		builtin = n.IDENT.Src()
	}
	n.TypeNode.check(c, builtin)
	n.setType(n)
}

func (n *TypeNode) check(c *ctx, builtin string) {
	if builtin == "" {
		panic(todo("", n.Position(), n.Source(false)))
	}

	switch builtin {
	case "bool":
		n.setType(PredefinedType(Bool))
	case "byte", "uint8":
		n.setType(PredefinedType(Uint8))
	case "uint16":
		n.setType(PredefinedType(Uint16))
	case "uint32":
		n.setType(PredefinedType(Uint32))
	case "uint64":
		n.setType(PredefinedType(Uint64))
	case "int8":
		n.setType(PredefinedType(Int8))
	case "int16":
		n.setType(PredefinedType(Int16))
	case "rune", "int32":
		n.setType(PredefinedType(Int32))
	case "int64":
		n.setType(PredefinedType(Int64))
	case "float32":
		n.setType(PredefinedType(Float32))
	case "float64":
		n.setType(PredefinedType(Float64))
	case "complex64":
		n.setType(PredefinedType(Complex64))
	case "complex128":
		n.setType(PredefinedType(Complex128))
	case "string":
		n.setType(PredefinedType(String))
	case "int":
		n.setType(PredefinedType(Int))
	case "uint":
		n.setType(PredefinedType(Uint))
	case "uintptr":
		n.setType(PredefinedType(Uintptr))
	case "any":
		n.setType(any)
	case "comparable":
		n.setType(comparable)
	default:
		panic(todo("", n.Position(), n.Source(false), builtin))
	}
}

func (n *ConstDeclNode) check(c *ctx) {
	for n := n.ConstSpecList; n != nil; n = n.List {
		n.check(c)
	}
}

func (n *ConstSpecListNode) check(c *ctx) {
	n.ConstSpec.check(c)
}

func (n *ConstSpecNode) check(c *ctx) {
	id := n.IdentifierList.IDENT
	e := n.ExpressionList.Expression
	if !id.IsValid() || e == nil {
		return
	}

	nm := id.Src()
	if c.pkg.isBuiltin {
		switch nm {
		case "true":
			e.setType(PredefinedType(Bool))
			e.setValue(trueVal)
		case "false":
			e.setType(PredefinedType(Bool))
			e.setValue(falseVal)
		case "iota":
			e.setType(PredefinedType(UntypedInt))
		default:
			panic(todo("", n.Position(), n.Source(false), nm))
		}
		return
	}

	e.check(c)
	panic(todo("", n.Position(), n.Source(false)))
}

func (n *BasicLitNode) check(c *ctx)            { panic(todo("", n.Position(), n.Source(false))) }
func (n *BinaryExpression) check(c *ctx)        { panic(todo("", n.Position(), n.Source(false))) }
func (n *CompositeLitNode) check(c *ctx)        { panic(todo("", n.Position(), n.Source(false))) }
func (n *ConversionNode) check(c *ctx)          { panic(todo("", n.Position(), n.Source(false))) }
func (n *FunctionLitNode) check(c *ctx)         { panic(todo("", n.Position(), n.Source(false))) }
func (n *MethodExprNode) check(c *ctx)          { panic(todo("", n.Position(), n.Source(false))) }
func (n *OperandNameNode) check(c *ctx)         { panic(todo("", n.Position(), n.Source(false))) }
func (n *OperandNode) check(c *ctx)             { panic(todo("", n.Position(), n.Source(false))) }
func (n *ParenthesizedExpression) check(c *ctx) { panic(todo("", n.Position(), n.Source(false))) }
func (n *PrimaryExprNode) check(c *ctx)         { panic(todo("", n.Position(), n.Source(false))) }
func (n *UnaryExprNode) check(c *ctx)           { panic(todo("", n.Position(), n.Source(false))) }

func (n *VarDeclNode) check(c *ctx) {
	for n := n.VarSpecList; n != nil; n = n.List {
		n.check(c)
	}
}

func (n *VarSpecListNode) check(c *ctx) {
	n.VarSpec.check(c)
}

func (n *VarSpecNode) check(c *ctx) {
	panic(todo("", n.Position(), n.Source(false)))
}
