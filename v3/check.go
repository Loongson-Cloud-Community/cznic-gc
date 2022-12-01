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
	for l := n.ImportDeclList; l != nil; l = l.List {
		l.ImportDecl.check(c)
	}
	return //TODO-
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
		default:
			panic(todo("%v: %T %s", l.Position(), x, x.Source(false)))
		}
	}
}

func (n *ImportDeclNode) check(c *ctx) {
	for l := n.ImportSpecList; l != nil; l = l.List {
		l.ImportSpec.check(c)
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
	for l := n.TypeSpecList; l != nil; l = l.List {
		switch x := l.TypeSpec.(type) {
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
	n.TypeNode.check(c, builtin, n.LexicalScope())
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
	n.TypeNode.check(c, builtin, n.LexicalScope())
	n.setType(n)
}

func (n *TypeNode) check(c *ctx, builtin string, sc *Scope) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	if builtin == "" {
		switch x := n.TypeName.(type) {
		case Token:
			switch y := sc.lookup(x); z := y.n.(type) {
			case *TypeDefNode:
				z.check(c)
				n.setType(z)
			default:
				panic(todo("%v: %T %s", n.Position(), z, n.Source(false)))
			}
		default:
			panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
		}
		return
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
	case "float32", "FloatType":
		n.setType(PredefinedType(Float32))
	case "float64":
		n.setType(PredefinedType(Float64))
	case "complex64", "ComplexType":
		n.setType(PredefinedType(Complex64))
	case "complex128":
		n.setType(PredefinedType(Complex128))
	case "string":
		n.setType(PredefinedType(String))
	case "int", "Type", "Type1", "IntegerType":
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
	for l := n.ConstSpecList; l != nil; l = l.List {
		l.check(c)
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
	for l := n.VarSpecList; l != nil; l = l.List {
		l.check(c)
	}
}

func (n *VarSpecListNode) check(c *ctx) {
	n.VarSpec.check(c)
}

func (n *VarSpecNode) check(c *ctx) {
	n.TypeNode.check(c, "", n.LexicalScope())
	ids := n.IdentifierList.Len()
	exprs := n.ExpressionList.Len()
	sc := n.LexicalScope()
	switch x := checkBalance(ids, exprs); x {
	case balanceExtraLhs:
		if exprs == 0 {
			for l := n.IdentifierList; l != nil; l = l.List {
				n.check1(c, sc, l.IDENT)
			}
			return
		}

		panic(todo("", n.Position(), n.Source(false), ids, exprs, x))
	default:
		panic(todo("", n.Position(), n.Source(false), ids, exprs, x))
	}
}

func (n *VarSpecNode) check1(c *ctx, sc *Scope, id Token) {
	switch s := sc.lookup(id); x := s.n.(type) {
	case *VarSpecNode:
		ids := x.IdentifierList.Len()
		exprs := x.ExpressionList.Len()
		switch y := checkBalance(ids, exprs); y {
		case balanceExtraLhs:
			if exprs == 0 {
				return
			}

			panic(todo("", x.Position(), x.Source(false), ids, exprs, y))
		default:
			panic(todo("", x.Position(), x.Source(false), ids, exprs, y))
		}
	default:
		panic(todo("%v: %T %s %q", n.Position(), x, n.Source(false), id.Src()))
	}
}

func (n *FunctionDeclNode) check(c *ctx) {
	n.Signature.check(c)
	panic(todo("", n.Position(), n.Source(false)))
}

func (n *SignatureNode) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	n.Parameters.check(c)
	//TODO n.Result.check(c)
	panic(todo("", n.Position(), n.Source(false)))
}

func (n *ParametersNode) check(c *ctx) {
	for l := n.ParameterDeclList; l != nil; l = l.List {
		l.check(c)
	}
}

func (n *ParameterDeclListNode) check(c *ctx) {
	n.ParameterDecl.check(c)
}

func (n *ParameterDeclNode) check(c *ctx) {
	panic(todo("", n.Position(), n.Source(false)))
}
