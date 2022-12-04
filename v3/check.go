// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"go/constant"
	"go/token"
	"path/filepath"
)

type ctx struct {
	ast  *AST
	cfg  *Config
	errs errList
	pkg  *Package

	disableTypeNameCheck int
}

func (c *ctx) err(n Node, msg string, args ...interface{}) {
	var pos token.Position
	if n != nil {
		pos = n.Position()
	}
	c.errs.err(pos, msg, args...)
}

func newCtx(cfg *Config) *ctx { return &ctx{cfg: cfg} }

func (n *Package) check(c *ctx) error {
	if n == nil {
		return nil
	}

	if n.isChecked {
		return nil
	}

	defer func() { n.isChecked = true }()

	c.pkg = n
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

	for l := n.ImportDeclList; l != nil; l = l.List {
		l.ImportDecl.check(c)
	}
	// return //TODO-
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
	if n == nil {
		return
	}

	for l := n.ImportSpecList; l != nil; l = l.List {
		l.ImportSpec.check(c)
	}
}

func (n *ImportSpecNode) check(c *ctx) {
	if n == nil {
		return
	}

	switch {
	case n.PERIOD.IsValid():
		panic(todo(""))
	case n.PackageName != nil:
		panic(todo(""))
	default:
		//TODO version
		tc := c.pkg.typeCheck
		switch tc {
		case TypeCheckAll:
			// nop
		default:
			panic(todo("", tc))
		}
		p, err := c.cfg.newPackage(c.pkg.FSPath, constant.StringVal(n.ImportPath.Value()), "", nil, false, tc, c.pkg.guard)
		panic(todo("", n.Position(), n.Source(false), p != nil, err))
	}
}

func (n *TypeDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

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
	if n == nil {
		return
	}

	var builtin string
	if c.pkg.isBuiltin {
		builtin = n.IDENT.Src()
	}
	n.TypeNode.check(c, builtin, n.LexicalScope())
}

func (n *TypeDefNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	var builtin string
	if c.pkg.isBuiltin {
		builtin = n.IDENT.Src()
	}
	return n.setType(n.TypeNode.check(c, builtin, n.LexicalScope()))
}

func (n *TypeNode) check(c *ctx, builtin string, sc *Scope) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	switch builtin {
	case "", "error":
		// nop
	case "bool":
		return n.setType(PredefinedType(Bool))
	case "byte", "uint8":
		return n.setType(PredefinedType(Uint8))
	case "uint16":
		return n.setType(PredefinedType(Uint16))
	case "uint32":
		return n.setType(PredefinedType(Uint32))
	case "uint64":
		return n.setType(PredefinedType(Uint64))
	case "int8":
		return n.setType(PredefinedType(Int8))
	case "int16":
		return n.setType(PredefinedType(Int16))
	case "rune", "int32":
		return n.setType(PredefinedType(Int32))
	case "int64":
		return n.setType(PredefinedType(Int64))
	case "float32", "FloatType":
		return n.setType(PredefinedType(Float32))
	case "float64":
		return n.setType(PredefinedType(Float64))
	case "complex64", "ComplexType":
		return n.setType(PredefinedType(Complex64))
	case "complex128":
		return n.setType(PredefinedType(Complex128))
	case "string":
		return n.setType(PredefinedType(String))
	case "int", "Type", "Type1", "IntegerType":
		return n.setType(PredefinedType(Int))
	case "uint":
		return n.setType(PredefinedType(Uint))
	case "uintptr":
		return n.setType(PredefinedType(Uintptr))
	case "any":
		return n.setType(any)
	case "comparable":
		return n.setType(comparable)
	default:
		panic(todo("%v %q %q", n.Position(), n.Source(false), builtin))
	}

	switch {
	case n.TypeName != nil:
		switch x := n.TypeName.(type) {
		case Token:
			switch y := sc.lookup(x); z := y.n.(type) {
			case *TypeDefNode:
				if c.disableTypeNameCheck == 0 {
					z.check(c)
				}
				return n.setType(z)
			case *AliasDeclNode:
				return n.setType(z.TypeNode.check(c, "", z.LexicalScope()))
			default:
				panic(todo("%v: %T %s %v %p", n.Position(), z, n.Source(false), sc.kind, sc.Parent()))
			}
		default:
			panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
		}
	case n.TypeLit != nil:
		return n.setType(n.TypeLit.check(c))
	}
	panic(todo("%v %q", n.Position(), n.Source(false)))
}

func (n *ConstDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	for l := n.ConstSpecList; l != nil; l = l.List {
		l.check(c)
	}
}

func (n *ConstSpecListNode) check(c *ctx) {
	if n == nil {
		return
	}

	n.ConstSpec.check(c)
}

func (n *ConstSpecNode) check(c *ctx) {
	if n == nil {
		return
	}

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
			// nop
		default:
			panic(todo("", n.Position(), n.Source(false), nm))
		}
		return
	}

	e.check(c)
	panic(todo("", n.Position(), n.Source(false)))
}

func (n *SliceTypeNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	c.disableTypeNameCheck++

	defer func() { c.disableTypeNameCheck-- }()

	return n.setType(&SliceType{Elem: n.ElementType.check(c, "", n.LexicalScope())})
}

func (n *MapTypeNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	c.disableTypeNameCheck++

	defer func() { c.disableTypeNameCheck-- }()

	sc := n.LexicalScope()
	return n.setType(&MapType{Key: n.KeyType.check(c, "", sc), Elem: n.ElementType.check(c, "", sc)})
}

func (n *ChannelTypeNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	c.disableTypeNameCheck++

	defer func() { c.disableTypeNameCheck-- }()

	dir := SendRecv
	if n.ARROW.IsValid() {
		if n.ARROW.index < n.CHAN.index { // <-chan
			dir = RecvOnly
		} else {
			dir = SendOnly
		}
	}
	return n.setType(&ChannelType{Dir: dir, Elem: n.ElementType.check(c, "", n.LexicalScope())})
}

func (n *ArrayTypeNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *LiteralValueNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *BasicLitNode) check(c *ctx) Type {
	// if n == nil {
	// 	return Invalid
	// }

	// if !n.enter(c, n) {
	// 	return n.Type()
	// }

	// defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *BinaryExpression) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *CompositeLitNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *ConversionNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *FunctionLitNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *FunctionTypeNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *InterfaceTypeNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	r := &InterfaceType{}
	for l := n.InterfaceElemList; l != nil; l = l.List {
		l.InterfaceElem.check(c)
		r.Elems = append(r.Elems, l.InterfaceElem)
	}
	return n.setType(r)
}

func (n *InterfaceElemNode) check(c *ctx) {
	if n == nil {
		return
	}

	n.MethodElem.check(c)
	n.TypeElem.check(c)
}

func (n *TypeElemListNode) check(c *ctx) {
	if n == nil {
		return
	}

	panic(todo("%v: %q", n.Position(), n.Source(false)))
}

func (n *MethodElemNode) check(c *ctx) {
	if n == nil {
		return
	}

	n.Signature.check(c)
}

func (n *MethodExprNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *OperandNameNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *OperandNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *ParenthesizedExpression) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *PointerTypeNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *PrimaryExprNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *StructTypeNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *UnaryExprNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *VarDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	for l := n.VarSpecList; l != nil; l = l.List {
		l.check(c)
	}
}

func (n *VarSpecListNode) check(c *ctx) {
	if n == nil {
		return
	}

	n.VarSpec.check(c)
}

func (n *VarSpecNode) check(c *ctx) {
	if n == nil {
		return
	}

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
	if n == nil {
		return
	}

	n.Signature.check(c)
}

func (n *SignatureNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	in := n.Parameters.check(c)
	out := n.Result.check(c)
	return n.setType(newTupleType([]Type{in, out}))
}

func (n *ResultNode) check(c *ctx) (r *TupleType) {
	if n == nil {
		return newTupleType(nil)
	}

	return n.Parameters.check(c)
}

func (n *ParametersNode) check(c *ctx) (r *TupleType) {
	r = newTupleType(nil)
	if n == nil {
		return r
	}

	for l := n.ParameterDeclList; l != nil; l = l.List {
		r.Types = append(r.Types, l.check(c))
	}
	return r
}

func (n *ParameterDeclListNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	return n.ParameterDecl.check(c)
}

func (n *ParameterDeclNode) check(c *ctx) Type {
	if n == nil {
		return Invalid
	}

	return n.TypeNode.check(c, "", n.LexicalScope())
}

// Type implements Type.
func (n *KeyedElementNode) Type() Type { return n.Element2.Type() }

// Value implements Value.
func (n *KeyedElementNode) Value() constant.Value { return n.Element2.Value() }

func (n *KeyedElementNode) check(c *ctx) Type {
	n.Element.check(c)
	return n.Element2.check(c)
}

func (n *KeyedElementNode) setType(typ Type) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *KeyedElementNode) setValue(val constant.Value) {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}
