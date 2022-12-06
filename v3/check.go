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
	trc("PKG %q", n.ImportPath)
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
		case *MethodDeclNode:
			x.check(c)
		default:
			panic(todo("%v: %T %s", l.Position(), x, x.Source(false)))
		}
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
		}(l, r)
	}
	wg.Wait()
	fileScope := c.ast.FileScope
	pkgScope := c.pkg.Scope
	for _, v := range a {
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
	panic(todo("%v: %s", n.Position(), n.Source(false)))
}

func (n *ImportSpecNode) check(c *ctx) (*Package, error) {
	if n == nil {
		return nil, nil
	}

	switch {
	case n.PERIOD.IsValid():
		panic(todo(""))
	case n.PackageName.IsValid():
		panic(todo(""))
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

func (n *TypeDeclNode) check(c *ctx) {
	if n == nil {
		return
	}

	for l := n.TypeSpecList; l != nil; l = l.List {
		switch x := l.TypeSpec.(type) {
		case *TypeDefNode:
			x.check(c, nil)
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

	if c.pkg.isBuiltin {
		n.TypeNode.checkBuiltin(c, n.IDENT.Src())
		return
	}

	n.TypeNode.check(c)
}

func (n *TypeDefNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	if c.pkg.isBuiltin {
		return n.setType(n.TypeNode.checkBuiltin(c, n.IDENT.Src()))
	}

	return n.setType(n.TypeNode.check(c))
}

func (n *TypeNode) checkBuiltin(c *ctx, builtin string) (r Type) {
	switch builtin {
	case "error":
		return n.check(c)
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
}

func (n *TypeNode) check(c *ctx) (r Type) {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	// defer func() {
	// 	trc("%v: %T %s", n.Position(), r, r)
	// }()

	switch {
	case n.TypeName != nil:
		sc := n.TypeName.LexicalScope()
		switch x := n.TypeName.Name.(type) {
		case Token:
			switch y := sc.lookup(x); z := y.n.(type) {
			case *TypeDefNode:
				return n.setType(z)
			case *AliasDeclNode:
				return n.setType(z.TypeNode.check(c))
			default:
				panic(todo("%v: %T %s %v %p", n.Position(), z, n.Source(false), sc.kind, sc.Parent()))
			}
		default:
			panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
		}
	case n.TypeLit != nil:
		return n.setType(n.TypeLit.check(c, nil))
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

	et := e.check(c, &n.ExpressionList.Expression)
	if n.TypeNode != nil {
		panic(todo("", n.Position(), n.Source(false), et))
	}
}

func (n *SliceTypeNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	return n.setType(&SliceType{Elem: n.ElementType.check(c)})
}

func (n *MapTypeNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	return n.setType(&MapType{Key: n.KeyType.check(c), Elem: n.ElementType.check(c)})
}

func (n *ChannelTypeNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	dir := SendRecv
	if n.ARROW.IsValid() {
		if n.ARROW.index < n.CHAN.index { // <-chan
			dir = RecvOnly
		} else {
			dir = SendOnly
		}
	}
	return n.setType(&ChannelType{Dir: dir, Elem: n.ElementType.check(c)})
}

func (n *PointerTypeNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	return n.setType(&PointerType{Elem: n.BaseType.check(c)})
}

func (n *ArrayTypeNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *LiteralValueNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *BasicLitNode) check(c *ctx, _ *Expression) Type {
	return n.Type()
}

// Type implements Expression.
func (n *BasicLitNode) Type() Type {
	switch n.Token.Ch() {
	case INT:
		return untypedInt
	case CHAR:
		return untypedRune
	case FLOAT:
		return untypedFloat
	default:
		panic(todo("", dump(n.Token)))
	}
}

func (n *BinaryExpression) check(c *ctx, _ *Expression) (r Type) {
	if n == nil {
		return Invalid
	}

	// trc("(IN) %v: LHS %T %q %q RHS %T %q", n.Op.Position(), n.LHS, n.LHS.Source(false), n.Op.Ch(), n.RHS, n.RHS.Source(false))
	// defer func() {
	// 	trc("(OUT) %v: LHS %q %q RHS %q", n.Op.Position(), n.LHS.Source(false), n.Op.Ch(), n.RHS.Source(false))
	// 	trc("(OUT.A) %v: lhs %T, typ %T %[3]s, val %T %[4]v, rhs %T, typ %T %[6]s, val %T %[7]v, n.Type() %T, r %T , n.Value() %T", n.Op.Position(), n.LHS, n.LHS.Type(), n.LHS.Value(), n.RHS, n.RHS.Type(), n.RHS.Value(), n.Type(), r, n.Value())
	// 	trc("(OUT.B) %v: %s %s (r %s %s), %s %s", n.Op.Position(), n.Type().Kind(), n.Type(), r.Kind(), r, n.Value().Kind(), n.Value())
	// }()

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	lhs := n.LHS.check(c, &n.LHS)
	rhs := n.RHS.check(c, &n.RHS)
	lv := n.LHS.Value()
	rv := n.RHS.Value()
	// trc("(IN.A) %v: LHS %T %s, val %s %s, RHS %T %s, val %s %s", n.Op.Position(), lhs, lhs, lv.Kind(), lv, rhs, rhs, rv.Kind(), rv)
	if lhs == Invalid || rhs == Invalid {
		return Invalid
	}

	switch n.Op.Ch() {
	case ADD:
		// +    sum                    integers, floats, complex values, strings
		return n.setType(n.checkOperands(c, true, true, true, true))
	case SUB, MUL, QUO:
		// -    difference             integers, floats, complex values
		// *    product                integers, floats, complex values
		// /    quotient               integers, floats, complex values
		return n.setType(n.checkOperands(c, true, true, true, false))
	case SHL, SHR:
		if !isAnyInteger(lhs) {
			c.err(n.Op, "shift operand must be an integer: %v", lhs)
			return Invalid
		}

		if !isAnyInteger(rhs) {
			c.err(n.Op, "shift count must be an integer: %v", rhs)
			return Invalid
		}

		// integer << integer >= 0
		switch rv.Kind() {
		case constant.Int:
			if constant.Sign(rv) < 0 {
				c.err(n.Op, "shift count must be >= 0")
				return Invalid
			}

			cnt64, ok := constant.Uint64Val(rv)
			if !ok {
				c.err(n.Op, "invalid shift count: %v", rv)
				return Invalid
			}
			cnt := uint(cnt64)
			if !ok || uint64(cnt) != cnt64 {
				c.err(n.Op, "invalid shift count: %d", cnt64)
				return Invalid
			}

			if v := constant.Shift(lv, n.Op.Ch(), cnt); v.Kind() != constant.Unknown {
				n.setValue(v)
			}
			return n.setType(lhs)
		case constant.Unknown:
			return n.setType(lhs)
		default:
			panic(todo("", n.Op.Position(), n.Source(false), lhs, n.Op.Ch(), rhs, rv.Kind()))
		}
	default:
		panic(todo("", n.Op.Position(), n.Source(false), lhs, n.Op.Ch(), rhs))
	}
}

func (n *BinaryExpression) checkOperands(c *ctx, intsOK, floatsOK, complexOK, stringsOK bool) Type {
	lhs := n.LHS.Type()
	rhs := n.RHS.Type()
	lv := n.LHS.Value()
	rv := n.RHS.Value()

	if lhs.Kind() == rhs.Kind() {
		if intsOK && isAnyInteger(lhs) || floatsOK && isAnyFloat(lhs) || complexOK && isAnyComplex(lhs) {
			if v := constant.BinaryOp(lv, n.Op.Ch(), rv); v.Kind() != constant.Unknown {
				n.setValue(v)
			}
			return lhs
		}

		panic(todo("", n.Op.Position(), n.Source(false), n.LHS.Type(), n.Op.Ch(), n.RHS.Type()))
	}

	if isAnyUntyped(lhs) && isAnyUntyped(rhs) && lv.Kind() != constant.Unknown && rv.Kind() != constant.Unknown {
		if v := constant.BinaryOp(lv, n.Op.Ch(), rv); v.Kind() != constant.Unknown {
			n.setValue(v)
			return typeFromValue(v)
		}
	}

	switch lhs.Kind() {
	default:
		panic(todo("", n.Op.Position(), n.Source(false), n.LHS.Type(), n.Op.Ch(), n.RHS.Type()))
	}
}

func (n *CompositeLitNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *ConversionNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *FunctionLitNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *FunctionTypeNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *InterfaceTypeNode) check(c *ctx, _ *Expression) Type {
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

	n.Signature.check(c, nil)
}

func (n *MethodExprNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *OperandNameNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	sc := n.LexicalScope()
	switch x := n.Name.(type) {
	case Token:
		switch y := sc.lookup(x); z := y.n.(type) {
		case *ConstSpecNode:
			n.resolvedTo = z
			if z.TypeNode != nil {
				return z.TypeNode.check(c)
			}

			return z.ExpressionList.Expression.check(c, &z.ExpressionList.Expression)
		case *TypeDefNode:
			n.resolvedTo = z
			return z
		default:
			panic(todo("%v: %T %s %v %p", n.Position(), z, n.Source(false), sc.kind, sc.Parent()))
		}
	default:
		panic(todo("%v: %T %s", n.Position(), x, n.Source(false)))
	}
}

func (n *OperandNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	panic(todo("", n.Position(), n.Source(false)))
}

func (n *PrimaryExprNode) check(c *ctx, ep *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	typ := n.Operand.check(c, &n.Operand)
	switch x := n.Operand.(type) {
	case *OperandNameNode:
		switch y := x.ResolvedTo().(type) {
		case *TypeDefNode:
			//TODO rewrite *ep to ConversionNode
			panic(todo("%v: %T %s", x.Position(), y, x.Source(false)))
		default:
			panic(todo("%v: %T %s", x.Position(), y, x.Source(false)))
		}
	}
	val := n.Operand.Value()
	for _, v := range n.PostfixList {
		panic(todo("", v.Position(), v.Source(false)))
	}
	panic(todo("", n.Position(), n.Source(false), typ, val))
}

func (n *StructTypeNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	var a []*FieldDeclNode
	for l := n.FieldDeclList; l != nil; l = l.List {
		switch fd := l.FieldDecl; {
		case fd.EmbeddedField != nil:
			panic(todo("", fd.Position(), fd.Source(false)))
		default:
			fd.TypeNode.check(c)
			for l := fd.IdentifierList; l != nil; l = l.List {
				fd2 := *fd
				id := *l
				id.List = nil
				fd2.IdentifierList = &id
				a = append(a, &fd2)
			}
		}
	}
	return n.setType(&StructType{Fields: a})
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

	n.TypeNode.check(c)
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

	n.Signature.check(c, nil)
}

func (n *SignatureNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	if !n.enter(c, n) {
		return n.Type()
	}

	defer n.exit()

	in := n.Parameters.check(c, nil)
	out := n.Result.check(c)
	return n.setType(newTupleType([]Type{in, out}))
}

func (n *ResultNode) check(c *ctx) (r Type) {
	if n == nil {
		return newTupleType(nil)
	}

	return n.Parameters.check(c, nil)
}

func (n *ParametersNode) check(c *ctx, _ *Expression) Type {
	r := newTupleType(nil)
	if n == nil {
		return r
	}

	for l := n.ParameterDeclList; l != nil; l = l.List {
		r.Types = append(r.Types, l.check(c, nil))
	}
	return r
}

func (n *ParameterDeclListNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	return n.ParameterDecl.check(c)
}

func (n *ParameterDeclNode) check(c *ctx) (r Type) {
	if n == nil {
		return Invalid
	}

	return n.TypeNode.check(c)
}

// Type implements Type.
func (n *KeyedElementNode) Type() Type { return n.Element2.Type() }

// Value implements Value.
func (n *KeyedElementNode) Value() constant.Value { return n.Element2.Value() }

func (n *KeyedElementNode) check(c *ctx, _ *Expression) Type {
	n.Element.check(c, &n.Element)
	return n.Element2.check(c, &n.Element2)
}

func (n *KeyedElementNode) setType(typ Type) Type {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *KeyedElementNode) setValue(val constant.Value) {
	panic(todo("%v: %T %s", n.Position(), n, n.Source(false)))
}

func (n *MethodDeclNode) check(c *ctx) {
	tt := n.Receiver.check(c, nil).(*TupleType)
	rx := Invalid
	switch len(tt.Types) {
	case 1:
		rx = tt.Types[0]
	default:
		c.err(n.Receiver, "invalid receiver")
	}
	_ = rx
	//TODO panic(todo("%v: %T %s %T %q", n.Position(), n, n.Source(false), rx, rx))
}

func (n *OperandNameNode) Type() Type {
	switch x := n.ResolvedTo().(type) {
	case *ConstSpecNode:
		if x.TypeNode != nil {
			panic(todo("%v: %T", n.Position(), x))
		}

		return x.ExpressionList.Expression.Type()
	default:
		panic(todo("%v: %T", n.Position(), x))
	}
}

func (n *OperandNameNode) Value() constant.Value {
	switch x := n.ResolvedTo().(type) {
	case *ConstSpecNode:
		return x.ExpressionList.Expression.Value()
	case *TypeDefNode:
		return unknown
	default:
		panic(todo("%v: %T", n.Position(), x))
	}
}

func (n *OperandNameNode) setType(typ Type) Type {
	panic(todo("", n.Position()))
}

func (n *OperandNameNode) setValue(val constant.Value) {
	panic(todo("", n.Position()))
}

func (n *ParenthesizedExpression) check(c *ctx, _ *Expression) Type {
	return n.Expression.check(c, &n.Expression)
}

func (n *ParenthesizedExpression) Type() Type {
	return n.Expression.Type()
}

func (n *ParenthesizedExpression) Value() constant.Value {
	return n.Expression.Value()
}

func (n *ParenthesizedExpression) setType(typ Type) Type {
	panic(todo("", n.Position()))
}

func (n *ParenthesizedExpression) setValue(val constant.Value) {
	panic(todo("", n.Position()))
}

func (n *UnaryExprNode) check(c *ctx, _ *Expression) Type {
	if n == nil {
		return Invalid
	}

	switch t := n.UnaryExpr.check(c, &n.UnaryExpr); n.Op.Ch() {
	case ARROW:
		if x, ok := t.(*ChannelType); ok {
			return x.Elem
		}
	default:
		return t
	}

	return Invalid
}

func (n *UnaryExprNode) Type() Type {
	panic(todo("", n.Position(), n.Source(false)))
}

func (n *UnaryExprNode) setType(typ Type) Type {
	panic(todo("", n.Position(), n.Source(false)))
}
