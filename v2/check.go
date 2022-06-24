// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"fmt"
	"go/constant"
	"go/token"
	"sort"
	"strconv"
)

var (
	universe = Scope{
		Nodes: map[string]Scoped{
			"bool":       {Node: PredefinedType(Bool)},
			"byte":       {Node: PredefinedType(Uint8)},
			"complex128": {Node: PredefinedType(Complex128)},
			"complex64":  {Node: PredefinedType(Complex64)},
			"float32":    {Node: PredefinedType(Float32)},
			"float64":    {Node: PredefinedType(Float64)},
			"int":        {Node: PredefinedType(Int)},
			"int16":      {Node: PredefinedType(Int16)},
			"int32":      {Node: PredefinedType(Int32)},
			"int64":      {Node: PredefinedType(Int64)},
			"int8":       {Node: PredefinedType(Int8)},
			"nil":        {Node: PredefinedType(UntypedNil)},
			"rune":       {Node: PredefinedType(Int32)},
			"string":     {Node: PredefinedType(String)},
			"uint":       {Node: PredefinedType(Uint)},
			"uint16":     {Node: PredefinedType(Uint16)},
			"uint32":     {Node: PredefinedType(Uint32)},
			"uint64":     {Node: PredefinedType(Uint64)},
			"uint8":      {Node: PredefinedType(Uint8)},
			"uintptr":    {Node: PredefinedType(Uintptr)},
		},
	}

	noScope = &Scope{}
)

// Scoped represents a node bound to a name and the offset where the visibility
// starts.  Declarations outside of a function/method reports their visibility
// starts at zero.
type Scoped struct {
	Node        Node
	VisibleFrom int
}

// Scope binds names to nodes.
type Scope struct {
	Nodes  map[string]Scoped
	Parent *Scope
}

// IsPackage reports whether s is a package scope.
func (s *Scope) IsPackage() bool { return s.Parent != nil && s.Parent.Parent == nil }

func (s *Scope) add(c *ctx, nm string, visibileFrom int, n Node) {
	if nm == "_" {
		if s.IsPackage() {
			c.pkg.Blanks = append(c.pkg.Blanks, n)
		}
		return
	}

	if s.Nodes == nil {
		s.Nodes = map[string]Scoped{}
	}
	if x, ok := s.Nodes[nm]; ok {
		// trc("%v: %q (%v: %v: %v:)", n.Position(), nm, origin(4), origin(3), origin(2))
		c.err(n, "%s redeclared, previous declaration at %v:", nm, x.Node.Position())
		return
	}

	s.Nodes[nm] = Scoped{n, visibileFrom}
}

type packagesKey struct {
	pkg        *Package
	src        *SourceFile
	importPath string
}

type ctx0 struct {
	checker  PackageChecker
	errors   errList
	packages map[packagesKey]*Package
	pkg      *Package
	stack    []Node
}

type ctx struct {
	*ctx0

	iota int64

	pushNamed bool
}

func newCtx(checker PackageChecker, pkg *Package) *ctx {
	return &ctx{
		ctx0: &ctx0{
			checker:  checker,
			packages: map[packagesKey]*Package{},
			pkg:      pkg,
		},
	}
}

func (c *ctx) setIota(n int64) *ctx {
	r := *c
	r.iota = n
	return &r
}

func (c *ctx) setPushNamed() *ctx {
	r := *c
	r.pushNamed = true
	return &r
}

func (c *ctx) err(n Node, s string, args ...interface{}) {
	var pos token.Position
	if n != nil {
		pos = n.Position()
	}
	c.errors.err(pos, s, args...)
}

func (c *ctx) packageLoader(pkg *Package, src *SourceFile, importPath Token) (r *Package) {
	pth, err := strconv.Unquote(importPath.Src())
	key := packagesKey{pkg, src, pth}
	if err != nil {
		c.packages[key] = nil
		c.err(importPath, errorf("%s", err))
		return nil
	}

	var ok bool
	if r, ok = c.packages[key]; ok {
		return r
	}

	p, err := c.checker.PackageLoader(pkg, src, pth)
	if err != nil {
		c.packages[key] = nil
		c.err(importPath, errorf("%s", err))
		return nil
	}

	c.packages[key] = p
	return p
}

func (c *ctx) symbolResolver(scope *Scope, pkg *Package, ident Token, passFileScope bool) (r Node) {
	fileScope := noScope
	if passFileScope {
		file := pkg.sourceFiles[ident.source]
		fileScope = &file.Scope
	}
	var err error
	if r, err = c.checker.SymbolResolver(scope, fileScope, pkg, ident); err != nil {
		c.err(ident, errorf("%s", err))
		return nil
	}

	return r
}

func (c *ctx) symbol(expr Expression) Node {
	for {
		switch x := expr.(type) {
		case *QualifiedIdent:
			return x
		default:
			c.err(expr, errorf("TODO %T", x))
			return nil
		}
	}
}

func (c *ctx) checkExprOrType(p *Expression) Node {
	n := *p
	switch x := n.check(c).(type) {
	case Expression:
		*p = x
		return x
	default:
		return x
	}
}

func (c *ctx) checkType(n Node) Type {
	switch x := n.(type) {
	case checker:
		switch y := x.check(c).(type) {
		case Type:
			return y
		case *StructTypeNode:
			return y.Type()
		case *TypeNameNode:
			return y.Type()
		case *Ident:
			return y.Type()
		case *PointerTypeNode:
			return y.Type()
		case *ArrayTypeNode:
			return y.Type()
		case *QualifiedIdent:
			return y.Type()
		case *TypeDef:
			return y.Type()
		case *Variable:
			return y.Type()
		case *AliasDecl:
			return y.Type()
		case *FunctionDecl:
			return y.Type()
		case *ParenType:
			return y.Type()
		case *Signature:
			return y.Type()
		case *PredefinedType:
			return y
		default:
			c.err(n, errorf("TODO %T", y))
			return Invalid
		}
	default:
		c.err(n, errorf("TODO %T", x))
		return Invalid
	}
}

func (c *ctx) checkExpr(p *Expression) (constant.Value, Type) {
	n := *p
	switch x := n.check(c).(type) {
	case Expression:
		*p = x
		return x.Value(), x.Type()
	default:
		c.err(n, errorf("TODO %T", x))
		return unknown, Invalid
	}
}

func (c *ctx) exprToType(n Expression) Node {
	switch x := n.(type) {
	case *UnaryExpr:
		switch x.Op.Ch {
		case '*':
			switch z := x.Expr.(type) {
			case *QualifiedIdent:
				return &PointerTypeNode{Star: x.Op, BaseType: z}
			case *Ident:
				return &PointerTypeNode{Star: x.Op, BaseType: z}
			default:
				c.err(n, errorf("TODO %T %s %T", x, x.Op.Ch.str(), z))
				return Invalid
			}
		default:
			c.err(n, errorf("TODO %T %s", x, x.Op.Ch.str()))
			return Invalid
		}
	default:
		c.err(n, errorf("TODO %T", x))
		return Invalid
	}
}

func (n *SourceFile) check(c *ctx) {
	pkgName := n.PackageClause.PackageName.Src()
	switch {
	case c.pkg.Name == "":
		c.pkg.Name = pkgName
	default:
		if pkgName != c.pkg.Name {
			c.err(n.PackageClause.PackageName, "expected package name %q, got %q", c.pkg.Name, pkgName)
		}
	}
	n.checkImports(c)
	if len(c.errors) != 0 {
		return
	}

	n.collectTLDs(c)
}

func (n *SourceFile) collectTLDs(c *ctx) {
	for _, tld := range n.TopLevelDecls {
		switch x := tld.(type) {
		case *ConstDecl:
			for _, cs := range x.ConstSpecs {
				for i, id := range cs.IdentifierList {
					c.pkg.Scope.add(c, id.Ident.Src(), 0, &Constant{node: cs, Expr: cs.exprList[i].Expr, Ident: id.Ident})
				}
			}
		case *FunctionDecl:
			switch nm := x.FunctionName.Src(); nm {
			case "init":
				c.pkg.Inits = append(c.pkg.Inits, x)
			default:
				c.pkg.Scope.add(c, x.FunctionName.Src(), 0, x)
			}
		case *MethodDecl:
			if len(x.Receiver.ParameterList) == 0 {
				c.err(x, "missing receiver")
				break
			}

			switch rx := x.Receiver.ParameterList[0].Type.(type) {
			case *PointerTypeNode:
				switch t := rx.BaseType.(type) {
				case *TypeNameNode:
					if t.Name.PackageName.IsValid() {
						c.err(t.Name, "cannot define new methods on non-local type %s", t.Name.Source(false))
						break
					}

					c.pkg.Scope.add(c, fmt.Sprintf("%s.%s", t.Name.Ident.Src(), x.MethodName.Src()), 0, x)
				default:
					c.err(x, errorf("TODO %T", t))
				}
			case *TypeNameNode:
				if rx.Name.PackageName.IsValid() {
					c.err(rx.Name, "cannot define new methods on non-local type %s", rx.Name.Source(false))
					break
				}

				c.pkg.Scope.add(c, fmt.Sprintf("%s.%s", rx.Name.Ident.Src(), x.MethodName.Src()), 0, x)
			case Token:
				c.pkg.Scope.add(c, fmt.Sprintf("%s.%s", rx.Src(), x.MethodName.Src()), 0, x)
			default:
				c.err(x, errorf("TODO %T", rx))
			}
		case *TypeDecl:
			for _, ts := range x.TypeSpecs {
				switch y := ts.(type) {
				case *AliasDecl:
					c.pkg.Scope.add(c, y.Ident.Src(), 0, ts)
				case *TypeDef:
					c.pkg.Scope.add(c, y.Ident.Src(), 0, ts)
				default:
					c.err(y, errorf("TODO %T", y))
				}
			}
		case *VarDecl:
			for _, vs := range x.VarSpecs {
				for i, id := range vs.IdentifierList {
					var expr Expression
					if i < len(vs.ExprList) {
						expr = vs.ExprList[i].Expr
					}
					c.pkg.Scope.add(c, id.Ident.Src(), 0, &Variable{Expr: expr, Ident: id.Ident, TypeNode: vs.Type})
				}
			}
		default:
			c.err(tld, "unexpected top level declaration node type: %T", x)
		}
	}
}

func (n *SourceFile) checkImports(c *ctx) {
	for _, id := range n.ImportDecls {
		for _, is := range id.ImportSpecs {
			pkg := c.packageLoader(c.pkg, n, is.ImportPath)
			if pkg == nil {
				return
			}

			qualifier := pkg.Name
			if is.Qualifier.IsValid() {
				qualifier = is.Qualifier.Src()
			}
			n.Scope.add(c, qualifier, 0, pkg)
			if _, ok := c.pkg.Scope.Nodes[qualifier]; !ok {
				c.pkg.Scope.add(c, qualifier, 0, pkg)
			}
		}
	}
}

// Package collects source files.
type Package struct {
	ImportPath  string
	Inits       []*FunctionDecl
	Blanks      []Node
	Name        string
	Scope       *Scope
	SourceFiles []*SourceFile
	sourceFiles map[*source]*SourceFile
}

// NewPackage returns a newly created Package or an error, if any.
func NewPackage(importPath string, files []*SourceFile) (r *Package, err error) {
	sourceFiles := map[*source]*SourceFile{}
	var ps *Scope
	for _, file := range files {
		if ps == nil {
			ps = file.packageScope
		}
		sourceFiles[file.PackageClause.Package.source] = file
	}
	if ps != nil {
		ps.Parent = &universe
	}
	return &Package{ImportPath: importPath, SourceFiles: files, Scope: ps, sourceFiles: sourceFiles}, nil
}

// Position implements Node. Position return a zero value.
func (n *Package) Position() (r token.Position) {
	if len(n.SourceFiles) != 0 {
		r = n.SourceFiles[0].PackageClause.Position()
	}
	return r
}

// Source implements Node. Source returns a zero value.
func (n *Package) Source(full bool) []byte { return nil }

// Check type checks n.
func (n *Package) Check(checker PackageChecker) error {
	c := newCtx(checker, n)
	for _, file := range n.SourceFiles {
		file.check(c)
	}
	n.checkDeclarations(c)
	n.checkFunctionBodies(c)
	return c.errors.Err()
}

func (n *Package) checkFunctionBodies(c *ctx) {
	if !c.checker.CheckFunctions() {
		return
	}

	var tldNames []string
	for tldName := range n.Scope.Nodes {
		tldNames = append(tldNames, tldName)
	}
	sort.Strings(tldNames)
	for _, tldName := range tldNames {
		switch x := n.Scope.Nodes[tldName].Node.(type) {
		case *FunctionDecl:
			x.checkBody(c)
		case *MethodDecl:
			c.err(x, errorf("TODO %T", x))
		}
	}
}

func (n *Package) checkDeclarations(c *ctx) {
	var tldNames []string
	for tldName := range n.Scope.Nodes {
		tldNames = append(tldNames, tldName)
	}
	sort.Strings(tldNames)
	for _, tldName := range tldNames {
		switch x := n.Scope.Nodes[tldName].Node.(type) {
		case checker:
			x.check(c)
		case *Package:
			// nop
		default:
			c.err(x, errorf("TODO %T", x))
		}
	}
}

func (n *Arguments) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	var resolvedTo Node
	var resolvedIn *Package
	switch x := c.checkExprOrType(&n.PrimaryExpr).(type) {
	case *Ident:
		resolvedTo = x.ResolvedTo()
	case *QualifiedIdent:
		resolvedIn = x.ResolvedIn()
		resolvedTo = x.ResolvedTo()
	case *ParenType:
		if len(n.ExprList) != 1 {
			c.err(n, errorf("TODO %T", x))
			return n
		}

		r := Expression(&Conversion{ConvertType: x, LParen: n.LParen, Expr: n.ExprList[0].Expr, Comma: n.ExprList[0].Comma, RParen: n.RParen})
		c.checkExpr(&r)
		return r
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}

	switch x := resolvedTo.(type) {
	case
		*AliasDecl,
		*TypeDef,
		PredefinedType:

		if len(n.ExprList) != 1 {
			c.err(n, errorf("TODO %T", x))
			return n
		}

		r := Expression(&Conversion{ConvertType: n.PrimaryExpr, LParen: n.LParen, Expr: n.ExprList[0].Expr, Comma: n.ExprList[0].Comma, RParen: n.RParen})
		c.checkExpr(&r)
		return r
	case *FunctionDecl:
		ft, ok := x.Type().(*FunctionType)
		if !ok {
			return n
		}

		switch len(ft.Result.Types) {
		case 1:
			n.typ = ft.Result.Types[0]
		default:
			n.typ = ft.Result
		}
		if len(ft.Parameters.Types) != len(n.ExprList) {
			c.err(n, errorf("TODO %T", n))
			return n
		}

		if resolvedIn != nil {
			switch resolvedIn.ImportPath {
			case "unsafe":
				switch x.FunctionName.Src() {
				case "Alignof", "Offsetof", "Sizeof":
					return n
				}
			}
		}

		for i, exprItem := range n.ExprList {
			_, et := c.checkExpr(&exprItem.Expr)
			et = c.singleType(exprItem.Expr, et)
			pt := ft.Parameters.Types[i]
			if !c.assignable(exprItem, et, pt) {
				c.err(n, errorf("TODO %T", n))
				continue
			}

			c.convertValue(exprItem, exprItem.Expr.Value(), pt)
		}
		return n
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}
}

func (c *ctx) assignable(n Node, expr, to Type) bool {
	if expr.Kind() == to.Kind() {
		return true
	}

	switch expr.Kind() {
	case UntypedInt, UntypedFloat, UntypedComplex:
		return isArithmeticType(to)
	case UntypedBool:
		return to.Kind() == Bool
	case UntypedString:
		return to.Kind() == String
	case UntypedNil:
		switch to.Kind() {
		case Pointer, Slice, Map, Function, Interface, Chan:
			return true
		default:
			return false
		}
	default:
		c.err(n, errorf("TODO %s -> %s", expr, to))
		return false
	}
}

func (n *BasicLit) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("internal error: %T %T %T", n, n.Type(), n.Value()))
	return n
}

func (n *BinaryExpr) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	x, a := c.checkExpr(&n.A)
	y, b := c.checkExpr(&n.B)
	switch n.Op.Ch {
	case SHL, SHR:
		// The right operand in a shift expression must have integer type or be an
		// untyped constant representable by a value of type uint.
		switch {
		case isIntegerType(b):
			// ok
		case y.Kind() != constant.Unknown:
			c.err(n, errorf("TODO %v", n.Op.Ch.str()))
			return n
		default:
			c.err(n, errorf("TODO %v", n.Op.Ch.str()))
			return n
		}

		// If the left operand of a non-constant shift expression is an untyped
		// constant, it is first implicitly converted to the type it would assume if
		// the shift expression were replaced by its left operand alone.
		switch {
		case y.Kind() == constant.Unknown && x.Kind() != constant.Unknown:
			c.err(n, errorf("TODO %v", n.Op.Ch.str()))
			return n
		default:
			n.typ = a
			n.val = constant.BinaryOp(x, xlat[n.Op.Ch], y)
		}
	case LOR, LAND:
		c.err(n, errorf("TODO %v", n.Op.Ch.str()))
	case EQ, NE, '<', LE, '>', GE:
		c.err(n, errorf("TODO %v", n.Op.Ch.str()))
	default:
		if !isArithmeticType(a) && !isUntypedArithmeticType(a) || !isArithmeticType(b) && !isUntypedArithmeticType(b) {
			c.err(n, errorf("TODO %v %v", a, b))
			break
		}

		// For other binary operators, the operand types must be identical unless the
		// operation involves shifts or untyped constants.

		// Except for shift operations, if one operand is an untyped constant and the
		// other operand is not, the constant is implicitly converted to the type of
		// the other operand.
		switch {
		case x.Kind() == constant.Unknown && y.Kind() == constant.Unknown:
			if a.Kind() != b.Kind() {
				c.err(n, errorf("TODO %v", n.Op.Ch.str()))
				break
			}

			n.typ = a
		case x.Kind() == constant.Unknown && y.Kind() != constant.Unknown:
			c.convertValue(n, y, a)
			n.typ = a
		case x.Kind() != constant.Unknown && y.Kind() == constant.Unknown:
			c.convertValue(n, x, b)
			n.typ = b
		default: // case x.Kind() != constant.Unknown && y.Kind() != constant.Unknown:
			n.val = constant.BinaryOp(x, xlat[n.Op.Ch], y)
			switch n.val.Kind() {
			case constant.Int:
				n.typ = UntypedIntType
			case constant.Float:
				n.typ = UntypedFloatType
			case constant.Complex:
				n.typ = UntypedComplexType
			default:
				c.err(n, errorf("TODO %v", n.Op.Ch.str()))
			}
		}
	}
	return n
}

func (n *CompositeLit) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	n.typ = c.checkType(n.LiteralType)
	//TODO check n.LiteralValue
	return n
}

func (n *Constant) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	n.val, n.typ = c.checkExpr(&n.Expr)
	return n
}

func (n *Conversion) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	n.typ = c.checkType(n.ConvertType)
	c.checkExpr(&n.Expr)
	if n.Type().Kind() != InvalidKind && n.Expr.Type().Kind() != InvalidKind {
		n.val = c.convert(n.Expr, n.Type())
	}
	return n
}

func (n *ParenType) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	n.typ = c.checkType(n.TypeNode)
	return n
}

func (n *FunctionLit) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
	return n

	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *GenericOperand) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
	return n

	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *Ident) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	switch n.resolvedTo = c.symbolResolver(n.lexicalScope, c.pkg, n.Token, true); x := n.ResolvedTo().(type) {
	case Type:
		n.typ = x
	case *TypeDef:
		n.typ = c.checkType(x)
	case *AliasDecl:
		n.typ = c.checkType(x)
	case *FunctionDecl:
		n.typ = c.checkType(x)
	case *Variable:
		n.typ = c.checkType(x)
	default:
		c.err(n, errorf("TODO %T", x))
	}
	return n
}

func (n *Index) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
	return n

	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *MethodExpr) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
	return n

	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *ParenExpr) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	switch x := c.checkExprOrType(&n.Expr).(type) {
	case *PointerTypeNode:
		r := &ParenType{LParen: n.LParen, TypeNode: c.exprToType(n.Expr), RParen: n.RParen}
		c.checkType(r)
		return r
	case *Ident:
		switch y := x.ResolvedTo().(type) {
		case *Variable:
			n.typ = y.Type()
		default:
			c.err(n, errorf("TODO %T %T", x, y))
		}
	case *BinaryExpr:
		n.typ = x.Type()
	case *UnaryExpr:
		n.typ = x.Type()
	default:
		c.err(n, errorf("TODO %T", x))
	}
	return n
}

func (n *QualifiedIdent) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	switch {
	case n.PackageName.IsValid():
		switch x := c.symbolResolver(n.lexicalScope, c.pkg, n.PackageName, true).(type) {
		case *Package:
			n.resolvedIn = x
			n.resolvedTo = c.symbolResolver(x.Scope, x, n.Ident, false)
		case *Variable:
			r := Expression(&Selector{PrimaryExpr: &Ident{lexicalScoper: n.lexicalScoper, Token: n.PackageName}, Dot: n.Dot, Ident: n.Ident})
			c.checkExpr(&r)
			return r
		default:
			c.err(n, errorf("TODO %T", x))
		}
	default:
		n.resolvedIn = c.pkg
		n.resolvedTo = c.symbolResolver(n.lexicalScope, c.pkg, n.Ident, true)
	}

	switch x := n.ResolvedTo().(type) {
	case PredefinedType:
		n.typ = x
	case *TypeDef:
		n.typ = c.checkType(x)
	case *FunctionDecl:
		n.typ = c.checkType(x)
	case *AliasDecl:
		n.typ = c.checkType(x)
	default:
		c.err(n, errorf("TODO %T", x))
	}
	return n

}

func (n *Selector) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	_, t := c.checkExpr(&n.PrimaryExpr)
	if x, ok := t.(*TypeDef); ok {
		t = x.Type()
	}
	if x, ok := t.(*PointerType); ok {
		t = x.Elem
	}
	if x, ok := t.(*TypeDef); ok {
		t = x.Type()
	}
	if x, ok := t.(*StructType); ok {
		if f := x.FieldByName(n.Ident.Src()); f != nil {
			n.typ = f.Type()
			return n
		}
	}

	c.err(n.Dot, errorf("TODO %T %v", t, t))
	return n
}

func (n *SliceExpr) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
	return n

	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *TypeAssertion) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
	return n

	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *TypeSwitchGuard) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
	return n

	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *UnaryExpr) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	var resolvedTo Node
	switch x := c.checkExprOrType(&n.Expr).(type) {
	case *QualifiedIdent:
		resolvedTo = x.ResolvedTo()
	case *Ident:
		resolvedTo = x.ResolvedTo()
	case
		PredefinedType,
		*Arguments,
		*BasicLit,
		*Conversion,
		*ParenExpr,
		*Selector:

		// ok
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}

	switch x := resolvedTo.(type) {
	case nil:
		// ok
	case Type:
		switch n.Op.Ch {
		case '*':
			r := &PointerTypeNode{Star: n.Op, BaseType: x}
			c.checkType(r)
			return r
		default:
			c.err(n, errorf("TODO %T", x))
			return n
		}
	case *Variable:
		// ok
	default:
		c.err(n.Expr, errorf("TODO %T", x))
		return n
	}

	t := c.singleType(n.Expr, n.Expr.Type())
	n.typ = t
	n.val = n.Expr.Value()
	if !n.Op.IsValid() {
		return n
	}

	v := n.Value()
	switch n.Op.Ch {
	case '&':
		n.typ = newPointer(c.pkg, t)
	case '-', '+':
		if !isArithmeticType(t) && !isUntypedArithmeticType(t) {
			c.err(n, errorf("TODO %s %v", n.Op.Ch.str(), t))
			break
		}

		if v.Kind() == constant.Unknown {
			break
		}

		w := constant.UnaryOp(xlat[n.Op.Ch], v, 0) //TODO prec
		if w.Kind() == constant.Unknown {
			c.err(n, errorf("TODO %s", n.Op.Ch.str()))
			break
		}

		n.val = w
	case '*':
		switch x := n.Type().(type) {
		case *PointerType:
			n.typ = x.Elem
		default:
			c.err(n, errorf("TODO %T", x))
		}
	default:
		c.err(n, errorf("TODO %T %s", n, n.Op.Ch.str()))
	}
	return n
}

func (n *Variable) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	if n.TypeNode != nil {
		n.typ = c.checkType(n.TypeNode)
	}
	if n.Expr != nil {
		c.checkExpr(&n.Expr)
	}
	switch {
	case n.TypeNode == nil && n.Expr == nil:
		c.err(n, errorf("TODO %T", n))
	case n.TypeNode == nil && n.Expr != nil:
		n.typ = c.defaultType(n.Expr.Type())
	case n.TypeNode != nil && n.Expr == nil:
		// nop
	default: //case n.Type != nil && n.Expr != nil:
		c.err(n, errorf("TODO %T", n))
	}
	return n
}

func (n *Signature) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	r := &FunctionType{Parameters: tuple(c, n.Parameters.ParameterList), Result: &TupleType{}}
	switch x := n.Result.(type) {
	case *TypeNameNode:
		r.Result.Types = append(r.Result.Types, c.checkType(x))
	case nil:
		// ok
	case *Parameters:
		r.Result = tuple(c, x.ParameterList)
	default:
		c.err(x, errorf("TODO %T", x))
	}
	n.typ = r
	return n
}

func tuple(c *ctx, a []*ParameterDecl) (r *TupleType) {
	r = &TupleType{}
	for _, v := range a {
		r.Types = append(r.Types, c.checkType(v.Type))
	}
	return r
}

func (n *FunctionDecl) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	ft, ok := c.checkType(n.Signature).(*FunctionType)
	if ok {
		ft.node = n
		n.typ = ft
	}
	return n
}

func (n *FunctionDecl) checkBody(c *ctx) {
	body := n.FunctionBody
	if body == nil {
		return
	}

	s := &body.Scope
	vis := body.LBrace.Offset()
	ft, ok := n.Type().(*FunctionType)
	if !ok {
		return
	}

	types := ft.Parameters.Types
	for _, v := range n.Signature.Parameters.ParameterList {
		for _, w := range v.IdentifierList {
			id := w.Ident
			v := &Variable{Ident: id}
			v.typ = types[0]
			types = types[1:]
			v.guard = checked
			s.add(c, id.Src(), vis, v)
		}
	}
	types = ft.Result.Types
	switch x := n.Signature.Result.(type) {
	case *Parameters:
		for _, v := range x.ParameterList {
			for _, w := range v.IdentifierList {
				id := w.Ident
				v := &Variable{Ident: id}
				v.typ = types[0]
				types = types[1:]
				v.guard = checked
				s.add(c, id.Src(), vis, v)
			}
		}
	case nil:
		// ok
	default:
		c.err(n, errorf("TODO %T", x))
	}
}

func (n *MethodDecl) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
	return n

	//TODO c.err(n, errorf("TODO %T", n))
}
