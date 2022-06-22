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
		Nodes: map[string]Node{
			"bool":       PredefinedType(Bool),
			"byte":       PredefinedType(Uint8),
			"complex128": PredefinedType(Complex128),
			"complex64":  PredefinedType(Complex64),
			"float32":    PredefinedType(Float32),
			"float64":    PredefinedType(Float64),
			"int":        PredefinedType(Int),
			"int16":      PredefinedType(Int16),
			"int32":      PredefinedType(Int32),
			"int64":      PredefinedType(Int64),
			"int8":       PredefinedType(Int8),
			"nil":        PredefinedType(UntypedNil),
			"rune":       PredefinedType(Int32),
			"string":     PredefinedType(String),
			"uint":       PredefinedType(Uint),
			"uint16":     PredefinedType(Uint16),
			"uint32":     PredefinedType(Uint32),
			"uint64":     PredefinedType(Uint64),
			"uint8":      PredefinedType(Uint8),
			"uintptr":    PredefinedType(Uintptr),
		},
	}

	noScope = &Scope{}
)

// Scope binds names to nodes.
type Scope struct {
	Nodes  map[string]Node
	Parent *Scope
}

// IsPackage reports whether s is a package scope.
func (s *Scope) IsPackage() bool { return s.Parent != nil && s.Parent.Parent == nil }

func (s *Scope) add(c *ctx, nm string, n Node) {
	if nm == "_" {
		if s.IsPackage() {
			c.pkg.Blanks = append(c.pkg.Blanks, n)
		}
		return
	}

	if s.Nodes == nil {
		s.Nodes = map[string]Node{}
	}
	if x, ok := s.Nodes[nm]; ok {
		// trc("%v: %q (%v: %v: %v:)", n.Position(), nm, origin(4), origin(3), origin(2))
		c.err(n, "%s redeclared, previous declaration at %v:", nm, x.Position())
		return
	}

	s.Nodes[nm] = n
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
	nm := ident.Src()
	fileScope := noScope
	if passFileScope {
		file := pkg.sourceFiles[ident.source]
		fileScope = &file.Scope
	}
	var err error
	if r, err = c.checker.SymbolResolver(scope, fileScope, pkg, nm); err != nil {
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

func (c *ctx) check(n Node) Node {
	switch x := n.(type) {
	case typeChecker:
		return x.check(c)
	default:
		c.err(n, errorf("TODO %T", x))
		return Invalid
	}
}

func (c *ctx) checkType(n Node) Type {
	switch x := c.check(n).(type) {
	case Type:
		return x
	case *StructTypeNode:
		return x.Type()
	case *TypeNameNode:
		return x.Type()
	case *Ident:
		return x.Type()
	case *PointerTypeNode:
		return x.Type()
	case *ArrayTypeNode:
		return x.Type()
	case *QualifiedIdent:
		return x.Type()
	default:
		c.err(n, errorf("TODO %T", x))
		return Invalid
	}
}

func (c *ctx) checkExpr(n Expression) Expression {
	switch x := c.check(n).(type) {
	case Expression:
		return x
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}
}

func (c *ctx) resolveQualifiedIdent(n *QualifiedIdent) (r Node) {
	if n.PackageName.IsValid() {
		switch x := c.symbolResolver(n.lexicalScope, c.pkg, n.PackageName, true).(type) {
		case *Package:
			n.resolvedIn = x
			n.resolvedTo = c.symbolResolver(x.Scope, x, n.Ident, false)
			return n.ResolvedTo()
		default:
			c.err(n, errorf("TODO %T", x))
			return nil
		}
	}

	n.resolvedIn = c.pkg
	n.resolvedTo = c.symbolResolver(n.lexicalScope, c.pkg, n.Ident, true)
	return n.ResolvedTo()
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
					c.pkg.Scope.add(c, id.Ident.Src(), &Constant{node: cs, Expr: cs.expressionList[i].Expression, Ident: id.Ident})
				}
			}
		case *FunctionDecl:
			switch nm := x.FunctionName.Src(); nm {
			case "init":
				c.pkg.Inits = append(c.pkg.Inits, x)
			default:
				c.pkg.Scope.add(c, x.FunctionName.Src(), x)
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

					c.pkg.Scope.add(c, fmt.Sprintf("%s.%s", t.Name.Ident.Src(), x.MethodName.Src()), x)
				default:
					c.err(x, errorf("TODO %T", t))
				}
			case *TypeNameNode:
				if rx.Name.PackageName.IsValid() {
					c.err(rx.Name, "cannot define new methods on non-local type %s", rx.Name.Source(false))
					break
				}

				c.pkg.Scope.add(c, fmt.Sprintf("%s.%s", rx.Name.Ident.Src(), x.MethodName.Src()), x)
			case Token:
				c.pkg.Scope.add(c, fmt.Sprintf("%s.%s", rx.Src(), x.MethodName.Src()), x)
			default:
				c.err(x, errorf("TODO %T", rx))
			}
		case *TypeDecl:
			for _, ts := range x.TypeSpecs {
				switch y := ts.(type) {
				case *AliasDecl:
					c.pkg.Scope.add(c, y.Ident.Src(), ts)
				case *TypeDef:
					c.pkg.Scope.add(c, y.Ident.Src(), ts)
				default:
					c.err(y, errorf("TODO %T", y))
				}
			}
		case *VarDecl:
			for _, vs := range x.VarSpecs {
				for i, id := range vs.IdentifierList {
					var expr Expression
					if i < len(vs.ExpressionList) {
						expr = vs.ExpressionList[i].Expression
					}
					c.pkg.Scope.add(c, id.Ident.Src(), &Variable{Expr: expr, Ident: id.Ident, TypeNode: vs.Type})
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
			n.Scope.add(c, qualifier, pkg)
			if _, ok := c.pkg.Scope.Nodes[qualifier]; !ok {
				c.pkg.Scope.add(c, qualifier, pkg)
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
	//TODO check func/method bodies
	return c.errors.Err()
}

func (n *Package) checkDeclarations(c *ctx) {
	var tldNames []string
	for tldName := range n.Scope.Nodes {
		tldNames = append(tldNames, tldName)
	}
	sort.Strings(tldNames)
	for _, tldName := range tldNames {
		switch x := n.Scope.Nodes[tldName].(type) {
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
	switch x := n.PrimaryExpr.check(c).(type) {
	case *Ident:
		resolvedTo = x.ResolvedTo()
	case *QualifiedIdent:
		resolvedIn = x.ResolvedIn()
		resolvedTo = x.ResolvedTo()
	case *ParenType:
		if len(n.ExpressionList) != 1 {
			c.err(n, errorf("TODO %T", x))
			return n
		}

		return (&Conversion{ConvertType: x, LParen: n.LParen, Expression: n.ExpressionList[0].Expression, Comma: n.ExpressionList[0].Comma, RParen: n.RParen}).check(c)
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}

	switch x := resolvedTo.(type) {
	case
		*AliasDecl,
		*TypeDef,
		PredefinedType:

		if len(n.ExpressionList) != 1 {
			c.err(n, errorf("TODO %T", x))
			return n
		}

		return (&Conversion{ConvertType: n.PrimaryExpr, LParen: n.LParen, Expression: n.ExpressionList[0].Expression, Comma: n.ExpressionList[0].Comma, RParen: n.RParen}).check(c)
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
		if len(ft.Parameters) != len(n.ExpressionList) {
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

		for i, exprItem := range n.ExpressionList {
			exprItem.Expression = c.checkExpr(exprItem.Expression)
			et := c.singleType(exprItem.Expression, exprItem.Expression.Type())
			pt := ft.Parameters[i].Type()
			if !c.assignable(exprItem, et, pt) {
				c.err(n, errorf("TODO %T", n))
				continue
			}

			c.convertValue(exprItem, exprItem.Expression.Value(), pt)
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

func (n *BinaryExpression) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	n.A = c.checkExpr(n.A)
	n.B = c.checkExpr(n.B)
	a, b := c.singleType(n.A, n.A.Type()), c.singleType(n.B, n.B.Type())
	x, y := n.A.Value(), n.B.Value()
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
			n.v = constant.BinaryOp(x, xlat[n.Op.Ch], y)
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
			n.v = constant.BinaryOp(x, xlat[n.Op.Ch], y)
			switch n.v.Kind() {
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

	switch x := n.Expr.check(c.setIota(n.node.iota)).(type) {
	case Expression:
		n.typ = x.Type()
		n.v = x.Value()
	default:
		c.err(n, errorf("TODO %T", x))
	}
	return n
}

func (n *Conversion) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	switch x := c.check(n.ConvertType).(type) {
	case *Ident:
		switch y := x.ResolvedTo().(type) {
		case PredefinedType:
			n.typ = y
		case *AliasDecl:
			n.typ = y.Type()
		default:
			c.err(n, errorf("TODO %T %T", x, y))
			return n
		}
	case *QualifiedIdent:
		switch y := x.ResolvedTo().(type) {
		case *TypeDef:
			n.typ = x.Type()
		default:
			c.err(n, errorf("TODO %T %T", x, y))
			return n
		}
	case *ParenType:
		n.typ = x.Type()
	case *TypeDef:
		n.typ = x.Type()
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}

	n.Expression = c.checkExpr(n.Expression)
	if n.Type().Kind() != InvalidKind && n.Expression.Type().Kind() != InvalidKind {
		n.v = c.convert(n.Expression, n.Type())
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
		x.check(c)
		n.typ = x.Type()
	case *AliasDecl:
		x.check(c)
		n.typ = x.Type()
	case *FunctionDecl:
		x.check(c)
		n.typ = x.Type()
	case *Variable:
		x.check(c)
		n.typ = x.Type()
	case nil:
		// nop here
	default:
		panic(todo("%v: %T %s", n.Position(), x, x.Source(false)))
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

	switch x := n.Expression.check(c).(type) {
	case *PointerTypeNode:
		return (&ParenType{LParen: n.LParen, TypeNode: x, RParen: n.RParen}).check(c)
	case *Ident:
		switch y := x.ResolvedTo().(type) {
		case *Variable:
			n.typ = y.Type()
		default:
			c.err(n, errorf("TODO %T %T", x, y))
		}
	case *BinaryExpression:
		n.Expression = x
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

	switch x := c.resolveQualifiedIdent(n).(type) {
	case PredefinedType:
		n.typ = x
	case *TypeDef:
		n.typ = x.Type()
	case *FunctionDecl:
		n.typ = x.Type()
	case *AliasDecl:
		n.typ = x.Type()
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

	var t Type
	switch x := n.PrimaryExpr.check(c).(type) {
	case *Conversion:
		t = x.Type()
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}

	if x, ok := t.(*PointerType); ok {
		t = x.Elem
	}
	if x, ok := t.(*StructType); ok {
		if f := x.FieldByName(n.Ident.Src()); f != nil {
			n.typ = f.Type()
			return n
		}
	}

	c.err(n, errorf("TODO %T", n))
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
	switch x := n.UnaryExpr.check(c).(type) {
	case *QualifiedIdent:
		resolvedTo = x.ResolvedTo()
	case *Ident:
		resolvedTo = x.ResolvedTo()
	case
		*Arguments,
		*BasicLit,
		*ParenExpr:

		// ok
	case *Conversion:
		n.UnaryExpr = x
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}

	switch x := resolvedTo.(type) {
	case nil:
		// ok
	case *TypeDef:
		switch n.UnaryOp.Ch {
		case '*':
			return (&PointerTypeNode{Star: n.UnaryOp, BaseType: x}).check(c)
		default:
			c.err(n, errorf("TODO %T", x))
			return n
		}
	case *Variable:
		// ok
	default:
		c.err(n, errorf("TODO %T", x))
		return n
	}

	t := c.singleType(n.UnaryExpr, n.UnaryExpr.Type())
	n.typ = t
	n.v = n.UnaryExpr.Value()
	if !n.UnaryOp.IsValid() {
		return n
	}

	v := n.Value()
	switch n.UnaryOp.Ch {
	case '&':
		n.typ = newPointer(c.pkg, t)
	case '-', '+':
		if !isArithmeticType(t) && !isUntypedArithmeticType(t) {
			c.err(n, errorf("TODO %s %v", n.UnaryOp.Ch.str(), t))
			break
		}

		if v.Kind() == constant.Unknown {
			break
		}

		w := constant.UnaryOp(xlat[n.UnaryOp.Ch], v, 0) //TODO prec
		if w.Kind() == constant.Unknown {
			c.err(n, errorf("TODO %s", n.UnaryOp.Ch.str()))
			break
		}

		n.v = w
	default:
		c.err(n, errorf("TODO %T %s", n, n.UnaryOp.Ch.str()))
	}
	return n

	//TODO n.UnaryExpr.check(c)
	//TODO t := n.UnaryExpr.Type()
	//TODO switch x := t.(type) {
	//TODO case *TupleType:
	//TODO 	if len(x.Types) != 1 {
	//TODO 		c.err(n, errorf("TODO %v", x))
	//TODO 		return
	//TODO 	}

	//TODO 	t = x.Types[0]
	//TODO }
	//TODO if !n.UnaryOp.IsValid() {
	//TODO 	n.typ = t
	//TODO 	return
	//TODO }

	//TODO v := n.UnaryExpr.Value()
	//TODO switch n.UnaryOp.Ch {
	//TODO case '*':
	//TODO 	switch x := t.(type) {
	//TODO 	case *PointerType:
	//TODO 		n.typ = x.Elem
	//TODO 	}
	//TODO case '&':
	//TODO 	n.typ = newPointer(c.pkg, t)
	//TODO case '-', '+':
	//TODO 	if !isArithmeticType(t) && !isUntypedArithmeticType(t) {
	//TODO 		c.err(n, errorf("TODO %s %v", n.UnaryOp.Ch.str(), t))
	//TODO 		return
	//TODO 	}

	//TODO 	n.typ = t
	//TODO 	if v.Kind() == constant.Unknown {
	//TODO 		break
	//TODO 	}

	//TODO 	w := constant.UnaryOp(xlat[n.UnaryOp.Ch], v, 0) //TODO prec
	//TODO 	if w.Kind() == constant.Unknown {
	//TODO 		c.err(n, errorf("TODO %s", n.UnaryOp.Ch.str()))
	//TODO 		return
	//TODO 	}

	//TODO 	n.v = w
	//TODO default:
	//TODO 	c.err(n, errorf("TODO %s", n.UnaryOp.Ch.str()))
	//TODO }
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
		n.Expr = c.checkExpr(n.Expr)
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

func (n *FunctionDecl) check(c *ctx) Node {
	if !n.enter(c, n) {
		return n
	}

	defer n.exit()

	n.typ = n.Signature.check(c, n)
	return n
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
