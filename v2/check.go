// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"fmt"
	"go/token"
	"sort"
	"strconv"
)

var (
	universe = Scope{
		Nodes: map[string]Node{
			"bool":       PredefinedType(Bool),
			"complex128": PredefinedType(Complex128),
			"complex64":  PredefinedType(Complex64),
			"float32":    PredefinedType(Float32),
			"float64":    PredefinedType(Float64),
			"int":        PredefinedType(Int),
			"int16":      PredefinedType(Int16),
			"int32":      PredefinedType(Int32),
			"int64":      PredefinedType(Int64),
			"int8":       PredefinedType(Int8),
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

type ctx struct {
	checker  PackageChecker
	errors   errList
	packages map[packagesKey]*Package
	pkg      *Package
	stack    []Node

	iota int64

	pushNamed bool
}

func newCtx(checker PackageChecker, pkg *Package) *ctx {
	return &ctx{
		checker:  checker,
		packages: map[packagesKey]*Package{},
		pkg:      pkg,
	}
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

func (c *ctx) check(n Node) Type {
	switch x := n.(type) {
	case typeChecker:
		x.check(c)
		return x.Type()
	default:
		c.err(n, errorf("TODO %T", x))
		return Invalid
	}
}

func (c *ctx) resolveQualifiedType(n *QualifiedIdent) Type {
	switch x := c.resolveQualifiedIdent(n).(type) {
	case *TypeDef:
		x.check(c)
		return x.Type()
	case Type:
		return x
	case *AliasDecl:
		x.check(c)
		return x.Type()
	default:
		c.err(n, errorf("TODO %T", x))
		return nil
	}
}

func (c *ctx) resolveQualifiedIdent(n *QualifiedIdent) (r Node) {
	if n.PackageName.IsValid() {
		switch x := c.symbolResolver(n.lexicalScope, c.pkg, n.PackageName, true).(type) {
		case nil:
			return nil
		case *Package:
			return c.symbolResolver(x.Scope, x, n.Ident, false)
		default:
			c.err(n, errorf("TODO %T", x))
			return nil
		}
	}

	return c.symbolResolver(n.lexicalScope, c.pkg, n.Ident, true)
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

func (n *Arguments) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	n.PrimaryExpr.check(c)
	//TODO check arg list
	switch pe := n.PrimaryExpr.Type().(type) {
	case *InvalidType:
		switch x := n.PrimaryExpr.(type) {
		case *ParenExpr:
			n.typ = x.ParenType()
			n.convType.typ = n.Type()
		default:
			c.err(n, errorf("TODO %T", x))
		}
	default:
		c.err(n, errorf("TODO %T", pe))
	}
}

func (n *BasicLit) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("internal error: %T %T %T", n, n.Type(), n.Value()))
}

func (n *BinaryExpression) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *CompositeLit) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	tc := n.LiteralType.(typeChecker)
	tc.check(c)
	n.typ = tc.Type()
	//TODO check n.LiteralValue
}

func (n *Constant) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	iota := c.iota
	c.iota = n.node.iota
	defer func() { c.iota = iota }()

	n.Expr.check(c)
	n.typ = n.Expr.Type()
	n.v = n.Expr.Value()
}

func (n *Conversion) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *FunctionLit) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *GenericOperand) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *Ident) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	switch x := c.symbolResolver(n.lexicalScope, c.pkg, n.Token, true).(type) {
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
}

func (n *Index) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *MethodExpr) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *ParenExpr) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	n.Expression.check(c)
	switch x := n.Expression.(type) {
	case *UnaryExpr:
		if t := x.UnaryExpr.Type(); t != Invalid && x.UnaryOp.Ch == '*' {
			n.parenType.typ = &PointerType{Elem: t}
		}
	}
}

func (n *QualifiedIdent) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	switch x := c.resolveQualifiedIdent(n).(type) {
	case Type:
		n.typ = x
	case *TypeDef:
		x.check(c)
		n.typ = x.Type()
	default:
		c.err(n, errorf("TODO %T", x))
	}
}

func (n *Selector) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	n.PrimaryExpr.check(c)
	switch x := n.PrimaryExpr.Type().(type) {
	case *PointerType:
		switch y := x.Elem.(type) {
		case *StructType:
			if f := y.FieldByName(n.Ident.Src()); f != nil {
				n.typ = f.Type()
			}
		default:
			c.err(n, errorf("TODO %T %T", n, y))
		}
	default:
		c.err(n, errorf("TODO %T %s", n, n.Source(false)))
	}
}

func (n *SliceExpr) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *TypeAssertion) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *TypeSwitchGuard) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *UnaryExpr) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	n.UnaryExpr.check(c)
	t := n.UnaryExpr.Type()
	if !n.UnaryOp.IsValid() {
		n.typ = t
		return
	}

	switch n.UnaryOp.Ch {
	case '*':
		switch x := t.(type) {
		case *PointerType:
			n.typ = x.Elem
		}
	default:
		c.err(n, errorf("TODO %s", n.UnaryOp.Ch))
	}
}

func (n *Variable) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	switch {
	case n.TypeNode == nil && n.Expr == nil:
		c.err(n, errorf("TODO %T", n))
	case n.TypeNode == nil && n.Expr != nil:
		n.Expr.check(c)
		n.typ = n.Expr.Type()
	case n.TypeNode != nil && n.Expr == nil:
		tc := n.TypeNode.(typeChecker)
		tc.check(c)
		n.typ = tc.Type()
	default: //case n.Type != nil && n.Expr != nil:
		c.err(n, errorf("TODO %T", n))
	}
}

func (n *FunctionDecl) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	n.typ = n.Signature.check(c)
}

func (n *MethodDecl) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}