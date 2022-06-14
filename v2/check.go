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
)

// Scope binds names to nodes.
type Scope struct {
	Nodes  map[string]Node
	Parent *Scope
}

// IsUniverse reports whether s is the universe scope.
func (s *Scope) IsUniverse() bool { return s.Parent == nil }

// IsPackage reports whether s is a package scope.
func (s *Scope) IsPackage() bool { return s.Parent != nil && s.Parent.IsUniverse() }

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
		trc("%v: %q (%v: %v: %v:)", n.Position(), nm, origin(4), origin(3), origin(2))
		c.err(n, "%s redeclared, previous declaration at %v:", nm, x.Position())
		return
	}

	s.Nodes[nm] = n
}

type ctx struct {
	cfg    *CheckConfig
	errors errList
	pkg    *Package
	scope  *Scope

	iota int64
}

func newCtx(cfg *CheckConfig, pkg *Package) *ctx {
	return &ctx{
		cfg: cfg,
		pkg: pkg,
	}
}

func (c *ctx) err(n Node, s string, args ...interface{}) {
	var pos token.Position
	if n != nil {
		pos = n.Position()
	}
	c.errors.err(pos, s, args...)
}

func (c *ctx) check(n Node) Type {
	switch x := n.(type) {
	case *StructTypeNode:
		return x.check(c)
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
				case *TypeDef:
					c.pkg.Scope.add(c, y.Ident.Src(), &TypeName{node: y})
				case *AliasDecl:
					c.pkg.Scope.add(c, y.Ident.Src(), &AliasType{node: y})
				default:
					c.err(ts, errorf("TODO %T", y))
				}
			}
		case *VarDecl:
			for _, vs := range x.VarSpecs {
				for i, id := range vs.IdentifierList {
					var expr Expression
					if i < len(vs.ExpressionList) {
						expr = vs.ExpressionList[i].Expression
					}
					c.pkg.Scope.add(c, id.Ident.Src(), &Variable{Expr: expr, Ident: id.Ident})
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
			importPath, err := strconv.Unquote(is.ImportPath.Src())
			if err != nil {
				c.err(is.ImportPath, "%s", err)
				return
			}

			pkg, err := c.cfg.Loader(importPath)
			if err != nil {
				c.err(is.ImportPath, "%s", err)
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

const (
	checkZero = iota
	checkChecking
	checkChecked
)

// Package collects source files.
type Package struct {
	ImportPath  string
	Inits       []*FunctionDecl
	Blanks      []Node
	Name        string
	Scope       Scope
	SourceFiles []*SourceFile
}

// NewPackage returns a newly created Package or an error, if any.
func NewPackage(importPath string, files []*SourceFile) (r *Package, err error) {
	return &Package{ImportPath: importPath, SourceFiles: files, Scope: Scope{Parent: &universe}}, nil
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
func (n *Package) Check(cfg *CheckConfig) error {
	if cfg.Loader == nil {
		return fmt.Errorf("no loader configured")
	}

	if cfg.Resolver == nil {
		return fmt.Errorf("no resolver configured")
	}
	c := newCtx(cfg, n)
	for _, file := range n.SourceFiles {
		file.Scope.Parent = &n.Scope
		file.check(c)
	}
	c.scope = &n.Scope
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

	c.err(n, errorf("TODO %T", n))
}

func (n *BasicLit) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
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

	c.err(n, errorf("TODO %T", n))
}

func (n *Constant) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	iota := c.iota
	c.iota = n.node.iota
	defer func() { c.iota = iota }()

	c.err(n, errorf("TODO %T", n))
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

	c.err(n, errorf("TODO %T", n))
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

	c.err(n, errorf("TODO %T", n))
}

func (n *QualifiedIdent) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
}

func (n *Selector) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
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

	c.err(n, errorf("TODO %T", n))
}

func (n *Variable) check(c *ctx) {
	if !n.enter(c, n) {
		return
	}

	defer n.exit()

	c.err(n, errorf("TODO %T", n))
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
