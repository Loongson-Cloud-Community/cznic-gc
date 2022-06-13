// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"fmt"
	"go/token"
	"sort"
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
		return
	}

	if s.Nodes == nil {
		s.Nodes = map[string]Node{}
	}
	if x, ok := s.Nodes[nm]; ok {
		c.err(n, "%s redeclared, previous declaration at %v:", nm, x.Position())
		return
	}

	s.Nodes[nm] = n
}

// CheckConfig configures the type checker.
type CheckConfig struct {
	Loader   func(importPath string) (*Package, error)
	Resolver func(pkg *Package, ident string) Node
}

type ctx struct {
	cfg       *CheckConfig
	errors    errList
	pkg       *Package
	scope     *Scope
	tldCycles map[string]byte
}

func newCtx(cfg *CheckConfig, pkg *Package) *ctx {
	return &ctx{
		cfg:       cfg,
		pkg:       pkg,
		tldCycles: map[string]byte{},
	}
}

func (c *ctx) err(n Node, s string, args ...interface{}) {
	var pos token.Position
	if n != nil {
		pos = n.Position()
	}
	c.errors.err(pos, s, args...)
}

func (n *SourceFile) check(c *ctx) {
	pkgName := n.PackageClause.Package.Src()
	switch {
	case c.pkg.Name == "":
		c.pkg.Name = pkgName
	default:
		if pkgName != c.pkg.Name {
			c.err(n.PackageClause.PackageName, "expected package name %s", c.pkg.Name)
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
					c.pkg.Scope.add(c, id.Ident.Src(), &Constant{Expr: cs.expressionList[i], Ident: id.Ident, iota: cs.iota})
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
					var expr Node
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
			pkg, err := c.cfg.Loader(is.ImportPath.Src())
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
	c := newCtx(cfg, n)
	for _, file := range n.SourceFiles {
		file.Scope.Parent = &n.Scope
		file.check(c)
	}
	c.scope = &n.Scope
	n.checkDeclarations(c)
	return c.errors.Err()
}

// TLD const, type, var
func (n *Package) checkDeclarations(c *ctx) {
	var tldNames []string
	for tldName := range n.Scope.Nodes {
		tldNames = append(tldNames, tldName)
	}
	sort.Strings(tldNames)
	for _, tldName := range tldNames {
		n.checkDeclaration(c, tldName)
	}
}

func (n *Package) checkDeclaration(c *ctx, nm string) {
	defer func() {
		c.tldCycles[nm] = checkChecked
	}()

	decl := n.Scope.Nodes[nm]
	switch c.tldCycles[nm] {
	case checkChecked:
		return
	case checkChecking:
		c.err(n.Scope.Nodes[nm], errorf("type checking loop: %s", nm))
		return
	}

	c.tldCycles[nm] = checkChecking
	switch x := decl.(type) {
	case *Constant:
		x.check(c, nm)
	case *TypeName:
		x.check(c, nm)
	case *Variable:
		x.check(c, nm)
	case *AliasType:
		x.check(c, nm)
	case *FunctionDecl, *MethodDecl:
		// nop
	default:
		c.err(x, errorf("TODO %T", x))
	}
}

func (n *TypeName) check(c *ctx, nm string) {
	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *Variable) check(c *ctx, nm string) {
	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *AliasType) check(c *ctx, nm string) {
	//TODO c.err(n, errorf("TODO %T", n))
}

func (n *Constant) check(c *ctx, nm string) {
	//TODO c.err(n, errorf("TODO %T", n))
}
