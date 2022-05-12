// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"go/token"
)

var (
	_ = []Node{
		(*SourceFile)(nil),
		(*PackageClause)(nil),
		(*ImportDecl)(nil),
		(*TopLevelDecl)(nil),
	}
)

type parser struct{}

func newParser() *parser {
	panic(todo(""))
}

// ParseSourceFile parses buf and returns the *SourceFile or an error, if any.
// Positions are reported as if buf is coming from a file named name. The
// buffer becomes owned by the *SourceFile and must not be modified after
// calling ParseSourceFile.
func ParseSourceFile(name string, buf []string) (*SourceFile, error) {
	panic(todo(""))
}

// SourceFile represents a Go source file.
//
// Each source file consists of a package clause defining the package to which
// it belongs, followed by a possibly empty set of import declarations that
// declare packages whose contents it wishes to use, followed by a possibly
// empty set of declarations of functions, types, variables, and constants.
//
//	SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
type SourceFile struct {
	PackageClause *PackageClause
	Semicolon     Token
	ImportDecls   []*ImportDecl
	TopLevelDecls []*TopLevelDecl
}

// Position implements Node.
func (n *SourceFile) Position() token.Position { return n.PackageClause.Position() }

// PackageClause represents the package clause od a source file.
//
// A package clause begins each source file and defines the package to which the file belongs.
//
//	PackageClause  = "package" PackageName .
//	PackageName    = identifier .
type PackageClause struct {
	//TODO
}

// Position implements Node.
func (n *PackageClause) Position() token.Position { panic(todo("")) }

// ImportDecl represents an import declaration of a source file.
//
// An import declaration states that the source file containing the declaration
// depends on functionality of the imported package (§Program initialization
// and execution) and enables access to exported identifiers of that package.
// The import names an identifier (PackageName) to be used for access and an
// ImportPath that specifies the package to be imported.
//
//	ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
//	ImportSpec       = [ "." | PackageName ] ImportPath .
//	ImportPath       = string_lit .
type ImportDecl struct{}

// Position implements Node.
func (n *ImportDecl) Position() token.Position { panic(todo("")) }

// TopLevelDecl represents a declaration, function declaration or a method
// declaration.
//
//	TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
type TopLevelDecl struct{}

// Position implements Node.
func (n *TopLevelDecl) Position() token.Position { panic(todo("")) }
