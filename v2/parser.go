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

// ParseSourceFileConfig represents configuration used by ParseSourceFile.
type ParseSourceFileConfig struct {
	Tags      []string
	AllErrors bool
}

// ParseSourceFile parser buf and returns the *SourceFile or an error, if any.
// Positions are reported as if buf is coming from a file named name. The
// buffer becomes owned by the *SourceFile and must not be modified after
// calling ParseSourceFile.
//
// The parser normally stops parsing after some number of errors. Passing
// allErrros == true overides that.
func ParseSourceFile(cfg *ParseSourceFileConfig, name string, buf []string) (*SourceFile, error) {
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

type PackageClause struct{}

// Position implements Node.
func (n *PackageClause) Position() token.Position { panic(todo("")) }

type ImportDecl struct{}

// Position implements Node.
func (n *ImportDecl) Position() token.Position { panic(todo("")) }

type TopLevelDecl struct{}

// Position implements Node.
func (n *TopLevelDecl) Position() token.Position { panic(todo("")) }
