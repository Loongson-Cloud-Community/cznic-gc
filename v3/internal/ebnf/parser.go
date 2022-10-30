package main

import (
	"go/scanner"
	"go/token"
)

type parser struct {
	f    *token.File
	path string
	toks []tok

	ix int

	closed bool
}

func newParser(path string, src []byte) (r *parser, err error) {
	r = &parser{
		path: path,
	}
	var s scanner.Scanner
	fs := token.NewFileSet()
	r.f = fs.AddFile(path, -1, len(src))
	s.Init(r.f, src, func(pos token.Position, msg string) { err = errorf("%v: %s", pos, msg) }, 0)
	for {
		pos, t, lit := s.Scan()
		r.toks = append(r.toks, tok{pos, t, lit})
		if err != nil {
			return nil, err
		}

		if t == token.EOF {
			return r, nil
		}
	}
}

func (p *parser) errPosition() token.Position {
	return p.f.PositionFor(p.toks[p.ix].pos, true)
}

func (p *parser) c() token.Token { return p.peek(0) }
func (p *parser) close()         { p.closed = true }
func (p *parser) n()             { p.ix++ }

func (p *parser) peek(n int) token.Token {
	if p.closed {
		return p.toks[len(p.toks)].tok
	}

	return p.toks[p.ix+n].tok
}

func (p *parser) must(t token.Token) bool {
	if p.c() != t {
		p.close()
		return false
	}

	p.n()
	return true
}

// SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
func (p *parser) parse() (err error) {
	if !(p.packageClause() && p.must(token.SEMICOLON)) {
		return errorf("%s: syntax error", p.errPosition())
	}

	for p.importDecl() && p.must(token.SEMICOLON) {
	}
	for p.topLevelDecl() && p.must(token.SEMICOLON) {
	}

	if p.c() != token.EOF {
		return errorf("%s: syntax error", p.errPosition())
	}

	return nil
}

// TopLevelDecl = Declaration
// 	| FunctionDecl
// 	| MethodDecl .
func (p *parser) topLevelDecl() bool {
	switch p.c() {
	case token.CONST:
		return false
	case token.FUNC:
		return false
	case token.TYPE:
		return false
	case token.VAR:
		return false
	default:
		return false
	}
}

// ImportDecl = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
func (p *parser) importDecl() bool {
	if p.c() != token.IMPORT {
		return false
	}

	return false
}

// PackageClause = "package" PackageName .
func (p *parser) packageClause() bool {
	return p.must(token.PACKAGE) && p.packageName()
}

// PackageName = identifier .
func (p *parser) packageName() bool { return p.must(token.IDENT) }
