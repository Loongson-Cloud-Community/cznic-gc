// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"fmt"
	"io"
	"io/fs"
	"path"
	"sort"
	"strings"
)

// Config configures NewPackage and Package.Check.
//
// Config instances can be shared, the instance is never mutated once created.
type Config struct {
	buildTags []string
	env       map[string]string
	fs        fs.FS
	goarch    string
	goos      string
	lookup    func(importPath string) (fsPath string, err error)
}

// NewConfig returns a newly created config or an error, if any.
//
// Process environment is not used, only whatever KEY=VALUE pairs are passed in
// the env argument.
func NewConfig(fs fs.FS, goos, goarch string, env, buildTags []string, lookup func(importPath string) (fsPath string, err error)) (*Config, error) {
	r := &Config{
		buildTags: buildTags,
		env:       map[string]string{},
		fs:        fs,
		goarch:    goarch,
		goos:      goos,
		lookup:    lookup,
	}
	for _, v := range env {
		switch x := strings.IndexByte(v, '='); {
		case x < 0:
			r.env[v] = ""
		default:
			r.env[v[:x]] = v[x+1:]
		}
	}
	return r, nil
}

// Package represents a Go package.
type Package struct {
	AST        map[string]*AST // AST maps fsPaths of individual files to their respective ASTs
	FSPath     string
	GoFiles    []fs.FileInfo
	ImportPath string
	Scope      *Scope // Package scope.
	cfg        *Config
}

// NewPackage returns a Package, possibly cached, for importPath or an error,
// if any.
func NewPackage(cfg *Config, importPath string) (*Package, error) {
	fsPath, err := cfg.lookup(importPath)
	if err != nil {
		return nil, fmt.Errorf("lookup %s: %v", importPath, err)
	}

	pat := fsPath + "/*.go"
	matches, err := fs.Glob(cfg.fs, pat)
	if err != nil {
		return nil, fmt.Errorf("glob %s: %v", pat, err)
	}

	if len(matches) == 0 {
		return nil, fmt.Errorf("no Go files in %s", fsPath)
	}

	r := &Package{
		AST:        map[string]*AST{},
		FSPath:     fsPath,
		ImportPath: importPath,
		Scope:      newScope(nil, scPackage),
		cfg:        cfg,
	}
	sort.Strings(matches)
	for _, v := range matches {
		path := path.Join(fsPath, v)
		f, err := cfg.fs.Open(path)
		if err != nil {
			return nil, fmt.Errorf("open package file %q: %v", path, err)
		}

		//TODO parallel
		if err := func() error {
			defer f.Close()

			fi, err := f.Stat()
			if err != nil {
				return fmt.Errorf("stat %s: %v", path, err)
			}

			if !fi.Mode().IsRegular() {
				return fmt.Errorf("%s: not a regular file", path)
			}

			b, err := io.ReadAll(f)
			if err != nil {
				return fmt.Errorf("reading %s: %v", path, err)
			}

			//TODO must share errList
			p := newParser(r.Scope, path, b, false)
			ast, err := p.parse()
			if err != nil {
				return fmt.Errorf("%v", err)
			}

			r.AST[path] = ast
			r.GoFiles = append(r.GoFiles, fi)
			return nil
		}(); err != nil {
			return nil, err
		}
	}
	return r, nil
}
