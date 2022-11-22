// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"fmt"
	"go/build"
	"io"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"sort"
	"strings"
	"unicode"
)

type Cache interface {
	Get(importPath, version string) *Package
	Put(*Package)
}

type ConfigOption func(*Config) error

// Config configures NewPackage and Package.Check.
//
// Config instances can be shared, the instance is never mutated once created
// and configured.
type Config struct {
	buildTags     []string
	cache         Cache
	env           map[string]string
	fs            fs.FS
	goarch        string
	goos          string
	gopath        string
	goroot        string
	lookup        func(dir, importPath, version string) (fsPath string, err error)
	searchGoPaths []string
	searchGoroot  []string

	configured  bool
	isDefaultFS bool
}

// NewConfig returns a newly created config or an error, if any.
func NewConfig(opts ...ConfigOption) (*Config, error) {
	r := &Config{
		env:         map[string]string{},
		fs:          os.DirFS("/"),
		isDefaultFS: true,
	}

	defer func() { r.configured = true }()

	r.lookup = r.defaultLookup
	for _, opt := range opts {
		if err := opt(r); err != nil {
			return nil, err
		}
	}
	ctx := build.Default
	r.goos = r.getenv("GOOS", ctx.GOOS)
	r.goarch = r.getenv("GOARCH", ctx.GOARCH)
	r.goroot = r.getenv("GOROOT", ctx.GOROOT)
	r.searchGoroot = []string{path.Join(r.goroot, "src")}
	r.gopath = r.getenv("GOPATH", ctx.GOPATH)
	r.searchGoPaths = filepath.SplitList(r.gopath)
	for i, v := range r.searchGoPaths {
		r.searchGoPaths[i] = path.Join(v, "src")
	}
	return r, nil
}

func (c *Config) defaultLookup(dir, importPath, version string) (fsPath string, err error) {
	if importPath == "" {
		return "", fmt.Errorf("import path cannot be emtpy")
	}

	// Implementation restriction: A compiler may restrict ImportPaths to non-empty
	// strings using only characters belonging to Unicode's L, M, N, P, and S
	// general categories (the Graphic characters without spaces) and may also
	// exclude the characters !"#$%&'()*,:;<=>?[\]^`{|} and the Unicode replacement
	// character U+FFFD.
	if strings.ContainsAny(importPath, "!\"#$%&'()*,:;<=>?[\\]^`{|}\ufffd") {
		return "", fmt.Errorf("invalid import path: %s", importPath)
	}

	for _, r := range importPath {
		if !unicode.Is(unicode.L, r) &&
			!unicode.Is(unicode.M, r) &&
			!unicode.Is(unicode.N, r) &&
			!unicode.Is(unicode.P, r) &&
			!unicode.Is(unicode.S, r) {
			return "", fmt.Errorf("invalid import path: %s", importPath)
		}
	}
	var search []string
	ip0 := importPath
	switch slash := strings.IndexByte(importPath, '/'); {
	case strings.HasPrefix(importPath, "./"):
		if dir != "" {
			return c.defaultLookup("", path.Join(filepath.ToSlash(dir), importPath), version)
		}

		return "", fmt.Errorf("invalid import path: %s", importPath)
	case strings.HasPrefix(importPath, "/"):
		return importPath, nil
	case slash > 0:
		ip0 = importPath[:slash]
	default:
		ip0 = importPath
	}
	if ip0 != "" {
		switch {
		case strings.Contains(ip0, "."):
			search = c.searchGoPaths
		default:
			search = c.searchGoroot
		}
	}
	for _, v := range search {
		fsPath = path.Join(v, importPath)
		if strings.HasPrefix(fsPath, "/") {
			fsPath = fsPath[1:]
		}
		dir, err := c.fs.Open(fsPath)
		if err != nil {
			continue
		}

		fi, err := dir.Stat()
		dir.Close()
		if err != nil {
			continue
		}

		if fi.IsDir() {
			return fsPath, nil
		}
	}

	return "", fmt.Errorf("cannot find package %s, searched %v", importPath, search)
}

func (c *Config) getenv(nm, deflt string) (r string) {
	if r = c.env[nm]; r != "" {
		return r
	}

	if r = os.Getenv(nm); r != "" {
		return r
	}

	return deflt
}

// ConfigBuildTags configures build tags.
func ConfigBuildTags(tags []string) ConfigOption {
	return func(cfg *Config) error {
		if cfg.configured {
			return fmt.Errorf("ConfigBuildTags: Config instance already configured")
		}

		cfg.buildTags = tags
		return nil
	}
}

// ConfigEnviron configures environment variables.
func ConfigEnviron(env []string) ConfigOption {
	return func(cfg *Config) error {
		if cfg.configured {
			return fmt.Errorf("ConfigEnviron: Config instance already configured")
		}

		for _, v := range env {
			switch x := strings.IndexByte(v, '='); {
			case x < 0:
				cfg.env[v] = ""
			default:
				cfg.env[v[:x]] = v[x+1:]
			}
		}
		return nil
	}
}

// ConfigFS configures a file system.
func ConfigFS(fs fs.FS) ConfigOption {
	return func(cfg *Config) error {
		if cfg.configured {
			return fmt.Errorf("ConfigFS: Config instance already configured")
		}

		cfg.fs = fs
		cfg.isDefaultFS = false
		return nil
	}
}

// ConfigLookup configures a lookup function.
func ConfigLookup(f func(dir, importPath, version string) (fsPath string, err error)) ConfigOption {
	return func(cfg *Config) error {
		if cfg.configured {
			return fmt.Errorf("ConfigLookup: Config instance already configured")
		}

		cfg.lookup = f
		return nil
	}
}

// ConfigCache configures a cache.
func ConfigCache(c Cache) ConfigOption {
	return func(cfg *Config) error {
		if cfg.configured {
			return fmt.Errorf("ConfigCache: Config instance already configured")
		}

		cfg.cache = c
		return nil
	}
}

// Package represents a Go package.
type Package struct {
	AST          map[string]*AST  // AST maps fsPaths of individual files to their respective ASTs
	ErrorGoFiles map[string]error // errors for particular files, if any
	FSPath       string
	GoFiles      []fs.FileInfo
	ImportPath   string
	Scope        *Scope // Package scope.
	Version      string
	cfg          *Config
}

// NewPackage returns a Package, possibly cached, for importPath@version or an
// error, if any.
func (cfg *Config) NewPackage(dir, importPath, version string) (*Package, error) {
	fsPath, err := cfg.lookup(dir, importPath, version)
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
		Version:    version,
		cfg:        cfg,
	}
	sort.Strings(matches)
	for _, path := range matches {
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
				return nil
			}

			b, err := io.ReadAll(f)
			if err != nil {
				return fmt.Errorf("reading %s: %v", path, err)
			}

			//TODO must share errList
			p := newParser(r.Scope, path, b, false)
			ast, err := p.parse()
			if err != nil {
				if r.ErrorGoFiles == nil {
					r.ErrorGoFiles = map[string]error{}
				}
				r.ErrorGoFiles[path] = err
				return nil
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
