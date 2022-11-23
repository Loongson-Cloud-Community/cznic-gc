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
	"path/filepath"
	"sort"
	"strings"
	"sync"
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
	lookup        func(rel, importPath, version string) (fsPath string, err error)
	parallel      *parallel
	searchGoPaths []string
	searchGoroot  []string

	configured bool
}

// NewConfig returns a newly created config or an error, if any.
func NewConfig(opts ...ConfigOption) (*Config, error) {
	r := &Config{
		env:      map[string]string{},
		parallel: newParallel(),
	}

	defer func() { r.configured = true }()

	r.lookup = r.DefaultLookup
	for _, opt := range opts {
		if err := opt(r); err != nil {
			return nil, err
		}
	}
	ctx := build.Default
	r.goos = r.getenv("GOOS", ctx.GOOS)
	r.goarch = r.getenv("GOARCH", ctx.GOARCH)
	r.goroot = r.getenv("GOROOT", ctx.GOROOT)
	r.searchGoroot = []string{filepath.Join(r.goroot, "src")}
	r.gopath = r.getenv("GOPATH", ctx.GOPATH)
	r.searchGoPaths = filepath.SplitList(r.gopath)
	for i, v := range r.searchGoPaths {
		r.searchGoPaths[i] = filepath.Join(v, "src")
	}
	return r, nil
}

func (c *Config) open(name string) (fs.File, error) {
	if c.fs == nil {
		return os.Open(name)
	}

	return c.fs.Open(name)
}

func (c *Config) glob(pattern string) (matches []string, err error) {
	if c.fs == nil {
		return filepath.Glob(pattern)
	}

	return fs.Glob(c.fs, pattern)
}

// Default lookup translates import paths, possibly relative to rel, to file system paths.
func (c *Config) DefaultLookup(rel, importPath, version string) (fsPath string, err error) {
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
		if rel != "" {
			panic(todo(""))
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
		fsPath = filepath.Join(v, importPath)
		dir, err := c.open(fsPath)
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

func (c *Config) DefaultFileFilter(matchedFSPaths []string, withTests bool) (pkgFiles []string, err error) {
	w := 0
	for _, v := range matchedFSPaths {
		base := filepath.Base(v)
		base = base[:len(base)-len(filepath.Ext(base))]
		const testSuffix = "_test"
		if strings.HasSuffix(base, testSuffix) {
			if !withTests {
				continue
			}

			base = base[:len(base)-len(testSuffix)]
		}
		if x := strings.LastIndexByte(base, '_'); x > 0 {
			last := base[x+1:]
			base = base[:x]
			var prevLast string
			if x := strings.LastIndexByte(base, '_'); x > 0 {
				prevLast = base[x+1:]
			}
			switch {
			case last != "" && prevLast != "":
				//  *_GOOS_GOARCH
				if knownOS[prevLast] && prevLast != c.goos {
					continue
				}

				if knownArch[last] && last != c.goarch {
					continue
				}
			case last != "":
				// *_GOOS or *_GOARCH
				if knownOS[prevLast] && prevLast != c.goos {
					continue
				}

				if knownArch[last] && last != c.goarch {
					continue
				}
			}
		}

		matchedFSPaths[w] = v
		w++
	}
	return matchedFSPaths[:w], nil
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

// ConfigFS configures a file system used for opening Go source files. If not
// explicitly configured, a default os.DirFS("/") is used on Unix-like
// operating systems. On Windows it will be rooted on the volume where
// runtime.GOROOT() is.
func ConfigFS(fs fs.FS) ConfigOption {
	return func(cfg *Config) error {
		if cfg.configured {
			return fmt.Errorf("ConfigFS: Config instance already configured")
		}

		cfg.fs = fs
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
	AST            map[string]*AST // AST maps fsPaths of individual files to their respective ASTs
	FSPath         string
	GoFiles        []fs.FileInfo
	ImportPath     string
	InvalidGoFiles map[string]error // errors for particular files, if any
	Scope          *Scope           // Package scope.
	Version        string
	cfg            *Config
}

// NewPackage returns a Package, possibly cached, for importPath@version or an
// error, if any. The fileFilter argument can be nil, in such case
// cfg.DefaultFileFilter is used, which ignores Files with suffix _test.go
// unless withTests is true.
func (cfg *Config) NewPackage(dir, importPath, version string, fileFilter func(matchedFSPaths []string, withTests bool) (pkgFiles []string, err error), withTests bool) (*Package, error) {
	fsPath, err := cfg.lookup(dir, importPath, version)
	if err != nil {
		return nil, fmt.Errorf("lookup %s: %v", importPath, err)
	}

	pat := filepath.Join(fsPath, "*.go")
	matches, err := cfg.glob(pat)
	if err != nil {
		return nil, fmt.Errorf("glob %s: %v", pat, err)
	}

	if len(matches) == 0 {
		return nil, fmt.Errorf("no Go files in %s", fsPath)
	}

	if fileFilter == nil {
		fileFilter = cfg.DefaultFileFilter
	}
	if matches, err = fileFilter(matches, withTests); err != nil {
		return nil, fmt.Errorf("matching Go files in %s: %v", fsPath, err)
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
	var mu sync.Mutex
	var wg sync.WaitGroup

	defer func() { wg.Wait() }()

	for _, path := range matches {
		f, err := cfg.open(path)
		if err != nil {
			return r, fmt.Errorf("opening file %q: %v", path, err)
		}

		var fi fs.FileInfo
		if fi, err = f.Stat(); err != nil {
			return r, fmt.Errorf("stat %s: %v", path, err)
		}

		if !fi.Mode().IsRegular() {
			continue
		}

		r.GoFiles = append(r.GoFiles, fi)

		wg.Add(1)
		path := path
		cfg.parallel.exec(func() {
			var err error

			defer func() {
				f.Close()
				if err != nil {
					mu.Lock()

					defer mu.Unlock()

					if r.InvalidGoFiles == nil {
						r.InvalidGoFiles = map[string]error{}
					}
					r.InvalidGoFiles[path] = err
				}
				wg.Done()
			}()

			var b []byte
			if b, err = io.ReadAll(f); err != nil {
				err = fmt.Errorf("reading %s: %v", path, err)
				return
			}

			p := newParser(newScope(nil, scPackage), path, b, false)
			var ast *AST
			if ast, err = p.parse(); err != nil {
				return
			}

			mu.Lock()

			defer mu.Unlock()

			r.AST[path] = ast
		})
	}
	return r, nil
}
