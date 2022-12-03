// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate stringer -output stringer.go -linecomment -type=Kind,guard,ScopeKind,ChanDir,TypeCheck

package gc // modernc.org/gc/v3

import (
	"fmt"
	"go/build"
	"go/build/constraint"
	"go/token"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"unicode"

	"github.com/hashicorp/golang-lru/v2"
)

type FileFilter func(cfg *Config, importPath string, matchedFSPaths []string, withTestFiles bool) (pkgFiles []string, err error)

type TypeCheck int

const (
	TypeCheckNone TypeCheck = iota
	TypeCheckAll
)

type cacheKey struct {
	buildTagsKey string
	cfg          *Config
	fsPath       string
	goarch       string
	goos         string
	gopathKey    string
	goroot       string
	importPath   string
	typeCheck    TypeCheck

	withTestFiles bool
}

type Cache struct {
	sync.Mutex
	lru *lru.TwoQueueCache[cacheKey, *Package]
}

func NewCache(size int) (*Cache, error) {
	c, err := lru.New2Q[cacheKey, *Package](size)
	if err != nil {
		return nil, err
	}

	return &Cache{lru: c}, nil
}

func MustNewCache(size int) *Cache {
	c, err := NewCache(size)
	if err != nil {
		panic(todo("", err))
	}

	return c
}

type ConfigOption func(*Config) error

// Config configures NewPackage
//
// Config instances can be shared, they are not mutated once created and
// configured.
type Config struct {
	buildTagMap   map[string]bool
	buildTags     []string
	buildTagsKey  string // Zero byte separated
	builtin       *Package
	cache         *Cache
	env           map[string]string
	fs            fs.FS
	goarch        string
	goos          string
	gopath        string
	gopathKey     string // Zero byte separated
	goroot        string
	lookup        func(rel, importPath, version string) (fsPath string, err error)
	parallel      *parallel
	searchGoPaths []string
	searchGoroot  []string

	configured bool
}

// NewConfig returns a newly created config or an error, if any.
func NewConfig(opts ...ConfigOption) (r *Config, err error) {
	r = &Config{
		buildTagMap: map[string]bool{},
		env:         map[string]string{},
		parallel:    newParallel(),
	}

	defer func() { r.configured = true }()

	r.lookup = r.DefaultLookup
	ctx := build.Default
	r.goos = r.getenv("GOOS", ctx.GOOS)
	r.goarch = r.getenv("GOARCH", ctx.GOARCH)
	r.goroot = r.getenv("GOROOT", ctx.GOROOT)
	r.gopath = r.getenv("GOPATH", ctx.GOPATH)
	r.buildTags = append(r.buildTags, r.goos, r.goarch)
	for _, opt := range opts {
		if err := opt(r); err != nil {
			return nil, err
		}
	}
	if r.getenv("CGO_ENABLED", "1") == "1" {
		r.buildTags = append(r.buildTags, "cgo")
	}
	for i, v := range r.buildTags {
		tag := strings.TrimSpace(v)
		r.buildTags[i] = tag
		r.buildTagMap[tag] = true
	}
	sort.Strings(r.buildTags)
	r.buildTagsKey = strings.Join(r.buildTags, "\x00")
	r.searchGoroot = []string{filepath.Join(r.goroot, "src")}
	r.searchGoPaths = filepath.SplitList(r.gopath)
	r.gopathKey = strings.Join(r.searchGoPaths, "\x00")
	for i, v := range r.searchGoPaths {
		r.searchGoPaths[i] = filepath.Join(v, "src")
	}
	if r.builtin, err = r.NewPackage("", "builtin", "", nil, false, TypeCheckAll); err != nil {
		return nil, err
	}

	r.builtin.isBuiltin = true
	if err := r.builtin.check(newCtx(r)); err != nil {
		return nil, err
	}

	return r, nil
}

func (c *Config) get(k *cacheKey) *Package {
	if c.cache == nil {
		return nil
	}

	if pkg, ok := c.cache.lru.Get(*k); ok {
		return pkg
	}

	return nil
}

func (c *Config) put(k *cacheKey, p *Package) {
	if c.cache == nil {
		return
	}

	c.cache.lru.Add(*k, p)
}

func (c *Config) stat(name string) (fs.FileInfo, error) {
	if c.fs == nil {
		return os.Stat(name)
	}

	name = filepath.ToSlash(name)
	if x, ok := c.fs.(fs.StatFS); ok {
		return x.Stat(name)
	}

	f, err := c.fs.Open(name)
	if err != nil {
		return nil, err
	}

	defer f.Close()

	return f.Stat()
}

func (c *Config) open(name string) (fs.File, error) {
	if c.fs == nil {
		return os.Open(name)
	}

	name = filepath.ToSlash(name)
	return c.fs.Open(name)
}

func (c *Config) glob(pattern string) (matches []string, err error) {
	if c.fs == nil {
		return filepath.Glob(pattern)
	}

	pattern = filepath.ToSlash(pattern)
	return fs.Glob(c.fs, pattern)
}

func (c *Config) checkConstraints(pos token.Position, sep string) bool {
	if !strings.Contains(sep, "//go:build") && !strings.Contains(sep, "+build") {
		return true
	}

	lines := strings.Split(sep, "\n")
	var build, plusBuild []string
	for i, line := range lines {
		if constraint.IsGoBuild(line) && i < len(lines)-1 && lines[i+1] == "" {
			build = append(build, line)
		}
		if constraint.IsPlusBuild(line) {
			plusBuild = append(plusBuild, line)
		}
	}
	switch len(build) {
	case 0:
		// ok
	case 1:
		expr, err := constraint.Parse(build[0])
		if err != nil {
			return true
		}

		return expr.Eval(func(tag string) bool {
			switch tag {
			case "unix":
				return unixOS[c.goos]
			default:
				return c.buildTagMap[tag]
			}
		})
	default:
		panic(todo("%v: %q", pos, build))
	}

	for _, line := range plusBuild {
		expr, err := constraint.Parse(line)
		if err != nil {
			return true
		}

		if !expr.Eval(func(tag string) bool {
			switch tag {
			case "unix":
				return unixOS[c.goos]
			default:
				return c.buildTagMap[tag]
			}
		}) {
			return false
		}
	}
	return true
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

func DefaultFileFilter(cfg *Config, importPath string, matchedFSPaths []string, withTestFiles bool) (pkgFiles []string, err error) {
	w := 0
	for _, v := range matchedFSPaths {
		base := filepath.Base(v)
		base = base[:len(base)-len(filepath.Ext(base))]
		const testSuffix = "_test"
		if strings.HasSuffix(base, testSuffix) {
			if !withTestFiles {
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
				if knownOS[prevLast] && prevLast != cfg.goos {
					continue
				}

				if knownArch[last] && last != cfg.goarch {
					continue
				}
			case last != "":
				// *_GOOS or *_GOARCH
				if knownOS[prevLast] && prevLast != cfg.goos {
					continue
				}

				if knownArch[last] && last != cfg.goarch {
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

		cfg.buildTags = append(cfg.buildTags, tags...)
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
func ConfigCache(c *Cache) ConfigOption {
	return func(cfg *Config) error {
		if cfg.configured {
			return fmt.Errorf("ConfigCache: Config instance already configured")
		}

		cfg.cache = c
		return nil
	}
}

type importGuard struct {
	m     map[string]struct{}
	stack []string
}

func newImportGuard() *importGuard { return &importGuard{m: map[string]struct{}{}} }

// Package represents a Go package. The instance must not be mutated.
type Package struct {
	AST            map[string]*AST // AST maps fsPaths of individual files to their respective ASTs
	FSPath         string
	GoFiles        []fs.FileInfo
	ImportPath     string
	InvalidGoFiles map[string]error // errors for particular files, if any
	Scope          *Scope           // Package scope.
	Version        string
	cfg            *Config
	guard          *importGuard
	mu             sync.Mutex
	typeCheck      TypeCheck

	isBuiltin bool
	isChecked bool
}

// NewPackage returns a Package, possibly cached, for importPath@version or an
// error, if any. The fileFilter argument can be nil, in such case
// DefaultFileFilter is used, which ignores Files with suffix _test.go unless
// withTestFiles is true.
func (c *Config) NewPackage(dir, importPath, version string, fileFilter FileFilter, withTestFiles bool, typeCheck TypeCheck) (pkg *Package, err error) {
	return c.newPackage(dir, importPath, version, fileFilter, withTestFiles, typeCheck, newImportGuard())
}

func (c *Config) newPackage(dir, importPath, version string, fileFilter FileFilter, withTestFiles bool, typeCheck TypeCheck, guard *importGuard) (pkg *Package, err error) {
	if _, ok := guard.m[importPath]; ok {
		return nil, fmt.Errorf("import cycle %v", guard.stack)
	}

	guard.stack = append(guard.stack, importPath)
	fsPath, err := c.lookup(dir, importPath, version)
	if err != nil {
		return nil, fmt.Errorf("lookup %s: %v", importPath, err)
	}

	pat := filepath.Join(fsPath, "*.go")
	matches, err := c.glob(pat)
	if err != nil {
		return nil, fmt.Errorf("glob %s: %v", pat, err)
	}

	if len(matches) == 0 {
		return nil, fmt.Errorf("no Go files in %s", fsPath)
	}

	if fileFilter == nil {
		fileFilter = DefaultFileFilter
	}
	if matches, err = fileFilter(c, importPath, matches, withTestFiles); err != nil {
		return nil, fmt.Errorf("matching Go files in %s: %v", fsPath, err)
	}

	var k *cacheKey
	if c.cache != nil {
		k = &cacheKey{
			buildTagsKey:  c.buildTagsKey,
			cfg:           c,
			fsPath:        fsPath,
			goarch:        c.goarch,
			goos:          c.goos,
			gopathKey:     c.gopathKey,
			goroot:        c.goroot,
			importPath:    importPath,
			typeCheck:     typeCheck,
			withTestFiles: withTestFiles,
		}
		if pkg = c.get(k); pkg != nil && pkg.matches(k, matches) {
			return pkg, nil
		}

		defer func() {
			if pkg != nil && err == nil {
				c.put(k, pkg)
			}
		}()
	}

	r := &Package{
		AST:        map[string]*AST{},
		FSPath:     fsPath,
		ImportPath: importPath,
		Version:    version,
		cfg:        c,
		guard:      guard,
		typeCheck:  typeCheck,
	}

	defer func() { r.guard = nil }()

	sort.Strings(matches)
	var mu sync.Mutex
	var wg sync.WaitGroup

	defer func() {
		wg.Wait()
		sort.Slice(r.GoFiles, func(i, j int) bool { return r.GoFiles[i].Name() < r.GoFiles[j].Name() })
		if err != nil || len(r.InvalidGoFiles) != 0 || typeCheck == TypeCheckNone {
			return
		}

		//TODO err = pkg.check(newCtx(c))
	}()

	for _, path := range matches {
		f, err := c.open(path)
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

		wg.Add(1)
		c.parallel.exec(func(path string, f fs.File, fi fs.FileInfo) func() {
			return func() {
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

				p := newParser(newScope(nil, PackageScope), path, b, false)
				if p.peek(0) == PACKAGE {
					tok := Token{p.s.source, p.s.toks[p.ix].ch, int32(p.ix)}
					if !c.checkConstraints(tok.Position(), tok.Sep()) {
						return
					}
				}

				var ast *AST
				if ast, err = p.parse(); err != nil {
					return
				}

				mu.Lock()

				defer mu.Unlock()

				r.AST[path] = ast
				r.GoFiles = append(r.GoFiles, fi)
			}
		}(path, f, fi))
	}
	return r, nil
}

func (p *Package) matches(k *cacheKey, matches []string) bool {
	matched := map[string]struct{}{}
	for _, match := range matches {
		matched[match] = struct{}{}
	}
	for _, cachedInfo := range p.GoFiles {
		name := cachedInfo.Name()
		path := filepath.Join(p.FSPath, name)
		if _, ok := matched[path]; !ok {
			return false
		}

		info, err := k.cfg.stat(path)
		if err != nil {
			return false
		}

		if info.IsDir() ||
			info.Size() != cachedInfo.Size() ||
			info.ModTime().After(cachedInfo.ModTime()) ||
			info.Mode() != cachedInfo.Mode() {
			return false
		}
	}
	return true
}
