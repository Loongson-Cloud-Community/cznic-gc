// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"flag"
	"fmt"
	goparser "go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"sort"
	"strings"
	"sync"
	"testing"
)

var (
	oRE     = flag.String("re", "", "")
	oReport = flag.Bool("report", false, "")
	oTrc    = flag.Bool("trc", false, "")

	re *regexp.Regexp
)

func TestMain(m *testing.M) {
	flag.BoolVar(&noBack, "noback", false, "panic on parser back")
	flag.BoolVar(&panicBack, "panicback", false, "panic on parser back")
	flag.BoolVar(&trcTODOs, "trctodo", false, "")
	flag.Parse()
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	os.Exit(m.Run())
}

func stack() string { return string(debug.Stack()) }

type golden struct {
	a  []string
	f  *os.File
	mu sync.Mutex
	t  *testing.T

	discard bool
}

func newGolden(t *testing.T, fn string) *golden {
	if re != nil {
		return &golden{discard: true}
	}

	f, err := os.Create(filepath.FromSlash(fn))
	if err != nil { // Possibly R/O fs in a VM
		base := filepath.Base(filepath.FromSlash(fn))
		f, err = os.CreateTemp("", base)
		if err != nil {
			t.Fatal(err)
		}

		t.Logf("writing results to %s\n", f.Name())
	}

	return &golden{t: t, f: f}
}

func (g *golden) w(s string, args ...interface{}) {
	if g.discard {
		return
	}

	g.mu.Lock()

	defer g.mu.Unlock()

	if s = strings.TrimRight(s, " \t\n\r"); !strings.HasSuffix(s, "\n") {
		s += "\n"
	}
	g.a = append(g.a, fmt.Sprintf(s, args...))
}

func (g *golden) close() {
	if g.discard || g.f == nil {
		return
	}

	defer func() { g.f = nil }()

	sort.Strings(g.a)
	if _, err := g.f.WriteString(strings.Join(g.a, "")); err != nil {
		g.t.Fatal(err)
	}

	if err := g.f.Sync(); err != nil {
		g.t.Fatal(err)
	}

	if err := g.f.Close(); err != nil {
		g.t.Fatal(err)
	}
}

func isKnownBad(fn string, pos token.Position) bool {
	fs := token.NewFileSet()
	ast, err := goparser.ParseFile(fs, fn, nil, goparser.SkipObjectResolution|goparser.ParseComments)
	if err != nil {
		return true
	}

	for _, v := range ast.Comments {
		for _, w := range v.List {
			if strings.Contains(w.Text, "ERROR") && fs.PositionFor(w.Slash, true).Line == pos.Line {
				return true
			}
		}
	}

	return false
}

func TestParser(t *testing.T) {
	gld := newGolden(t, "testdata/test_parse.golden")

	defer gld.close()

	p := newParallel()
	t.Run("cd", func(t *testing.T) { testParser(p, t, ".", gld) })
	t.Run("goroot", func(t *testing.T) { testParser(p, t, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, skip %v, ok %v, fail %v", h(p.files), h(p.skipped), h(p.ok), h(p.fails))
	if p.fails != 0 {
		t.Logf("Shortest failing file: %s, %v tokens", p.minToksPath, p.minToks)
		return
	}

	t.Logf("Max backtrack: %s, %v tokens\n\t%v (%v:)", p.maxBacktrackPath, h(p.maxBacktrack), p.maxBacktrackPos, p.maxBacktrackOrigin)
	t.Logf("Max backtracks: %s, %v tokens", p.maxBacktracksPath, h(p.maxBacktracks))
	t.Logf("Max budget used: %s, %v for %v tokens", p.maxBudgetPath, h(p.maxBudget), h(p.maxBudgetToks))
	if *oReport {
		t.Logf("\n%s", p.a.report())
	}
}

func testParser(p *parallel, t *testing.T, root string, gld *golden) {
	if err := filepath.Walk(filepath.FromSlash(root), func(path0 string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		if re != nil && !re.MatchString(path0) {
			return nil
		}

		if filepath.Ext(path0) != ".go" {
			return nil
		}

		p.addFile()
		path := path0
		p.exec(func() (err error) {
			if *oTrc {
				fmt.Fprintln(os.Stderr, path)
			}

			var pp *parser

			defer func() {
				if err != nil {
					p.addFail()
					if pp != nil {
						p.recordMinToks(path, len(pp.toks))
					}
				}
				if pp != nil {
					from := pp.toks[pp.maxBackRange[0]].Position()
					to := pp.toks[pp.maxBackRange[1]].Position()
					p.recordMaxBacktrack(path, pp.maxBack, fmt.Sprintf("%v: - %v:", from, to), pp.maxBackOrigin)
					p.recordMaxBacktracks(path, pp.backs)
					p.recordMaxBudget(path, parserBudget-pp.budget, len(pp.toks))
					if *oReport {
						p.a.merge(pp.a)
					}
				}
			}()

			b, err := os.ReadFile(path)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			if pp, err = newParser(path, b, *oReport); err != nil {
				pp = nil
				p.addSkipped()
				return nil
			}

			if err := pp.parse(); err != nil {
				if isKnownBad(path, pp.errPosition()) {
					pp = nil
					p.addSkipped()
					return nil
				}

				return errorf("%s", err)
			}

			p.addOk()
			gld.w("%s\n", path)
			return nil
		})
		return nil
	}); err != nil {
		t.Error(err)
	}
}

func TestGoParser(t *testing.T) {
	gld := newGolden(t, "testdata/test_parse.go.golden")

	defer gld.close()

	p := newParallel()
	t.Run("cd", func(t *testing.T) { testGoParser(p, t, ".", gld) })
	t.Run("goroot", func(t *testing.T) { testGoParser(p, t, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, skip %v, ok %v, fail %v", h(p.files), h(p.skipped), h(p.ok), h(p.fails))
}

func testGoParser(p *parallel, t *testing.T, root string, gld *golden) {
	if err := filepath.Walk(filepath.FromSlash(root), func(path0 string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}

		if re != nil && !re.MatchString(path0) {
			return nil
		}

		if filepath.Ext(path0) != ".go" {
			return nil
		}

		p.addFile()
		path := path0
		p.exec(func() (err error) {
			if *oTrc {
				fmt.Fprintln(os.Stderr, path)
			}

			defer func() {
				if err != nil {
					p.addFail()
				}
			}()

			b, err := os.ReadFile(path)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			if _, err = goparser.ParseFile(token.NewFileSet(), path, b, goparser.SkipObjectResolution); err != nil {
				if pos, ok := extractPos(err.Error()); !ok || isKnownBad(path, pos) {
					p.addSkipped()
					return nil
				}

				return errorf("%s", err)
			}

			p.addOk()
			gld.w("%s\n", path)
			return nil
		})
		return nil
	}); err != nil {
		t.Error(err)
	}
}
