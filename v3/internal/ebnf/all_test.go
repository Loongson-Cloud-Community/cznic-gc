package main

import (
	"flag"
	"fmt"
	goparser "go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strings"
	"sync"
	"testing"
)

const (
	pegEBNF         = "peg.ebnf"
	startProduction = "SourceFile"
)

var (
	oRE     = flag.String("re", "", "")
	oTrc    = flag.Bool("trc", false, "")
	oTrcPEG = flag.Bool("trcpeg", false, "")

	re *regexp.Regexp
)

func TestMain(m *testing.M) {
	flag.BoolVar(&trcTODOs, "trctodo", false, "")
	flag.Parse()
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	os.Exit(m.Run())
}

func TestSpecEBNF(t *testing.T) {
	b, _, err := verifySpecEBNF(filepath.Join(runtime.GOROOT(), "doc", "go_spec.html"), startProduction, nil)
	if err != nil {
		t.Fatal(err)
	}

	g, err := newGrammar("spec", startProduction, b)
	if err != nil {
		t.Fatal(err)
	}

	for k := range g.leftRecursive {
		t.Logf("left recursive: %v", k)
	}
}

func loadPEG(fn string) (*grammar, error) {
	b, err := os.ReadFile(fn)
	if err != nil {
		return nil, err
	}

	return newGrammar(fn, startProduction, b)
}

func TestPEGEBNF(t *testing.T) {
	testGrammar(t, pegEBNF)
}

func testGrammar(t *testing.T, fn string) {
	peg, err := loadPEG(fn)
	if err != nil {
		t.Fatal(err)
	}

	var a []string
	for nm, p := range peg.g {
		var b []string
		for k := range peg.exprClosure(p.Expr) {
			b = append(b, tokSource(k))
		}
		sort.Strings(b)
		a = append(a, fmt.Sprintf("%s case %v:", nm, strings.Join(b, ", ")))
	}
	sort.Strings(a)
	if err := os.WriteFile(fn+".fs", []byte(strings.Join(a, "\n")), 0660); err != nil {
		t.Fatal(err)
	}

	for k := range peg.leftRecursive {
		t.Errorf("left recursive: %v", k)
	}
}

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

func TestEBNFParser(t *testing.T) {
	peg, err := loadPEG(pegEBNF)
	if err != nil {
		t.Fatal(err)
	}

	gld := newGolden(t, "testdata/test_parse.ebnf.golden")

	defer gld.close()

	p := newParallel()
	t.Run("cd", func(t *testing.T) { testEBNFParser(p, t, peg, ".", gld) })
	t.Run("goroot", func(t *testing.T) { testEBNFParser(p, t, peg, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, skip %v, ok %v, fail %v", h(p.files), h(p.skipped), h(p.ok), h(p.fails))
}

func testEBNFParser(p *parallel, t *testing.T, g *grammar, root string, gld *golden) {
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
		switch s := filepath.ToSlash(path0); {
		case
			strings.HasSuffix(s, "test/fixedbugs/issue29264.go"),
			strings.HasSuffix(s, "test/fixedbugs/issue29312.go"):

			p.addSkipped()
			return nil
		}
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

			pp, err := newEBNFParser(g, path, b, *oTrcPEG)
			if err != nil {
				p.addSkipped()
				return nil
			}

			if err := pp.parse(startProduction); err != nil {
				if isKnownBad(path, pp.errPosition()) {
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
	return //TODO-
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
		t.Logf("Shortest failing file: %s, %v tokens", p.minPath, p.minToks)
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
						p.min(path, len(pp.toks))
					}
				}
			}()

			b, err := os.ReadFile(path)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			if pp, err = newParser(path, b); err != nil {
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
