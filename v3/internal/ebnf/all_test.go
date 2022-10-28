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
	spec, err := verifySpecEBNF(filepath.Join(runtime.GOROOT(), "doc", "go_spec.html"), startProduction, nil)
	if err != nil {
		t.Fatal(err)
	}

	for _, v := range leftRecursive(spec, startProduction) {
		var a []string
		for _, w := range v {
			a = append(a, w.Name.String)
		}
		t.Logf("left recursive: %v", a)
	}
}

func loadPEG() (*grammar, error) {
	const fn = "peg.ebnf"
	b, err := os.ReadFile(fn)
	if err != nil {
		return nil, err
	}

	return newGrammar(fn, startProduction, b)
}

func TestPEGEBNF(t *testing.T) {
	peg, err := loadPEG()
	if err != nil {
		t.Fatal(err)
	}

	var a []string
	for nm, p := range peg.ebnf {
		var b []string
		for k := range peg.followSets[p] {
			switch k {
			case epsilon:
				b = append(b, "ε")
			default:
				b = append(b, fmt.Sprint(k))
			}
		}
		sort.Strings(b)
		a = append(a, fmt.Sprintf("%s %q", nm, b))
	}
	sort.Strings(a)
	if err := os.WriteFile("closures", []byte(strings.Join(a, "\n")), 0660); err != nil {
		t.Fatal(err)
	}

	for _, v := range leftRecursive(peg.ebnf, startProduction) {
		var a []string
		for _, w := range v {
			a = append(a, w.Name.String)
		}
		t.Errorf("left recursive: %v", a)
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

func TestPEG(t *testing.T) {
	peg, err := loadPEG()
	if err != nil {
		t.Fatal(err)
	}

	gld := newGolden(t, fmt.Sprintf("testdata/test_parse.golden"))

	defer gld.close()

	p := newParallel()
	t.Run("cd", func(t *testing.T) { testPEG(p, t, peg, ".", gld) })
	t.Run("goroot", func(t *testing.T) { testPEG(p, t, peg, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, skip %v, ok %v, fail %v", h(p.files), h(p.skipped), h(p.ok), h(p.fails))
}

func testPEG(p *parallel, t *testing.T, g *grammar, root string, gld *golden) {
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

		path := path0
		p.exec(func() (err error) {
			if *oTrc {
				fmt.Fprintln(os.Stderr, path)
			}
			p.addFile()

			defer func() {
				if err != nil {
					p.addFail()
				}
			}()

			b, err := os.ReadFile(path)
			if err != nil {
				return errorf("%s: %v", path, err)
			}

			pp, err := newParser(g, path, b, *oTrcPEG)
			if err != nil {
				return errorf("%s: %v", path, err)
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
