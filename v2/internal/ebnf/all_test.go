package main

import (
	"bytes"
	"flag"
	"fmt"
	goparser "go/parser"
	"go/token"
	"io/ioutil"
	"math"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strings"
	"sync"
	"testing"

	"github.com/dustin/go-humanize"
	"modernc.org/ebnf"
	"modernc.org/ebnfutil"
	"modernc.org/gc/v2"
)

const (
	goEBNF          = "go.ebnf"
	startProduction = "SourceFile"
	body            = gc.XOR_ASSIGN + 1
)

var (
	oClosures         = flag.Bool("cls", false, "")
	oFailNow          = flag.Bool("failnow", false, "")
	oRE               = flag.String("re", "", "")
	oTrc              = flag.Bool("trc", false, "")
	oTrcAccept        = flag.Bool("trca", false, "")
	oTrcEnter         = flag.Bool("trce", false, "")
	oTrcGoParserFails = flag.Bool("trcg", false, "")
	oTrcReject        = flag.Bool("trcr", false, "")

	re *regexp.Regexp
)

func TestMain(m *testing.M) {
	flag.Parse()
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	os.Exit(m.Run())
}

type golden struct {
	a  []string
	f  *os.File
	mu sync.Mutex
	t  *testing.T

	discard bool
}

func newGolden(t *testing.T, fn string) *golden {
	if re != nil || *oFailNow {
		return &golden{discard: true}
	}

	f, err := os.Create(filepath.FromSlash(fn))
	if err != nil { // Possibly R/O fs in a VM
		base := filepath.Base(filepath.FromSlash(fn))
		f, err = ioutil.TempFile("", base)
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

func loadGrammar(fn, out, start string) (ebnf.Grammar, map[string]followset, error) {
	f, err := os.Open(fn)
	if err != nil {
		return nil, nil, err
	}

	defer f.Close()

	g, err := ebnfutil.Parse(fn, f)
	if err != nil {
		return nil, nil, err
	}

	if err := g.Verify(start); err != nil {
		return nil, nil, err
	}

	g = g.Normalize()
	fs := closures(g)
	var reps map[string]bool
	if out != "" {
		b := bytes.NewBuffer(nil)
		var a []string
		for k := range reps {
			a = append(a, k)
		}
		sort.Strings(a)
		if len(a) != 0 {
			b.WriteString("// Repetitions\n// ")
			b.WriteString(strings.Join(a, "\n// "))
			b.WriteString("\n\n")
		}
		b.WriteString(g.String())
		a = a[:0]
		b.WriteString("\n\n// Follow sets\n//\n")
		ml := 0
		for k := range fs {
			if len(k) > ml {
				ml = len(k)
			}
			a = append(a, k)
		}
		sort.Strings(a)
		var a2 []string
		for _, k := range a {
			a2 = a2[:0]
			for k2 := range fs[k] {
				a2 = append(a2, xlat3[k2])
			}
			sort.Strings(a2)
			fmt.Fprintf(b, "// %*s case %s:\n", ml+1, k, strings.Join(a2, ", "))
		}
		b.WriteString("\n")
		if err := os.WriteFile(out, b.Bytes(), 0640); err != nil {
			return nil, nil, err
		}

		g, _, err := g.BNF(startProduction, nil)
		if err != nil {
			return nil, nil, err
		}

		b.Reset()
		b.WriteString(g.String())
		b.WriteString("\n\n// Follow sets\n//\n")
		for _, k := range a {
			a2 = a2[:0]
			for k2 := range fs[k] {
				a2 = append(a2, xlat3[k2])
			}
			sort.Strings(a2)
			fmt.Fprintf(b, "// %*s case %s:\n", ml+1, k, strings.Join(a2, ", "))
		}
		b.WriteString("\n")
		if err := os.WriteFile("go.bnf", b.Bytes(), 0640); err != nil {
			return nil, nil, err
		}
	}

	return ebnf.Grammar(g), fs, nil
}

type followset map[string]struct{}

func closures(g ebnfutil.Grammar) (ret map[string]followset) {
	ret = map[string]followset{}
	var f func(ebnf.Expression) followset
	f = func(expr ebnf.Expression) (r followset) {
		switch x := expr.(type) {
		case *ebnf.Production:
			nm := x.Name.String
			if r := ret[nm]; r != nil {
				return r
			}

			r = followset{}
			// trc("%T %v", x, nm)
			ret[nm] = r
			for k, v := range f(x.Expr) {
				r[k] = v
			}
		case ebnf.Sequence:
			// trc("%T", x)
			r = followset{}
			for _, v := range x {
				s := f(v)
				for k, v := range s {
					r[k] = v
				}
				if _, ok := s[""]; !ok {
					delete(r, "")
					break
				}
			}
		case ebnf.Alternative:
			r = followset{}
			for _, v := range x {
				for k, v := range f(v) {
					r[k] = v
				}
			}
		case *ebnf.Group:
			return f(x.Body)
		case *ebnf.Option:
			r = followset{}
			for k, v := range f(x.Body) {
				r[k] = v
			}
			r[""] = struct{}{}
		case *ebnf.Repetition:
			r = followset{}
			for k, v := range f(x.Body) {
				r[k] = v
			}
			r[""] = struct{}{}
		case *ebnf.Name:
			// trc("%T %v", x, x.String)
			return f(g[x.String])
		case *ebnf.Token:
			// trc("%T %v", x, x.String)
			return followset{x.String: struct{}{}}
		case nil:
			return followset{"": struct{}{}}
		default:
			panic(todo("%T", x))
		}
		return r
	}
	for k := range g {
		f(g[k])
	}
	return ret
}

func Test0(t *testing.T) {
	if _, _, err := loadGrammar(goEBNF, "normalized."+goEBNF, startProduction); err != nil {
		t.Fatal(err)
	}
}

func h(v interface{}) string {
	switch x := v.(type) {
	case int:
		return humanize.Comma(int64(x))
	case int32:
		return humanize.Comma(int64(x))
	case int64:
		return humanize.Comma(x)
	case uint32:
		return humanize.Comma(int64(x))
	case uint64:
		if x <= math.MaxInt64 {
			return humanize.Comma(int64(x))
		}
	}
	return fmt.Sprint(v)
}

func TestParse(t *testing.T) {
	gld := newGolden(t, fmt.Sprintf("testdata/test_parse.golden"))

	defer gld.close()

	g, fs, err := loadGrammar(goEBNF, "", startProduction)
	if err != nil {
		t.Fatal(err)
	}

	if !*oClosures {
		fs = nil
	}
	p := newParallel()
	t.Run("cd", func(t *testing.T) { testParser(p, t, g, fs, ".", gld) })
	t.Run("goroot", func(t *testing.T) { testParser(p, t, g, fs, runtime.GOROOT(), gld) })
	if err := p.wait(); err != nil {
		t.Error(err)
	}
	t.Logf("TOTAL files %v, tokens %v, bytes %v, skip %v, ok %v, fails %v", h(p.files), h(p.nTokens), h(p.nBytes), h(p.skips), h(p.oks), h(p.fails))
}

func testParser(p *parallel, t *testing.T, g ebnf.Grammar, fs map[string]followset, root string, gld *golden) {
	if err := filepath.Walk(root, func(path0 string, info os.FileInfo, err error) error {
		if *oFailNow && p.nfail() {
			return nil
		}

		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		switch {
		case re == nil:
			sp := filepath.ToSlash(path0)
			switch {
			case
				strings.Contains(sp, "test/fixedbugs/issue29264.go"), //TODO
				strings.Contains(sp, "test/fixedbugs/issue29312.go"): //TODO

				return nil
			}
		default:
			if !re.MatchString(path0) {
				return nil
			}
		}

		if filepath.Ext(path0) != ".go" {
			return nil
		}

		path := path0
		p.file()
		p.exec(func() error {
			b, err := os.ReadFile(path)
			p.bytes(len(b))
			if err != nil {
				p.fail()
				trc("FAIL %v: %v", path, err)
				return err
			}

			if *oTrc {
				trc("%v: parse %v", p.nfile(), path)
			}
			pp, err := newParser(g, fs, path, b)
			p.tokens(len(pp.in))
			if err != nil {
				p.fail()
				trc("FAIL %v: %v", path, err)
				return err
			}

			if err := pp.parse(g[startProduction].Expr); err != nil {
				if parserFails(path, b) {
					p.skip()
					return nil
				}

				p.fail()
				trc("FAIL %v: %v", path, err)
				return err
			}

			p.ok()
			gld.w("%s\n", path)
			return nil

		})
		return nil
	}); err != nil {
		t.Error(err)
	}
}

func parserFails(fn string, src []byte) bool {
	sp := filepath.ToSlash(fn)
	switch {
	case
		strings.Contains(sp, "src/cmd/compile/internal/types2/testdata/check/decls0.go"),
		strings.Contains(sp, "src/cmd/compile/internal/types2/testdata/check/expr0.go"),
		strings.Contains(sp, "src/go/types/testdata/check/decls0.go"),
		strings.Contains(sp, "src/go/types/testdata/check/expr0.go"),
		strings.Contains(sp, "test/fixedbugs/bug002.go"),
		strings.Contains(sp, "test/fixedbugs/bug003.go"),
		strings.Contains(sp, "test/fixedbugs/bug004.go"),
		strings.Contains(sp, "test/fixedbugs/bug280.go"),
		strings.Contains(sp, "test/fixedbugs/bug287.go"),
		strings.Contains(sp, "test/fixedbugs/bug299.go"),
		strings.Contains(sp, "test/fixedbugs/issue15055.go"),
		strings.Contains(sp, "test/fixedbugs/issue24470.go"),
		strings.Contains(sp, "test/fixedbugs/issue4470.go"):

		return true
	}
	_, err := goparser.ParseFile(token.NewFileSet(), fn, src, 0)
	if *oTrcGoParserFails {
		trc("", fn, err)
	}
	return err != nil
}

type state struct {
	index     int
	loophacks []bool

	loophack bool
}

type parser struct {
	fs    map[string]map[gc.Ch]struct{}
	g     ebnf.Grammar
	in    []gc.Token
	start string
	state

	accepts    int
	enters     int
	maxIndex   int
	rejects    int
	tokenTests int

	hasFS bool
}

func newParser(g ebnf.Grammar, fs0 map[string]followset, fn string, b []byte) (*parser, error) {
	fs := map[string]map[gc.Ch]struct{}{}
	for k, v := range fs0 {
		x := map[gc.Ch]struct{}{}
		for k2 := range v {
			x[xlat2[k2]] = struct{}{}
		}
		fs[k] = x
	}
	s, err := gc.NewScanner(fn, b)
	if err != nil {
		return nil, err
	}

	var in []gc.Token
	for s.Scan() {
		in = append(in, s.Tok)
	}
	if s.Tok.Ch != gc.EOF {
		return nil, fmt.Errorf("internal error")
	}

	in = append(in, s.Tok)
	return &parser{g: g, fs: fs, in: in, hasFS: len(fs0) != 0}, nil
}

func (p *parser) parse(expr ebnf.Expression) error {
	if !p.run(expr) {
		return fmt.Errorf("%v (%v), enters %v, token tests %v, maxIndex %v", p.in[p.index], p.in[p.maxIndex], p.enters, p.tokenTests, p.maxIndex)
	}

	return nil
}

var (
	xlat1 = map[gc.Ch]string{
		-1:   "",
		body: "BODY",

		'!': "!",
		'%': "%",
		'&': "&",
		'(': "(",
		')': ")",
		'*': "*",
		'+': "+",
		',': ",",
		'-': "-",
		'.': ".",
		'/': "/",
		':': ":",
		';': ";",
		'<': "<",
		'=': "=",
		'>': ">",
		'[': "[",
		']': "]",
		'^': "^",
		'{': "{",
		'|': "|",
		'}': "}",
		'~': "~",

		gc.ADD_ASSIGN:     "+=",
		gc.AND_ASSIGN:     "&=",
		gc.AND_NOT:        "&^",
		gc.AND_NOT_ASSIGN: "&^=",
		gc.ARROW:          "<-",
		gc.BREAK:          "break",
		gc.CASE:           "case",
		gc.CHAN:           "chan",
		gc.CONST:          "const",
		gc.CONTINUE:       "continue",
		gc.DEC:            "dec",
		gc.DEFAULT:        "default",
		gc.DEFER:          "defer",
		gc.DEFINE:         ":=",
		gc.ELLIPSIS:       "...",
		gc.ELSE:           "else",
		gc.EOF:            "EOF",
		gc.EQ:             "==",
		gc.FALLTHROUGH:    "fallthrough",
		gc.FLOAT_LIT:      "FLOAT",
		gc.FOR:            "for",
		gc.FUNC:           "func",
		gc.GE:             ">=",
		gc.GO:             "go",
		gc.GOTO:           "goto",
		gc.IDENTIFIER:     "IDENT",
		gc.IF:             "if",
		gc.IMAG_LIT:       "IMAG",
		gc.IMPORT:         "import",
		gc.INC:            "++",
		gc.INTERFACE:      "interface",
		gc.INT_LIT:        "INT",
		gc.LAND:           "&&",
		gc.LE:             "<=",
		gc.LOR:            "||",
		gc.MAP:            "map",
		gc.MUL_ASSIGN:     "*=",
		gc.NE:             "!=",
		gc.OR_ASSIGN:      "|=",
		gc.PACKAGE:        "package",
		gc.QUO_ASSIGN:     "/=",
		gc.RANGE:          "range",
		gc.REM_ASSIGN:     "%=",
		gc.RETURN:         "return",
		gc.RUNE_LIT:       "CHAR",
		gc.SELECT:         "select",
		gc.SHL:            "<<",
		gc.SHL_ASSIGN:     "<<=",
		gc.SHR:            ">>",
		gc.SHR_ASSIGN:     ">>=",
		gc.STRING_LIT:     "STRING",
		gc.STRUCT:         "struct",
		gc.SUB_ASSIGN:     "-=",
		gc.SWITCH:         "switch",
		gc.TYPE:           "type",
		gc.VAR:            "var",
		gc.XOR_ASSIGN:     "^=",
	}

	xlat2 = map[string]gc.Ch{}

	xlat3 = map[string]string{
		"":     "-1",
		"BODY": "body",

		"!": "'!'",
		"%": "'%'",
		"&": "'&'",
		"(": "'('",
		")": "')'",
		"*": "'*'",
		"+": "'+'",
		",": "','",
		"-": "'-'",
		".": "'.'",
		"/": "'/'",
		":": "':'",
		";": "';'",
		"<": "'<'",
		"=": "'='",
		">": "'>'",
		"[": "'['",
		"]": "']'",
		"^": "'^'",
		"{": "'{'",
		"|": "'|'",
		"}": "'}'",
		"~": "'~'",

		"+=":          "ADD_ASSIGN",
		"&=":          "AND_ASSIGN",
		"&^":          "AND_NOT",
		"&^=":         "AND_NOT_ASSIGN",
		"<-":          "ARROW",
		"break":       "BREAK",
		"case":        "CASE",
		"chan":        "CHAN",
		"const":       "CONST",
		"continue":    "CONTINUE",
		"dec":         "DEC",
		"default":     "DEFAULT",
		"defer":       "DEFER",
		":=":          "DEFINE",
		"...":         "ELLIPSIS",
		"else":        "ELSE",
		"EOF":         "EOF",
		"==":          "EQ",
		"fallthrough": "FALLTHROUGH",
		"FLOAT":       "FLOAT_LIT",
		"for":         "FOR",
		"func":        "FUNC",
		">=":          "GE",
		"go":          "GO",
		"goto":        "GOTO",
		"IDENT":       "IDENTIFIER",
		"if":          "IF",
		"IMAG":        "IMAG_LIT",
		"import":      "IMPORT",
		"++":          "INC",
		"interface":   "INTERFACE",
		"INT":         "INT_LIT",
		"&&":          "LAND",
		"<=":          "LE",
		"||":          "LOR",
		"map":         "MAP",
		"*=":          "MUL_ASSIGN",
		"!=":          "NE",
		"|=":          "OR_ASSIGN",
		"package":     "PACKAGE",
		"/=":          "QUO_ASSIGN",
		"range":       "RANGE",
		"%=":          "REM_ASSIGN",
		"return":      "RETURN",
		"CHAR":        "RUNE_LIT",
		"select":      "SELECT",
		"<<":          "SHL",
		"<<=":         "SHL_ASSIGN",
		">>":          "SHR",
		">>=":         "SHR_ASSIGN",
		"STRING":      "STRING_LIT",
		"struct":      "STRUCT",
		"-=":          "SUB_ASSIGN",
		"switch":      "SWITCH",
		"type":        "TYPE",
		"var":         "VAR",
		"^=":          "XOR_ASSIGN",
	}
)

func init() {
	for k, v := range xlat1 {
		xlat2[v] = k
	}
}

func (p *parser) run(expr ebnf.Expression) (r bool) {
	state := p.state
	ix := p.index
	if ix >= len(p.in) {
		return false
	}

	p.enters++
	if *oTrcEnter {
		trc("(%v) ENTER(%d) `%v`, next %v:", p.in[ix], p.enters, dump(expr), pos(p.in[p.index]))
	}
	if ix > p.maxIndex {
		p.maxIndex = ix
	}

	var fs map[gc.Ch]struct{}
	var nm string
	defer func() {
		next := p.index
		if next >= len(p.in) {
			next = len(p.in) - 1
		}
		if !r {
			p.rejects++
			if *oTrcReject {
				trc("(%v) REJECT(%d) `%v`, next %v:", p.in[ix], p.rejects, dump(expr), pos(p.in[next]))
				if fs != nil {
					var a []string
					for k := range fs {
						switch {
						case k < 128:
							a = append(a, string(rune(k)))
						default:
							a = append(a, k.String())
						}
					}
					sort.Strings(a)
					trc("%s FS %q", nm, a)
				}
			}
			p.state = state
		} else {
			p.accepts++
			if *oTrcAccept {
				trc("(%v) ACCEPT(%d) `%v`, next %v:", p.in[ix], p.accepts, dump(expr), pos(p.in[next]))
			}
		}
	}()

	// trc("%v: running `%v`", p.in[ix], dump(expr))
	switch x := expr.(type) {
	case ebnf.Sequence:
		for _, v := range x {
			if !p.run(v) {
				return false
			}
		}
	case *ebnf.Name:
		nm = x.String
		if p.hasFS {
			ch := p.in[ix].Ch
			if ch == '{' && p.loophack {
				ch = body
			}
			fs = p.fs[nm]
			_, ok := fs[ch]
			if !ok {
				if _, ok := fs[-1]; !ok {
					return false
				}
			}
		}
		// trc("%q -> %T", x.String, p.g[x.String].Expr)
		if nm == "lbrace" && p.in[ix].Ch == '{' {
			p.loophacks = append(p.loophacks, p.loophack)
		}
		return p.run(p.g[nm].Expr)
	case *ebnf.Token:
		p.tokenTests++
		s := x.String
		ch := p.in[ix].Ch
		switch ch {
		case gc.FOR, gc.IF, gc.SELECT, gc.SWITCH:
			p.loophack = true
			// trc("%v: p.loophack = %v", p.in[ix], p.loophack)
		case '(', '[':
			if p.loophack || len(p.loophacks) != 0 {
				p.loophacks = append(p.loophacks, p.loophack)
				p.loophack = false
				// trc("%v: p.loophack = %v (PUSH)", p.in[ix], p.loophack)
			}
		case ')', ']':
			if n := len(p.loophacks); n != 0 {
				p.loophack = p.loophacks[n-1]
				// trc("%v: p.loophack = %v (POP)", p.in[ix], p.loophack)
				p.loophacks = p.loophacks[:n-1]
			}
		case '{':
			if p.loophack {
				ch = body
				p.loophack = false
				// trc("%v: p.loophack = %v (BODY)", p.in[ix], p.loophack)
			}
		}
		switch c := x.String[0]; {
		case c >= 'a' && c <= 'z':
			r = s == p.in[ix].Src()
		case c >= 'A' && c <= 'Z':
			switch s {
			case "IDENT":
				r = ch == gc.IDENTIFIER
			case "STRING":
				r = ch == gc.STRING_LIT
			case "INT":
				r = ch == gc.INT_LIT
			case "FLOAT":
				r = ch == gc.FLOAT_LIT
			case "IMAG":
				r = ch == gc.IMAG_LIT
			case "CHAR":
				r = ch == gc.RUNE_LIT
			case "BODY":
				r = ch == body
				// trc("%v: BODY? %v", p.in[ix], r)
			case "EOF":
				r = ch == gc.EOF
			default:
				panic(todo("%q", s))
			}
		case len(s) == 1:
			r = ch == gc.Ch(s[0])
		default:
			switch s {
			case "...":
				r = ch == gc.ELLIPSIS
			case "<-":
				r = ch == gc.ARROW
			case ":=":
				r = ch == gc.DEFINE
			case "!=":
				r = ch == gc.NE
			case "<=":
				r = ch == gc.LE
			case "==":
				r = ch == gc.EQ
			case ">=":
				r = ch == gc.GE
			case "++":
				r = ch == gc.INC
			case "--":
				r = ch == gc.DEC
			case "&&":
				r = ch == gc.LAND
			case "<<":
				r = ch == gc.SHL
			case ">>":
				r = ch == gc.SHR
			case "&^":
				r = ch == gc.AND_NOT
			case "||":
				r = ch == gc.LOR
			case "+=":
				r = ch == gc.ADD_ASSIGN
			case "%=":
				r = ch == gc.REM_ASSIGN
			case "&=":
				r = ch == gc.AND_ASSIGN
			case "&^=":
				r = ch == gc.AND_NOT_ASSIGN
			case "*=":
				r = ch == gc.MUL_ASSIGN
			case "-=":
				r = ch == gc.SUB_ASSIGN
			case "/=":
				r = ch == gc.QUO_ASSIGN
			case "<<=":
				r = ch == gc.SHL_ASSIGN
			case ">>=":
				r = ch == gc.SHR_ASSIGN
			case "^=":
				r = ch == gc.XOR_ASSIGN
			case "|=":
				r = ch == gc.OR_ASSIGN
			case "#fixlbr":
				if n := len(p.loophacks); n != 0 {
					p.loophack = p.loophacks[n-1]
					p.loophacks = p.loophacks[:n-1]
				}
				return true
			default:
				panic(todo("%q", s))
			}
		}
		if !r {
			return false
		}

		p.index++
	case *ebnf.Repetition:
		for p.run(x.Body) {
		}
	case ebnf.Alternative:
		for _, v := range x {
			if p.run(v) {
				return true
			}
		}
		return false
	case *ebnf.Option:
		p.run(x.Body)
	case *ebnf.Group:
		return p.run(x.Body)
	case nil:
		// ok (EmptyStmt)
	default:
		panic(todo("%T %v", x, p.in[ix]))
	}
	return true
}

func dump(expr ebnf.Expression) string {
	var a []string
	switch x := expr.(type) {
	case ebnf.Sequence:
		for _, v := range x {
			a = append(a, dump(v))
		}
		return strings.Join(a, " ")
	case *ebnf.Name:
		return x.String
	case *ebnf.Token:
		return fmt.Sprintf("%q", x.String)
	case *ebnf.Repetition:
		return fmt.Sprintf("{ %s }", dump(x.Body))
	case *ebnf.Group:
		return fmt.Sprintf("( %s )", dump(x.Body))
	case ebnf.Alternative:
		for _, v := range x {
			a = append(a, dump(v))
		}
		return strings.Join(a, " | ")
	case *ebnf.Option:
		return fmt.Sprintf("[ %s ]", dump(x.Body))
	case nil:
		return "<nil>"
	default:
		panic(todo("%T", x))
	}
}

func pos(n gc.Node) (r token.Position) {
	r = n.Position()
	if r.IsValid() {
		r.Filename = filepath.Base(r.Filename)
	}
	return r
}
