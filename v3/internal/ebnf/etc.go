package main

import (
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/dustin/go-humanize"
	"golang.org/x/exp/ebnf"
)

var (
	trcTODOs       bool
	extendedErrors = true
)

// origin returns caller's short position, skipping skip frames.
func origin(skip int) string {
	pc, fn, fl, _ := runtime.Caller(skip)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
		if strings.HasPrefix(fns, "func") {
			num := true
			for _, c := range fns[len("func"):] {
				if c < '0' || c > '9' {
					num = false
					break
				}
			}
			if num {
				return origin(skip + 2)
			}
		}
	}
	return fmt.Sprintf("%s:%d:%s", filepath.Base(fn), fl, fns)
}

// todo prints and returns caller's position and an optional message tagged with TODO. Output goes to stderr.
func todo(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	r := fmt.Sprintf("%s\n\tTODO %s", origin(2), s)
	// fmt.Fprintf(os.Stderr, "%s\n", r)
	// os.Stdout.Sync()
	return r
}

// trc prints and returns caller's position and an optional message tagged with TRC. Output goes to stderr.
func trc(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	r := fmt.Sprintf("%s: TRC %s", origin(2), s)
	fmt.Fprintf(os.Stderr, "%s\n", r)
	os.Stderr.Sync()
	return r
}

func printEBNF(w io.Writer, g ebnf.Grammar) {
	var a []string
	for k := range g {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		p := g[k]
		fmt.Fprintf(w, "%s = ", p.Name.String)
		if p.Expr != nil {
			printEBNFExpression(w, p.Expr)
		}
		fmt.Fprintf(w, " .\n")
	}
}

func printEBNFExpression(w io.Writer, e ebnf.Expression) {
	switch x := e.(type) {
	case ebnf.Sequence:
		for i, v := range x {
			if i != 0 {
				fmt.Fprintf(w, " ")
			}
			printEBNFExpression(w, v)
		}
	case *ebnf.Name:
		fmt.Fprintf(w, "%s", x.String)
	case *ebnf.Token:
		s := x.String
		s = strings.ReplaceAll(s, "&lt;", "<")
		s = strings.ReplaceAll(s, "&gt;", ">")
		s = strings.ReplaceAll(s, "&amp;", "&")
		fmt.Fprintf(w, "%q", s)
	case *ebnf.Option:
		fmt.Fprintf(w, "[ ")
		printEBNFExpression(w, x.Body)
		fmt.Fprintf(w, " ]")
	case *ebnf.Group:
		fmt.Fprintf(w, "( ")
		printEBNFExpression(w, x.Body)
		fmt.Fprintf(w, " )")
	case ebnf.Alternative:
		for i, v := range x {
			if i != 0 {
				fmt.Fprintf(w, " | ")
			}
			printEBNFExpression(w, v)
		}
	case *ebnf.Repetition:
		fmt.Fprintf(w, "{ ")
		printEBNFExpression(w, x.Body)
		fmt.Fprintf(w, " }")
	case *ebnf.Range:
		printEBNFExpression(w, x.Begin)
		fmt.Fprintf(w, " ... ")
		printEBNFExpression(w, x.End)
	case nil:
		// ok
	default:
		panic(todo("%T", x))
	}
}

func ebnfEpressionString(e ebnf.Expression) string {
	var b strings.Builder
	printEBNFExpression(&b, e)
	return b.String()
}

func leftRecursive(g ebnf.Grammar, start string) (r [][]*ebnf.Production) {
	p := g[start]
	m := map[*ebnf.Production]int{p: 1}
	detected := map[*ebnf.Production]struct{}{}

	var f func(ebnf.Expression, int, []*ebnf.Production) int
	f = func(e ebnf.Expression, pos int, stack []*ebnf.Production) (npos int) {
		switch x := e.(type) {
		case ebnf.Sequence:
			for _, v := range x {
				pos = f(v, pos, stack)
			}
			return pos
		case *ebnf.Name:
			nm := x.String
			if first := nm[0]; first >= 'a' && first <= 'z' {
				return pos + 1
			}

			p := g[nm]
			if _, ok := detected[p]; ok {
				return pos
			}

			sv := m[p]
			defer func() { m[p] = sv }()

			if sv == pos {
				detected[p] = struct{}{}
				for sp := len(stack) - 1; ; sp-- {
					if stack[sp] == p {
						r = append(r, append([]*ebnf.Production(nil), stack[sp:]...))
						return pos
					}
				}
				panic(todo(""))
			}

			if sv != 0 {
				return pos
			}

			m[p] = pos
			return f(p.Expr, pos, append(stack, p))
		case *ebnf.Token:
			return pos + 1
		case ebnf.Alternative:
			moved := true
			for _, v := range x {
				if f(v, pos, stack) == pos {
					moved = false
				}
			}
			if moved {
				pos++
			}
			return pos
		case *ebnf.Repetition:
			f(x.Body, pos, stack)
			return pos
		case *ebnf.Group:
			return f(x.Body, pos, stack)
		case *ebnf.Option:
			f(x.Body, pos, stack)
			return pos
		case nil:
			return pos + 1
		default:
			panic(todo("%T", x))
		}
		panic(todo(""))
	}

	f(p.Expr, 1, []*ebnf.Production{p})
	return r
}

// errorf constructs an error value. If ExtendedErrors is true, the error will
// contain its origin.
func errorf(s string, args ...interface{}) error {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	if trcTODOs && strings.HasPrefix(s, "TODO") {
		fmt.Fprintf(os.Stderr, "%s (%v)\n", s, origin(2))
		os.Stderr.Sync()
	}
	switch {
	case extendedErrors:
		return fmt.Errorf("%s (%v: %v: %v)", s, origin(4), origin(3), origin(2))
	default:
		return fmt.Errorf("%s", s)
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

type parallel struct {
	errors []error
	limit  chan struct{}
	sync.Mutex
	wg sync.WaitGroup

	fails   int32
	files   int32
	ok      int32
	skipped int32
}

func newParallel() *parallel {
	return &parallel{
		limit: make(chan struct{}, runtime.GOMAXPROCS(0)),
	}
}

func (p *parallel) AddSkipped() { atomic.AddInt32(&p.skipped, 1) }
func (p *parallel) addFail()    { atomic.AddInt32(&p.fails, 1) }
func (p *parallel) addFile()    { atomic.AddInt32(&p.files, 1) }
func (p *parallel) addOk()      { atomic.AddInt32(&p.ok, 1) }

func (p *parallel) err(err error) {
	if err == nil {
		return
	}

	s := err.Error()
	if x := strings.Index(s, "TODO"); x >= 0 {
		fmt.Println(s[x:])
	}
	p.Lock()
	p.errors = append(p.errors, err)
	p.Unlock()
}

func (p *parallel) exec(run func() error) {
	p.limit <- struct{}{}
	p.wg.Add(1)

	go func() {
		defer func() {
			p.wg.Done()
			<-p.limit
		}()

		p.err(run())
	}()
}

func (p *parallel) wait() error {
	p.wg.Wait()
	if len(p.errors) == 0 {
		return nil
	}

	var a []string
	for _, v := range p.errors {
		a = append(a, v.Error())
	}
	return fmt.Errorf("%s", strings.Join(a, "\n"))
}
