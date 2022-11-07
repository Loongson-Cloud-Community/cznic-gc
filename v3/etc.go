// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // modernc.org/gc/v3

import (
	"fmt"
	"go/token"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/dustin/go-humanize"
)

// The list of tokens.
const (
	// Special tokens
	ILLEGAL = token.ILLEGAL
	EOF     = token.EOF
	COMMENT = token.COMMENT

	// Identifiers and basic type literals
	// (these tokens stand for classes of literals)
	IDENT  = token.IDENT  // main
	INT    = token.INT    // 12345
	FLOAT  = token.FLOAT  // 123.45
	IMAG   = token.IMAG   // 123.45i
	CHAR   = token.CHAR   // 'a'
	STRING = token.STRING // "abc"

	// Operators and delimiters
	ADD = token.ADD // +
	SUB = token.SUB // -
	MUL = token.MUL // *
	QUO = token.QUO // /
	REM = token.REM // %

	AND     = token.AND     // &
	OR      = token.OR      // |
	XOR     = token.XOR     // ^
	SHL     = token.SHL     // <<
	SHR     = token.SHR     // >>
	AND_NOT = token.AND_NOT // &^

	ADD_ASSIGN = token.ADD_ASSIGN // +=
	SUB_ASSIGN = token.SUB_ASSIGN // -=
	MUL_ASSIGN = token.MUL_ASSIGN // *=
	QUO_ASSIGN = token.QUO_ASSIGN // /=
	REM_ASSIGN = token.REM_ASSIGN // %=

	AND_ASSIGN     = token.AND_ASSIGN     // &=
	OR_ASSIGN      = token.OR_ASSIGN      // |=
	XOR_ASSIGN     = token.XOR_ASSIGN     // ^=
	SHL_ASSIGN     = token.SHL_ASSIGN     // <<=
	SHR_ASSIGN     = token.SHR_ASSIGN     // >>=
	AND_NOT_ASSIGN = token.AND_NOT_ASSIGN // &^=

	LAND  = token.LAND  // &&
	LOR   = token.LOR   // ||
	ARROW = token.ARROW // <-
	INC   = token.INC   // ++
	DEC   = token.DEC   // --

	EQL    = token.EQL    // ==
	LSS    = token.LSS    // <
	GTR    = token.GTR    // >
	ASSIGN = token.ASSIGN // =
	NOT    = token.NOT    // !

	NEQ      = token.NEQ      // !=
	LEQ      = token.LEQ      // <=
	GEQ      = token.GEQ      // >=
	DEFINE   = token.DEFINE   // :=
	ELLIPSIS = token.ELLIPSIS // ...

	LPAREN = token.LPAREN // (
	LBRACK = token.LBRACK // [
	LBRACE = token.LBRACE // {
	COMMA  = token.COMMA  // ,
	PERIOD = token.PERIOD // .

	RPAREN    = token.RPAREN    // )
	RBRACK    = token.RBRACK    // ]
	RBRACE    = token.RBRACE    // }
	SEMICOLON = token.SEMICOLON // ;
	COLON     = token.COLON     // :

	// Keywords
	BREAK    = token.BREAK
	CASE     = token.CASE
	CHAN     = token.CHAN
	CONST    = token.CONST
	CONTINUE = token.CONTINUE

	DEFAULT     = token.DEFAULT
	DEFER       = token.DEFER
	ELSE        = token.ELSE
	FALLTHROUGH = token.FALLTHROUGH
	FOR         = token.FOR

	FUNC   = token.FUNC
	GO     = token.GO
	GOTO   = token.GOTO
	IF     = token.IF
	IMPORT = token.IMPORT

	INTERFACE = token.INTERFACE
	MAP       = token.MAP
	PACKAGE   = token.PACKAGE
	RANGE     = token.RANGE
	RETURN    = token.RETURN

	SELECT = token.SELECT
	STRUCT = token.STRUCT
	SWITCH = token.SWITCH
	TYPE   = token.TYPE
	VAR    = token.VAR

	// additional tokens, handled in an ad-hoc manner
	TILDE = token.TILDE
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
//lint:ignore U1000 whatever
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
//lint:ignore U1000 whatever
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

func extractPos(s string) (p token.Position, ok bool) {
	var prefix string
	if len(s) > 1 && s[1] == ':' { // c:\foo
		prefix = s[:2]
		s = s[2:]
	}
	// "testdata/parser/bug/001.c:1193: ..."
	a := strings.Split(s, ":")
	// ["testdata/parser/bug/001.c" "1193" "..."]
	if len(a) < 2 {
		return p, false
	}

	line, err := strconv.Atoi(a[1])
	if err != nil {
		return p, false
	}

	col, err := strconv.Atoi(a[2])
	if err != nil {
		col = 1
	}

	return token.Position{Filename: prefix + a[0], Line: line, Column: col}, true
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

type data struct {
	line  int
	cases int
	cnt   int
}

type analyzer struct {
	sync.Mutex
	m map[int]*data // line: data
}

func newAnalyzer() *analyzer {
	return &analyzer{m: map[int]*data{}}
}

func (a *analyzer) record(line, cnt int) {
	d := a.m[line]
	if d == nil {
		d = &data{line: line}
		a.m[line] = d
	}
	d.cases++
	d.cnt += cnt
}

func (a *analyzer) merge(b *analyzer) {
	a.Lock()
	defer a.Unlock()

	for k, v := range b.m {
		d := a.m[k]
		if d == nil {
			d = &data{line: k}
			a.m[k] = d
		}
		d.cases += v.cases
		d.cnt += v.cnt
	}
}

func (a *analyzer) report() string {
	var rows []*data
	for _, v := range a.m {
		rows = append(rows, v)
	}
	sort.Slice(rows, func(i, j int) bool {
		a := rows[i]
		b := rows[j]
		if a.cases < b.cases {
			return true
		}

		if a.cases > b.cases {
			return false
		}

		// a.cases == b.cases
		if a.cnt < b.cnt {
			return true
		}

		if a.cnt > b.cnt {
			return false
		}

		// a.cnt == b.cnt
		return a.line < b.line
	})
	var b strings.Builder
	var cases, cnt int
	for _, row := range rows {
		cases += row.cases
		cnt += row.cnt
		avg := float64(row.cnt) / float64(row.cases)
		fmt.Fprintf(&b, "parser.go:%d:\t%16s %16s %8.1f\n", row.line, h(row.cases), h(row.cnt), avg)
	}
	avg := float64(cnt) / float64(cases)
	fmt.Fprintf(&b, "<total>\t\t%16s %16s %8.1f\n", h(cases), h(cnt), avg)
	return b.String()
}

type parallel struct {
	a                  *analyzer
	errors             []error
	limit              chan struct{}
	maxBacktrackOrigin string
	maxBacktrackPath   string
	maxBacktrackPos    string
	maxBacktracksPath  string
	maxBudgetPath      string
	minToksPath        string
	sync.Mutex
	wg sync.WaitGroup

	fails         int32
	files         int32
	maxBacktrack  int
	maxBacktracks int
	maxBudget     int
	maxBudgetToks int
	minToks       int
	ok            int32
	skipped       int32
}

func newParallel() *parallel {
	return &parallel{
		a:     newAnalyzer(),
		limit: make(chan struct{}, runtime.GOMAXPROCS(0)),
	}
}

func (p *parallel) addSkipped() { atomic.AddInt32(&p.skipped, 1) }
func (p *parallel) addFail()    { atomic.AddInt32(&p.fails, 1) }
func (p *parallel) addFile()    { atomic.AddInt32(&p.files, 1) }
func (p *parallel) addOk()      { atomic.AddInt32(&p.ok, 1) }

func (p *parallel) recordMaxBacktrack(path string, back int, pos, origin string) {
	p.Lock()
	defer p.Unlock()

	if back > p.maxBacktrack {
		p.maxBacktrack = back
		p.maxBacktrackOrigin = origin
		p.maxBacktrackPos = pos
		p.maxBacktrackPath = path
	}
}

func (p *parallel) recordMaxBacktracks(path string, back int) {
	p.Lock()
	defer p.Unlock()

	if back > p.maxBacktracks {
		p.maxBacktracks = back
		p.maxBacktracksPath = path
	}
}

func (p *parallel) recordMaxBudget(path string, budget, toks int) {
	p.Lock()
	defer p.Unlock()

	if budget > p.maxBudget {
		p.maxBudget = budget
		p.maxBudgetToks = toks
		p.maxBudgetPath = path
	}
}

func (p *parallel) recordMinToks(path string, toks int) {
	p.Lock()
	defer p.Unlock()

	if p.minToks == 0 || toks < p.minToks {
		p.minToks = toks
		p.minToksPath = path
	}
}

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
