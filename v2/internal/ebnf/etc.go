package main

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"sync/atomic"
)

var (
	_ = todo //TODOOK
	_ = trc  //TODOOK

	extendedErrors bool
)

func origin(skip int) string {
	pc, fn, fl, _ := runtime.Caller(skip)
	fn = filepath.Base(fn)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
	}
	return fmt.Sprintf("%s:%d:%s", fn, fl, fns)
}

func todo(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	pc, fn, fl, _ := runtime.Caller(1)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
	}
	r := fmt.Sprintf("%s:%d:%s: TODOTODO %s", fn, fl, fns, s) //TODOOK
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

func trc(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	_, fn, fl, _ := runtime.Caller(1)
	r := fmt.Sprintf("%s:%d: TRC %s", filepath.Base(fn), fl, s)
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

type parallel struct {
	errors []error
	limit  chan struct{}
	sync.Mutex
	wg sync.WaitGroup

	fails   int32
	files   int32
	nBytes  int32
	nTokens int32
	oks     int32
	skips   int32
}

func newParallel() *parallel {
	return &parallel{
		limit: make(chan struct{}, runtime.GOMAXPROCS(0)),
	}
}

func (p *parallel) bytes(n int)  { atomic.AddInt32(&p.nBytes, int32(n)) }
func (p *parallel) fail()        { atomic.AddInt32(&p.fails, 1) }
func (p *parallel) file()        { atomic.AddInt32(&p.files, 1) }
func (p *parallel) nfail() bool  { return atomic.LoadInt32(&p.fails) != 0 }
func (p *parallel) nfile() int   { return int(atomic.LoadInt32(&p.files)) }
func (p *parallel) ok()          { atomic.AddInt32(&p.oks, 1) }
func (p *parallel) skip()        { atomic.AddInt32(&p.skips, 1) }
func (p *parallel) tokens(n int) { atomic.AddInt32(&p.nTokens, int32(n)) }

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
