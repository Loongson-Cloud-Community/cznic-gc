// Copyright 2022 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
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
	r := fmt.Sprintf("%s:%d: TRC %s", fn, fl, s)
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

// errorf constructs and error message. If extendedErrors is true, the error will
// contain a mini stack trace.
func errorf(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	switch {
	case extendedErrors:
		return fmt.Sprintf("%s (%v: %v)", s, origin(3), origin(2))
	default:
		return fmt.Sprintf("%s", s)
	}
}

type parallel struct {
	errors []error
	limit  chan struct{}
	sync.Mutex
	wg sync.WaitGroup

	fails int32
	files int32
	oks   int32
	skips int32
}

func newParallel() *parallel {
	return &parallel{
		limit: make(chan struct{}, runtime.GOMAXPROCS(0)),
	}
}

func (p *parallel) fail() { atomic.AddInt32(&p.fails, 1) }
func (p *parallel) file() { atomic.AddInt32(&p.files, 1) }
func (p *parallel) ok()   { atomic.AddInt32(&p.oks, 1) }
func (p *parallel) skip() { atomic.AddInt32(&p.skips, 1) }

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

func nodeSource(full bool, n ...Node) []byte {
	var a []Token
	for _, v := range n {
		nodeSource0(&a, v)
	}
	sort.Slice(a, func(i, j int) bool { return a[i].off < a[j].off })
	var b bytes.Buffer
	for i, t := range a {
		switch {
		case full:
			b.WriteString(t.Sep())
		default:
			if i != 0 && len(t.Sep()) != 0 {
				b.WriteByte(' ')
			}
		}
		b.WriteString(t.Src())
	}
	return b.Bytes()
}

func nodeSource0(a *[]Token, n interface{}) {
	if n == nil {
		return
	}

	if x, ok := n.(Token); ok && x.IsValid() {
		*a = append(*a, x)
		return
	}

	t := reflect.TypeOf(n)
	v := reflect.ValueOf(n)
	var zero reflect.Value
	if t.Kind() == reflect.Pointer {
		t = t.Elem()
		v = v.Elem()
		if v == zero {
			return
		}
	}

	switch t.Kind() {
	case reflect.Struct:
		nf := t.NumField()
		for i := 0; i < nf; i++ {
			f := t.Field(i)
			if !f.IsExported() {
				continue
			}

			if v == zero || v.IsZero() {
				continue
			}

			nodeSource0(a, v.Field(i).Interface())
		}
	case reflect.Slice:
		ne := v.Len()
		for i := 0; i < ne; i++ {
			nodeSource0(a, v.Index(i).Interface())
		}
	default:
		panic(todo("", t.Kind()))
	}
}
