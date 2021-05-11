// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

import (
	"flag"
	"fmt"
	goscanner "go/scanner"
	gotoken "go/token"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"strings"
	"testing"
)

func caller(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(2)
	fmt.Fprintf(os.Stderr, "# caller: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	_, fn, fl, _ = runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "# \tcallee: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func dbg(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	pc, fn, fl, _ := runtime.Caller(1)
	f := runtime.FuncForPC(pc)
	fmt.Fprintf(os.Stderr, "# dbg %s:%d:%s: ", path.Base(fn), fl, f.Name())
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func stack() []byte { return debug.Stack() }

func use(...interface{}) {}

func init() {
	use(caller, dbg, stack) //TODOOK
}

// ----------------------------------------------------------------------------

var (
	oRE     = flag.String("re", "", "")
	re      *regexp.Regexp
	tempDir string
)

func TestMain(m *testing.M) {
	flag.BoolVar(&errTrace, "errtrc", false, "")
	flag.Parse()
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}
	os.Exit(testMain(m))
}

func testMain(m *testing.M) int {
	var err error
	tempDir, err = ioutil.TempDir("", "run-test-")
	if err != nil {
		panic(err) //TODOOK
	}

	defer os.RemoveAll(tempDir)

	return m.Run()
}

func testScanner(t *testing.T, root, skip string) {
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		switch {
		case re == nil:
			if strings.Contains(filepath.ToSlash(path), "/errchk/") {
				return nil
			}

			if strings.Contains(filepath.ToSlash(path), "/testdata/") {
				return nil
			}

			if skip != "" && strings.Contains(filepath.ToSlash(path), skip) {
				return nil
			}
		default:
			if !re.MatchString(path) {
				return nil
			}
		}

		b, err := ioutil.ReadFile(path)
		if err != nil {
			return err
		}

		switch filepath.Ext(path) {
		case ".go":
			fs := gotoken.NewFileSet()
			fi := fs.AddFile(path, -1, len(b))
			var s0 goscanner.Scanner
			var err0 error
			s0.Init(fi, b, func(pos gotoken.Position, msg string) {
				err0 = fmt.Errorf("%v: %s", pos, msg)
			}, 0)
			s, err := NewScanner(b, path, false)
			if err != nil {
				return err
			}

			for {
				pos0, tok0, lit0 := s0.Scan()
				position0 := fi.Position(pos0)
				eof0 := tok0 == gotoken.EOF
				// off0 := s.off //TODO-
				eof := !s.Scan()
				// trc("%v: tok0 %q lit0 %q, %v: tok %q lit %q, off0 %v, off %v", fi.Position(pos0), tok0, lit0, s.Tok.Position(), s.Tok.Ch, s.Tok.Src(), off0, s.off) //TODO-
				err := s.Err()
				if g, e := s.Tok.Token(), tok0; g != e {
					t.Logf("%v: tok0 %q lit0 %q, %v: tok %q lit %q", fi.Position(pos0), tok0, lit0, s.Tok.Position(), s.Tok.Ch, s.Tok.Src())
					t.Fatalf("%v: token, got %v, expected %v", position0, g, e)
					return nil
				}

				if g, e := string(s.Tok.Src()), lit0; g != e {
					switch {
					case tok0 == gotoken.SEMICOLON && lit0 != ";":
						// Ok, our result for injected semis is different.
					case noGoLit(s.Tok.Ch):
						// Ok, go/scanner does not return the literal string.
					default:
						t.Logf("%v: tok0 %q lit0 %q, %v: tok %q lit %q", fi.Position(pos0), tok0, lit0, s.Tok.Position(), s.Tok.Ch, s.Tok.Src())
						t.Fatalf("%v: source, got %q, expected %q", position0, g, e)
						return nil
					}
				}

				if g, e := s.Tok.Position().String(), position0.String(); g != e {
					ok := false
					switch {
					case eof || eof0:
						if a, b := s.Tok.Position().Offset, position0.Offset; a-b == 1 || b-a == 1 {
							ok = true
						}
					case tok0 == gotoken.SEMICOLON && lit0 == "\n":
						ok = s.Tok.Position().Filename == position0.Filename && s.Tok.Position().Line == position0.Line
					}
					if !ok {
						t.Logf("%v: tok0 %q lit0 %q, %v: tok %q lit %q", fi.Position(pos0), tok0, lit0, s.Tok.Position(), s.Tok.Ch, s.Tok.Src())
						t.Fatalf("%v: got %v:", e, g)
						return nil
					}
				}

				if g, e := err, err0; (g != nil) != (e != nil) {
					t.Logf("%v: tok0 %q lit0 %q, %v: tok %q lit %q", fi.Position(pos0), tok0, lit0, s.Tok.Position(), s.Tok.Ch, s.Tok.Src())
					t.Fatalf("%v: error, got %v, expected %v", position0, g, e)
					return nil
				}

				if g, e := eof, eof0; g != e {
					t.Logf("%v: tok0 %q lit0 %q, %v: tok %q lit %q", fi.Position(pos0), tok0, lit0, s.Tok.Position(), s.Tok.Ch, s.Tok.Src())
					t.Fatalf("%v: EOF, got %v, expected %v", position0, g, e)
					return nil
				}

				if eof {
					return nil
				}
			}
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
}

func noGoLit(c Ch) bool {
	switch c {
	case
		'!',
		'%',
		'&',
		'(',
		')',
		'*',
		'+',
		',',
		'-',
		'.',
		'/',
		':',
		'<',
		'=',
		'>',
		'[',
		']',
		'^',
		'{',
		'|',
		'}',
		ADD_ASSIGN,
		AND_ASSIGN,
		AND_NOT,
		AND_NOT_ASSIGN,
		ARROW,
		DEC,
		DEFINE,
		ELLIPSIS,
		EQ,
		GE,
		INC,
		LAND,
		LE,
		LOR,
		MUL_ASSIGN,
		NE,
		OR_ASSIGN,
		QUO_ASSIGN,
		REM_ASSIGN,
		SHL,
		SHL_ASSIGN,
		SHR,
		SHR_ASSIGN,
		SUB_ASSIGN,
		XOR_ASSIGN:

		return true
	}

	return false
}

func TestScanner(t *testing.T) {
	t.Run(".", func(t *testing.T) { testScanner(t, ".", "") })
	t.Run("GOROOT", func(t *testing.T) { testScanner(t, runtime.GOROOT(), "/test/") })
}

func BenchmarkScanner(b *testing.B) {
	root := runtime.GOROOT()
	skip := filepath.ToSlash(root + "/test/")
	var sz int64
	files := 0
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		err := filepath.Walk(runtime.GOROOT(), func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if info.IsDir() {
				return nil
			}

			if strings.HasPrefix(filepath.ToSlash(path), skip) {
				return nil
			}

			buf, err := ioutil.ReadFile(path)
			if err != nil {
				return err
			}

			if i == 0 {
				sz += int64(len(buf))
				files++
			}
			switch filepath.Ext(path) {
			case ".go":
				s, err := NewScanner(buf, path, false)
				if err != nil {
					return err
				}

				for s.Scan() {
				}
				if err := s.Err(); err != nil {
					b.Fatalf("%s: %v", path, err)
				}
			}
			return nil
		})
		if err != nil {
			b.Fatal(err)
		}
	}
	b.SetBytes(sz)
}

func BenchmarkGoScanner(b *testing.B) {
	root := runtime.GOROOT()
	skip := filepath.ToSlash(root + "/test/")
	var sz int64
	files := 0
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		err := filepath.Walk(runtime.GOROOT(), func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if info.IsDir() {
				return nil
			}

			if strings.HasPrefix(filepath.ToSlash(path), skip) {
				return nil
			}

			buf, err := ioutil.ReadFile(path)
			if err != nil {
				return err
			}

			if i == 0 {
				sz += int64(len(buf))
				files++
			}
			switch filepath.Ext(path) {
			case ".go":
				fs := gotoken.NewFileSet()
				fi := fs.AddFile(path, -1, len(buf))
				var s goscanner.Scanner
				s.Init(fi, buf, nil, goscanner.ScanComments)
				for {
					_, tok, _ := s.Scan()
					if tok == gotoken.EOF {
						break
					}
				}
			}
			return nil
		})
		if err != nil {
			b.Fatal(err)
		}
	}
	b.SetBytes(sz)
}
