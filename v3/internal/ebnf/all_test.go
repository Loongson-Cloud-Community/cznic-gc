package main

import (
	"bytes"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"golang.org/x/exp/ebnf"
)

const (
	startProduction = "SourceFile"
)

var (
	spec, peg ebnf.Grammar
)

func init() {
	var err error
	if spec, err = verifySpecEBNF(filepath.Join(runtime.GOROOT(), "doc", "go_spec.html"), startProduction, nil); err != nil {
		panic(err)
	}

	var b bytes.Buffer
	printEBNF(&b, spec)
	if err = os.WriteFile("spec.ebnf", b.Bytes(), 0660); err != nil {
		panic(err)
	}

	s, err := os.ReadFile("peg.ebnf")
	if err != nil {
		panic(err)
	}

	if peg, err = ebnf.Parse("peg.ebnf", bytes.NewBuffer(s)); err != nil {
		panic(err)
	}

	if ebnf.Verify(peg, startProduction); err != nil {
		panic(err)
	}
}

func TestMain(m *testing.M) {
	os.Exit(m.Run())
}

func TestSpecEBNF(t *testing.T) {
	for _, v := range leftRecursive(spec, startProduction) {
		var a []string
		for _, w := range v {
			a = append(a, w.Name.String)
		}
		t.Logf("left recursive: %v", a)
	}
}

func TestPEGEBNF(t *testing.T) {
	for _, v := range leftRecursive(peg, startProduction) {
		var a []string
		for _, w := range v {
			a = append(a, w.Name.String)
		}
		t.Errorf("left recursive: %v", a)
	}
}
