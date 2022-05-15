// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc // import "modernc.org/gc/v2"

var (
	_ = []Node{}
)

// ParseSourceFileConfig configures ParseSourceFile.
type ParseSourceFileConfig struct {
	// Accept, if non nil, is called once the package clause and imports are
	// parsed. If Accept return false the parsing stops and an error is returned.
	// Passing nil Accept is the same as passing a function that always returns
	// true.
	//TODO Accept func(*SourceFile) bool

	AllErrors bool
}

type parser struct {
	cfg           *ParseSourceFileConfig
	lbrace        int
	lbraceStack   []int
	loophackStack []bool
	s             *Scanner

	loophack bool
}

func newParser(cfg *ParseSourceFileConfig, s *Scanner) *parser {
	return &parser{cfg: cfg, s: s}
}

func (p *parser) ch() Ch { return p.s.Tok.Ch }

func (p *parser) Err() error { return p.s.Err() }

func (p *parser) err(msg string, args ...interface{}) { p.errNode(p.s.Tok, msg, args...) }

func (p *parser) errNode(n Node, msg string, args ...interface{}) {
	p.s.errs.err(n.Position(), msg, args...)
	if !p.cfg.AllErrors && len(p.s.errs) >= 10 {
		p.s.close()
	}
}

func (p *parser) must(c Ch) (r Token) {
	r = p.s.Tok
	if r.Ch != c {
		p.err("expected %v, got %v", c.str(), r.Ch.str())
	}
	p.s.Scan()
	return r
}

func (p *parser) opt(c Ch) (r Token) {
	if p.ch() != c {
		return r
	}

	r = p.s.Tok
	p.s.Scan()
	return r
}

func (p *parser) shift() (r Token) {
	r = p.s.Tok
	p.s.Scan()
	switch r.Ch {
	case FOR, IF, SELECT, SWITCH:
		p.loophack = true
	case '(', '[':
		if p.loophack || len(p.loophackStack) != 0 {
			p.loophackStack = append(p.loophackStack, p.loophack)
			p.loophack = false
		}
	case ')', ']':
		if n := len(p.loophackStack); n != 0 {
			p.loophack = p.loophackStack[n-1]
			p.loophackStack = p.loophackStack[:n-1]
		}
	case '{':
		p.lbrace++
		if p.loophack {
			r.Ch = body
			p.loophack = false
		}
	case '}':
		p.lbrace--
		if n := len(p.lbraceStack); n != 0 && p.lbraceStack[n-1] == p.lbrace {
			p.lbraceStack = p.lbraceStack[:n-1]
			p.loophack = true
		}
	}
	return r
}

func (p *parser) fixLbr() {
	n := p.lbrace - 1
	switch p.ch() {
	case '}':
		p.loophack = true
		return
	case '{':
		n--
	}

	p.lbraceStack = append(p.lbraceStack, n)
}

// ParseSourceFile parses buf and returns a *SourceFile or an error, if any.
// Positions are reported as if buf is coming from a file named name. The
// buffer becomes owned by the *SourceFile and must not be modified after
// calling ParseSourceFile.
func ParseSourceFile(cfg *ParseSourceFileConfig, name string, buf []byte) (*file, error) {
	s, err := NewScanner(name, buf)
	if err != nil {
		return nil, err
	}

	p := newParser(cfg, s)
	p.shift()
	p.err(errorf("TODO %v", p.s.Tok.str()))
	return nil, p.Err()
}

type file struct {
	//TODO
}
