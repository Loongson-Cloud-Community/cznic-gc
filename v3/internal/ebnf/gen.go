package main

import (
	"bytes"
	"fmt"
	"go/token"
	"os"
	"sort"
	"strings"

	"golang.org/x/exp/ebnf"
	"modernc.org/mathutil"
)

func generate(dst, src string) error {
	g, err := newGen(dst, src)
	if err != nil {
		return err
	}

	return g.gen()
}

type gen struct {
	b        bytes.Buffer
	dst      string
	listItem []struct{ fldName, varName string }
	peg      *grammar

	nextID int
}

func newGen(dst, src string) (*gen, error) {
	peg, err := loadPEG(src)
	if err != nil {
		return nil, err
	}

	return &gen{dst: dst, peg: peg}, nil
}

func (g *gen) w(format string, args ...interface{}) { fmt.Fprintf(&g.b, format, args...) }

func (g *gen) id() (r int) { r = g.nextID; g.nextID++; return r }

func (g *gen) tok(s string) token.Token {
	if r, ok := toks[s]; ok {
		return r
	}

	panic(todo("%q", s))
}

func (g *gen) isToken(e ebnf.Expression, r *token.Token) bool {
	switch x := e.(type) {
	case *ebnf.Name:
		nm := x.String
		if token.IsExported(nm) {
			return g.isToken(g.peg.g[nm].Expr, r)
		}

		*r = g.tok(nm)
		return true
	case *ebnf.Token:
		*r = g.tok(x.String)
		return true
	default:
		return false
	}
}

func noPreBlock(nm string) string {
	const tag = "PreBlock"
	if strings.HasSuffix(nm, tag) {
		nm = nm[:len(nm)-len(tag)]
	}
	return nm
}

func (g *gen) gen() (err error) {
	trc("")
	defer func() {
		if e := recover(); e != nil {
			trc("", err)
			trc("", e)
			if err == nil {
				err = fmt.Errorf("%v", e)
			}
		}
		if err != nil {
			return
		}

		err = os.WriteFile(g.dst, g.b.Bytes(), 0660)
	}()

	// From https://golang.org/s/generatedcode (golang/go#13560 (comment));
	//
	// Generated files are marked by a line of text that matches the regular expression, in Go syntax:
	//
	//     ^// Code generated .* DO NOT EDIT\.$
	//

	g.w("// Code generated by '$ go test -gen -run TestGen' DO NOT EDIT.")
	g.w("\n\npackage main")
	g.w("\n\nimport \"go/token\"")
	var a []string
	for v := range g.peg.g {
		a = append(a, v)
	}
	sort.Strings(a)
	for _, nm := range a {
		g.nextID = 0
		p := g.peg.g[nm]
		if !token.IsExported(nm) {
			continue
		}

		n := g.newNode(p, nm)
		n.declareType()
		g.w("\n\nfunc (p *parser) %s() *%sNode {", unexport(nm), nm)
		n.declareVars()
		ctx := g.peg.productionFollowSets[p]
		if *oAssert && !ctx.hasEpsilon() {
			g.w("\nswitch p.c() {")
			g.w("\ncase %s:", ctx.caseStr())
			g.w("\ndefault:")
			g.w("\npanic(todo(``, p.c()))")
			g.w("\n}")
		}
		dirty := map[string]struct{}{}
		switch x := p.Expr.(type) {
		case nil:
			n.ret()
		case ebnf.Alternative:
			g.expression(n, nil, ctx, x, "\nreturn nil", false, dirty)
			n.ret()
		case *ebnf.Group:
			panic(todo(""))
		case *ebnf.Name:
			g.expression(n, nil, ctx, x, "\nreturn nil", false, dirty)
			n.ret()
		case *ebnf.Option:
			g.expression(n, nil, ctx, x, "\npanic(`internal error`)", false, dirty)
			n.ret()
		case *ebnf.Repetition:
			panic(todo(""))
		case ebnf.Sequence:
			g.expression(n, nil, ctx, x, "\nreturn nil", false, dirty)
			n.ret()
		case *ebnf.Token:
			g.expression(n, nil, ctx, x, "\nreturn nil", false, dirty)
			n.ret()
		default:
			id := g.id()
			g.w("\nix := p.ix")
			l := g.expression(n, nil, ctx, x, fmt.Sprintf("\ngoto _%d", id), false, dirty)
			n.ret()
			if l {
				g.w("\n_%d:", id)
				g.w("\np.back(ix)")
				g.w("\nreturn nil")
			}
		}
		g.w("\n}")
	}
	return nil
}

func (g *gen) elsePart(out string) (r bool) {
	defer func() {
		if out == "" {
			r = false
		}
	}()

	if out != "" {
		g.w(" else {")
		g.w("%s", out)
		g.w("\n}")
		r = true
	}
	return r
}

func (g *gen) defaultPart(out string) (r bool) {
	defer func() {
		if out == "" {
			r = false
		}
	}()

	if out != "" {
		g.w("\n;default:%s", out)
		r = true
	}
	return r
}

// nil c -> unconstrained by caller
func (g *gen) expression(n, inRepeat *node, ctx followSet, e ebnf.Expression, out string, braced bool, dirty map[string]struct{}) (r bool) {
	defer func() {
		if out == "" {
			r = false
		}
	}()

	g.w("\n// %T %s ctx [%v]", e, ebnfString(e), ctx.caseStr())
	switch x := e.(type) {
	case ebnf.Alternative:
		c := g.peg.followSet(x)
		if len(ctx) != 0 {
			c = c.intersect(ctx)
		}
		return g.alt(n, inRepeat, c, x, out, dirty)
	case *ebnf.Group:
		c := g.peg.followSet(x.Body)
		if len(ctx) != 0 {
			c = c.intersect(ctx)
		}
		return g.expression(n, inRepeat, c, x.Body, out, false, dirty)
	case *ebnf.Name:
		if inRepeat != nil {
			g.listItem = append(g.listItem, struct{ fldName, varName string }{inRepeat.field(x).name, n.varName(x)})
		}
		nm := x.String
		if token.IsExported(nm) {
			if inRepeat == nil {
				dirty[fmt.Sprintf("%s = nil", n.varName(x))] = struct{}{}
			}
			switch p := g.peg.g[nm]; {
			case p.Expr == nil:
				g.w("\nif %s = p.%s(); %[1]s == nil {", n.varName(x), unexport(nm))
				g.w("%s", out)
				g.w("\n}")
			default:
				c := g.peg.productionFollowSets[p]
				if len(ctx) != 0 {
					c = c.intersect(ctx)
				}
				switch {
				case len(ctx) != 0 && ctx.isSubsetOf(c):
					g.w("\nif %s = p.%s(); %[1]s == nil {", n.varName(x), unexport(nm))
					g.w("%s", out)
					g.w("\n}")
				default:
					g.w("\nswitch p.c() {")
					g.w("\ncase %v:", c.caseStr())
					g.w("\nif %s = p.%s(); %[1]s == nil {", n.varName(x), unexport(nm))
					g.w("%s", out)
					g.w("\n}")
					if !c.hasEpsilon() {
						g.defaultPart(out)
					}
					g.w("\n}")
				}
			}
			return true
		}

		if inRepeat == nil {
			dirty[fmt.Sprintf("%s = Token{}", n.varName(x))] = struct{}{}
		}
		t := g.tok(x.String)
		if ctx.has(t) {
			g.w("\n%s = p.expect(%s)", n.varName(x), tokSource(t))
			return false
		}

		g.w("\nif %s, ok = p.accept(%s); !ok {", n.varName(x), tokSource(t))
		g.w("%s", out)
		g.w("\n}")
		return true
	case *ebnf.Option:
		c := g.peg.followSet(x.Body)
		if len(ctx) != 0 {
			c = c.intersect(ctx)
		}
		//	expr(cleanup)
		//	goto ok
		//	cleanup:
		//	...
		//	ok:
		dirty2 := map[string]struct{}{}
		cleanup := g.id()
		ok := g.id()
		var l bool
		switch {
		case len(ctx) != 0 && ctx.isSubsetOf(c):
			l = g.expression(n, inRepeat, c, x.Body, fmt.Sprintf("\ngoto _%d", cleanup), false, dirty2)
		default:
			g.w("\nswitch p.c() {")
			g.w("\ncase %v:", c.caseStr())
			l = g.expression(n, inRepeat, c, x.Body, fmt.Sprintf("\ngoto _%d", cleanup), false, dirty2)
			g.w("\n}")
			// if l {
			// 	g.w("\n_%d:", ok)
			// }
			// return false
		}
		if l {
			g.w("\ngoto _%d", ok)
			g.w("\n_%d:", cleanup)
			var a []string
			for k := range dirty2 {
				a = append(a, k)
			}
			sort.Strings(a)
			for _, v := range a {
				g.w("\n%s", v)
			}
			g.w("\n_%d:", ok)
		}
		for k := range dirty2 {
			dirty[k] = struct{}{}
		}
		return false
	case *ebnf.Repetition:
		//TODO? specialize for single item .Body
		c := g.peg.followSet(x.Body)
		if len(ctx) != 0 {
			c = c.intersect(ctx)
		}
		switch {
		case len(ctx) != 0 && ctx.isSubsetOf(c):
			panic(todo(""))
			// id := g.id()
			// g.w("\n_%d:", id)
			// ok := g.id()
			// l := g.expression(n, c, x.Body, fmt.Sprintf("\ngoto _%d", ok), true)
			// g.w("\ngoto _%d", id)
			// if l {
			// 	g.w("\n_%[1]d:", ok)
			// }
		default:
			id := g.id()
			g.w("\n_%d:", id)
			ok := g.id()
			g.w("\n{")
			sv := g.listItem
			items := n.items[x]
			for _, f := range items.fields {
				g.w("\nvar %s %s", n.unexport(f.name), f.typ)
			}
			g.w("\nswitch p.c() {")
			g.w("\ncase %v:", c.caseStr())
			l := g.expression(items, items, c, x.Body, fmt.Sprintf("\ngoto _%d", ok), true, dirty)
			g.w("\n%s = append(%[1]s, %s{", n.varName(x), n.field(x).typ[2:])
			for _, v := range g.listItem {
				g.w("%s: %s, ", v.fldName, v.varName)
			}
			g.w("})")
			g.listItem = sv
			g.w("\ngoto _%d", id)
			g.w("\n}")
			if l {
				g.w("\n_%[1]d:", ok)
			}
			g.w("\n}")

		}
		return false
	case ebnf.Sequence:
		var t token.Token
		hasToken := false
		for _, v := range x {
			if g.isToken(v, &t) {
				hasToken = true
				break
			}
		}
		out2 := out
		if !strings.Contains(out, "p.back(ix)") {
			out2 = fmt.Sprintf("\np.back(ix);%s", out)
		}
		var ctx2 followSet
		switch {
		case braced:
			ctx2, r = g.sequenceFilter(x, out)
			if hasToken {
				g.w("\nix := p.ix")
				if ctx != nil {
					g.w("\n_ = ix")
				}
			}
			for _, v := range x {
				if g.expression(n, inRepeat, ctx, v, out2, false, dirty) {
					r = true
				}
				ctx = ctx2
				ctx2 = nil
			}
		default:
			g.w("\n{")
			ctx2, r = g.sequenceFilter(x, out)
			g.w("\nix := p.ix")
			if ctx != nil {
				g.w("\n_ = ix")
			}
			for _, v := range x {
				if g.expression(n, inRepeat, ctx, v, out2, false, dirty) {
					r = true
				}
				ctx = ctx2
				ctx2 = nil
			}
			g.w("\n}")
		}
		return r
	case *ebnf.Token:
		if inRepeat != nil {
			g.listItem = append(g.listItem, struct{ fldName, varName string }{inRepeat.field(x).name, n.varName(x)})
		}
		if inRepeat == nil {
			dirty[fmt.Sprintf("%s = Token{}", n.varName(x))] = struct{}{}
		}
		switch t := g.tok(x.String); {
		case t == epsilon:
			panic(todo(""))
		case ctx.has(t):
			g.w("\n%s = p.expect(%s)", n.varName(x), tokSource(t))
			return false
		default:
			g.w("\nif %s, ok = p.accept(%s); !ok {", n.varName(x), tokSource(t))
			g.w("%s", out)
			g.w("\n}")
			return true
		}
	default:
		g.w("\n//TODO %T: '%s'", x, ebnfString(e))
		g.w("%s", out)
		return true
	}
}

func (g *gen) sequenceFilter(x ebnf.Sequence, out string) (fs followSet, r bool) {
	defer func() {
		if out == "" {
			r = false
		}
	}()

	var t0, t1 token.Token
	if len(x) < 2 || !g.isToken(x[0], &t0) {
		return nil, false
	}

	switch {
	case g.isToken(x[1], &t1):
		g.w("\nif p.peek(1) != %s {", tokSource(t1))
		g.w("%s", out)
		g.w("\n}")
		return followSet{t1: {}}, true
	default:
		fs := g.peg.followSet(x[1])
		if fs.hasEpsilon() {
			return nil, false
		}

		g.w("\nswitch p.peek(1) {")
		g.w("\ncase %s:", fs.caseStr())
		g.defaultPart(out)
		g.w("\n}")
		return fs, true
	}
}

func (g *gen) alt(n, inRepeat *node, ctx followSet, x ebnf.Alternative, out string, dirty map[string]struct{}) (r bool) {
	defer func() {
		if out == "" {
			r = false
		}
	}()

	m := map[token.Token][]int{}
	for i, v := range x {
		c := g.peg.followSet(v).clone()
		if len(ctx) != 0 {
			c = c.intersect(ctx)
		}
		for k := range c {
			m[k] = append(m[k], i)
		}
	}
	var mX []token.Token
	for k := range m {
		mX = append(mX, k)
	}
	sort.Slice(mX, func(i, j int) bool { return mX[i] < mX[j] })
	disjoint := map[string]followSet{}
	for _, t := range mX {
		is := m[t]
		if len(is) == 0 {
			continue
		}

		k := fmt.Sprint(is)
		k = k[1 : len(k)-1]
		c := disjoint[k]
		c.add(t)
		disjoint[k] = c
	}
	var disjointX []string
	for k := range disjoint {
		disjointX = append(disjointX, k)
	}
	sort.Slice(disjointX, func(i, j int) bool {
		a := ints(disjointX[i])
		b := ints(disjointX[j])
		for k := range a[:mathutil.Min(len(a), len(b))] {
			if a[k] < b[k] {
				return true
			}

			if a[k] > b[k] {
				return false
			}
		}
		return len(a) < len(b)
	})
	// for _, v := range disjointX {
	// 	g.w("\n// case %v: %v", disjoint[v].caseStr(), v)
	// }
	g.w("\nswitch p.c() {")
	needDefault := true
	for _, k := range disjointX {
		c := disjoint[k]
		if c.hasEpsilon() {
			needDefault = false
		}
		var xs []ebnf.Expression
		for _, v := range ints(k) {
			xs = append(xs, x[v])
		}
		switch {
		case len(c) == 1 && c.hasEpsilon():
			g.w("\ndefault: // %v %v", c.caseStr(), k)
			r = g.altCases(n, inRepeat, c, xs, out, dirty)
		default:
			g.w("\ncase %v: // %v", c.caseStr(), k)
			r = g.altCases(n, inRepeat, c, xs, out, dirty)
		}
	}
	if needDefault {
		g.w("\n;default:%s", out)
		r = true
	}
	g.w("\n}")
	return r
}

func (g *gen) altCases(n, inRepeat *node, c followSet, x []ebnf.Expression, out string, dirty map[string]struct{}) (r bool) {
	defer func() {
		if out == "" {
			r = false
		}
	}()

	dirty2 := map[string]struct{}{}
	switch {
	case len(x) == 1:
		//	expr(cleanup)
		//	goto ok
		//	cleanup:
		//	...
		//	goto out
		//	ok:
		cleanup := g.id()
		ok := g.id()
		l := g.expression(n, inRepeat, c, x[0], fmt.Sprintf("\ngoto _%d", cleanup), false, dirty2)
		if l {
			g.w("\ngoto _%d", ok)
			g.w("\n_%d:;", cleanup)
			var a []string
			for k := range dirty2 {
				a = append(a, k)
			}
			sort.Strings(a)
			for _, v := range a {
				g.w("\n%s", v)
			}
			g.w("%s", out)
			r = true
			g.w("\n_%d:;", ok)
		}
	default:
		for _, v := range x {
			next := g.id()
			l := g.expression(n, inRepeat, c, v, fmt.Sprintf("\ngoto _%d", next), false, dirty2)
			g.w("\nbreak")
			if l {
				g.w("\n_%d:", next)
				var a []string
				for k := range dirty2 {
					a = append(a, k)
				}
				sort.Strings(a)
				for _, v := range a {
					g.w("\n%v", v)
				}
				dirty2 = map[string]struct{}{}
			}
		}
		g.w("%s", out)
		r = true
	}
	for k := range dirty2 {
		dirty[k] = struct{}{}
	}
	return r
}

type field struct {
	e        ebnf.Expression
	name     string
	typ      string
	inRepeat *node
}

type node struct {
	expr2field    map[ebnf.Expression]*field
	fieldSuffixes map[string]int
	fields        []*field
	g             *gen
	items         map[*ebnf.Repetition]*node
	p             *ebnf.Production
	pname         string
}

func (g *gen) newNode(p *ebnf.Production, pname string) *node {
	n := &node{
		expr2field:    map[ebnf.Expression]*field{},
		fieldSuffixes: map[string]int{},
		g:             g,
		items:         map[*ebnf.Repetition]*node{},
		p:             p,
		pname:         pname,
	}
	n.collectFields(p.Expr, 0, nil)
	m := map[string]bool{}
	w := 0
	for _, f := range n.fields {
		if !m[f.name] {
			n.fields[w] = f
			w++
		}
		m[f.name] = true
	}
	n.fields = n.fields[:w]
	return n
}

func (g *gen) newItemNode(p *ebnf.Repetition) *node {
	n := &node{
		expr2field:    map[ebnf.Expression]*field{},
		fieldSuffixes: map[string]int{},
		g:             g,
	}
	n.collectFields(p.Body, 0, nil)
	m := map[string]bool{}
	w := 0
	for _, f := range n.fields {
		if !m[f.name] {
			n.fields[w] = f
			w++
		}
		m[f.name] = true
	}
	n.fields = n.fields[:w]
	return n
}

func (n *node) field(e ebnf.Expression) *field     { return n.expr2field[e] }
func (n *node) fieldName(e ebnf.Expression) string { return n.expr2field[e].name }
func (n *node) varName(e ebnf.Expression) string   { return n.unexport(n.expr2field[e].name) }

func (n *node) addField(f *field) {
	n.fieldSuffixes[f.name]++
	suffix := n.fieldSuffixes[f.name]
	if suffix > 1 {
		f.name += fmt.Sprint(suffix)
	}
	n.expr2field[f.e] = f
	n.fields = append(n.fields, f)
}

func (n *node) collectFields(expr ebnf.Expression, lvl int, inRepeat *node) {
	switch x := expr.(type) {
	case ebnf.Alternative:
		for _, v := range x {
			n.collectFields(v, lvl+1, inRepeat)
			if lvl == 0 {
				n.fieldSuffixes = map[string]int{}
			}
		}
	case *ebnf.Group:
		n.collectFields(x.Body, lvl+1, inRepeat)
	case *ebnf.Name:
		nm := x.String
		switch {
		case token.IsExported(nm):
			n.addField(&field{x, nm, fmt.Sprintf("*%sNode", nm), inRepeat})
		default:
			tok := n.g.tok(nm)
			nm = tokSource(tok)
			n.addField(&field{x, nm, "Token", inRepeat})
		}
	case *ebnf.Option:
		n.collectFields(x.Body, lvl+1, inRepeat)
	case *ebnf.Repetition:
		item := n.g.newItemNode(x)
		n.items[x] = item
		var b strings.Builder
		b.WriteString("[]struct{")
		for _, f := range item.fields {
			fmt.Fprintf(&b, "%s %s;", f.name, f.typ)
		}
		b.WriteString("}")
		n.addField(&field{x, "List", b.String(), inRepeat})
	case ebnf.Sequence:
		for _, v := range x {
			n.collectFields(v, lvl+1, inRepeat)
		}
	case *ebnf.Token:
		tok := n.g.tok(x.String)
		nm := tokSource(tok)
		n.addField(&field{x, nm, "Token", inRepeat})
	case nil:
		//
	default:
		panic(todo("%T", x))
	}
}

func (n *node) declareType() {
	n.g.w("\n\n// %sNode represents the production", n.pname)
	n.g.w("\n//\n//\t%s = %s .", n.pname, ebnfString(n.p.Expr))
	n.g.w("\ntype %sNode struct{", n.pname)
	for _, f := range n.fields {
		if f.inRepeat == nil {
			n.g.w("\n\t%s\t%s", f.name, f.typ)
		}
	}
	n.g.w("\n}")
	n.g.w("\n\n// Source implements Node.")
	n.g.w("\nfunc (n *%sNode) Source(full bool) string { return nodeSource(n, full) }", n.pname)
	n.g.w("\n\n// Position implements Node.")
	n.g.w("\nfunc (n *%sNode) Position() token.Position { ", n.pname)
	if x, ok := n.p.Expr.(ebnf.Sequence); ok {
		switch y := x[0].(type) {
		case *ebnf.Token:
			n.g.w("return n.%s.Position() }", n.fieldName(y))
			return
		case *ebnf.Name:
			nm := y.String
			if !token.IsExported(nm) {
				n.g.w("return n.%s.Position() }", n.g.tok(nm))
				return
			}

			n.g.w("return n.%s.Position() }", n.fieldName(y))
			return
		}
	}
	if x, ok := n.p.Expr.(*ebnf.Name); ok {
		nm := x.String
		if !token.IsExported(nm) {
			n.g.w("return n.%s.Position() }", n.g.tok(nm))
			return
		}

		n.g.w("return n.%s.Position() }", n.fieldName(x))
		return
	}
	n.g.w("panic(\"TODO\") }")
}

func (n *node) declareVars() {
	n.g.w("\nvar (")
	n.g.w("\n\tok bool")
	for _, f := range n.fields {
		if f.inRepeat == nil {
			n.g.w("\n\t%s\t%s", n.unexport(f.name), f.typ)
		}
	}
	n.g.w("\n)")
	n.g.w("\n_ = ok")
}

var rename = map[string]string{
	"Type": "typeNode",
}

func (n *node) unexport(s string) string {
	if r := rename[s]; r != "" {
		return r
	}

	for i := 0; i < len(s); i++ {
		if c := s[i]; c >= 'a' && c <= 'z' {
			return strings.ToLower(s[:1]) + s[1:]
		}
	}

	return strings.ToLower(s) + "Tok"
}

func (n *node) ret() {
	n.g.w("\nreturn &%sNode{", n.pname)
	for _, f := range n.fields {
		if f.inRepeat == nil {
			n.g.w("\n%s: %s, ", f.name, n.varName(f.e))
			continue
		}
	}
	n.g.w("\n}")
}
