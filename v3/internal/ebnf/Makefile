.PHONY:	clean edit editor test gen

clean:
	rm -f log-* cpu.test mem.test *.out
	go clean

edit:
	@touch log
	@if [ -f "Session.vim" ]; then gvim -S & else gvim -p Makefile *.go & fi

editor:
	gofmt -l -s -w *.go
	go test -c -o /dev/null 2>&1 | tee log-editor

gen:
	echo "package main; type SourceFileNode struct{}; func (p *parser) sourceFile() *SourceFileNode { return nil }" > parser.go
	go test -v -gen -run TestGen 2>&1 | tee log-gen
	gofmt -l -s -w *.go

test:
	go test -v -failfast 2>&1 | tee log-test
	@git diff testdata/
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true
