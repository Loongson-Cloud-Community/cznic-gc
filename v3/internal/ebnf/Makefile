.PHONY:	clean edit editor test gen report assert

assert:
	echo "package main; type SourceFileNode struct{}; func (p *parser) sourceFile() *SourceFileNode { return nil }" > parser.go
	go test -v -gen -assert -run TestGen 2>&1 | tee log-gen
	gofmt -l -s -w *.go

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
	go test -v -failfast -src $$HOME/src 2>&1 | tee log-test
	@git diff testdata/
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true

report:
	go test -v -failfast -run TestParser -src $$HOME/src -report 2>&1 | tee log-test
	@git diff testdata/
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true
