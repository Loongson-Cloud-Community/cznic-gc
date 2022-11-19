.PHONY:	clean edit editor test test2 back report report2 parser2 benchmarks

clean:
	rm -f log-* cpu.test mem.test *.out
	go clean

edit:
	@touch log
	@if [ -f "Session.vim" ]; then gvim -S & else gvim -p Makefile *.go & fi

editor:
	gofmt -l -s -w *.go
	go test -c -o /dev/null 2>&1 | tee log-editor

test:
	go test -v -failfast 2>&1 | tee log-test
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true

test2:
	go test -v -failfast -src $$HOME/src 2>&1 | tee log-test
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true

parser2:
	go test -v -failfast -run TestParser -src $$HOME/src 2>&1 | tee log-test
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true

back:
	go test -v -failfast -noback 2>&1 | tee log-test
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true

report:
	go test -v -failfast -run TestParser -report 2>&1 | tee log-test
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true

report2:
	go test -v -failfast -run TestParser -src $$HOME/src -report 2>&1 | tee log-test
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-test || true
	grep 'FAIL' log-test || true


benchmarks:
	go test -v -run @ -bench . 2>&1 | tee log-benchmarks

