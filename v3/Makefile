.PHONY:	clean edit editor test test2 back report report2 parser2 benchmarks benchmarks2 mem memgo

clean:
	rm -f cpu.test mem.test *.out
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
	go test -v -failfast -src $$HOME/src 2>&1 | tee log-test2
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-test2 || true
	grep 'FAIL' log-test2 || true

parser2:
	go test -v -failfast -run TestParser -src $$HOME/src 2>&1 | tee log-parser2
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-parser2 || true
	grep 'FAIL' log-parser2 || true

back:
	go test -v -failfast -noback 2>&1 | tee log-back
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-back || true
	grep 'FAIL' log-back || true

report:
	go test -v -failfast -run TestParser -report 2>&1 | tee log-report
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-report || true
	grep 'FAIL' log-report || true

report2:
	go test -v -failfast -run TestParser -src $$HOME/src -report 2>&1 | tee log-report2
	@git diff testdata/ || true
	@git status
	@grep TOTAL log-report2 || true
	grep 'FAIL' log-report2 || true


benchmarks:
	go test -v -run @ -bench . 2>&1 | tee log-benchmarks

benchmarks2:
	go test -v -run @ -bench . -bsrc $$HOME/src 2>&1 | tee log-benchmarks2

mem:
	go test -run @ -bench BenchmarkParser -memprofile mem.out
	go tool pprof --lines --alloc_space *.test mem.out

memgo:
	go test -run @ -bench BenchmarkGoParser -memprofile mem.out
	go tool pprof --lines --alloc_space *.test mem.out
