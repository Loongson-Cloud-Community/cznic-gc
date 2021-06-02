# Copyright 2021 The Gc Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

.PHONY:	all clean edit editor later nuke todo

grep=--include=*.go
ngrep='TODOOK\|testdata\|TODO-\|assets.*.go\|stringer.go'


all:
	@LC_ALL=C date
	@go version 2>&1 | tee log
	@gofmt -l -s -w *.go 2>&1 | tee -a log
	@go install -v ./... 2>&1 | tee -a log
	@go test 2>&1 | tee -a log
	@go test 2>&1 -timeout 1h 2>&1 | tee -a log
	@go vet 2>&1 | grep -v $(ngrep) || true 2>&1 | tee -a log
	@golint 2>&1 | grep -v $(ngrep) || true 2>&1 | tee -a log
	@make todo 2>&1 | tee -a log
	@misspell *.go 2>&1 | tee -a log
	@nilness . ./... 2>&1 | tee -a log
	@staticcheck | grep -v 'scanner\.go' || true 2>&1 | tee -a log
	@maligned || true 2>&1 | tee -a log
	@LC_ALL=C date 2>&1 | tee -a log
	@grep -n --color=always 'FAIL\|PASS' log 

clean:
	go clean
	rm -f *~ *.test *.out log log-* *.log

edit:
	@touch log
	@if [ -f "Session.vim" ]; then gvim -S & else gvim -p Makefile *.go & fi

editor: stringer.go
	gofmt -l -s -w *.go 2>&1 | tee log
	GO111MODULE=off go test -c -o /dev/null 2>&1 | tee -a log
	GO111MODULE=off go install -v ./... 2>&1 | tee -a log
	@gofmt -l -s -w .

stringer.go: scanner.go
	stringer -output stringer.go -linecomment -type=Ch
	gofmt -l -s -w .

later:
	@grep -n $(grep) LATER * || true
	@grep -n $(grep) MAYBE * || true

nuke: clean;
	go clean -i
	rm -f tex.p tex.pas tex.pool tex.tex tex.dvi tex.pdf parser.y y.output

todo:
	@grep -nr $(grep) ^[[:space:]]*_[[:space:]]*=[[:space:]][[:alpha:]][[:alnum:]]* * | grep -v $(ngrep) || true
	@grep -nrw $(grep) 'TODO\|panic' * | grep -v $(ngrep) || true
	@grep -nr $(grep) BUG * | grep -v $(ngrep) || true
	@grep -nr $(grep) [^[:alpha:]]println * | grep -v $(ngrep) || true
	@grep -nir $(grep) 'work.*progress' || true
