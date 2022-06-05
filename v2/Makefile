# Copyright 2021 The Gc Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

.PHONY:	all clean edit editor tmp tmp2

all:

clean:
	rm -f log-* cpu.test mem.test *.out
	go clean

edit:
	@touch log
	@if [ -f "Session.vim" ]; then gvim -S & else gvim -p Makefile *.go & fi

editor:
	stringer -output stringer.go -linecomment -type=Ch
	gofmt -l -s -w *.go
	go test -c -o /dev/null
	go install -v | tee log-editor

tmp:
	go test -v -run TestParser -trc 2>&1 | tee log
	grep -a '^TODO' log | sort | uniq -c | sort -n | tee log-2
	grep -a 'TOTAL\|\[^"\]\<FAIL\>\|\<PASS\>' log || true

tmp2:
	GOMAXPROCS=1 go test -v -race -run TestParser/ -trc -re /usr/local/go/test/fixedbugs/issue15055.go 2>&1 | tee log
	grep -a '^TODO' log | sort | uniq -c | sort -n | tee log-2
	grep -a 'TOTAL\|\[^"\]\<FAIL\>\|\<PASS\>' log || true
