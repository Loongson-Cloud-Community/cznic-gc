// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate stringer -output stringer.go -type=DeclarationKind,ScopeKind,ChanDir,sentinel
//go:generate go run generate.go

// Package gc is a Go compiler front end. Work in progess. API not stable.
package gc // import "modernc.org/gc"
