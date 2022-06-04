// Copyright 2021 The Gc Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate stringer -output stringer.go -linecomment -type=Ch

package gc // import "modernc.org/gc/v2"

import (
	"bytes"
)

// nodeSource returns the source form of n. Setting full to false will replace
// every non-empty token separator by a single space character and drop the
// first token separator entirely, if any.
func nodeSource(full bool, n Node) []byte { //TODO n.Source(w io.Writer)
	var b bytes.Buffer
	nodeSource0(&b, n, full)
	return b.Bytes()
}
