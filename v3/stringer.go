// Code generated by "stringer -output stringer.go -linecomment -type=Kind"; DO NOT EDIT.

package gc

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[InvalidKind-0]
	_ = x[Array-1]
	_ = x[Bool-2]
	_ = x[Chan-3]
	_ = x[Complex128-4]
	_ = x[Complex64-5]
	_ = x[Float32-6]
	_ = x[Float64-7]
	_ = x[Function-8]
	_ = x[Int-9]
	_ = x[Int16-10]
	_ = x[Int32-11]
	_ = x[Int64-12]
	_ = x[Int8-13]
	_ = x[Interface-14]
	_ = x[Map-15]
	_ = x[Pointer-16]
	_ = x[Slice-17]
	_ = x[String-18]
	_ = x[Struct-19]
	_ = x[Tuple-20]
	_ = x[Uint-21]
	_ = x[Uint16-22]
	_ = x[Uint32-23]
	_ = x[Uint64-24]
	_ = x[Uint8-25]
	_ = x[Uintptr-26]
	_ = x[UntypedBool-27]
	_ = x[UntypedComplex-28]
	_ = x[UntypedFloat-29]
	_ = x[UntypedInt-30]
	_ = x[UntypedNil-31]
	_ = x[UntypedString-32]
}

const _Kind_name = "<invalid type>arrayboolchancomplex128complex64float32float64functionintint16int32int64int8interfacemappointerslicestringstructtupleuintuint16uint32uint64uint8uintptruntyped booluntyped complexuntyped floatuntyped intuntyped niluntyped string"

var _Kind_index = [...]uint8{0, 14, 19, 23, 27, 37, 46, 53, 60, 68, 71, 76, 81, 86, 90, 99, 102, 109, 114, 120, 126, 131, 135, 141, 147, 153, 158, 165, 177, 192, 205, 216, 227, 241}

func (i Kind) String() string {
	if i < 0 || i >= Kind(len(_Kind_index)-1) {
		return "Kind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _Kind_name[_Kind_index[i]:_Kind_index[i+1]]
}