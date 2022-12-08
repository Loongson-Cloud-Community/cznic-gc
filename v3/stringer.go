// Code generated by "stringer -output stringer.go -linecomment -type=Kind,guard,ScopeKind,ChanDir,TypeCheck"; DO NOT EDIT.

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
	_ = x[UnsafePointer-27]
	_ = x[UntypedBool-28]
	_ = x[UntypedComplex-29]
	_ = x[UntypedFloat-30]
	_ = x[UntypedInt-31]
	_ = x[UntypedNil-32]
	_ = x[UntypedRune-33]
	_ = x[UntypedString-34]
}

const _Kind_name = "<invalid type>arrayboolchancomplex128complex64float32float64functionintint16int32int64int8interfacemappointerslicestringstructtupleuintuint16uint32uint64uint8uintptrunsafe.Pointeruntyped booluntyped complexuntyped floatuntyped intuntyped niluntyped runeuntyped string"

var _Kind_index = [...]uint16{0, 14, 19, 23, 27, 37, 46, 53, 60, 68, 71, 76, 81, 86, 90, 99, 102, 109, 114, 120, 126, 131, 135, 141, 147, 153, 158, 165, 179, 191, 206, 219, 230, 241, 253, 267}

func (i Kind) String() string {
	if i >= Kind(len(_Kind_index)-1) {
		return "Kind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _Kind_name[_Kind_index[i]:_Kind_index[i+1]]
}
func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[guardUnchecked-0]
	_ = x[guardChecking-1]
	_ = x[guardChecked-2]
}

const _guard_name = "guardUncheckedguardCheckingguardChecked"

var _guard_index = [...]uint8{0, 14, 27, 39}

func (i guard) String() string {
	if i >= guard(len(_guard_index)-1) {
		return "guard(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _guard_name[_guard_index[i]:_guard_index[i+1]]
}
func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[scZero-0]
	_ = x[UniverseScope-1]
	_ = x[PackageScope-2]
	_ = x[FileScope-3]
	_ = x[OtherScope-4]
}

const _ScopeKind_name = "scZeroUniverseScopePackageScopeFileScopeOtherScope"

var _ScopeKind_index = [...]uint8{0, 6, 19, 31, 40, 50}

func (i ScopeKind) String() string {
	if i < 0 || i >= ScopeKind(len(_ScopeKind_index)-1) {
		return "ScopeKind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _ScopeKind_name[_ScopeKind_index[i]:_ScopeKind_index[i+1]]
}
func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[SendRecv-0]
	_ = x[SendOnly-1]
	_ = x[RecvOnly-2]
}

const _ChanDir_name = "SendRecvSendOnlyRecvOnly"

var _ChanDir_index = [...]uint8{0, 8, 16, 24}

func (i ChanDir) String() string {
	if i < 0 || i >= ChanDir(len(_ChanDir_index)-1) {
		return "ChanDir(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _ChanDir_name[_ChanDir_index[i]:_ChanDir_index[i+1]]
}
func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[TypeCheckNone-0]
	_ = x[TypeCheckAll-1]
}

const _TypeCheck_name = "TypeCheckNoneTypeCheckAll"

var _TypeCheck_index = [...]uint8{0, 13, 25}

func (i TypeCheck) String() string {
	if i < 0 || i >= TypeCheck(len(_TypeCheck_index)-1) {
		return "TypeCheck(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _TypeCheck_name[_TypeCheck_index[i]:_TypeCheck_index[i+1]]
}
