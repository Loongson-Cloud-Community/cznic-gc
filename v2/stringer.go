// Code generated by "stringer -output stringer.go -linecomment -type=Ch"; DO NOT EDIT.

package gc

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[beforeTokens-57344]
	_ = x[ADD_ASSIGN-57345]
	_ = x[AND_ASSIGN-57346]
	_ = x[AND_NOT-57347]
	_ = x[AND_NOT_ASSIGN-57348]
	_ = x[ARROW-57349]
	_ = x[BREAK-57350]
	_ = x[CASE-57351]
	_ = x[CHAN-57352]
	_ = x[CONST-57353]
	_ = x[CONTINUE-57354]
	_ = x[DEC-57355]
	_ = x[DEFAULT-57356]
	_ = x[DEFER-57357]
	_ = x[DEFINE-57358]
	_ = x[ELLIPSIS-57359]
	_ = x[ELSE-57360]
	_ = x[EOF-57361]
	_ = x[EQ-57362]
	_ = x[FALLTHROUGH-57363]
	_ = x[FLOAT_LIT-57364]
	_ = x[FOR-57365]
	_ = x[FUNC-57366]
	_ = x[GE-57367]
	_ = x[GO-57368]
	_ = x[GOTO-57369]
	_ = x[IDENTIFIER-57370]
	_ = x[IF-57371]
	_ = x[IMAG-57372]
	_ = x[IMPORT-57373]
	_ = x[INC-57374]
	_ = x[INTERFACE-57375]
	_ = x[INT_LIT-57376]
	_ = x[LAND-57377]
	_ = x[LE-57378]
	_ = x[LOR-57379]
	_ = x[MAP-57380]
	_ = x[MUL_ASSIGN-57381]
	_ = x[NE-57382]
	_ = x[OR_ASSIGN-57383]
	_ = x[PACKAGE-57384]
	_ = x[QUO_ASSIGN-57385]
	_ = x[RANGE-57386]
	_ = x[REM_ASSIGN-57387]
	_ = x[RETURN-57388]
	_ = x[RUNE_LIT-57389]
	_ = x[SELECT-57390]
	_ = x[SHL-57391]
	_ = x[SHL_ASSIGN-57392]
	_ = x[SHR-57393]
	_ = x[SHR_ASSIGN-57394]
	_ = x[STRING_LIT-57395]
	_ = x[STRUCT-57396]
	_ = x[SUB_ASSIGN-57397]
	_ = x[SWITCH-57398]
	_ = x[TYPE-57399]
	_ = x[VAR-57400]
	_ = x[XOR_ASSIGN-57401]
	_ = x[afterTokens-57402]
}

const _Ch_name = "beforeTokens+=&=&^&^=<-breakcasechanconstcontinue--defaultdefer:=...elseend of file==fallthroughfloating point literalforfunc>=gogotoidentifierif123.45iimport++interfaceinteger literal&&<=||map*=!=|=package/=range%=returnrune literalselect<<<<=>>>>=string literalstruct-=switchtypevar^=afterTokens"

var _Ch_index = [...]uint16{0, 12, 14, 16, 18, 21, 23, 28, 32, 36, 41, 49, 51, 58, 63, 65, 68, 72, 83, 85, 96, 118, 121, 125, 127, 129, 133, 143, 145, 152, 158, 160, 169, 184, 186, 188, 190, 193, 195, 197, 199, 206, 208, 213, 215, 221, 233, 239, 241, 244, 246, 249, 263, 269, 271, 277, 281, 284, 286, 297}

func (i Ch) String() string {
	i -= 57344
	if i < 0 || i >= Ch(len(_Ch_index)-1) {
		return "Ch(" + strconv.FormatInt(int64(i+57344), 10) + ")"
	}
	return _Ch_name[_Ch_index[i]:_Ch_index[i+1]]
}
