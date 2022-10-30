AddOp case '+', '-', '^', '|':
AliasDecl case IDENT:
Arguments case '(':
ArrayLength case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ArrayType case '[':
AssignOp case '=', ADD_ASSIGN, AND_ASSIGN, AND_NOT_ASSIGN, MUL_ASSIGN, OR_ASSIGN, QUO_ASSIGN, REM_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, SUB_ASSIGN, XOR_ASSIGN:
Assignment case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
AssignmentPreBlock case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
BaseType case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
BasicLit case CHAR, FLOAT, IMAG, INT, STRING:
BinaryOp case '%', '&', '*', '+', '-', '/', '<', '>', '^', '|', AND_NOT, EQL, GEQ, LAND, LEQ, LOR, NEQ, SHL, SHR:
Block case '{':
BreakStmt case BREAK:
Channel case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ChannelType case ARROW, CHAN:
CommCase case CASE, DEFAULT:
CommClause case CASE, DEFAULT:
CompositeLit case '[', IDENT, MAP, STRUCT:
Condition case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ConstDecl case CONST:
ConstSpec case IDENT:
ContinueStmt case CONTINUE:
Conversion case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
Declaration case CONST, TYPE, VAR:
DeferStmt case DEFER:
Element case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ElementList case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ElementType case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
EmbeddedField case '*', IDENT:
EmptyStmt case ε:
ExprCaseClause case CASE, DEFAULT:
ExprSwitchCase case CASE, DEFAULT:
ExprSwitchStmt case SWITCH:
Expression case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ExpressionList case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ExpressionListPreBlock case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ExpressionPreBlock case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ExpressionStmt case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ExpressionStmtPreBlock case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
FallthroughStmt case FALLTHROUGH:
FieldDecl case '*', IDENT:
FieldName case IDENT:
ForClause case '!', '&', '(', '*', '+', '-', ';', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ForStmt case FOR:
FunctionBody case '{':
FunctionDecl case FUNC:
FunctionLit case FUNC:
FunctionName case IDENT:
FunctionType case FUNC:
GoStmt case GO:
GotoStmt case GOTO:
IdentifierList case IDENT:
IfStmt case IF:
ImportDecl case IMPORT:
ImportPath case STRING:
ImportSpec case '.', IDENT, STRING:
IncDecStmt case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
Index case '[':
InitStmt case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT, ε:
InterfaceElem case '(', '*', '[', '~', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
InterfaceType case INTERFACE:
Key case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
KeyType case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
KeyedElement case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
Label case IDENT:
LabeledStmt case IDENT:
Literal case '[', CHAR, FLOAT, FUNC, IDENT, IMAG, INT, MAP, STRING, STRUCT:
LiteralPreBlock case CHAR, FLOAT, FUNC, IMAG, INT, STRING:
LiteralType case '[', IDENT, MAP, STRUCT:
LiteralValue case '{':
MapType case MAP:
MethodDecl case FUNC:
MethodElem case IDENT:
MethodExpr case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
MethodName case IDENT:
MulOp case '%', '&', '*', '/', AND_NOT, SHL, SHR:
Operand case '(', '[', CHAR, FLOAT, FUNC, IDENT, IMAG, INT, MAP, STRING, STRUCT:
OperandName case IDENT:
OperandPreBlock case '(', CHAR, FLOAT, FUNC, IDENT, IMAG, INT, STRING:
PackageClause case PACKAGE:
PackageName case IDENT:
ParameterDecl case '(', '*', '[', ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, MAP, STRUCT:
ParameterList case '(', '*', '[', ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, MAP, STRUCT:
Parameters case '(':
PointerType case '*':
PostStmt case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT, ε:
PrimaryExpr case '(', '*', '[', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
PrimaryExprPreBlock case '(', '*', '[', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
QualifiedIdent case IDENT:
RangeClause case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, RANGE, STRING, STRUCT:
Receiver case '(':
ReceiverType case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
RecvExpr case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
RecvStmt case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
RelOp case '<', '>', EQL, GEQ, LEQ, NEQ:
Result case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
ReturnStmt case RETURN:
SelectStmt case SELECT:
Selector case '.':
SendStmt case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
SendStmtPreBlock case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
ShortVarDecl case IDENT:
ShortVarDeclPreBlock case IDENT:
Signature case '(':
SimpleStmt case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT, ε:
SimpleStmtPreBlock case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT, ε:
Slice case '[':
SliceType case '[':
SourceFile case PACKAGE:
Statement case '!', '&', '(', '*', '+', '-', '[', '^', '{', ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, MAP, RETURN, SELECT, STRING, STRUCT, SWITCH, TYPE, VAR, ε:
StatementList case '!', '&', '(', '*', '+', '-', ';', '[', '^', '{', ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, MAP, RETURN, SELECT, STRING, STRUCT, SWITCH, TYPE, VAR, ε:
StructType case STRUCT:
SwitchStmt case SWITCH:
Tag case STRING:
TopLevelDecl case CONST, FUNC, TYPE, VAR:
Type case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
TypeArgs case '[':
TypeAssertion case '.':
TypeCaseClause case CASE, DEFAULT:
TypeConstraint case '(', '*', '[', '~', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
TypeDecl case TYPE:
TypeDef case IDENT:
TypeElem case '(', '*', '[', '~', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
TypeList case '(', '*', '[', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
TypeLit case '*', '[', ARROW, CHAN, FUNC, INTERFACE, MAP, STRUCT:
TypeName case IDENT:
TypeParamDecl case IDENT:
TypeParamList case IDENT:
TypeParameters case '[':
TypeSpec case IDENT:
TypeSwitchCase case CASE, DEFAULT:
TypeSwitchGuard case '(', '*', '[', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
TypeSwitchStmt case SWITCH:
TypeTerm case '(', '*', '[', '~', ARROW, CHAN, FUNC, IDENT, INTERFACE, MAP, STRUCT:
UnaryExpr case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
UnaryExprPreBlock case '!', '&', '(', '*', '+', '-', '[', '^', ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, MAP, STRING, STRUCT:
UnaryOp case '!', '&', '*', '+', '-', '^', ARROW:
UnderlyingType case '~':
VarDecl case VAR:
VarSpec case IDENT:
float_lit case FLOAT:
identifier case IDENT:
imaginary_lit case IMAG:
int_lit case INT:
rune_lit case CHAR:
string_lit case STRING: