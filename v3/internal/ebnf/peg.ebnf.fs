                       AliasDecl = IDENT
                       Arguments = LPAREN
                     ArrayLength = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                       ArrayType = LBRACK
                      Assignment = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
              AssignmentPreBlock = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                        BaseType = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                        BasicLit = CHAR, FLOAT, IMAG, INT, STRING
                           Block = LBRACE
                       BreakStmt = BREAK
                         Channel = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                     ChannelType = ARROW, CHAN
                        CommCase = CASE, DEFAULT
                      CommClause = CASE, DEFAULT
                    CompositeLit = IDENT, LBRACK, MAP, STRUCT
            CompositeLitPreBlock = LBRACK, MAP, STRUCT
                       Condition = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                       ConstDecl = CONST
                       ConstSpec = IDENT
                    ContinueStmt = CONTINUE
                      Conversion = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                     Declaration = CONST, TYPE, VAR
                       DeferStmt = DEFER
                         Element = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                     ElementList = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                     ElementType = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                   EmbeddedField = IDENT, MUL
                       EmptyStmt =  /* ε */
                  ExprCaseClause = CASE, DEFAULT
                  ExprSwitchCase = CASE, DEFAULT
                  ExprSwitchStmt = SWITCH
                      Expression = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
              ExpressionPreBlock = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                  ExpressionList = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
          ExpressionListPreBlock = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                  ExpressionStmt = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
          ExpressionStmtPreBlock = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                 FallthroughStmt = FALLTHROUGH
                       FieldDecl = IDENT, MUL
                       ForClause = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, SEMICOLON, STRING, STRUCT, SUB, XOR
                         ForStmt = FOR
                    FunctionBody = LBRACE
                    FunctionDecl = FUNC
                     FunctionLit = FUNC
                    FunctionName = IDENT
                    FunctionType = FUNC
                          GoStmt = GO
                        GotoStmt = GOTO
                  IdentifierList = IDENT
                          IfStmt = IF
                      ImportDecl = IMPORT
                      ImportPath = STRING
                      ImportSpec = IDENT, PERIOD, STRING
                      IncDecStmt = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                           Index = LBRACK
                        InitStmt = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */
                   InterfaceElem = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE
                   InterfaceType = INTERFACE
                         KeyType = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                    KeyedElement = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                           Label = IDENT
                     LabeledStmt = IDENT
                         Literal = CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, MAP, STRING, STRUCT
                 LiteralPreBlock = CHAR, FLOAT, FUNC, IMAG, INT, LBRACK, MAP, STRING, STRUCT
                     LiteralType = IDENT, LBRACK, MAP, STRUCT
             LiteralTypePreBlock = LBRACK, MAP, STRUCT
                    LiteralValue = LBRACE
                         MapType = MAP
                      MethodDecl = FUNC
                      MethodElem = IDENT
                      MethodExpr = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                      MethodName = IDENT
                         Operand = CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, LPAREN, MAP, STRING, STRUCT
                 OperandPreBlock = CHAR, FLOAT, FUNC, IDENT, IMAG, INT, LBRACK, LPAREN, MAP, STRING, STRUCT
                     OperandName = IDENT
                   PackageClause = PACKAGE
                     PackageName = IDENT
                   ParameterDecl = ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                   ParameterList = ARROW, CHAN, ELLIPSIS, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                      Parameters = LPAREN
                     PointerType = MUL
                        PostStmt = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */
                     PrimaryExpr = ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT
             PrimaryExprPreBlock = ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT
                  QualifiedIdent = IDENT
                     RangeClause = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, RANGE, STRING, STRUCT, SUB, XOR
                        Receiver = LPAREN
                    ReceiverType = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                        RecvExpr = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                        RecvStmt = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                          Result = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                      ReturnStmt = RETURN
                      SelectStmt = SELECT
                        Selector = PERIOD
                        SendStmt = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                SendStmtPreBlock = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                    ShortVarDecl = IDENT
            ShortVarDeclPreBlock = IDENT
                       Signature = LPAREN
                      SimpleStmt = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */
              SimpleStmtPreBlock = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR /* ε */
                           Slice = LBRACK
                       SliceType = LBRACK
                      SourceFile = PACKAGE
                       Statement = ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */
                   StatementList = ADD, AND, ARROW, BREAK, CHAN, CHAR, CONST, CONTINUE, DEFER, FALLTHROUGH, FLOAT, FOR, FUNC, GO, GOTO, IDENT, IF, IMAG, INT, INTERFACE, LBRACE, LBRACK, LPAREN, MAP, MUL, NOT, RETURN, SELECT, SEMICOLON, STRING, STRUCT, SUB, SWITCH, TYPE, VAR, XOR /* ε */
                      StructType = STRUCT
                      SwitchStmt = SWITCH
                             Tag = STRING
                    TopLevelDecl = CONST, FUNC, TYPE, VAR
                            Type = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                        TypeArgs = LBRACK
                   TypeAssertion = PERIOD
                  TypeCaseClause = CASE, DEFAULT
                  TypeConstraint = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE
                        TypeDecl = TYPE
                         TypeDef = IDENT
                        TypeElem = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE
                        TypeList = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT
                         TypeLit = ARROW, CHAN, FUNC, INTERFACE, LBRACK, MAP, MUL, STRUCT
                        TypeName = IDENT
                   TypeParamDecl = IDENT
                   TypeParamList = IDENT
                  TypeParameters = LBRACK
                        TypeSpec = IDENT
                  TypeSwitchCase = CASE, DEFAULT
                 TypeSwitchGuard = ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRING, STRUCT
                  TypeSwitchStmt = SWITCH
                        TypeTerm = ARROW, CHAN, FUNC, IDENT, INTERFACE, LBRACK, LPAREN, MAP, MUL, STRUCT, TILDE
                       UnaryExpr = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
               UnaryExprPreBlock = ADD, AND, ARROW, CHAN, CHAR, FLOAT, FUNC, IDENT, IMAG, INT, INTERFACE, LBRACK, LPAREN, MAP, MUL, NOT, STRING, STRUCT, SUB, XOR
                  UnderlyingType = TILDE
                         VarDecl = VAR
                         VarSpec = IDENT