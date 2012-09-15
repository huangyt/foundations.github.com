/*
TESTNODE: 'a'& XTOKEN* %; // XTOKEN; // | 'c' 'd' | 'e' | 'f';
XTOKEN: 'b';
*/

/*
  Syntax definition for AAL language
*/
{
  acdk.aci.parser.WhiteSpaceParseNode.registerParseNode(compiler);
  compiler.getParseEnv().addIgnoreToken(compiler.getParseNode("WS").getScannerTokenId());
  
  acdk.aci.parser.LineCommentParseNodeParseNode.registerParseNode(compiler);
  compiler.getParseEnv().addIgnoreToken(compiler.getParseNode("LINECOMMENT").getScannerTokenId());
  
  acdk.aci.parser.BlockCommentParseNode.registerParseNode(compiler);
  compiler.getParseEnv().addIgnoreToken(compiler.getParseNode("BLOCKCOMMENT").getScannerTokenId());
  
  acdk.aci.parser.IdentifierParseNode.registerParseNode(compiler);
  compiler.registerTerminal(new acdk.aci.parser.StringTerminalParseNode("STRING_LITERAL"));
  
  compiler.registerTerminal(new acdk.aci.parser.DecimalLiteralParseNode("DEC_LITERAL", "^([0-9])+[LISB]?"));
  compiler.registerTerminal(new acdk.aci.parser.DecimalLiteralParseNode("SIGNED_DEC_LITERAL", "^([\\+\\-])?([0-9])+[LISB]?"));
  compiler.registerTerminal(new acdk.aci.parser.DecimalLiteralParseNode("HEX_LITERAL", "^0[xX][0-9a-fA-F]+[LISB]?"));
  compiler.registerTerminal(new acdk.aci.parser.DecimalLiteralParseNode("OCTAL_LITERAL", "^0[0-7]+"));
  compiler.registerTerminal(new acdk.aci.parser.FloatLiteralParseNode("FLOAT_LITERAL", "^([\\+\\-])?([0-9]+)\\.([0-9]+)"));
  
  #include "AalHelper.csf"
  #include "aal_IfStm.csf"
  #include "aal_EqualExpr.csf"
  #include "aal_LiteralExpr.csf"
}

// CodeText: LINECOMMENT;


Literal: BOOLEAN_LITERAL | DEC_LITERAL | SIGNED_DEC_LITERAL | HEX_LITERAL | FLOAT_LITERAL | OCTAL_LITERAL| STRING_LITERAL ;

BOOLEAN_LITERAL: 'true' $ | 'false' $ | 'nil' | 'Nil' $;

CodeText: Statements;


TypeDecl: ClassDeclDef | InterfaceDeclDef | FunctionDeclDef;

// one line comment
VarDecl: LVarDecl;

LVarDecl: FqTypeName VarName ( ';'& | VarInitializer ';'& ) $;

VarInitializer: '='& ( AssignmentExpr | ConditionalExpr ) $;

Block: '{'& Statements '}'& $;

EvalBlock: '['& CodeText ']'& $;

Statements: ( Statement )*;

Statement
: EvalBlock  
| Block  
| ReturnStatement  
| IfStatement  
| ContinueStatement  
| BreakStatement  
| DoStatement  
| WhileStatement  
| ForStatement  
| GotoStatement  
| ThrowStatement  
| TryCatchStatement  
| LabeledStatement 
| SwitchStatement  
| DefunDecl  
| ExtendStatement 
| VarDecl  
| TypeDecl  
| NamespaceDecl  
| UsingDecl  
| ExprStatement  
| EmptyStatement
;
                                     
ExprStatement: Expression ';'& $;

NamespaceDecl: 'namespace'& TypeName '{'& Statements '}'& $;

UsingDecl: 'using'& FqTypeName ';'& $;

ClassDeclDef: 'class'& ! TypeName (ClassDef | ';'&) $;

InterfaceDeclDef: 'interface'& ! TypeName (InterfaceDef | ';'&) $;

ClassDef: DerivedDef ClassDeclBody;

InterfaceDef: InterfaceDerivedDef ClassDeclBody;

DerivedDef: [ DerivedSuperDef ] [ DerivedInterfaceDef ] $;

InterfaceDerivedDef: [ DerivedInterfaceDef ] $;

DerivedSuperDef: 'extends'& ! FqTypeName $;

DerivedInterfaceDef: 'implements'& ! FqTypeName ( ','& FqTypeName )* $;

ClassDeclBody: '{'& ( ClassDeclElement )* '}'& ;

ClassDeclElement: ClassDeclMember | ClassDeclMethod | ClassDeclConstructor | ClassDeclOperator;

ClassDeclMember: ClassMemberAttr FqTypeName VarName ( ';'& | VarInitializer ';'& ) $;

ClassDeclMethod: ClassMemberAttr ReturnType MethodName  FunctionParamsDecl !  ( ';' | FunctionBlock ) $ ;

ClassDeclConstructor: ClassMemberAttr MethodName FunctionParamsDecl ClassDeclConstructorInitializerList  ( ';' | FunctionBlock ) $;

ClassDeclOperator: ClassMemberAttr ReturnType 'operator'&  Operator FunctionParamsDecl !  ( ';' | FunctionBlock ) $ ;

DefunDecl: 'defun'& TypeName ClassMemberAttr ReturnType FunctionParamsDecl ';'& $;

ReturnType: FqTypeName $;

MethodName: IDENTIFIER $;

ClassDeclConstructorInitializerList:  [ ':'& ClassDeclConstructorInitializer ( ','& ClassDeclConstructorInitializer )* ] $;

ClassDeclConstructorInitializer: FqTypeName Arguments $;

ClassMemberAttr: ( 'public' | 'protected' | 'private' | 'foreign' | 'static' )* %;

FunctionDeclDef: FunctionDecl ; //( ';' | FunctionBlock ) $;

FunctionDecl: ClassDeclMethod $;

FunctionParamsDecl: '('& ')'& $ |  '('& Parameter ( ','& Parameter )* ')'& $;

FunctionBlock: Block $;

Arguments: '('& ')'& $ | '('& ArgumentList ')'& $;

ArgumentList: Argument ( ','& Argument )* %;

Argument: [ Label ':'& ] Expression $;

FqTypeName: TypeName ( '.'& FqTypeName )* ArrayDims  $;

ArrayDims: ( ArrayDim )* %;

ArrayDim: '['& ']'& $;

Expression: AssignmentExpr;

EmptyExpression:  $;

BaseExpr: AssignmentExpr ;

AssignmentExpr: ConditionalExpr [ AssignmentOp AssignmentExpr ] %;

AssignmentOp: '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=';

ConditionalExpr: LogicalORExpr [ '?'& AssignmentExpr ':'& AssignmentExpr ] %;

LogicalORExpr: LogicalANDExpr [ LogicalOROp  LogicalORExpr ] %;

LogicalOROp: '||';

LogicalANDExpr: BitwiseORExpr [ LogicalANDOp  LogicalANDExpr ] %;

LogicalANDOp: '&&';

BitwiseORExpr: BitwiseXORExpr [ BitwiseOROp BitwiseORExpr ] %;

BitwiseOROp: '|';

BitwiseXORExpr: BitwiseANDExpr [ BitwiseXOROp BitwiseXORExpr ] %;

BitwiseXOROp: '^';

BitwiseANDExpr: EqualityExpr [ BitwiseANDOp BitwiseANDExpr ] %;

BitwiseANDOp: '&';

new aal.EqualityExprParseNode(EqualityExpr)
: EqualsExpr [ EqualityOp EqualityExpr ] %
;

EqualityOp: '==' | '!=';

EqualsExpr: RelationalExpr [ EqualsOp EqualsExpr ] %;

EqualsOp: '===' | '!==';

RelationalExpr: ShiftExpr [ RelationalOp RelationalExpr ] %;

RelationalOp: '<' | '>' | '<=' | '>=';

ShiftExpr: AdditiveExpr [ ShiftOp ShiftExpr ] %;

ShiftOp: '>>' | '>>>' | '<<';

AdditiveExpr: MultiplicativeExpr [ AdditiveOp AdditiveExpr ]  %;

AdditiveOp: '+' | '-';

MultiplicativeExpr: CastExpr [ MultiplicativeOp MultiplicativeExpr ] %;

MultiplicativeOp: '*' | '%' | '/' ;

CastExpr: '('& FqTypeName ')'&  CastExpr $ | PrefixExpr;

PrefixExpr: NewExpr | ClosureExpr | [ PrefixOp ] % PostfixExpr;

PrefixOp: '++' | '--' | '-' | '+' | '~' | '!';

NewExpr: 'new'& FqTypeName ( Arguments ) $;

ClosureExpr: 'new'& ClosureArguments ClassDef $;

ClosureArguments: '('& ')'& $ | '('& ClosureArgumentList ')'&  $;

ClosureArgumentList: VarName ( ','& VarName )* %;

NewArrayArguments: ( NewArrayArgument )+ $;

NewArrayArgument: '['& [ Expression ] ']'& $;

PostfixExpr: SubscribeExpr [ PostfixOp ] % [ PostfixExpr ] ;

PostfixOp: '++' | '--' $;

SubscribeExpr: PrimaryExpr ( ArraySubscribeExpr | MemberSubscribeExpr | FuncSubscribeExpr  )* %;

FuncSubscribeExpr: Arguments $;

ArraySubscribeExpr: '['& Expression ']'& $;

MemberSubscribeExpr: MemberSubscribeOp SubscribeExpr $;

MemberSubscribeOp: '.' | '->';

PrimaryExpr: '('& Expression ')'& | 'operator'&  Operator | VarNameExpr | LiteralExpr;

new aal.LiteralExprParseNode(LiteralExpr)
: Literal $
;

VarNameExpr: VarName $;

ReturnStatement: 'return'& [ Expression ]';'& $;

ContinueStatement: 'continue' ';'& $;

BreakStatement: 'break' ';'& $;

new aal.IfStatementParseNode(IfStatement)
: 'if'& ! '('& Expression ')'& Statement [ ElseStatement ]  $
;

ElseStatement: 'else'& ! Statement $;

DoStatement: 'do'& ! Statement 'while'& '('& Expression ')'& ';'& $ ;

WhileStatement: 'while'& '('& Expression ')'& Statement $ ;

GotoStatement: 'goto'& Label ';'& $ ;

LabeledStatement: Label ':'& [ Statement ] $ ;

EmptyStatement: ';'& $ ;

ForStatement: 'for'& ! '('& ( EmptyStatement| VarDecl | ExprStatement ) ( Expression | EmptyExpression) ';'& ( Expression | EmptyExpression ) ')'& Statement $ ;

SwitchStatement: 'switch'& '('& Expression ')'& '{'& ( CaseClause )* '}'& $ ;

CaseClause: ( 'case' Expression | 'default' ) ':'& ( Statement )* $;

ThrowStatement: 'throw'& [ Expression ] ';'& $;

TryCatchStatement: 'try'& Statement ( CatchBlock )* [ FinallyBlock ] $;

CatchBlock: 'catch'& '('& FqTypeName VarName ')'& Statement $;

FinallyBlock: 'finally'& Statement $;

ExtendStatement: 'extend' ClassMemberAttr ReturnType FqTypeName FunctionParamsDecl FunctionBlock $;

TypeName: IDENTIFIER $;

VarName: IDENTIFIER $;

Label: IDENTIFIER $;

LeftHandVariable: Identifier $;

Variable: Identifier $;

Identifier: IDENTIFIER $;

Parameter: FqTypeName VarName $;


Operator: ( '+' | '-' | '*' | '/' | '.' | 
                              '<<=' | '>>=' | '<<' | '>>' | ',' | '!' | '~' | 
                              '==' | '=' | '+=' | '-=' | '<' | '>' | '<=' | '>=' | '==' | '!=' | 
                              '[' ']' | '(' ')' ) $;

