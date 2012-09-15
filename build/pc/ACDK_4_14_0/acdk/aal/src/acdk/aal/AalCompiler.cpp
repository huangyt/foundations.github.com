// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#if 0 // currently not working

#include "AalCompiler.h"
#include "Identifier.h"
#include "ClassDecl.h"
#include "Expressions.h"
#include "SubscribeExpressions.h"
#include "Statements.h"

#include "TypeName.h"
#include "VarName.h"
#include "AalObject.h"

#include <acdk/lang/Void.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/System.h>

namespace acdk {
namespace aal {

using namespace acdk::lang::dmi;

int AalCompiler::ATT_IDENTIFIER = 0;
/*
USING_CLASS(acdk::aci::, Terminal);
USING_CLASS(acdk::aci::, StringTerminal);
USING_CLASS(acdk::aci::, Code);
USING_CLASS(acdk::aci::, TypeName);
USING_CLASS(acdk::aci::, VarName);
USING_CLASS(acdk::aci::, Identifier);
using acdk::aci::ScannerTokenStack;
*/

using namespace acdk::aci;


AalCompiler::AalCompiler()
: Compiler(new acdk::aci::Scanner("", acdk::aci::Scanner::ScanWs))
{
  initStdSyntax();
  initStdTypes();
}

void 
AalCompiler::initStdSyntax()
{
  
  ATT_IDENTIFIER = registerTerminal(new TerminalParseNode("IDENTIFIER", "[a-zA-Z_][a-zA-Z_0-9]*"));
  registerTerminal(new DecimalTerminalParseNode("DEC_LITERAL", "([0-9])+[LISB]?"));
  registerTerminal(new DecimalTerminalParseNode("SIGNED_DEC_LITERAL", "([\\+\\-])?([0-9])+[LISB]?"));
  registerTerminal(new DecimalTerminalParseNode("HEX_LITERAL", "0[xX][0-9a-fA-F]+[LISB]?"));
  registerTerminal(new DecimalTerminalParseNode("OCTAL_LITERAL", "0[0-7]+"));
  
 
  /*
  [eE]([\\+\\-])? ([0-9])+
  ([0-9])+\\.([0-9])* ([eE]([\\+\\-])? ([0-9])+)?|\.([0-9])+([eE]([\\+\\-])? ([0-9])+)?|([0-9])+([eE]([\\+\\-])? ([0-9])+)? 
  */
  // this not work, because it matches agains "i" registerTerminal("FLOATING_POINT_LITERAL", new Terminal("([0-9])+\\.([0-9])*([eE]([\\+\\-])?([0-9])+)?|\\.([0-9])+([eE]([\\+\\-])?([0-9])+)?|([0-9])+([eE]([\\+\\-])? ([0-9])+)?"));
  registerTerminal(new FloatTerminalParseNode("FLOATING_POINT_LITERAL", "([0-9])+\\.([0-9])+"));
  
  registerTerminal("STRING_LITERAL", new StringTerminal());
  registerTerminal("CXX_COMMENT", new CxxCommentTerminal());
  registerTerminal("C_COMMENT", new CCommentTerminal());
  registerTerminal("OPERATOR", new OperatorTerminal());
  registerRule(new BooleanLiteralParseNode("BOOLEAN_LITERAL", "'true' $ | 'false' $ | 'nil' $"));
  registerRule(new OperatorParseNode("Operator", 
                              "( '+' | '-' | '*' | '/' | '.' | "
                              "'<<=' | '>>=' | '<<' | '>>' | ',' | '!' | '~' | "
                              "'==' | '=' | '+=' | '-=' | '<' | '>' | '<=' | '>=' | '==' | '!=' | "
                              "'[' ']' | '(' ')' | OPERATOR) $" ));

  registerRule(new LiteralParseNode("Literal", "( BOOLEAN_LITERAL | STRING_LITERAL | HEX_LITERAL | FLOATING_POINT_LITERAL  | OCTAL_LITERAL | SIGNED_DEC_LITERAL ) %"));

  topNode = new CodeTextParseNode("CodeText", "Statements");

  registerRule(topNode);
  
  registerRule(new ParseNode("TypeDecl", "ClassDeclDef | InterfaceDeclDef | FunctionDeclDef"));
  registerRule(new ParseNode("VarDecl", "LVarDecl")); //| FunctionDeclDef
  registerRule(new LVarDeclParseNode("LVarDecl", "FqTypeName VarName ( ';'& | VarInitializer ';'& ) $"));
   

  
  registerRule(new ParseNode("VarInitializer", "'='& ( AssignmentExpr | ConditionalExpr ) $"));
  
  
  registerRule(new BlockParseNode("Block", "'{'& Statements '}'& $"));
  registerRule(new ParseNode("Statements", "( Statement )*"));
  registerRule(new ParseNode("Statement", "Block \n |"
                                     "ReturnStatement \n |"
                                     "IfStatement \n |"
                                     "ContinueStatement \n |"
                                     "BreakStatement \n |"
                                     "DoStatement \n |"
                                     "WhileStatement \n |"
                                     "ForStatement \n |"
                                     "GotoStatement \n |"
                                     "ThrowStatement \n |"
                                     "TryCatchStatement \n |"
                                     "LabeledStatement\n |"
                                     "SwitchStatement \n |"
                                     "DefunDecl \n | "
                                     "ExtendStatement\n |"
                                     "VarDecl \n | "
                                     "TypeDecl \n | "
                                     "NamespaceDecl \n | "
                                     "UsingDecl \n | "
                                     "ExprStatement \n |"
                                     "EmptyStatement"
                                     ), true);
  registerRule(new ExprStatementParseNode("ExprStatement", "Expression ';'& $"));
  
  registerRule(new NamespaceDeclParseNode("NamespaceDecl", "'namespace'& TypeName '{'& Statements '}'& $")); 
  registerRule(new UsingDeclParseNode("UsingDecl", "'using'& FqTypeName ';'& $")); 

  registerRule(new ClassDeclDefParseNode("ClassDeclDef", "'class'& ! TypeName (ClassDef | ';'&) $"));
  registerRule(new ClassDeclDefParseNode("InterfaceDeclDef", "'interface'& ! TypeName (InterfaceDef | ';'&) $"));
  registerRule(new ParseNode("ClassDef", "DerivedDef ClassDeclBody")); 
  registerRule(new ParseNode("InterfaceDef", "InterfaceDerivedDef ClassDeclBody")); 
  registerRule(new ParseNode("DerivedDef", "[ DerivedSuperDef ] [ DerivedInterfaceDef ] $"));
  registerRule(new ParseNode("InterfaceDerivedDef", "[ DerivedInterfaceDef ] $"));
  
  registerRule(new DerivedSuperDefParseNode("DerivedSuperDef", "'extends'& ! FqTypeName $"));
  registerRule(new DerivedInterfaceDefParseNode("DerivedInterfaceDef", "'implements'& ! FqTypeName ( ','& FqTypeName )* $"));
  registerRule(new ParseNode("ClassDeclBody", "'{'& ( ClassDeclElement )* '}'& "));
  registerRule(new ParseNode("ClassDeclElement", "ClassDeclMember | ClassDeclMethod | ClassDeclConstructor | ClassDeclOperator"));
  
  registerRule(new ClassDeclMemberParseNode("ClassDeclMember", "ClassMemberAttr FqTypeName VarName ( ';'& | VarInitializer ';'& ) $"));
  registerRule(new ClassDeclMethodParseNode("ClassDeclMethod", "ClassMemberAttr ReturnType MethodName  FunctionParamsDecl !  ( ';' | FunctionBlock ) $ "));
  registerRule(new ClassDeclMethodParseNode("ClassDeclConstructor", "ClassMemberAttr MethodName FunctionParamsDecl ClassDeclConstructorInitializerList  ( ';' | FunctionBlock ) $"));
  registerRule(new ClassDeclMethodParseNode("ClassDeclOperator", "ClassMemberAttr ReturnType 'operator'&  Operator FunctionParamsDecl !  ( ';' | FunctionBlock ) $ "));
  
  registerRule(new DefunDeclParseNode("DefunDecl", "'defun'& TypeName ClassMemberAttr ReturnType FunctionParamsDecl ';'& $"));

  registerRule(new ParseNode("ReturnType", "FqTypeName $"));
  registerRule(new VarNameParseNode("MethodName", "IDENTIFIER $"));
  registerRule(new ClassDeclConstructorInitializerListParseNode("ClassDeclConstructorInitializerList", " [ ':'& ClassDeclConstructorInitializer ( ','& ClassDeclConstructorInitializer )* ] $"));
  registerRule(new ClassDeclConstructorInitializerParseNode("ClassDeclConstructorInitializer",  "FqTypeName Arguments $"));
  
  registerRule(new ClassMemberAttrParseNode("ClassMemberAttr", "( 'public' | 'protected' | 'private' | 'foreign' | 'static' )* %"));
  //registerRule(new ParseNode("FunctionDeclDef", "FunctionDecl ( ';' | FunctionBlock ) $"));
  registerRule(new ParseNode("FunctionDeclDef", "ClassDeclMethod $"));
  
  //registerRule(new ParseNode("FunctionDecl", "FqTypeName VarName FunctionParamsDecl $"));

  //registerRule(new ParseNode("FunctionDeclParamList",  "'('& ')'& | '('& StandardParams ')'&"));
  registerRule(new FunctionParamsDeclParseNode("FunctionParamsDecl", "'('& ')'& $ |  '('& Parameter ( ','& Parameter )* ')'& $"));
  registerRule(new ParameterParseNode("Parameter", "FqTypeName VarName $"));
  //registerRule(new ParseNode("FunctionBody",  "FunctionBlock $"));
  registerRule(new FunctionBlockParseNode("FunctionBlock", "Block $"));
  registerRule(new ArgumentsParseNode("Arguments", "'('& ')'& $ | '('& ArgumentList ')'& $"));
  registerRule(new ParseNode("ArgumentList",  "Argument ( ','& Argument )* %"));
  registerRule(new ArgumentParseNode("Argument", "[ Label ':'& ] Expression $"));
  //registerRule(new ParseNode("NamedArgLabel",  "Label ':'& $"));
  

  /// acdk.lang.Object
  registerRule(new FqTypeNameParseNode("FqTypeName", "TypeName ( '.'& FqTypeName )* ArrayDims  $"));
  registerRule(new ParseNode("ArrayDims", "( ArrayDim )* %"));
  registerRule(new ParseNode("ArrayDim", "'['& ']'& $"));
  
  //registerRule("Expression", new ParseNode("BaseExpr ( ',' BaseExpr )* %"));
  registerRule(new ParseNode("Expression", "AssignmentExpr"));
  registerRule(new EmptyExpressionParseNode("EmptyExpression", " $"));
  registerRule(new ParseNode("BaseExpr", "AssignmentExpr ")); //| ConditionalExpr
  registerRule(new AssignmentExprParseNode("AssignmentExpr", "ConditionalExpr [ AssignmentOp AssignmentExpr ] %"));
  registerRule(new ParseNode("AssignmentOp", "'=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='")); 
  registerRule(new ConditionalExprParseNode("ConditionalExpr", "LogicalORExpr [ '?'& AssignmentExpr ':'& AssignmentExpr ] %"));
  registerRule(new LogicalExprParseNode("LogicalORExpr", "LogicalANDExpr [ LogicalOROp  LogicalORExpr ] %"));
  registerRule(new ParseNode("LogicalOROp", "'||'"));
  registerRule(new LogicalExprParseNode("LogicalANDExpr", "BitwiseORExpr [ LogicalANDOp  LogicalANDExpr ] %"));
  registerRule(new ParseNode("LogicalANDOp", "'&&'"));
  registerRule(new BitwiseInfixExprParseNode("BitwiseORExpr", "BitwiseXORExpr [ BitwiseOROp BitwiseORExpr ] %"));
  registerRule(new ParseNode("BitwiseOROp", "'|'"));
  registerRule(new BitwiseInfixExprParseNode("BitwiseXORExpr", "BitwiseANDExpr [ BitwiseXOROp BitwiseXORExpr ] %"));
  registerRule(new ParseNode("BitwiseXOROp", "'^'"));
  registerRule(new BitwiseInfixExprParseNode("BitwiseANDExpr", "EqualityExpr [ BitwiseANDOp BitwiseANDExpr ] %"));
  registerRule(new ParseNode("BitwiseANDOp", "'&'"));
  registerRule(new EqualityExprParseNode("EqualityExpr", "EqualsExpr [ EqualityOp EqualityExpr ] %"));
  registerRule(new ParseNode("EqualityOp", "'==' | '!='"));

  registerRule(new EqualsExprParseNode("EqualsExpr", "RelationalExpr [ EqualsOp EqualsExpr ] %"));
  registerRule(new ParseNode("EqualsOp", "'===' | '!=='"));

  registerRule(new RelationalExprParseNode("RelationalExpr", "ShiftExpr [ RelationalOp RelationalExpr ] %"));
  registerRule(new ParseNode("RelationalOp", "'<' | '>' | '<=' | '>='"));
  registerRule(new BitwiseInfixExprParseNode("ShiftExpr", "AdditiveExpr [ ShiftOp ShiftExpr ] %"));
  registerRule(new ParseNode("ShiftOp", "'>>' | '>>>' | '<<'"));
  registerRule(new AdditiveExprParseNode("AdditiveExpr", "MultiplicativeExpr [ AdditiveOp AdditiveExpr ]  %"
                                                , "< Val, < String (operator), < Val, > InVal"));

  registerRule(new ParseNode("AdditiveOp", "'+' | '-'"));
  registerRule(new MultiplicativeExprParseNode("MultiplicativeExpr", "CastExpr [ MultiplicativeOp MultiplicativeExpr ] %"));
  registerRule(new ParseNode("MultiplicativeOp", "'*' | '%' | '/' "));

  registerRule(new CastExprParseNode("CastExpr", "'('& FqTypeName ')'&  CastExpr $ | PrefixExpr"));                                        
  /*
  registerRule(new ParseNode("UnaryExpr", "'++' CastExpr \n |"
                                       "'--' CastExpr \n |"
                                       "'-' CastExpr \n |"
                                       "'+' CastExpr \n |"
                                       "'~' CastExpr \n |"
                                       "'!' CastExpr \n |" 
                                       "PostfixExpr"
                                         ));
  */
  registerRule(new PrefixExprParseNode("PrefixExpr", "NewExpr | ClosureExpr | [ PrefixOp ] % PostfixExpr"));
  registerRule(new ParseNode("PrefixOp", "'++' | '--' | '-' | '+' | '~' | '!'"));

  registerRule(new NewExprParseNode("NewExpr", "'new'& FqTypeName ( Arguments ) $"));
  registerRule(new ClosureExprParseNode("ClosureExpr", "'new'& ClosureArguments ClassDef $"));
  registerRule(new ParseNode("ClosureArguments", "'('& ')'& $ | '('& ClosureArgumentList ')'&  $"));
  registerRule(new ParseNode("ClosureArgumentList", "VarName ( ','& VarName )* %"));
  registerRule(new ParseNode("NewArrayArguments", "( NewArrayArgument )+ $"));
  registerRule(new ParseNode("NewArrayArgument", "'['& [ Expression ] ']'& $"));
  

  

  registerRule(new PostfixExprParseNode("PostfixExpr", "SubscribeExpr [ PostfixOp ] % [ PostfixExpr ] "));
  registerRule(new ParseNode("PostfixOp", "'++' | '--' $"));
   registerRule(new SubscribeExprParseNode("SubscribeExpr", "PrimaryExpr ( ArraySubscribeExpr | MemberSubscribeExpr | FuncSubscribeExpr  )* %"));
  registerRule(new FuncSubscribeExprParseNode("FuncSubscribeExpr", "Arguments $"));
  registerRule(new ArraySubscribeExprParseNode("ArraySubscribeExpr", "'['& Expression ']'& $"));
  registerRule(new MemberSubscribeExprParseNode("MemberSubscribeExpr", "MemberSubscribeOp SubscribeExpr $"));
  /*
  registerRule(new SubscribeExprParseNode("SubscribeExpr", "( ArraySubscribeExpr | MemberSubscribeExpr | FuncSubscribeExpr  ) | PrimaryExpr "));
  registerRule(new FuncSubscribeExprParseNode("FuncSubscribeExpr", "PrimaryExpr Arguments [ PostfixExpr ] $"));
  registerRule(new ArraySubscribeExprParseNode("ArraySubscribeExpr", "PrimaryExpr '['& Expression ']'& [ PostfixExpr ] $"));
  registerRule(new MemberSubscribeExprParseNode("MemberSubscribeExpr", "PrimaryExpr MemberSubscribeOp PostfixExpr $"));
  */
  registerRule(new ParseNode("MemberSubscribeOp", "'.' | '->'"));
  registerRule(new ParseNode("PrimaryExpr", "'('& Expression ')'& | 'operator'&  Operator | VarName | Literal")); 
  registerRule(new ReturnStatementParseNode("ReturnStatement", "'return'& [ Expression ]';'& $"));
  registerRule(new BreakStatementParseNode("ContinueStatement", "'continue' ';'& $"));
  registerRule(new BreakStatementParseNode("BreakStatement", "'break' ';'& $"));
  

  registerRule(new IfStatementParseNode("IfStatement", "'if'& ! '('& Expression ')'& Statement [ ElseStatement ]  $"));
  registerKeyword("if");

  registerRule(new ParseNode("ElseStatement", "'else'& ! Statement $"));
  
  registerRule(new DoStatementParseNode("DoStatement", "'do'& ! Statement 'while'& '('& Expression ')'& ';'& $ "));
 
  registerRule(new WhileStatementParseNode("WhileStatement", "'while'& '('& Expression ')'& Statement $ "));
  registerRule(new GotoStatementParseNode("GotoStatement", "'goto'& Label ';'& $ "));
  registerRule(new LabeledStatementParseNode("LabeledStatement", "Label ':'& [ Statement ] $ "));
  registerRule(new ParseNode("EmptyStatement", "';'& $ "));
  registerRule(new ForStatementParseNode("ForStatement", "'for'& ! '('& ( EmptyStatement| VarDecl | ExprStatement ) ( Expression | EmptyExpression) ';'& ( Expression | EmptyExpression ) ')'& Statement $ "));
  registerRule(new SwitchStatementParseNode("SwitchStatement", "'switch'& '('& Expression ')'& '{'& ( CaseClause )* '}'& $ "));
  registerRule(new CaseClauseParseNode("CaseClause", "( 'case' Expression | 'default' ) ':'& ( Statement )* $"));
  
  
  registerRule(new ThrowStatementParseNode("ThrowStatement", "'throw'& [ Expression ] ';'& $"));
  registerRule(new TryCatchStatementParseNode("TryCatchStatement", "'try'& Statement ( CatchBlock )* [ FinallyBlock ] $"));
  registerRule(new CatchBlockParseNode("CatchBlock", "'catch'& '('& FqTypeName VarName ')'& Statement $"));
  registerRule(new FinallyBlockParseNode("FinallyBlock", "'finally'& Statement $"));
  
  registerRule(new ExtendStatementParseNode("ExtendStatement", "'extend' ClassMemberAttr ReturnType FqTypeName FunctionParamsDecl FunctionBlock $"));

  //registerRule(new ParseNode("FqIdentifier", "IDENTIFIER ( '.'& IDENTIFIER )* %"));
  registerRule(new TypeNameParseNode("TypeName", "IDENTIFIER $"));
  registerRule(new VarNameParseNode("VarName", "IDENTIFIER $"));
  registerRule(new LabelParseNode("Label", "IDENTIFIER $"));
  
  registerRule(new LeftHandVariableParseNode("LeftHandVariable", "Identifier $", 
                                          "< String; > LValue"));
  
  registerRule(new VariableParseNode("Variable", "Identifier $", 
                                    "< String; > LValue"));
  registerRule(new IdentifierParseNode("Identifier", "IDENTIFIER $"));

}

void
AalCompiler::initStdTypes()
{
  if (_globals == Nil)
    _globals = new GlobalSymbolTable();
  
  //RCodeWithSymbolTable ct = new CodeWithSymbolTable(new ParseNode("CodeText", "Statements"));
  //codeStack().top()  = Nil;
  _globals->newType(DClazzInfo::getInstance(Integer::getTYPE()->objectClazzInfo()));
  _globals->newType(DClazzInfo::getInstance(Void::getTYPE()->objectClazzInfo()));
  _globals->newType(DClazzInfo::getInstance(Character::getTYPE()->objectClazzInfo()));
  _globals->newType(DClazzInfo::getInstance(Byte::getTYPE()->objectClazzInfo()));
  _globals->newType(DClazzInfo::getInstance(Boolean::getTYPE()->objectClazzInfo()));
  _globals->newType(DClazzInfo::getInstance(Short::getTYPE()->objectClazzInfo()));
  _globals->newType(DClazzInfo::getInstance(Long::getTYPE()->objectClazzInfo()));
  _globals->newType(DClazzInfo::getInstance(Float::getTYPE()->objectClazzInfo()));
  _globals->newType(DClazzInfo::getInstance(Double::getTYPE()->objectClazzInfo()));
  //codeStack().push(Nil);
}

void 
AalCompiler::initializeForEvaluation(IN(acdk::aci::REvalEnv) env)
{
  //AalObject::initStatics(env);
}

ScriptVar 
AalCompiler::compInvokeMethod(IN(REvalEnv) env, IN(RObject) target, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  int dmiflags = getDmiFlags(invokeflags);
  if (target != Nil && instanceof(target, AalObject) == true && (dmiflags & MiIvWeakBind) == false)
  {
   
    return target->invokeMethod(funcname, args, dc, Nil, dmiflags);
  }
  return Compiler::compInvokeMethod(env, target, funcname, args, namedArgs, dc, invokeflags);
}

ScriptVar 
AalCompiler::compInvokeStaticMethod(IN(REvalEnv) env, IN(RString) classname, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  RClass cls = Class::forName(classname);
  const acdk::lang::dmi::ClazzInfo* ci = cls->objectClazzInfo();
  int dmiflags = getDmiFlags(invokeflags); 
  if (AalObject::isAalClazz(ci) == true  && (dmiflags & MiIvWeakBind) == false)
  {
    
    // ## TODO performance, not duplicated search for class may invoke ci->static_dispatch directly
    return StdDispatch::invokeStaticMethod(classname, funcname, args, dc, namedArgs, dmiflags);
  }
  return Compiler::compInvokeStaticMethod(env, classname, funcname, args, namedArgs, dc, invokeflags);
}

ScriptVar 
AalCompiler::compInvokeHashMethod(IN(REvalEnv) env, IN(RObject) target, int methodhash, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  int dmiflags = getDmiFlags(invokeflags);
  if (target != Nil && instanceof(target, AalObject) == true && (dmiflags & MiIvWeakBind) == false)
  {
    ScriptVar erg;
    const acdk::lang::dmi::ClazzInfo* ci = target->getClazzInfo();
    target->standardDispatch("", erg, args, dc, namedArgs, dmiflags, ci, (const acdk::lang::dmi::ClazzMethodInfo*)methodhash);
    return erg;
  }
  return Compiler::compInvokeHashMethod(env, target, methodhash, args, namedArgs, dc, invokeflags);
}

ScriptVar 
AalCompiler::compInvokeStaticHashMethod(IN(REvalEnv) env, IN(RString) classname, int methodhash, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  RClass cls = Class::forName(classname);
  const acdk::lang::dmi::ClazzInfo* ci = cls->objectClazzInfo();
  int dmiflags = getDmiFlags(invokeflags); 
  if (AalObject::isAalClazz(ci) == true  && (dmiflags & MiIvWeakBind) == false)
  {
    
    // ## TODO performance, not duplicated search for class may invoke ci->static_dispatch directly
    ScriptVar ret;
    StdDispatch::StandardDispatch(classname, "", ret, args, dc, namedArgs, 
                                  dmiflags | MiIvViaHash, 0, (const acdk::lang::dmi::ClazzMethodInfo*)methodhash);
    return ret;    
  }
  return Compiler::compInvokeStaticHashMethod(env, classname, methodhash, args, namedArgs, dc, invokeflags);
}

AalInterpreter::AalInterpreter() 
{
  _compiler = new AalCompiler();
  _lastEnv = new EvalEnv(&_compiler);
}

void 
AalInterpreter::resetEnv()
{
  _lastEnv = new EvalEnv(&_compiler);
  _compiler->_globals = new GlobalSymbolTable();
}



void 
AalInterpreter::parseTreeInterpret(IN(RString) text, IN(RString) initalEl)
{
  _compiler->scanner->setInBuffer(text);
  RCode codetext =  _compiler->parseComplete(initalEl);
  if (codetext == Nil)
  {
    System::out->println("Cannot Parse [" + text + "] begins with [" + initalEl + "]");
  }
  else
  {
    codetext->setSymbolTable(_compiler->_globals);
    System::out->println("Parsed: [" + text + "] to");
    codetext->printCodeTree(::acdk::lang::System::out, "");
    codetext->postParse(&_compiler);
    //codetext->printCodeTree(::acdk::lang::System::out, "");
    RExecutableArray oca = new ExecutableArray(0);
    codetext->emitOpCode(&_compiler, oca);
    for (int i = 0; i < oca->length(); ++i)
    {
      System::out->print(RString("") + i + ": ");
      oca[i]->printOpCode(System::out);
    }
    _compiler->execute(oca, _lastEnv);
  }
}

void 
AalInterpreter::setGlobalVar(IN(RString) name, const acdk::lang::dmi::ScriptVar& value)
{
  RSymbolTable st = _compiler->_globals;
  int tv1 = st->newVarDecl(0, DClazzInfo::getInstance(value.getClazzInfo()), name);
  AfOp(OCO_CLVR, Nil, tv1, "global var").execute(_lastEnv);
   _lastEnv->push(value);
  AfOp(OCO_STORE, Nil, tv1, "global var").execute(_lastEnv);

}

acdk::lang::dmi::ScriptVar 
AalInterpreter::getGlobalVar(IN(RString) name)
{
  RSymbolTable st = _compiler->_globals;
  int idx = st->getVarIndex(name);
  if (idx == -1)
    return acdk::lang::dmi::ScriptVar();
  AfOp(OCO_LOAD, Nil, idx, "read global var").execute(_lastEnv);
  acdk::lang::dmi::ScriptVar erg = _lastEnv->pop();
  return erg;
}



} // aal
} // acdk

#endif // 0
