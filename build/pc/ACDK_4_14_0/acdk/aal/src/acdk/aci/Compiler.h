// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_Compiler_h
#define acdk_aci_Compiler_h

#include <acdk.h>
#include <acdk/io/PrintWriter.h>

#include "Config.h"
#include "parser/parser.h"
#include "parser/ParseEnv.h"
#include "parser/Scanner.h"
//#include "parser/TerminalParseNode.h"

//#include "Code.h"

#include "vm/EvalEnv.h"
#include "SymbolTable.h"

namespace acdk {
namespace aci {

namespace vm {

ACDK_DECL_CLASS(EvalEnv);
ACDK_DECL_INTERFACE(Executable);

} // vm


ACDK_DECL_CLASS(Compiler);
//ACDK_DECL_CLASS(Code);
//ACDK_DECL_CLASS(TypeVar);
//
//

using acdk::lang::dmi::ScriptVar;
using acdk::lang::dmi::ScriptVarArray;
using acdk::lang::dmi::DmiClient;
using namespace acdk::aci::vm;
using namespace acdk::aci::parser;

class ACDK_ACI_PUBLIC Compiler
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(Compiler)
public:
  typedef acdk::lang::sys::core_stack<RAstNode> CodeStack;
  typedef acdk::lang::sys::core_stack_scope<RAstNode> CodeStackScope;
  
  /// shared with Scanner
  acdk::aci::parser::RParseEnv _parseEnv; 

  RScanner scanner;
  RParseNode topNode;
  RSymbolTable _globals;
  foreign CodeStack _codeStack;
  /** for generic names */
  int _genLabelCounter;
  vm::RExecutableArray _opCodes;
  vm::REvalEnv _compilerEnv;
  //foreign EvalEnv* _compilerEnv;
  
  Compiler(IN(RScanner) sc = Nil);
  
  acdk::aci::parser::RParseEnv getParseEnv() { return _parseEnv; }

  foreign CodeStack& codeStack() { return _codeStack; }

  virtual void registerSyntaxRule(IN(RSyntaxParseNode) node, bool notreduce = false);
  /** 
    register a simple keyword.
    Normally called internally while parsing Rules
    @returns the Scanner token identifier
  */
  virtual int registerKeyword(IN(RString) kw);
  /**
    @returns the Scanner token identifier
  */
  virtual int registerTerminal(IN(RTerminalParseNode) code, byte prio = 5);
  //virtual int registerScannerTerminal(IN(RString) symbol, IN(parser::RScannerTerminal) scannerterminal);

  virtual bool isKeyword(IN(RString) kw) { return _parseEnv->isKeyword(kw); }
  bool notReduceRole(IN(RString) rule) { return _parseEnv->notRecudeRole(rule); }
  
  /**
    returns the first registered ParseNode of given rulename
  */
  RParseNode getParseNode(IN(RString) rulename);
  virtual RAstNode parseComplete(IN(RString) codetype);
  RTerminalParseNode getTerminal(IN(RString) rn);
  /**
    in parse phase the current code location
  */
  acdk::aci::util::RCodeLocation getCodeLocation() { return scanner->getClonedCodeLocation(); }
  /**
    @param codetype string of the lex token
    @param parent parent, which invokes the parsing
  */
  virtual RAstNode parseRule(IN(RString) ruleName, IN(RParseNode) parent);
  virtual void printSyntax(IN(acdk::io::RPrintWriter) out);
  /** 
    read a stream of token which only limited by ws
  */
  RString parseOpOrIdent();
  /**
    check if all rules in registered syntax are known
  */
  void checkRules() { _parseEnv->checkRules(this); }
  virtual RDClazzInfo findType(IN(RString) name, IN(RAstNode) code);
  virtual RSemanticElem findSubSem(IN(RString) name, IN(RString) op, IN(RAstNode) code);
  RString genLabel(IN(RString) prefix);
  /**
    replaces labels with pc addresses
  */
  static void resolveLabels(IN(RExecutableArray) oca);
  /**
    return a conter, incremented each call
  */
  int getCounter() { return ++_genLabelCounter; }
  /**
    Will be called while EvalEnv will be constructed
  */
  virtual void initializeForEvaluation(IN(REvalEnv) env) {}
  void execute(IN(RExecutableArray) oca);
  void execute(IN(RExecutableArray) oca, IN(REvalEnv) env);
  void addOpCode(IN(RExecutableArray) oca, IN(RExecutable) ex);
  RAstNode createCode(IN(RString) name);
  /**
    create compiler which shares all 
    information of current compiler
  */
  RCompiler createSubTextParser(IN(RString) codetext, IN(RAstNode) topCode);
  /**
    return an evaluation enviromnet for meta compiling
  */
  REvalEnv getCompilerEnv();
  /**
    return Props inplace syntax evaluation for the SyntaxParserEnv
  */
  acdk::cfgscript::RProps getSyntaxParserEnv();

  foreign virtual void compInvokeMethod(IN(REvalEnv) env);

  foreign virtual ScriptVar compInvokeMethod(IN(REvalEnv) env, IN(RObject) target, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
  
  foreign virtual ScriptVar compInvokeStaticMethod(IN(REvalEnv) env, IN(RString) classname, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
  foreign virtual void compInvokeHashMethod(IN(REvalEnv) env);
  foreign virtual ScriptVar compInvokeHashMethod(IN(REvalEnv) env, IN(RObject) target, int methodhash, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
  foreign virtual void compInvokeStaticHashMethod(IN(REvalEnv) env);
  foreign virtual ScriptVar compInvokeStaticHashMethod(IN(REvalEnv) env, IN(RString) classname, int methodhash, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
  foreign virtual void compInvokeStaticMethod(IN(REvalEnv) env);

  foreign virtual void compNewObject(IN(REvalEnv) env);
  foreign virtual ScriptVar compNewObject(IN(REvalEnv) env, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
  foreign virtual void compPeek(IN(REvalEnv) env);
  foreign virtual ScriptVar compPeek(IN(REvalEnv) env, IN(RObject) target, IN(RString) fieldname, DmiClient& dc, jlong invokeflags);
  foreign virtual void compPoke(IN(REvalEnv) env);
  foreign virtual void compPoke(IN(REvalEnv) env, IN(RObject) target, IN(RString) fieldname, IN(ScriptVar) value, DmiClient& dc, jlong invokeflags);
  foreign virtual void compPeekStatic(IN(REvalEnv) env);
  foreign virtual ScriptVar compPeekStatic(IN(REvalEnv) env, IN(RString) classname, IN(RString) fieldname, DmiClient& dc, jlong invokeflags);
  foreign virtual void compPokeStatic(IN(REvalEnv) env);
  foreign virtual void compPokeStatic(IN(REvalEnv) env, IN(RString) classname, IN(RString) fieldname, IN(ScriptVar) value, DmiClient& dc, jlong invokeflags);

  foreign virtual void compNewArray(IN(REvalEnv) env);
};


} // aci
} // acdk


#endif //acdk_aci_Compiler_h

