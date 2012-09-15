// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_AalCompiler_h
#define acdk_aal_AalCompiler_h


#include "../aci/Compiler.h"

namespace acdk {
namespace aal {

using acdk::lang::dmi::ScriptVar;
using acdk::lang::dmi::ScriptVarArray;
using acdk::lang::dmi::DmiClient;
USING_CLASS(acdk::aci::, EvalEnv);

/*
ACDK_DECL_CLASS(Compiler);
ACDK_DECL_CLASS(Code);
ACDK_DECL_CLASS(TypeVar);
ACDK_DECL_CLASS(Expression);
*/

ACDK_DECL_CLASS(AalCompiler);

class ACDK_AAL_PUBLIC AalCompiler
: extends acdk::aci::Compiler
{
public:
  static int ATT_IDENTIFIER;
  AalCompiler();
  void initStdSyntax();
  void initStdTypes();
  virtual void initializeForEvaluation(IN(REvalEnv) env);
  virtual ScriptVar compInvokeMethod(IN(REvalEnv) env, IN(RObject) target, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
  virtual ScriptVar compInvokeStaticMethod(IN(REvalEnv) env, IN(RString) classname, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
  virtual ScriptVar compInvokeHashMethod(IN(REvalEnv) env, IN(RObject) target, int methodhash, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
  virtual ScriptVar compInvokeStaticHashMethod(IN(REvalEnv) env, IN(RString) classname, int methodhash, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags);
};


ACDK_DECL_CLASS(AalInterpreter);

class ACDK_AAL_PUBLIC AalInterpreter
: extends acdk::lang::Object
{
  RAalCompiler _compiler;
  acdk::aci::REvalEnv _lastEnv;
public:
  AalInterpreter();
  
  void resetEnv();
  void parseTreeInterpret(IN(RString) text, IN(RString) initalEl = "CodeText");
  OUT(acdk::aci::REvalEnv) getLastEnv() { return _lastEnv; }
  void setGlobalVar(IN(RString) name, const acdk::lang::dmi::ScriptVar& value);
  acdk::lang::dmi::ScriptVar getGlobalVar(IN(RString) name);
};

}
}


#endif //acdk_aal_AalCompiler_h

