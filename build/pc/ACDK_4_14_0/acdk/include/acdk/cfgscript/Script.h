// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 

#ifndef acdk_cfgscript_Script_h
#define acdk_cfgscript_Script_h


#include "ExecutionStack.h"

namespace acdk {
namespace cfgscript {

USING_CLASS(::acdk::util::, Properties);

// used to avoid conflicts with acdk_aunit (TestRunner.h)

#if defined(testAssert)
# define TESTASSERTSIC testAssert
# undef testAssert
#endif
#if defined(testAssertComment)
# define TESTASSERTCOMMENTSIC testAssertComment
# undef testAssertComment
#endif

enum ScriptFlags
{
  /** read directly from given props */
  ScriptReadParent     =   0x0001,
  /** write into given props */
  ScriptWriteParent    =   0x0002,
  ScriptReadWriteParent  =   0x0003,
  /** dont set Nil, true, out, globals etc */
  ScriptNoDefaultProps =   0x0004,
  ScriptDefaultFlags = ScriptReadParent,
  /**
    run Script in isolated enviromnent
    if script doesn't run isolated and it contains
    class definitions (or loads other script classes via classloader)
    these classes and scripts will not be freed until program termination
  */    
  ScriptRunIsolated    =   DbgScriptRunIsolated
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, ScriptFlags);

ACDK_DECL_CLASS(Script);

/**
  Script represents a script file
  @note the Script class must not allocated on the stack, because
        it will register itself as ThreadLocal
  @see gw_ref[acdk_cfgscript_hb].
  
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CFGSCRIPT_LIB_PUBLIC Script
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(Script)
private:
  RString _filename;
  // only set if included
  RScript _parentScript;
  RStringArray _alreadyIncluded;
  RTokenizedSource _tokenized;
  
  bool _useStrict;
  /**
    a combination of ScriptVarCastFlags
  */
  short _castFlags;
  
public:
  /// script globals
  RProps currentProps;
  
public:

  Script(IN(RString) cfgfile, IN(RScript) parentScript = Nil)
  : _filename(cfgfile)
  , _parentScript(parentScript)
  , _useStrict(false)
  , _castFlags(acdk::lang::dmi::SVCastStdFlags)
  {
    if (parentScript != Nil)
      _alreadyIncluded = parentScript->_alreadyIncluded;
    else
      _alreadyIncluded = new StringArray(0);
  }
  ~Script();
  RString toString() { return _filename; }
  RString getFileName() { return _filename; }
  RString getScriptPath();
  RProps getGlobals() { return currentProps; }
  RScript getParentScript() { return _parentScript; }

  bool useStrict() { return _useStrict; }
  void useStrict(bool use) { _useStrict = use; }
  /**
    a combination of acdk::lang::dmi::ScriptVarCastFlags
  */
  short getCastFlags() { return _castFlags; }
  /**
    a combination of acdk::lang::dmi::ScriptVarCastFlags
  */
  void setCastFlags(int castFlags) { _castFlags = castFlags; }
  int readEval(IN(RProps) props, int flags = PropsParentRead | PropsNoParentWrite);
  //int readEval(IN(::acdk::io::RCharReader) in, IN(RProps) props, int flags = ScriptDefaultFlags);
  /**
    evaluates a complete script
  */
  int eval(IN(RString) text, IN(RProps) props, int flags = ScriptDefaultFlags);
  /**
    evaluates not a sequence of statements, but a single expression
    @return the expression evaluated
  */
  RDmiObject evalExpr(IN(RString) text, IN(RProps) props, int flags = ScriptDefaultFlags);
  int evalTemplate(IN(RString) text, IN(RProps) props, int flag = ScriptDefaultFlags);
  int evalTemplate(IN(acdk::io::RFile) sourceFile, IN(RProps) props, int flag = ScriptDefaultFlags);
#if defined(assert)
# undef assert
#endif
  /**
    this method will be called by scripts
    if the expression is false it write the asserting scripting
    line to ACDK_NLOG("acdk.cfgscript.Script.assert", Error)
  */
  void assert(bool test);
  /**
    same as assert(bool test) but write an info log entry in case of success
  */
  void assertTest(bool test);
  void assertTrue(bool test, IN(RString) msg);
  void testAssert(bool test);
  void testAssertComment(bool test, IN(RString) msg);

  void assertExists(IN(RProps) props, IN(RString) variable);
  
  bool include(IN(RString) fname, bool noDuplicates = true, bool changeDir = false);
  
  /**
    initialize properties as script environmnet
  */
  static void initAsEnvProps(IN(RProps) scriptenv);
 
  foreign INOUT(RTokenizedSource) getTokenized() { return _tokenized; }
  
  /**
    next statement will break to debugger
  */
  static void breakToDebug() { ExecutionStack::get()->breakToDebug(); }
  /**
    trace each executed line to System::out
  */
  static void traceOn() { ExecutionStack::get()->addDbgFlag(DbgPrintEachLine); }
  /**
    don't trace each executed line
  */
  static void traceOff() { ExecutionStack::get()->removeDbgFlag(DbgPrintEachLine); }

  static RString getScriptBackTrace(bool withSourcePos = true, bool withLocals = false) 
  { 
    return ExecutionStack::get()->getScriptBackTrace(withSourcePos, withLocals); 
  }
  static RExecutionStack getExecutionStack()
  {
    return ExecutionStack::get();
  }
  /**
    if Script exits with an exception the execution stack is still active.
    Therefore call clearStack to remove stack for this thread.
  */
  static void clearStack();
  
  static RString getCfgScriptCommandLineHelp();
  /**
    this method parses (and removes) the CfgScript command line options
  */
  static RStringArray parseCfgScriptOptions(IN(RStringArray) args, IN(RProps) envProps = Nil);

 
  static acdk::lang::dmi::RDmiDelegate createScriptDelegate(IN(RString) code)
  {
    acdk::lang::dmi::ScriptVarArray args;
    return createScriptDelegate(args, code);
  }
  static acdk::lang::dmi::RDmiDelegate createScriptDelegate(const acdk::lang::dmi::ScriptVar& arg1, IN(RString) code)
  {
    acdk::lang::dmi::ScriptVarArray args(1);
    args[0] = arg1;
    return createScriptDelegate(args, code);
  }
  static acdk::lang::dmi::RDmiDelegate createScriptDelegate(const acdk::lang::dmi::ScriptVar& arg1,
                                                            const acdk::lang::dmi::ScriptVar& arg2,
                                                            IN(RString) code)
  {
    acdk::lang::dmi::ScriptVarArray args(2);
    args[0] = arg1;
    args[1] = arg2;
    return createScriptDelegate(args, code);
  }
  static acdk::lang::dmi::RDmiDelegate createScriptDelegate(const acdk::lang::dmi::ScriptVar& arg1,
                                                            const acdk::lang::dmi::ScriptVar& arg2,
                                                            const acdk::lang::dmi::ScriptVar& arg3,
                                                            IN(RString) code)
  {
    acdk::lang::dmi::ScriptVarArray args(3);
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return createScriptDelegate(args, code);
  }
  static acdk::lang::dmi::RDmiDelegate createScriptDelegate(const acdk::lang::dmi::ScriptVar& arg1,
                                                            const acdk::lang::dmi::ScriptVar& arg2,
                                                            const acdk::lang::dmi::ScriptVar& arg3,
                                                            const acdk::lang::dmi::ScriptVar& arg4,
                                                            IN(RString) code)
  {
    acdk::lang::dmi::ScriptVarArray args(4);
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return createScriptDelegate(args, code);
  }
  static acdk::lang::dmi::RDmiDelegate createScriptDelegate(const acdk::lang::dmi::ScriptVarArray& args, IN(RString) code);
  
  
  
private:
  foreign int _readEval(IN(RProps) props, int flags);
  foreign int _readEval2(IN(RProps) props, bool inplace);
  foreign RString _getCurrentAssertLine();
};


#if defined(TESTASSERTSIC)
# define testAssert TESTASSERTSIC
#endif

#if defined(TESTASSERTCOMMENTSIC)
# define  testAssertComment TESTASSERTCOMMENTSIC
#endif

} // namespace cfgscript
} // namespace acdk 
  
#endif //acdk_cfgscript_Script_h
