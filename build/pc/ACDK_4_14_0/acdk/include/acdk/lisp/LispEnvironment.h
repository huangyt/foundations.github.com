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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispEnvironment.h,v 1.16 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_lisp_LispEnvironment_h
#define acdk_lisp_LispEnvironment_h

#include "LispCode.h"
#include "LispTokenizer.h"
#include "StackFrame.h"

#include <acdk/io/File.h>
#include <acdk/util/HashMap.h>
#include <acdk/util/HashSet.h>
#include <acdk/util/Properties.h>

#include <acdk/io/PrintWriter.h>
#include <acdk/io/InputReader.h>

namespace acdk {
namespace lisp {

typedef RLispVar (*LispNativFunction)(IN(RLispEnvironment) env, IN(RLispList) args);

using namespace acdk::lang;
using namespace acdk::io;
using namespace acdk::util;

USING_CLASS(acdk::io::, File);
USING_CLASS(acdk::io::, Reader);
USING_CLASS(acdk::io::, Writer);
USING_CLASS(acdk::util::, HashMap);



ACDK_DECL_CLASS(LispEnvironment);

/** 
  Represents an Lisp-Interpreter
  @author Roger Rene Kommer
  @version $Revision: 1.16 $
  @date $Date: 2005/04/08 10:53:20 $
 
*/
class ACDK_ACDK_LISP_PUBLIC LispEnvironment
: public acdk::lang::Object
{
  ACDK_WITH_METAINFO(LispEnvironment)
private:
  /// global instance
  static LispEnvironment* _lenv;
  /// this will be used if not already set
  static RLispEnvironment _glenv;
  /** normal buildin function*/
  static acdk::util::RHashMap __staticFuncs; // RString=RFunction
  static RLispAtom __trueVar; // true

  acdk::util::RProperties _environment; // RString=RString
  acdk::util::RHashMap _globals; // RString=RListVar
  
  
  acdk::util::RHashMap _defuns; // RString=RLispFunction
  
  acdk::util::RHashMap _macros; // RString=RLispFunction

  /** all fq names of included lsp-files */
  acdk::util::RHashSet _includes; // RString
  
  acdk::util::RHashSet _tracedSymbols; // RString
  bool _trace;
  
  RLispVar _lastEvaled;
  bool _returnNow;
  int _tracelevel;
  int _break;
  bool _exitNow;
  int _exitValue;
  RStringArray _cmlineArgs;

public:
  acdk::io::RPrintWriter out;
  acdk::io::RPrintWriter err;
  acdk::io::RInputReader in;
  /**
    The current Modul stack 
    */
  Stack<RFile> _modulStack;
  Stack<RLispStackFrame> _stackFrame;
  Stack<RLispVar> _evalStack;


  LispEnvironment(IN(acdk::util::RProperties) environment = Nil, IN(RStringArray) args = Nil, bool trace = false);
  ~LispEnvironment();
  /** 
    loads standard lisp files in $(ACDK_HOME)/cfg/lib/acdk/lisp/autoload.lsp.
    
    @throws Error if ACDK_HOME is not defined.
    @param loadCode load code instead of image
  */
  void init(bool loadCode = false);

  /** 
    initialize environmnet variables
  */
  void  initEnv();
  /**
    uninizialize the LispEnvironment.
    needed to release this instance.
  */
  void deinit();
  RString parseEval(const char*& ptr, int& rest);
  RLispVar parseEval(IN(RString) str);
  static RLispList parseToList(IN(RString) str);

  RLispCode parse(IN(RLispTokenizer) in, IN(acdk::io::RPrintWriter) out, bool interactiv = false, bool parseOneToken = false);
  
  void interactive(IN(acdk::io::RCharReader) in, IN(acdk::io::RCharWriter) out);
  
  virtual RLispVar eval(IN(RLispVar) list);
  /**
    Lookup for var in 
    <ul>
    <li> locals in scopes from inner to outer
    <li> functions
    <li> global variables (setg)
    <li> environmnet
  */
  RLispVar lookupVar(IN(RString) str, bool warn = true);
  /**
    Lookup for var in only local scopes
    returns Nil if not found
  */
  RLispVar lookupLocalVar(IN(RString) str);
  RFunction lookupFunction(IN(RString) str);
  bool isMacro(IN(RString) str) 
  {
    RFunction f = lookupFunction(str);
    if (f == Nil || instanceof(f, LispFunction) == false)
      return false;
    return RLispFunction(f)->isMacro();
  }
  virtual void bindLocal(IN(RString) symbol, IN(RLispVar) value, bool forcelocal = false);
  virtual void bindGlobal(IN(RString) symbol, IN(RLispVar) value);
  virtual void bindGlobal(IN(RString) symbol, IN(RString) value) { bindGlobal(symbol, new LispAtom(value)); }
  virtual void bindToEnv(IN(RString) symbol, IN(RLispVar) value);
  virtual void bindToEnv(IN(RString) symbol, IN(RString) value) { bindToEnv(symbol, new LispAtom(value)); }
  static RLispAtom t();
  static void registerFunction(const char* name, const char* decl, LispNativFunction func, bool preeval = true);
  void registerDefun(IN(RLispFunction) func);
  /** 
    load code into Lisp-Environment 
    @arg in the Intput
    @return The result of the evaluated code
  */
  RString load(IN(RLispTokenizer) tok);
  RString load(IN(RString) filename);

  
  RString loadUnparsedFile(IN(RString) filename); 
  /**
    Debugging function
  */
  void dumpEnv(IN(acdk::io::RCharWriter) out);
  /**
    Debugging function
  */
  bool trace() { return _trace; }
  /**
    Debugging function
  */
  void trace(bool t) { _trace = t; }
  /**
    Debugging function
  */
  bool trace(IN(RString) symbol) { return _tracedSymbols->contains((RObject)symbol); }
  /**
    Debugging function
  */
  void trace(IN(RString) symbol, bool dotrace) 
  { 
    if (dotrace)
      _tracedSymbols->remove((RObject)symbol); 
    else if (trace(symbol) == false)
      _tracedSymbols->add((RObject)symbol);
  }
  void traceln(IN(RString) out);
  void traceflush(IN(RString) str);
  void trace_begin(IN(RString) out);
  void trace_end(IN(RString) out);
  void setBreak(int b) { _break = b; }
  int getBreak() { return _break; }
  RLispVar debug_interactive(IN(RLispVar) var);
  
  RLispVar lastEvaled() { return _lastEvaled; }
  RString lastEvaledString() { return _lastEvaled == Nil ? RString("") : _lastEvaled->toString(); }
  void returnNow(bool set) { _returnNow = set; }
  bool returnNow() { return _returnNow; }
  void exitNow(int exitval) { _exitNow = true; _exitValue = exitval; }
  bool exitNow() { return _exitNow; }
  int exitValue() { return _exitValue; }
  
  /** returns an KeyIterator, where value is RFunction */
  acdk::util::RIterator functionIterator();
  acdk::util::RIterator buildinsIterator();
  acdk::util::RIterator defunsIterator();

  acdk::util::RProperties environment() { return _environment; }
  acdk::util::RHashMap globals() { return _globals; }
  static acdk::util::RHashMap _staticFuncs();
  acdk::util::RHashSet includes() { return _includes; }
  void setInOut(IN(RCharReader) rin, IN(RCharWriter) rout, IN(RCharWriter) rerr);

  
  RLispVar _eval(IN(RLispVar) list);
  RLispVar _eval(IN(RLispList) list);
  RLispVar _eval(IN(RLispSymbol) list);
  RLispVar _eval(IN(RLispAtom) list);

  /**
    store all compiled functions, macros an globals variables
    a file.
    Environment and build in functions will not be stored
  */
  void storeCompiled(IN(RString) file);
  void storeCompiled(IN(::acdk::io::RWriter) out);
  /**
    Loads image file
    @param replace replace current env.
  */
  void loadCompiled(IN(RString) file, bool replace);
  /**
    Loads image file
    @param replace replace current env.
  */
  void loadCompiled(IN(::acdk::io::RReader) in, bool replace);

  static RLispEnvironment lenv() 
  { 
    if (_lenv != 0)
      return _lenv; 
    _glenv = new LispEnvironment();
    return _lenv;
  }

  friend class LispBinaryCode;
};


class ACDK_ACDK_LISP_PUBLIC StackVarHolder
{
public:
  RLispEnvironment _env;
  StackVarHolder(IN(RLispEnvironment) env, bool boundtoparent = false) 
  : _env(env)
  {
    if (boundtoparent == true && env->_stackFrame.size() > 0)
      env->_stackFrame.push(new LispStackFrame(env->_stackFrame.top())); 
    else
      env->_stackFrame.push(new LispStackFrame());
  }
  ~StackVarHolder()
  {
    _env->_stackFrame.pop();
  }

};





void testLisp();

} // namespace lisp
} // namespace acdk

#endif //acdk_lisp_LispEnvironment_h

