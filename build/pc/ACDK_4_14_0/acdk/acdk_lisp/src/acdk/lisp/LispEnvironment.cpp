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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispEnvironment.cpp,v 1.34 2005/05/09 13:47:57 kommer Exp $



#include <acdk.h>

#include <acdk/lang/Error.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/MemReader.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/ByteToCharReader.h>

#include <acdk/util/Properties.h>
#include <acdk/util/HashMap.h>
#include <acdk/util/HashSet.h>

#include <acdk/util/Iterator.h>
#include <acdk/util/DoubleIterator.h>
#include <acdk/locale/Encoding.h>
#include <acdk/util/logging/Log.h>

#include "lisp.h"
#include "LispEnvironment.h"
#include "LispTokenizer.h"
#include "LispException.h"
#include "LispObject.h"
#include "LispBuildInFunction.h"

namespace acdk {
namespace lisp {

using namespace acdk::lang;
using namespace acdk::io;
//using namespace acdk::util;
using acdk::util::RIterator;
using acdk::util::HashMap;
using acdk::util::HashSet;
using acdk::util::RProperties;

//static 
LispEnvironment* LispEnvironment::_lenv = 0;
//static 
RLispEnvironment LispEnvironment::_glenv;

RLispVar
LispBuildInFunction::eval(IN(RLispEnvironment) env, IN(RLispList) args)
{
  return _function->eval(env, args);
}

//virtual 
RObject 
LispArray::clone(sys::Allocator* alc)
{
  //### not implemented yet
  //return new LispArray(this);
  return Nil;
}

void
dumpStackFrame(IN(RHashMap) cf, IN(RCharWriter) out)
{
  RIterator it = cf->keySet()->iterator();
  while (it->hasNext() == true) {
    RString str = (RString)it->next();
    RLispVar val = (RLispVar)cf->get((RObject)str);
    RString tstr = str + "=[" + (val == Nil ? RString("Nil") : val->toCode()) + "]\n";
    out->writeString(tstr);
    out->flush(); 
  }
}

void
getDumpedStackFrame(IN(RHashMap) cf, StringBuffer& sb)
{
  RIterator it = cf->keySet()->iterator();
  while (it->hasNext() == true) {
    RString str = (RString)it->next();
    RLispVar val = (RLispVar)cf->get((RObject)str);
    sb.append(str);
    sb.append("=[");
    str = (val == Nil ? RString("Nil") : val->toCode());
    sb.append(str);  
    sb.append("]\n");  
  }
}

void
getDumpedEnv(IN(acdk::util::RProperties) cf, StringBuffer& sb)
{
  RIterator it = cf->keySet()->iterator();
  while (it->hasNext() == true) {
    RString str = (RString)it->next();
    RString val = cf->getProperty(str);
    sb.append(str);
    sb.append("=[");
    sb.append(val);  
    sb.append("]\n");  
  }
}



RString 
LispStackFrame::toString() 
{ 
  StringBuffer sb(1024);
  getDumpedStackFrame(_current, sb);
  if (_parent != Nil) {
    sb.append("parent stack frame:\n");
    sb.append(_parent->toString());
  }
  return sb.toString();
}


acdk::util::RIterator 
LispEnvironment::functionIterator()
{
  return new acdk::util::DoubleIterator(_staticFuncs()->keySet()->iterator(), _defuns->keySet()->iterator());
}

acdk::util::RIterator 
LispEnvironment::buildinsIterator()
{
  return _staticFuncs()->keySet()->iterator();
}

acdk::util::RIterator 
LispEnvironment::defunsIterator()
{
  return _defuns->keySet()->iterator();
}

//static 
RHashMap LispEnvironment::__staticFuncs; // RString=RNativeFunc

void 
LispEnvironment::registerDefun(IN(RLispFunction) func)
{
  _defuns->put((RObject)func->name(), (RObject)func);
}


RLispVar lisp_plus(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_minus(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_multiply(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_divide(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_modulo(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_gt(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_listp(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_not(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_and(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_or(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_eq(IN(RLispEnvironment) env, IN(RLispList) ags);
RLispVar lisp_eql(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_eql(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_length(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_quote(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_backquote(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_comma(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_commaat(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_list(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_append(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_setnth(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_cons(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_car(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_cdr(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_eval(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_let(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_unpack(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_lambda(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_defun(IN(RLispEnvironment) env, IN(RLispList) args);

RLispVar lisp_defmacro(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_defclass(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_make_instance(IN(RLispEnvironment) env, IN(RLispList) args);

RLispVar lisp_progn(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_apply(IN(RLispEnvironment) env, IN(RLispList) args);

RLispVar lisp_if(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_cond(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_while(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_do(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_dolist(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_return(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_getv(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_setv(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_setq(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_setf(IN(RLispEnvironment) env, IN(RLispList) args);

RLispVar lisp_define(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_invoke(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_invoke_static(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_new(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_peek(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_peek_static(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_poke(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_poke_static(IN(RLispEnvironment) env, IN(RLispList) args);

RLispVar lisp_try(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_throw(IN(RLispEnvironment) env, IN(RLispList) args);

RLispVar lisp_dump(IN(RLispEnvironment) env, IN(RLispList) args);

RLispVar lisp_zerop(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_truep(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_listp(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_atomp(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_symbolp(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_trace(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_instanceof(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_isdef(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_internal(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_internalp(IN(RLispEnvironment) env, IN(RLispList) args);


RLispVar lisp_acdk_lisp_include(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_explore(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_dp(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_create_array(IN(RLispEnvironment) env, IN(RLispList) args);
RLispVar lisp_number_equal(IN(RLispEnvironment) env, IN(RLispList) args);



//static
void 
LispEnvironment::registerFunction(const char* name, const char* decl, LispNativFunction func, bool preeval)
{
  RHashMap sf = _staticFuncs();
  RString str = name;
  sf->put(&str, new LispCallBack(decl, func, preeval));
}

//static 
RHashMap 
LispEnvironment::_staticFuncs()
{
  if (__staticFuncs != Nil)
    return __staticFuncs;
  __staticFuncs = new HashMap();
  
  __staticFuncs->put((RObject)(RString)"zerop", 
    new LispCallBack("(defun zerop (val) \"return t if giben [val] is Nil\")", lisp_zerop, true));
  __staticFuncs->put((RObject)(RString)"truep", 
    new LispCallBack("(defun truep (val) \"return t if given [val] is a reference to t\")", lisp_truep, true));
  __staticFuncs->put((RObject)(RString)"atomp", 
      new LispCallBack("(defun atomp (val) \"return t if giben [val] is an atom\")", lisp_atomp, true));
  __staticFuncs->put((RObject)(RString)"listp", new LispCallBack("(defun listp (val) \"return t if giben [val] is an list\")", lisp_listp, true));
  __staticFuncs->put((RObject)(RString)"symbolp", new LispCallBack("(defun symbolp (val) \"return t if giben [val] is a symbol\")", lisp_symbolp, true));
  __staticFuncs->put((RObject)(RString)"instanceof", 
    new LispCallBack(
      "(defun instanceof (object classtype)"
      "\"(instanceof object 'classtype) or (instanceof object 'classname)\\n"
      "return t if given instance of [object] is type of [classtype], otherwise Nil\""
      ")"
      , lisp_instanceof, true));
  
  __staticFuncs->put((RObject)(RString)"trace", 
      new LispCallBack(
      "(defun trace (onoff)"
      "\"(trace [0|1])\\n"
      "trace 1 print eval on out\""
      ")"
      , lisp_trace, true));

  __staticFuncs->put((RObject)(RString)"dump", new LispCallBack(
      "(defun dump (&rest args) \"special args:\n"
      "\t'sf[level] level optional or x for all frames\tStack Frame\n"
      "\t'cs[level]\tcall stack\n\t'globals\tglobal varariables\n\t'env\tenvironment variables\n\tdefuns\tfunctions defintion\")", lisp_dump, true));

  __staticFuncs->put((RObject)(RString)"lambda", new LispCallBack("(defun lambda (&rest args) \"define a new function\")", lisp_lambda, false));
  __staticFuncs->put((RObject)(RString)"defun", new LispCallBack("(defun defun (&rest args) \"define a new function\")", lisp_defun, false));
  
  __staticFuncs->put((RObject)(RString)"progn", new LispCallBack("(defun progn (&rest args)) \"to define a block.\nAll args will be evaluated, the result is the result of the last arg\"", lisp_progn, false));
  __staticFuncs->put((RObject)(RString)"apply", new LispCallBack("(defun apply (&rest args))", lisp_apply, true));
  __staticFuncs->put((RObject)(RString)"defmacro", new LispCallBack("(defun defmacro (&rest args) \"not implemented yet\")", lisp_defmacro, false));
  __staticFuncs->put((RObject)(RString)"defclass", new LispCallBack("(defun defclass (&rest args) \"implements a object class\")", lisp_defclass, false));
  __staticFuncs->put((RObject)(RString)"make-instance", new LispCallBack("(defun make-instance (classname &rest args) \"creates an CLOS object instance\")", lisp_make_instance, false));
  

  __staticFuncs->put((RObject)(RString)"+", new LispCallBack("(defun + (first &rest args))", lisp_plus));
  __staticFuncs->put((RObject)(RString)"-", new LispCallBack("(defun - (first second))", lisp_minus));
  
  __staticFuncs->put((RObject)(RString)"*", new LispCallBack("(defun * (first &rest args))", lisp_multiply));
  __staticFuncs->put((RObject)(RString)">", new LispCallBack("(defun > (first second))", lisp_gt));
  __staticFuncs->put((RObject)(RString)"/", new LispCallBack("(defun / (first second))", lisp_divide));
  __staticFuncs->put((RObject)(RString)"%", new LispCallBack("(defun % (first second))", lisp_modulo));

  __staticFuncs->put((RObject)(RString)"not", new LispCallBack("(defun not (expr))", lisp_not));
  __staticFuncs->put((RObject)(RString)"and", new LispCallBack("(defun and (first &rest args))", lisp_and, false));
  __staticFuncs->put((RObject)(RString)"or", new LispCallBack("(defun or (first &rest args))", lisp_or, false));
  __staticFuncs->put((RObject)(RString)"eq", new LispCallBack("(defun eq (first second))", lisp_eq));
  __staticFuncs->put((RObject)(RString)"eql", new LispCallBack("(defun eql (first second))", lisp_eql));
  __staticFuncs->put((RObject)(RString)"equal", new LispCallBack("(defun equal (first second))", lisp_eql));
  __staticFuncs->put((RObject)(RString)"length", new LispCallBack("(defun length (list))", lisp_length));
  __staticFuncs->put((RObject)(RString)"quote", new LispCallBack("(defun quote (arg))", lisp_quote, false));
  __staticFuncs->put((RObject)(RString)"backquote", new LispCallBack("(defmacro backquote (arg))", lisp_backquote, false));

  __staticFuncs->put((RObject)(RString)"comma", new LispCallBack("(defmacro comma (arg))", lisp_comma, false, false));
  __staticFuncs->put((RObject)(RString)"commaat", new LispCallBack("(defmacro commaat (arg))", lisp_commaat, false, false));
  __staticFuncs->put((RObject)(RString)"list", new LispCallBack("(defun list (&rest elements))", lisp_list, true));
  __staticFuncs->put((RObject)(RString)"append", new LispCallBack("(defun append (list element))", lisp_append, true));
  __staticFuncs->put((RObject)(RString)"setnth", new LispCallBack("(defun setnth (list idx element))", lisp_setnth, true));
  __staticFuncs->put((RObject)(RString)"cons", new LispCallBack("(defun cons (list el))", lisp_cons, true));
  __staticFuncs->put((RObject)(RString)"car", new LispCallBack("(defun car (list))", lisp_car, true));
  __staticFuncs->put((RObject)(RString)"cdr", new LispCallBack("(defun cdr (list))", lisp_cdr, true));
  __staticFuncs->put((RObject)(RString)"eval", new LispCallBack("(defun eval (expr))", lisp_eval, true));
  __staticFuncs->put((RObject)(RString)"=", new LispCallBack("(defun = (first second))", lisp_number_equal, true));
  __staticFuncs->put((RObject)(RString)"unpack", new LispCallBack("(defun unpack (var) \"In case <var> contains LispVar it will return it\")", lisp_unpack, true));
  __staticFuncs->put((RObject)(RString)"getv", new LispCallBack("(defun getv (varname value))", lisp_getv, false));
  __staticFuncs->put((RObject)(RString)"setv", new LispCallBack("(defun setv (varname value))", lisp_setv, false));
  __staticFuncs->put((RObject)(RString)"setq", new LispCallBack("(defun setq (symbol value))", lisp_setq, false));
  __staticFuncs->put((RObject)(RString)"setg", new LispCallBack("(defun setg (symbol value))", lisp_setq, false));
  __staticFuncs->put((RObject)(RString)"setf", new LispCallBack("(defun setf (symbol value))", lisp_setf, false));
  __staticFuncs->put((RObject)(RString)"set", new LispCallBack("(defun set (symbol value))", lisp_setf, false));
  __staticFuncs->put((RObject)(RString)"let", new LispCallBack("(defun let (&rest args))", lisp_let, false));
  
  __staticFuncs->put((RObject)(RString)"if", new LispCallBack("(defmacro if (cond expr1 &optional expr2))", lisp_if, false));
  __staticFuncs->put((RObject)(RString)"cond", new LispCallBack("(defmacro cond (&rest condblocks) \"see lisp manual\")", lisp_cond, false));
    __staticFuncs->put((RObject)(RString)"while", new LispCallBack("(defmacro while (cond expr))", lisp_while, false));
  __staticFuncs->put((RObject)(RString)"dolist", new LispCallBack("(defmacro dolist ((element list erg) body1 &rest bodyn))", lisp_dolist, false));
  __staticFuncs->put((RObject)(RString)"do", new LispCallBack("(defmacro do (&rest args))", lisp_do, false));
  __staticFuncs->put((RObject)(RString)"return", new LispCallBack("(defun return (&optional retvalue))", lisp_return, true));
  __staticFuncs->put((RObject)(RString)"explore", new LispCallBack("(defun explore (object))", lisp_explore, true));
  __staticFuncs->put((RObject)(RString)"dp", new LispCallBack("(defun dp (&optional value))", lisp_dp, true));
  __staticFuncs->put((RObject)(RString)"create-array", new LispCallBack("(defun create-array (size))", lisp_create_array, true));
  __staticFuncs->put((RObject)(RString)"isdef", new LispCallBack("(defmacro isdef (symbol) \"return true if given symbol can be found in symbol table\")", lisp_isdef, false));
  __staticFuncs->put((RObject)(RString)"internal", new LispCallBack("(defmacro internal (symbol) \"returns the internal representation of symbol\")", lisp_internal, false));
  __staticFuncs->put((RObject)(RString)"internalp", new LispCallBack("(defmacro internalp (symbol) \"returns the internal representation of the object symbol points to\")", lisp_internalp, false));

  // while
  // apply
  
  __staticFuncs->put((RObject)(RString)"new", new LispCallBack("(defun new (classymbol &rest constructorargs))", lisp_new, true));
  __staticFuncs->put((RObject)(RString)"invoke", new LispCallBack("(defun invoke (object method, &rest methodargs))", lisp_invoke, true));
  __staticFuncs->put((RObject)(RString)"invoke-static", new LispCallBack("(defun invoke-static (classymbol method, &rest methodargs))", lisp_invoke_static, true));
  __staticFuncs->put((RObject)(RString)"peek", new LispCallBack("(defun peek (object membername)"
                                              "\"reads a public member variable of a ACDK object\")", lisp_peek, true));
  __staticFuncs->put((RObject)(RString)"peek-static", new LispCallBack("(defun peek-static (classymbol membername)"
                                                              "\"reads a static public member variable of the given ACDK class\")", lisp_peek_static, true));

  __staticFuncs->put((RObject)(RString)"poke", new LispCallBack("(defun poke (object membername value)"
                                                              "\"writes a given value to the ACDK object public member\")", lisp_poke, true));
  __staticFuncs->put((RObject)(RString)"poke-static", new LispCallBack("(defun poke-static (classymbol membername value)"
                                                              "\"writes a static public member variable of the given ACDK class\")", lisp_poke_static, true));

  __staticFuncs->put((RObject)(RString)"try", new LispCallBack("(defmacro try (&rest body))", lisp_try, false));
  __staticFuncs->put((RObject)(RString)"throw", new LispCallBack("(defmacro throw (exceptionsymbol &rest exargs))", lisp_throw, true));

  __staticFuncs->put((RObject)(RString)"include", new LispCallBack("(defmacro include (filename))", lisp_acdk_lisp_include, true));
  
  return __staticFuncs;
}


//static 
//RLispNil LispEnvironment::__nil;
//static 
RLispAtom LispEnvironment::__trueVar; // true

/*
//static 
RLispNil 
LispEnvironment::nil()
{
  if (__nil != Nil)
    return __nil;
  __nil = new LispNil();
  System::registerStaticReference(__nil);
  return __nil;
}
*/
//static 
RLispAtom 
LispEnvironment::t()
{
  if (__trueVar != Nil)
    return __trueVar;
  __trueVar = new LispAtom(RString("TRUE"));
  System::registerStaticReference(__trueVar);
  return __trueVar;
}


void 
LispEnvironment::initEnv()
{
 if (_environment->getProperty("*thisfile*") == Nil)
    _environment->setProperty("*thisfile*", RString("NIL"));

  _stackFrame.push(new LispStackFrame()); // for interaction
  RLispVar e = new LispAtom(ScriptVar(RObject(this)));
  bindGlobal("*env*", e);
  bindGlobal("env", e);
  bindLocal("env", e);
  RLispAtom temp = new LispAtom(ScriptVar(RObject(_cmlineArgs)));
  bindGlobal("*args*", RLispVar(temp));
  bindGlobal("args", RLispVar(temp));
  bindLocal("args", RLispVar(temp));
  
  bindGlobal("t", (RLispVar)t());
  bindLocal("T", (RLispVar)t());

  if (_environment->getProperty("*interactive*") == Nil)
    _environment->setProperty("*interactive*", RString("NIL"));
}

LispEnvironment::LispEnvironment(IN(RProperties) environment, IN(RStringArray) args, bool trace)
: Object(),
  _environment(environment),
  _globals(new HashMap()),
  _defuns(new HashMap()),
  _includes(new HashSet()),
  _tracedSymbols(new HashSet()),
  _trace(trace),
  _returnNow(false),
  _tracelevel(0),
  _break(0),
  _exitNow(false),
  _exitValue(0),
  _cmlineArgs(args),
  out(System::out),
  err(System::err),
  in(System::in)
{
  ACDK_SAFE_CONSTRUCTOR(); // to prevent delete this
  _lenv = this;
  if (_environment == Nil)
    _environment = System::getProperties();
 
  
}

LispEnvironment::~LispEnvironment()
{
  _lenv = 0;
}

void
LispEnvironment::init(bool loadCode)
{
  if (_environment->getProperty("ACDK_TOOLS_HOME") == Nil && _environment->getProperty("ACDKHOME") == Nil)
    THROW1(Error, "environment variable ACDKHOME or ACDK_TOOLS_HOME must be set as env-var or with -acdk-[tools-]home=[dir] as command line flag ");
  RString acdkhome = _environment->getProperty("ACDK_TOOLS_HOME");
  if (acdkhome == Nil)
    acdkhome = _environment->getProperty("ACDKHOME");
  if (loadCode == false)
  {
    RFile initimg = new File(acdkhome, "cfg/lib/acdk/lisp/autoload.limg");
    if (initimg->exists() == false)
    {
      System::out->println(initimg->getCanonicalPath() + " does not exists. Compile it");
      initEnv();
      setInOut(&System::in, &System::out, &System::err);
      RFile loadingfile = new File(acdkhome, "cfg/lib/acdk/lisp/autoload.lsp");
      load(loadingfile->getCanonicalPath());
      storeCompiled(initimg->getCanonicalPath());
    }
    if (initimg->exists() == false)
    {
      // ### error
    }
    RPrintWriter sicprintwriter = System::out;
    RWriter sicwriter = sicprintwriter->getWriter();
    System::out->println("Load image: " + initimg->getName());
    loadCompiled(initimg->getReader(), true);
    /*
    if (sicprintwriter != System::out || sicwriter != System::out->getWriter())
      std::cout << "oops" << std::endl;
      */
    initEnv();
    setInOut(&System::in, &System::out, &System::err);
    /*
    if (sicprintwriter != System::out || sicwriter != System::out)
      std::cout << "oops" << std::endl;
    */
    System::out->println("init finished");
  } else {
    initEnv();
    setInOut(&System::in, &System::out, &System::err);
    RFile loadingfile = new File(acdkhome, "cfg/lib/acdk/lisp/autoload.lsp");
    load(loadingfile->getCanonicalPath());
  }
}


void
LispEnvironment::deinit()
{
  bindGlobal("*env*", RLispVar(Nil));
  bindGlobal("env", RLispVar(Nil));
  bindLocal("env", RLispVar(Nil));
   __staticFuncs = Nil;
  __trueVar = Nil; 
}

void 
LispEnvironment::traceln(IN(RString) str)
{
  System::out->println(str);
  out->println(str);
}

void 
LispEnvironment::traceflush(IN(RString) str)
{
  out->print(str);
  out->flush();
}

void 
LispEnvironment::trace_begin(IN(RString) str)
{
  ++_tracelevel;

  StringBuffer sb;
  for (int i = 0; i < _tracelevel; i++)
    sb << "  ";
  sb << str;
  ACDK_NLOG("acdk.lisp.LispEnvironment", Trace, sb.toString());
  //out->println(str);
}

void 
LispEnvironment::trace_end(IN(RString) str)
{
  StringBuffer sb;
  for (int i = 0; i < _tracelevel; i++)
    sb << "  ";
  sb << str;
  ACDK_NLOG("acdk.lisp.LispEnvironment", Trace, sb.toString());
  --_tracelevel;
}



RLispVar 
LispEnvironment::debug_interactive(IN(RLispVar) var)
{
  
  LispTokenizer lisptokenizer((::acdk::io::RCharReader)in);
  RLispVar erg;
  while (exitNow() == false) 
  {
    try {
      out->print("[" + (var == Nil ? RString("NIL") : var->toCode()) + "] >");
      out->flush();
      RLispCode tcode = parse(SR(LispTokenizer, lisptokenizer), out, true);
      if (tcode == Nil)
        continue;
      RLispList code = tcode->code();
      
      RString cod = code->car()->toCode();
      if (cod->equals((RObject)(RString)"s") == true || cod->equals((RObject)(RString)"step") == true) {
        erg = _eval(var);
        out->println(">  to [" + (erg == Nil ? RString("NIL") : erg->toCode()) + "]");
        return erg;
      } else if (cod->equals((RObject)(RString)"n") == true || cod->equals((RObject)(RString)"next") == true) {
        int oldbreak = _break;
        _break = 0;
        erg = eval(var);
        _break = oldbreak;
        out->println(">  to [" + (erg == Nil ? RString("NIL") : erg->toCode()) + "]");
        return erg;
      } if (cod->equals((RObject)(RString)"c") == true || cod->equals((RObject)(RString)"continue") == true) {
        _break = 0;
        return eval(var);
      } else if (cod->equals((RObject)(RString)"?") == true || cod->equals((RObject)(RString)"h") == true || cod->equals((RObject)(RString)"help") == true) {
        out->println("Commands in debug mode:");
        out->println("\t?\tprint this help");
        out->println("\tn\tnext line");
        out->println("\ts\tstep into command");
        out->println("\tc\tcontinue");
        out->println("\tsee also (dump)");
        continue;
      }
      int oldbreak = _break;
      _break = 0;
      RLispVar erg;
      while (code != Nil) {
        StackHolder<RLispVar> __evalStackHolder(_evalStack, code->car());
        RString thecode = code->car()->toCode();
        erg = eval(code->car());
        code = code->cdr();
        if (_returnNow == true || exitNow() == true) {
          _returnNow = false;
          return erg;
        }
      }
      RString tstr = (erg == Nil ? RString("NIL") : erg->toCode());
      out->println(tstr);
      _break = oldbreak;
    } catch (RLispException lex) {
      lex->printStackTrace(err);
      err->println(lex->getMessage());
      setBreak(1);
    } catch (RThrowable ex) {
      ex->printStackTrace(err);
      err->println(ex->getMessage());
      setBreak(1);
    }
  }
  return erg;
}

#define REvalCatchException RLispException

RLispVar 
LispEnvironment::eval(IN(RLispVar) var)
{
  StackHolder<RLispVar> __stack(_evalStack, var);
  try {
    if (exitNow() == true)
      return Nil;
    if (trace() == false && getBreak() <= 0) {
      _lastEvaled = _eval(var);
#if 0
      if ((var == Nil) && (_lastEvaled == Nil))
        System::err->println(RString("Evaluated NIL to NIL"));
      else if (var == Nil)
        System::err->println(RString("Evaluated NIL to ") + _lastEvaled->getName() + "::" + _lastEvaled->toString());
      else if (_lastEvaled == Nil)
        System::err->println(RString("Evaluated ") + var->getName() + "::" + var->toString() + " to NIL");
      else
        System::err->println(RString("Evaluated ") + var->getName() + "::" + var->toString() + " to " + _lastEvaled->getName() + "::" + _lastEvaled->toString());
#endif
      return _lastEvaled;
    }
    if (getBreak() > 0) 
      return debug_interactive(var);
    if (var == Nil)
      trace_begin("Evaluated [NIL] ");
    else
      trace_begin("Evaluated [" + var->getName() + "::" + var->toCode() + "] ");
    _lastEvaled = _eval(var);
    if (_lastEvaled == Nil)
      trace_end("to [NIL]");
    else
      trace_end("to [" + _lastEvaled->getName() + "::" + _lastEvaled->toCode() + "]");
    return _lastEvaled;  
  } catch (RThrowable lex) {
    lex->printStackTrace(System::err);
    System::err->println(lex->getMessage());
    return debug_interactive(var);
  }
}

RLispVar 
LispEnvironment::_eval(IN(RLispVar) var)
{
  if (var == Nil)
    return Nil;
  if (instanceof(var, LispSymbol) == true)
    return _eval((RLispSymbol)var);
  else if (instanceof(var, LispAtom) == true)
    return _eval((RLispAtom)var);
  else if (instanceof(var, LispList) == true)
    return _eval((RLispList)var);
  else if (instanceof(var, LispBuildInFunction) == true)
    return var;
  else if (instanceof(var, LispObject) == true)
    return var;
  else if (instanceof(var, LispClass) == true)
    return var;
  else 
    THROW2(LispException, this, "Unknown ListVar type: " + var->toCode());
  return Nil;
}

RLispVar quote_invoke()
{
  static RLispVar qi = new LispSymbol("invoke");
  return qi;
}


RLispVar 
LispEnvironment::_eval(IN(RLispList) list)
{
  if (list->length() == 0)
    return Nil;
  RLispVar funcvar = list->car();
/*
#ifdef ACDK_DEBUG
  RString tcode = list->toCode();
#endif //ACDK_DEBUG
  */
  if (funcvar == Nil)
    THROW2(LispException, this, "function is NIL in [" + list->toCode() + "]");

  if (funcvar == t())
    return funcvar;
  if (instanceof(funcvar, LispList) == true)
    funcvar = _eval(RLispList(funcvar));
  
  if (instanceof(funcvar, Function) == true)
    return RFunction(funcvar)->eval(this, list);

  RString tstr = funcvar->toString();
  
  RFunction func = lookupFunction(funcvar->toString());
  if (func != Nil)
    return func->eval(this, list);
  RLispVar var = lookupVar(funcvar->toString());
  if (var == Nil)
    THROW2(LispException, this, "cannot find function or var: [" + funcvar->toCode() + "] in [" + list->toCode() + "]");
  if (instanceof(var, Function) == true)
    return RFunction(var)->eval(this, list);
  if (instanceof(var, LispAtom) == true)
  {
    RString lcode = list->toCode();
    RLispVar name = list->cdr()->car();
    RString membername = name->getStringToken();
    if (membername != Nil)
    {
      return _eval(list->unshift(quote_invoke()));
    }
    RString ncode = name->toCode();

  }
  THROW2(LispException, this, "var is not function type: [" + funcvar->toCode() + "] in [" + list->toCode() + "]");
  return Nil;
}

RLispVar 
LispEnvironment::_eval(IN(RLispSymbol) var)
{
  RLispVar erg = lookupVar(var->toString());
  if (erg == Nil)
    return Nil;
  //System::err->println(RString("_eval(") + var->toString() + "): " + erg->toString());
  if (instanceof(erg, LispAtom) == true) {
    RLispAtom atom = (RLispAtom)erg;
    if (atom->val().type == ScriptVar::ObjectType)
      if (atom->getObject() == Nil)
        return Nil;
  }
  return erg;
}

RLispVar 
LispEnvironment::_eval(IN(RLispAtom) var)
{
  return &var; //->toString();
}


//static 
RLispList 
LispEnvironment::parseToList(IN(RString) str)
{
  StringReader strreader(str);
  LispTokenizer in(&strreader);
  int openbrackets = 0;
  Stack<RLispCode> listStack;
  listStack.push(new LispCode()); // contains result list
  int tk;
  while ((tk = in.nextToken()) != StreamTokenizer::TT_EOF) {
    if (tk == '(') {
      listStack.push(new LispCode());
    } else if (tk == ')') {
      RLispList ll = listStack.pop()->code();
      if (ll == Nil)
        ll = new LispList();
      listStack.top()->append((RLispVar)ll);
    } else if (tk == StreamTokenizer::TT_WORD) {
      if (in.sval->equals("NIL") || 
          in.sval->equals("nil") == 0 || 
          in.sval->equals("Nil") == 0)
        listStack.top()->append(Nil);
      else
        listStack.top()->append(new LispSymbol(in.sval));

    } else {
      //System::out->println("Unknown token: " + in.sval);
      listStack.top()->append(new LispSymbol(in.sval));
    }
  }
  return listStack.top()->code();
}

bool 
isInBackQuote(Stack<RLispCode>& listStack)
{
  for (int i = listStack.size() - 1; i <= 0; ++i)
  {
    if (listStack.getFromTop(i)->quotech == '`')
      return true;
  }
  return false;
}

bool quotedSyntax(char quotech)
{
  return quotech == '\'' || quotech == '@' || quotech == '`';
}





RLispCode
LispEnvironment::parse(IN(RLispTokenizer) tin, IN(RPrintWriter) tout, bool interactiv/* = false*/, bool parseOneToken/* = false*/)
{
  int tk;
  Stack<RLispCode> listStack;
  listStack.push(new LispCode()); // contains result list
  int openbrackets = 0;
  bool firstSymbol = false;
  try {
    bool doBreak = false;
    while (doBreak == false && (tk = tin->nextToken()) != StreamTokenizer::TT_EOF) {
      if (parseOneToken == true)
        doBreak = true;

       if (tk == ';') {
         THROW0(Exception);
       }
// (
       if (tk == '(') {
        ++openbrackets;
        listStack.push(new LispCode());
        firstSymbol = true;

// )
      } else if (tk == ')') {
        firstSymbol = false;
        --openbrackets;
        RLispList ll = listStack.pop()->code();
        
        if (ll == Nil) { // empty list ()
          ll = new LispList();
        }
        RLispVar replaced = &ll;
        int qch = listStack.top()->quotech;
        if (qch == 'M')
        {
          _eval(ll); // registers Macro
        } 
        else if (qch == 'm')
        {
          //RString tstr = ll->toCode();
          RFunction func = lookupFunction(ll->car()->toString());
          if (func == Nil)
            ; // ### oops
          RLispVar erg = func->eval(this, ll);
          replaced = erg;
        }
        listStack.top()->append(replaced);
        if (openbrackets == 0 && interactiv == true) {
          if (listStack.top()->quotech != 0) 
          {
            
            listStack.top()->quotech = 0;
            
            RLispList tll = listStack.pop()->code();
            if (qch == 'M')
            {
              RString mcode = tll->toCode();
            }
            listStack.top()->append((RLispVar)tll);
          }
          break;
        }
// TT_NUMBER
      } else if (tk == StreamTokenizer::TT_NUMBER) {
        firstSymbol = false;
        if (tin->sval->length() == 1 && (tin->sval->equals("-")  || tin->sval->equals("+")))
          listStack.top()->append(new LispSymbol(tin->sval));  
        else
          listStack.top()->append(new LispAtom(tin->nval->toScriptVar()));
// TT_STRING
      } else if (tk == StreamTokenizer::TT_STRING) {
        firstSymbol = false;
        listStack.top()->append(new LispAtom(tin->sval));
// Symbol TT_WORD
      } else if (tk == StreamTokenizer::TT_WORD) {

        if (tin->sval->equals("NIL") || 
            tin->sval->equals("nil") || 
            tin->sval->equals("Nil"))
          listStack.top()->append(Nil);
        else if (tin->sval->equals("T")|| 
                 tin->sval->equalsIgnoreCase("true") == true)
          listStack.top()->append((RLispVar)t());
        else {
          if (firstSymbol == true)
          {
            if (tin->sval->equals("defmacro") == true)
              listStack.getFromTop(2)->quotech = 'M';
            else if (isMacro(tin->sval) == true)
              listStack.getFromTop(2)->quotech = 'm'; 
          }
          listStack.top()->append(new LispSymbol(tin->sval));
        }
        firstSymbol = false;

// quote
      } else if (tk == '\'' || tk == '`') { // quote or backquote
        firstSymbol = false;
        listStack.push(new LispCode());
        if (tk == '\'')
          listStack.top()->append(new LispSymbol("quote"));  
        else
          listStack.top()->append(new LispSymbol("backquote"));  
        listStack.top()->quotech = tk;
        continue; // not reach line if (listStack.top()->quoted) {
// comma
      } else if (tk == ',' || tk == '@') {
        firstSymbol = false;
        //if (isInBackQuote(listStack) == false)
        //  THROW1(ParseException,  "Operator , or ,@ are only allowed in backquoted code");
        
        listStack.push(new LispCode());
        if (tk == '@')
          listStack.top()->append(new LispSymbol("commaat"));  
        else
          listStack.top()->append(new LispSymbol("comma"));  
        listStack.top()->quotech = tk;
        RLispCode slc = parse(tin, tout, interactiv, true);
        listStack.top()->append(&slc->code()->car());
        listStack.top()->append(&listStack.pop()->code());
        continue; 

// other
      } else {
        firstSymbol = false;
        if (tk != StreamTokenizer::TT_WORD)
          ;//System::tout->println("Unknown token: " + tin->sval);
        listStack.top()->append(new LispSymbol(tin->sval));
      }
      if (interactiv == true && listStack.size() == 1)
        break;
      if (quotedSyntax(listStack.top()->quotech) == true)
      {
        listStack.top()->quotech = 0;
        RLispList ll = listStack.pop()->code();
        listStack.top()->append((RLispVar)ll);
      }
    }
    if (tk == StreamTokenizer::TT_EOF)
      return listStack.top();
    return listStack.top();
  } catch (acdk::text::RParseException ex) {
    ex->printStackTrace(System::err);
    System::err->println(tin->currentLineReference() + ": " + ex->getMessage() + "\n\t: " + tin->currentLine());
    return Nil;
  }
}

RString
LispEnvironment::load(IN(RLispTokenizer) tok)
{
  RLispCode code = parse(tok, System::err);
  if (code == Nil || code->code() == Nil)
    return Nil;
  RLispList codelist = code->code();
  //RString tstr = codelist->toCode();
  RLispVar erg;
  while (codelist != Nil) {
    StackHolder<RLispVar> __evalStackHolder(_evalStack, codelist->car());
    erg = eval(codelist->car());
    if (_returnNow == true) {
      _returnNow = false;
      break;
    }
    codelist = codelist->cdr();
  }
  if (erg == Nil)
    return Nil;
  return erg->toCode();
}

RString
LispEnvironment::load(IN(RString) filename)
{
  try {
    RFile file;
    RString sav = _environment->getProperty("*thisfile*");
    if (File(filename).isAbsolute() == false && _modulStack.empty() == false) {
      file = new File(_modulStack.top()->getParentFile(), filename);
    } else
      file = new File(filename);
    _modulStack.push(file);
    _environment->setProperty("*thisfile*", file->getCanonicalPath());
    RString erg = "";
    RFileReader fin = new FileReader(file);
    LispTokenizer tok(new ByteToCharReader(&fin, acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()));
    //while (erg != Nil && tok.eof() == false) // why this loop ??
    erg = load(SR(LispTokenizer, tok));
    _modulStack.pop();
    _environment->setProperty("*thisfile*", (sav == Nil)? RString("NIL") : sav);
    return erg;
  } catch (RLispException lex) {
    lex->printStackTrace(System::err);
    System::err->println(lex->getMessage());
    setBreak(1);
  }
  return Nil;
}
/* ###really needed ??
RString 
LispEnvironment::parseEval(const char*& ptr, int& rest)
{
  //RbyteArray carray = new byteArray((const byte*)ptr, rest);
  byteArray ba((const byte*)ptr, rest);
  MemReader memreader(&ba);
  LispTokenizer tok(&memreader);
  if (trace() == true)
    trace_begin("Parse [" + memreader.toString() + "]");
  RLispCode erg = parse(SR(LispTokenizer, tok), System::err);
  if (trace() == true)
    trace_end("to [" + erg->code()->toCode() + "]");
  
  RLispVar lerg = eval((RLispVar)erg->code());
  return lerg->toCode();
}
*/
RLispVar 
LispEnvironment::parseEval(IN(RString) str)
{
  StringReader sr(str);
  LispTokenizer tok(&sr);
  RLispCode erg = parse(&tok, System::err);
  // dbg
  //RString tstr = erg->code()->toCode();
  if (erg == Nil)
    return Nil;
  RLispList list = erg->code();
  RLispVar evaled;
  while (list != Nil)
  {
    evaled = eval(list->car());
    list = list->cdr();
  } 
  return evaled;

}

void 
LispEnvironment::setInOut(IN(RCharReader) rin, IN(RCharWriter) rout, IN(RCharWriter) rerr)
{
  if (instanceof(rin, InputReader))
    in = (RInputReader)rin;
  else
    in = new InputReader(rin);
  
  if (instanceof(rout, PrintWriter))
    out = (RPrintWriter)rout;
  else
    out = new PrintWriter(rout);
  if (instanceof(rerr, PrintWriter))
    err = (RPrintWriter)rerr;
  else
    err = new PrintWriter(rerr);
  RLispVar o = new LispAtom(ScriptVar((RObject)out));
  bindGlobal("out", o);
  bindGlobal("*out*", o);
  bindGlobal("*out", o);
  o = new LispAtom(ScriptVar((RObject)err));
  bindGlobal(RString("err"), o);
  bindGlobal("*err*", o);
  bindGlobal("*err", o);
  o = new LispAtom(ScriptVar((RObject)in));
  bindGlobal("in", o);
  bindGlobal("*in*", o);
  bindGlobal("*in", o);
}

void 
LispEnvironment::interactive(IN(RCharReader) rin, IN(RCharWriter) rout)
{
  setInOut(rin, rout, rout);
  RCharReader tin = (RCharReader)in;
  while (instanceof(tin, AbstractCharFilterReader) == true)
    tin = RAbstractCharFilterReader(tin)->getIn();
  LispTokenizer lisptokenizer(tin);
  _environment->setProperty("*interactive*", RString("TRUE"));

  RLispVar erg;
  while (true) {
    out->print("> ");
    out->flush();
    try {
      RLispCode tcode = parse(SR(LispTokenizer, lisptokenizer), out, true);
      RLispList code = tcode->code();
      if (trace() == true)
      {
        trace_begin("Parsed Code: ");
        trace_end("to " + code->toCode());
      }
      RLispVar erg;
      while (code != Nil) {
        StackHolder<RLispVar> __evalStackHolder(_evalStack, code->car());
/*
#ifdef ACDK_DEBUG
        RString thecode = code->car()->toCode();
#endif //ACDK_DEBUG
*/
        erg = eval(code->car());
        code = code->cdr();
        if (_returnNow == true) {
          _returnNow = false;
          setInOut(&System::in, &System::out, &System::err);
          return;
        }
      }
      RString tstr = (erg == Nil ? RString("NIL") : erg->toCode()); 
      out->println(tstr);
    } catch (RLispException lex) {
      err->println(lex->getMessage());
      setBreak(1);
    }
  }
}

void 
LispEnvironment::bindLocal(IN(RString) symbol, IN(RLispVar) value, bool forcelocal /* = false */)
{
  if (trace() == true)
    traceln("bindLocal: [ $" + Integer::toString((int)_stackFrame.top().getIPtr()) + "::" + symbol->toString() + "]=[" + (value == Nil ? RString("NIL") : value->toCode()) + "]");
#if 0
  if (value == Nil)
    System::err->println(RString("bindLocal: ") + "Unknown" + "::" + symbol + " = NIL");
  else
    System::err->println(RString("bindLocal: ") + value->getName() + "::" + symbol + " = " +  value->toString());
#endif
  _stackFrame.top()->put(symbol, value, forcelocal);
}
 
void 
LispEnvironment::bindGlobal(IN(RString) symbol, IN(RLispVar) value)
{
  if (trace() == true)
    traceln("bindGlobal: [" + symbol->toString() + "]=[" + (value == Nil ? RString("NIL") : value->toCode()) + "]");
#if 0
  if (value == Nil)
    System::err->println(RString("bindGlobal: ") + "Unknown" + "::" + symbol + " = NIL");
  else
    System::err->println(RString("bindGlobal: ") + value->getName() + "::" + symbol + " = " +  value->toString());
#endif
  _globals->put((RObject)symbol, (RObject)value);
}


void 
LispEnvironment::bindToEnv(IN(RString) symbol, IN(RLispVar) value)
{
  if (trace() == true)
    traceln("bindEnv: [" + symbol->toString() + "]=[" + (value == Nil ? RString("NIL") : value->toCode()) + "]");
#if 0
  if (value == Nil)
    System::err->println(RString("bindToEnv: ") + "Unknown" + "::" + symbol + " = NIL");
  else
    System::err->println(RString("bindToEnv: ") + value->getName() + "::" + symbol + " = " +  value->toString());
#endif
  RString str = (value == Nil)? RString("NIL") : value->toString();
  _environment->put(&symbol, &str);
}


RLispVar 
LispEnvironment::lookupVar(IN(RString) symbol, bool warn)
{
  RLispVar v;
  
  if (_stackFrame.top()->containsKey(symbol) == true) {
    v = (RLispVar)_stackFrame.top()->get(symbol);
#if 0
    if (v == Nil)
      System::err->println(RString("lookupVar(") + symbol + ") " + RString("local: ") + "NIL");
    else
      System::err->println(RString("lookupVar(") + symbol + ") " + RString("local: ") + v->getName() + "::" + v->toString());
#endif
    return v;
  }
  RFunction func = lookupFunction(symbol);
  if (func != Nil) {
    return new LispBuildInFunction(symbol, func);
  }
  if (_globals->containsKey((RObject)symbol)) {
    v = (RLispVar)_globals->get((RObject)symbol);
#if 0
    if (v == Nil)
      System::err->println(RString("lookupVar(") + symbol + ") " + RString("global: ") + "NIL");
    else
      System::err->println(RString("lookupVar(") + symbol + ") " + RString("global: ") + v->getName() + "::" + v->toString());
#endif
    return v;
  }
  RString str = _environment->getProperty(symbol);
  if (str != Nil) {
    if (str->equalsIgnoreCase("TRUE"))
      v = (RLispVar)t();
    else if (str->equalsIgnoreCase("NIL")) {
      v = new LispAtom(ScriptVar(RObject(Nil)));
    }
    else 
      v = new LispAtom(str);
#if 0
    if (v == Nil)
      System::err->println(RString("lookupVar(") + symbol + ") " + RString("env: ") + "NIL");
    else
      System::err->println(RString("lookupVar(") + symbol + ") " + RString("env: ") + v->getName() + "::" + v->toString());
#endif
    return v;
  }
#if 0
  System::err->println(RString("lookupVar(") + symbol + ") " + RString("undefined"));
#endif
  if (symbol->equals("ACDK_TOOLS_HOME") == true)
    return new LispAtom(System::getAcdkToolsHome());
  if (symbol->equals("ACDKHOME") == true || symbol->equals("ACDK_HOME") == true)
    return new LispAtom(System::getAcdkHome());
  if (warn == false)
    return Nil;
  traceln("** WARN: Symbol [" + symbol + "] is not defined");
  setBreak(1);
  return Nil; 
}
 
RLispVar 
LispEnvironment::lookupLocalVar(IN(RString) str)
{
  if (_stackFrame.top()->containsKey(str) == false) 
    return Nil;
  return (RLispVar)_stackFrame.top()->get(str);
}

RFunction 
LispEnvironment::lookupFunction(IN(RString) str)
{
  RFunction func = (RFunction)_staticFuncs()->get((RObject)str);
  if (func != Nil)
    return func;
  func = (RFunction)_defuns->get((RObject)str);
  return func;
}


void
dumpStackFrame(IN(RLispStackFrame) cf, IN(RCharWriter) out)
{
  RIterator it = cf->keySet()->iterator();
  while (it->hasNext() == true) 
  {
    RString str = (RString)it->next();
    RLispVar val = (RLispVar)cf->get(str);
    RString tstr = str + "=" + (val == Nil ? RString("Nil") : val->toCode()) + "\n";
    out->writeString(tstr);
    out->flush(); 
  }

}


void
LispEnvironment::dumpEnv(IN(RCharWriter) out)
{
  
  RLispStackFrame cf = _stackFrame.top();
  out->writeString("Current Stack:\n");
  dumpStackFrame(cf, out);
  out->writeString("Globals:\n");
  dumpStackFrame(_globals, out);
  out->writeString("Defuns:\n");
  dumpStackFrame(_defuns, out);
}

RString 
LispEnvironment::loadUnparsedFile(IN(RString) filename)
{
  File f(filename);
  if (f.exists() == false)
    return "";
  jlong l = f.length();
  RcharArray ch = new charArray(l);
  FileReader fin(filename);
  ByteToCharReader cin(&fin, acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder());
  return cin.readString();
}



} // namespace lisp
} // namespace acdk

