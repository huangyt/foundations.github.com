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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispInterpreter.h,v 1.17 2005/03/08 18:54:11 kommer Exp $

#ifndef acdk_lisp_LispInterpreter_h
#define acdk_lisp_LispInterpreter_h

#include <acdk.h>
#include "lisp.h"

#include "LispEnvironment.h"

#include <acdk/lang/System.h>
#include <acdk/lang/dmi/ScriptInterpreter.h>


namespace acdk {
namespace lisp {

using namespace acdk::lang;
USING_CLASS(::acdk::lang::dmi::, ScriptInterpreter);

ACDK_DECL_CLASS(LispInterpreter);



class ACDK_ACDK_LISP_PUBLIC LispInterpreter
: extends ::acdk::lang::Object
, implements ::acdk::lang::dmi::ScriptInterpreter
{
  ACDK_WITH_METAINFO(LispInterpreter)
private:
  RLispEnvironment _lenv;
public:
  LispInterpreter()
  : _lenv(new LispEnvironment(System::getProperties()))
  {
  }
  void init() { _lenv->init(); }
  /**
    Parse a file. On some interpreter also execute the script.
    @param file the file to parse
  */
  virtual void parse(IN(RFile) file)
  {
    _lenv->load(file->getCanonicalPath());
  }
  /**
    Parse a file. On some interpreter also execute the script.
    @param script the script to parse
  */
  virtual void parse(IN(RString) script)
  {
    _lenv->load(script);
  }
  /**
    Evaluate the code . On some interpreter it is equal to parse.
    @param script the script to eval
    @return the output or result of the script fragment
  */
  virtual RObject eval(IN(RString) code)
  {
    return &_lenv->parseEval(code);
  }
  /**
    Calls a script function.
    Note: may not all script interpreter support this function
    @param func the name of the function
    @param args the arguments for the function
    @return result of the call
  */
  virtual acdk::lang::dmi::ScriptVar call(IN(RString) func, acdk::lang::dmi::ScriptVarArray& args)
  {
    _throwNotImplementedYet("LispInterpreter::call()");
    return acdk::lang::dmi::ScriptVar(0);
  }
  /**
    Calls a script method of given object.
    @param obj the 'this' object of the script, which is a wrapper to the scripting
    @param func the name of the function
    @param args the arguments for the function
    @return result of the call
  */
  virtual acdk::lang::dmi::ScriptVar invoke(IN(RObject) obj, IN(RString) func, acdk::lang::dmi::ScriptVarArray& args)
  {
    _throwNotImplementedYet("LispInterpreter::invoke()");
    return acdk::lang::dmi::ScriptVar(0);
  }
  /**
    Do debug the given Script. May not work on all scripting languages
    @param in Inputstream for users input and script
    @param out Scripts output and users Echo output
    @param err Error stream
  */
  virtual void interactive(IN(RCharReader) in, IN(RCharWriter) out, IN(RCharWriter) err)
  {
    _lenv->interactive(in, out);
  }
  RLispEnvironment lispEnvironment() { return _lenv; }
};


} // namespace lisp
} // namespace acdk

#endif //acdk_lisp_LispInterpreter_h

