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

#include "PythonSys.h"
#include "PythonInterpreter.h"




namespace acdk {
namespace python {

void initAcdkPythonProxy();

//static 
bool PythonInterpreter::_initilized = false;

PythonInterpreter::PythonInterpreter()
: _lastReturnCode(0)
{
  if (PythonInterpreter::_initilized == false) {
    ::Py_Initialize();
    int i = Py_IsInitialized();
    initAcdkPythonProxy();
    PythonInterpreter::_initilized = true; 
  }
}

PythonInterpreter::~PythonInterpreter()
{
  Py_Finalize();
  PythonInterpreter::_initilized = false; 
}

//virtual 
void 
PythonInterpreter::parse(IN(::acdk::io::RFile) file)
{
  parse(file->getCanonicalPath());
}


//virtual 
void 
PythonInterpreter::parse(IN(RString) script) 
{
  RString fn = script;
  char* cfn = strdup(fn->c_str());
  FILE* fp =::fopen(cfn, "r");
  int ret = PyRun_AnyFile(fp, cfn);

}

//virtual 
RObject 
PythonInterpreter::eval(IN(RString) code)
{
  char* buffer = strdup(code->c_str());
  _lastReturnCode = PyRun_SimpleString(buffer);
  return Nil;
}

//virtual 
acdk::lang::dmi::ScriptVar 
PythonInterpreter::call(IN(RString) func, acdk::lang::dmi::ScriptVarArray& args) 
{
  return acdk::lang::dmi::ScriptVar ();
}


//virtual 
acdk::lang::dmi::ScriptVar 
PythonInterpreter::invoke(IN(RObject) obj, IN(RString) func, acdk::lang::dmi::ScriptVarArray& args) 
{
  return acdk::lang::dmi::ScriptVar ();
}


//virtual 
void 
PythonInterpreter::interactive(IN(::acdk::io::RCharReader) in, IN(::acdk::io::RCharWriter) out, IN(::acdk::io::RCharWriter) err) 
{
  int ret = PyRun_InteractiveLoop(stdin, "");
  _lastReturnCode = ret;
}

} // namespace python 
} //namespace acdk 

