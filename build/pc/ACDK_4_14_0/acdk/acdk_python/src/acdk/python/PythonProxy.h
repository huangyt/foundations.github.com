// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public
// License (LGPL) or the Q public License (QPL).
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
// $Header: /cvsroot/acdk/acdk/acdk_python/src/acdk/python/PythonProxy.h,v 1.2 2005/03/31 21:24:35 kommer Exp $

#ifndef acdk_python_PythonProxy_h
#define acdk_python_PythonProxy_h

#include <acdk.h>
#include "Config.h"
#include "PythonSys.h"

#ifdef _DEBUG
# undef _DEBUG
# define _DEBUG_WAS_DEFINED
#endif
#include <Python.h>
#ifdef _DEBUG_WAS_DEFINED
# define _DEBUG
#endif

namespace acdk {
namespace python {

/* 
  @bug this class is not yet implemented
*/
//ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiCiWeakBind))
class ACDK_PYTHON_PUBLIC PythonProxy
: extends ::acdk::lang::Object
{
  foreign PyObject* _pyObject;
public:
  foreign PythonProxy(PyObject* pyObject)
    : _pyObject(pyObject)
  {
  }
  const acdk::lang::dmi::ClazzMethodInfo* 
  standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/);
};


} // namespace python 
} //namespace acdk 


#endif //acdk_python_PythonProxy_h

