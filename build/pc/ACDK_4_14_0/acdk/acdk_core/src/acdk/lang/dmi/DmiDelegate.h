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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiDelegate.h,v 1.10 2005/04/06 19:11:00 kommer Exp $

#ifndef acdk_lang_dmi_DmiDelegate_h
#define acdk_lang_dmi_DmiDelegate_h


#include <acdk.h>
#include "DmiObject.h"
#include "DmiNamedArg.h"

namespace acdk {
namespace lang {
namespace dmi {


ACDK_DECL_INTERFACE(DmiDelegate);
/**
  standard interface for a delegate.
  @see gw_ref[acdk_hb_dmi_delegate]
*/
class ACDK_CORE_PUBLIC DmiDelegate
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DmiDelegate)
public:
  /** call the method with variable arguments */
  virtual RDmiObject call(IN(RDmiObjectArray) args) = 0;
  /** call the method with variable named arguments */
  virtual RDmiObject call(IN(RDmiNamedArgArray) namedArgs) = 0;
  /**
    internal 
    by default calls call(IN(RDmiObjectArray) args)
  */
  foreign virtual ScriptVar call(INOUT(ScriptVarArray) args);
  /** used for C++ usage */
  foreign ScriptVar call()
  {
    ScriptVarArray sv(0);
    return call(sv);
  }
  /** used for C++ usage */
  foreign ScriptVar call(IN(ScriptVar) sv0)
  {
    ScriptVarArray sv(1);
    sv[0] = sv0;
    return call(sv);
  }
  /** used for C++ usage */
  foreign ScriptVar call(IN(ScriptVar) sv0, IN(ScriptVar) sv1)
  {
    ScriptVarArray sv(2);
    sv[0] = sv0;
    sv[1] = sv1;
    return call(sv);
  }
  /** used for C++ usage */
  foreign ScriptVar call(IN(ScriptVar) sv0, IN(ScriptVar) sv1, IN(ScriptVar) sv2)
  {
    ScriptVarArray sv(3);
    sv[0] = sv0;
    sv[1] = sv1;
    sv[2] = sv2;
    return call(sv);
  }
  /** used for C++ usage */
  foreign ScriptVar call(IN(ScriptVar) sv0, IN(ScriptVar) sv1, IN(ScriptVar) sv2, IN(ScriptVar) sv3)
  {
    ScriptVarArray sv(4);
    sv[0] = sv0;
    sv[1] = sv1;
    sv[2] = sv2;
    sv[3] = sv3;
    return call(sv);
  }
  /** used for C++ usage */
  foreign ScriptVar call(IN(ScriptVar) sv0, IN(ScriptVar) sv1, IN(ScriptVar) sv2, IN(ScriptVar) sv3, IN(ScriptVar) sv4)
  {
    ScriptVarArray sv(5);
    sv[0] = sv0;
    sv[1] = sv1;
    sv[2] = sv2;
    sv[3] = sv3;
    sv[4] = sv4;
    return call(sv);
  }
  /** used for C++ usage */
  foreign ScriptVar call(IN(ScriptVar) sv0, IN(ScriptVar) sv1, IN(ScriptVar) sv2, IN(ScriptVar) sv3, 
                         IN(ScriptVar) sv4, IN(ScriptVar) sv5)
  {
    ScriptVarArray sv(6);
    sv[0] = sv0;
    sv[1] = sv1;
    sv[2] = sv2;
    sv[3] = sv3;
    sv[4] = sv4;
    sv[5] = sv5;
    return call(sv);
  }
};

ACDK_DECL_CLASS(StdDmiDelegate);
 

/**
  Standard DMI implementation Delegate call to underlying object via dmi.
  @see gw_ref[acdk_hb_dmi_delegate]
*/
class ACDK_CORE_PUBLIC StdDmiDelegate
: extends ::acdk::lang::Object
, implements DmiDelegate
{
  ACDK_WITH_METAINFO(StdDmiDelegate)
protected:
  RObject _object;
  RString _methodName;
  RClass _class;
public:
  /**
    invokes a non static method of given obj
  */
  StdDmiDelegate(IN(RObject) obj, IN(RString) methodName)
  : _object(obj)
  , _methodName(methodName)
  {
  }
  /**
    invokes a static method of given class
  */
  StdDmiDelegate(IN(RClass) cls, IN(RString) methodName)
  : _methodName(methodName)
  , _class(cls)
  {
  }
  /** get target object. May be Nil if used for a static method */
  RObject getObject() { return _object; }
  /** get target class. May be Nil if used for a non static method */
  RClass getClass() { return _class; }
  RString getMethodName() { return _methodName; }
  virtual RDmiObject call(IN(RDmiObjectArray) args);
  virtual RDmiObject call(IN(RDmiNamedArgArray) namedArgs);
  foreign ScriptVar call(INOUT(ScriptVarArray) args);
};

} // namespace dmi
} // namespace lang
} // namespace acdk

#endif // acdk_lang_dmi_DmiDelegate_h

