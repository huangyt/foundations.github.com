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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Constructor.h,v 1.20 2005/02/11 10:14:54 kommer Exp $

#ifndef acdk_lang_reflect_Constructor_h
#define acdk_lang_reflect_Constructor_h

#include "../ObjectArrayImpl.h"
#include "Parameter.h"

namespace acdk {
namespace lang {
namespace reflect {

using namespace acdk::lang;

ACDK_DECL_CLASS(Constructor);

/** 
  Represents a constructor of a class
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC Constructor
: extends AccessibleObject
, implements Member
, implements acdk::lang::dmi::MetaObject
{
  ACDK_WITH_METAINFO(Constructor)
private:
  foreign const acdk::lang::dmi::ClazzInfo* _theClazz;
  foreign const acdk::lang::dmi::ClazzMethodInfo* _methodInfo;
public:
  /**
    Internal constructor
    @param clazz must not be 0
    @param meth must not be 0
  */
  foreign Constructor(const acdk::lang::dmi::ClazzInfo* clazz, const acdk::lang::dmi::ClazzMethodInfo* meth)
  : AccessibleObject(),
    _theClazz(clazz),
    _methodInfo(meth)
  {
    setAccessible(meth->flags & dmi::MiPublic);
  }   
  /// implemented interface for MetaObject
  foreign virtual dmi::MetaInfo* getMetaInfo() { return (dmi::MetaInfo*)_methodInfo; }
  virtual RString toTypeString(int format = acdk::lang::dmi::TpFtFormatStandard) { return _methodInfo == 0 ? String::emptyString() : _methodInfo->toTypeString(_theClazz, format); }

  // Member
  foreign virtual RClass getDeclaringClass();
  foreign virtual int getModifiers();
  foreign virtual RString getName();
  
  int getParameterCount();
  RParameterArray getParameters();
  RParameter getParameter(int idx);
  RParameter getParameter(IN(RString) idx);

  virtual bool equals(IN(RObject) obj);
  virtual RClassArray getExceptionTypes();
  virtual RClassArray getParameterTypes();
  virtual int hashCode();
  virtual RObject newInstance(IN(RObjectArray) initargs);
  virtual RString toString();
  foreign const acdk::lang::dmi::ClazzInfo* getObjectClazzInfo() { return _theClazz; }
  foreign const acdk::lang::dmi::ClazzMethodInfo* getObjectMethodInfo() { return _methodInfo; }
};

} // Reflect
} // Lang
} // acdk

#endif //acdk_lang_reflect_Constructor_h

