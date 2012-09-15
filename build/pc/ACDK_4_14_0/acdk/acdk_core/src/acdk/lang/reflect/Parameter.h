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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Parameter.h,v 1.12 2005/02/11 10:14:54 kommer Exp $

#ifndef acdk_lang_reflect_Parameter_h
#define acdk_lang_reflect_Parameter_h


#include "../IllegalAccessException.h"
#include "../IllegalArgumentException.h"
#include "InvocationTargetException.h"

namespace acdk {
namespace lang {
namespace reflect {

using namespace acdk::lang;


ACDK_DECL_CLASS(Parameter);

/** 
  a method parameter
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC Parameter
: extends AccessibleObject
, implements acdk::lang::dmi::MetaObject
{
  ACDK_WITH_METAINFO(Parameter)
private:
  foreign const acdk::lang::dmi::ClazzInfo* _theClazz;
  foreign const acdk::lang::dmi::ClazzMethodInfo* _methodInfo;
  foreign const acdk::lang::dmi::ClazzMethodArgInfo* _paramInfo;
public:
  /**
    Internal constructor
    @param clazz must not be 0
    @param meth must not be 0
  */
  foreign Parameter(const acdk::lang::dmi::ClazzInfo* clazz, 
            const acdk::lang::dmi::ClazzMethodInfo* meth,
            const acdk::lang::dmi::ClazzMethodArgInfo* paramInfo
            )
  : AccessibleObject()
  , _theClazz(clazz)
  , _methodInfo(meth)
  , _paramInfo(paramInfo)
  {
    setAccessible(paramInfo->flags & acdk::lang::dmi::MiPublic);
  }   
  /// implemented interface for MetaObject
  foreign virtual dmi::MetaInfo* getMetaInfo() { return (dmi::MetaInfo*)_paramInfo; }

  // Member
  foreign virtual RMethod getMethod();
  foreign virtual int getModifiers();
  foreign virtual RString getName();

  // Own
  virtual RClass getType();
  virtual bool equals(IN(RObject) obj);
  virtual int hashCode();
  virtual RString toString();
  /** render the method to an identifier including 'namemangling' */
  virtual RString toIndentifier();
  virtual RString toTypeString(int format = acdk::lang::dmi::TpFtFormatStandard) { return _paramInfo == 0 ? String::emptyString() : _paramInfo->toTypeString(format); }
};

} // reflect
} // lang
} // acdk

#endif //acdk_lang_reflect_Parameter_h

