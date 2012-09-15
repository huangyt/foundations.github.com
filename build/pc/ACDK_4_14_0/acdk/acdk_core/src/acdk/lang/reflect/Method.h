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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Method.h,v 1.26 2005/02/11 10:14:54 kommer Exp $

#ifndef acdk_lang_reflect_Method_h
#define acdk_lang_reflect_Method_h


#include "../IllegalAccessException.h"
#include "../IllegalArgumentException.h"
#include "InvocationTargetException.h"
#include "Parameter.h"

namespace acdk {
namespace lang {
namespace reflect {

using namespace acdk::lang;

ACDK_DECL_CLASS(Method);

/** 
  Represents a method of a class
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC Method
: extends AccessibleObject
, implements Member
, implements acdk::lang::dmi::MetaObject
{
  ACDK_WITH_METAINFO(Method)
private:
  foreign const acdk::lang::dmi::ClazzInfo* _theClazz;
  foreign const acdk::lang::dmi::ClazzMethodInfo* _methodInfo;
public:
  /**
    Internal constructor
    @param clazz must not be 0
    @param meth must not be 0
  */
  foreign Method(const acdk::lang::dmi::ClazzInfo* clazz, const acdk::lang::dmi::ClazzMethodInfo* meth)
  : AccessibleObject(),
    _theClazz(clazz),
    _methodInfo(meth)
  {
    setAccessible(meth->flags & acdk::lang::dmi::MiPublic);
  }   
  /// implemented interface for MetaObject
  foreign virtual dmi::MetaInfo* getMetaInfo() { return (dmi::MetaInfo*)_methodInfo; }

  // Member
  foreign virtual RClass getDeclaringClass();
  foreign virtual int getModifiers();
  foreign virtual RString getName();
  RString getAlternativeName();
  // Own
  virtual RClass getReturnType();
  virtual RClassArray getParameterTypes();
  virtual RClassArray getExceptionTypes();
  int getParameterCount();
  RParameterArray getParameters();
  RParameter getParameter(int idx);
  RParameter getParameter(IN(RString) idx);

  virtual bool equals(IN(RObject) obj);
  virtual int hashCode();
  virtual RString toString();
  /** render the method to an identifier including 'namemangling' */
  virtual RString toIndentifier();
  virtual RString toTypeString(int format = acdk::lang::dmi::TpFtFormatStandard) { return _methodInfo == 0 ? String::emptyString() : _methodInfo->toTypeString(_theClazz, format); }
  virtual RObject invoke(IN(RObject) obj, IN(RObjectArray) args) THROWS3(::acdk::lang::RIllegalAccessException, 
								                                                         ::acdk::lang::RIllegalArgumentException,
								                                                         ::acdk::lang::reflect::RInvocationTargetException);
  /**
    return a static string encodes the operator char like '=', '+' etc
  */
  foreign foreign static char* encodeOperatorChar(char c);
  
  

  /**
    return the encoded operator chars to a function name
    "+=" returns "operator_pl_as"
    May throw ::acdk::lang::RIllegalArgumentException if operator is not known
  */
  static RString encodeOperatorToFuncName(IN(RString) opchars);
  /*
    ### FIXME
  foreign static char decodeFuncNameToOpChar(String::iterator& it, String::iterator end);
  foreign static RString decodeFuncNameToOperator(String::iterator begin, String::iterator end);
  */
  /**
    expects an mappend operator name like "operator_pl_as" return "+="
    ### FIXME
  static RString decodeFuncNameToOperator(IN(RString) opstr) { return  decodeFuncNameToOperator(opstr->begin(), opstr->end()); }
  */
  
};

} // reflect
} // lang
} // acdk

#endif //acdk_lang_reflect_Method_h

