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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Enumeration.h,v 1.13 2005/03/06 11:54:22 kommer Exp $

#ifndef acdk_lang_reflect_Enumeration_h
#define acdk_lang_reflect_Enumeration_h


#include "../IllegalAccessException.h"
#include "../IllegalArgumentException.h"
#include "InvocationTargetException.h"

namespace acdk {
namespace lang {
namespace reflect {





ACDK_DECL_CLASS(Enumeration);
ACDK_DECL_CLASS(EnumerationValue);

/**
  represents an enumeration value
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC EnumerationValue
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(EnumerationValue)
private:
  foreign const acdk::lang::dmi::ClazzEnumInfo* _enumInfo;
  foreign const acdk::lang::dmi::ClazzEnumValueInfo* _enumValue;
public:
  foreign EnumerationValue(const acdk::lang::dmi::ClazzEnumInfo* ei,
                   const acdk::lang::dmi::ClazzEnumValueInfo* ev)
  : _enumInfo(ei)
  , _enumValue(ev)
  {
  }
  foreign virtual RString getName() { return _enumValue->name; }
  foreign virtual RString toString() { return getName(); }
  virtual RString toTypeString(int format = acdk::lang::dmi::TpFtFormatStandard) { return _enumValue == 0 ? String::emptyString() : _enumValue->toTypeString(format); }
  foreign int getValue() { return _enumValue->value; }
  REnumeration getEnumeration();
};


ACDK_DECL_CLASS(Enumeration);

/**
  Represents the Enumeration
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC Enumeration
: extends AccessibleObject
, implements acdk::lang::dmi::MetaObject
{
  ACDK_WITH_METAINFO(Enumeration)
private:
  foreign const acdk::lang::dmi::ClazzEnumInfo* _enumInfo;

public:
  /**
    internal constructor
  */
  foreign Enumeration(const acdk::lang::dmi::ClazzEnumInfo* ei)
  : AccessibleObject(true)
  , _enumInfo(ei)
  {
  }
  /**
    return the Enumeration with given name.
    @param enumName name of Enumeration in the form
    @param namesp namespace of the enumeration, may be Nil
    @return Nil if given Enumeration is not registered
  */
  static REnumeration getEnumeration(IN(RString) enumName, IN(RString) namesp);
  /**
    return the Enumeration with given name.
    @param enumName name of Enumeration in the form
    @param namesp namespace of the enumeration, may be Nil
    @return Nil if given Enumeration is not registered
  */
  static REnumerationValue getEnumerationValue(IN(RString) enumName, IN(RString) namesp);
  /**
    return the symbolic name (without namespace) 
      for the given value.
    return Nil if this enumeration has no such value
    If an enumeration multiple enum values has the same integer value
    this method returns the first found.
  */
  RString getEnumNameForValue(int value);
  /**
    return the symbolic name (without namespace) 
      for the given value.
    return Nil if no information for enumName can be found or this enumeration has no such value
    @seealso getEnumNameForValue
  */
  static RString getEnumNameForValue(IN(RString) enumName, int value);
  /**
    returns all registered Enumerations
  */
  static REnumerationArray getEnumerations();


  /// implemented interface for MetaObject
  foreign virtual dmi::MetaInfo* getMetaInfo() { return (dmi::MetaInfo*)_enumInfo; }
  foreign const acdk::lang::dmi::ClazzEnumInfo* getClazzEnumInfo() { return _enumInfo; }
  foreign virtual RString getName();
  virtual bool equals(IN(RObject) obj);
  int compareTo(IN(REnumeration) other) { return getName()->compareTo(other->getName()); }
  foreign int compareTo(IN(RObject) other) { return compareTo(REnumeration(other)); }
  virtual int hashCode() { return getName()->hashCode(); }
  virtual RString toString() { return getName(); }
  virtual RString toIndentifier();
  virtual RString toTypeString(int format = acdk::lang::dmi::TpFtFormatStandard) { return _enumInfo == 0 ? String::emptyString() : _enumInfo->toTypeString(format); }
  REnumerationValueArray getValues();

};

inline
REnumeration
EnumerationValue::getEnumeration()
{
  return new Enumeration(_enumInfo);
}

} // reflect
} // lang
} // acdk

#endif //acdk_lang_reflect_Enumeration_h

