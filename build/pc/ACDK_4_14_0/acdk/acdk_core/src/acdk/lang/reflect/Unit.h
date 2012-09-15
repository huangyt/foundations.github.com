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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Unit.h,v 1.15 2005/02/11 10:14:54 kommer Exp $

#ifndef acdk_lang_reflect_Unit_h
#define acdk_lang_reflect_Unit_h


#include "../IllegalAccessException.h"
#include "../IllegalArgumentException.h"
#include "InvocationTargetException.h"
#include "Enumeration.h"

namespace acdk {
namespace lang {
namespace reflect {

ACDK_DECL_CLASS(Unit);

/**
  Represents the Unit: a namespace with ACDK classes
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC Unit
: extends AccessibleObject
, implements acdk::lang::dmi::MetaObject
{
  ACDK_WITH_METAINFO(Unit)
private:
  foreign const acdk::lang::dmi::UnitInfo* _unitInfo;

public:
  /**
    internal constructor
  */
  foreign Unit(const acdk::lang::dmi::UnitInfo* unitInfo)
  : AccessibleObject()
  , _unitInfo(unitInfo)
  {
    setAccessible(unitInfo->flags & acdk::lang::dmi::MiPublic);
  }
  /**
    return the unit with given name.
    @param unitname name of unit in the form
      "acdk::util::logging" or
      "acdk/util/logging"
    @return Nil if given unit is not registered
  */
  static RUnit getUnit(IN(RString) unitname);
  /**
    returns all registered Units
  */
  static RUnitArray getUnits();
  /**
    returns all classes registered in this unit
  */
  RClassArray getClasses();

  /**
    returns the parent of this unit
    If no parent returns Nil
    This method only regards already loaded packages
  */
  RUnit getParent();
  /**
    return the childs (nested) units
    @param recursive if true all nested childs. Otherwise only direct childs.
    This method only regards already loaded packages
  */
  RUnitArray getChilds(bool recursive = false);

  /**
    return all enumerations in this unit
  */
  REnumerationArray getEnumerations();
  /// implemented interface for MetaObject
  foreign virtual dmi::MetaInfo* getMetaInfo() { return (dmi::MetaInfo*)_unitInfo; }

  foreign virtual RString getName();
  virtual bool equals(IN(RObject) obj);
  int compareTo(IN(RUnit) obj) { return getName()->compareTo(obj->getName()); }
  foreign int compareTo(IN(RObject) obj) { return compareTo(RUnit(obj)); }
  virtual int hashCode() { return getName()->hashCode(); }
  virtual RString toString() { return getName(); }
  virtual RString toIndentifier();
  virtual RString toTypeString(int format = acdk::lang::dmi::TpFtFormatStandard) { return _unitInfo == 0 ? String::emptyString() : _unitInfo->toTypeString(format); }
};

} // reflect
} // lang
} // acdk

#endif //acdk_lang_reflect_Unit_h

