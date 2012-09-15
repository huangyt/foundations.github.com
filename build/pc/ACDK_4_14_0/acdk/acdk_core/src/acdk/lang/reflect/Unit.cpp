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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Unit.cpp,v 1.11 2005/02/05 10:44:59 kommer Exp $

#include <acdk.h>
#include "Unit.h"

namespace acdk {
namespace lang {
namespace dmi {
extern UnitInfo* _unitInfoRoot;
} // dmi

namespace reflect {

//foreign virtual 
RString 
Unit::getName()
{
  return _unitInfo->toTypeString(acdk::lang::dmi::TpFtFqName | acdk::lang::dmi::TpFtAcdkType);
}


//virtual 
bool 
Unit::equals(IN(RObject) obj)
{
  if (instanceof(obj, Unit) == false)
    return false;
  return RUnit(obj)->getName()->equals(getName()) == true;
}


RClassArray 
Unit::getClasses()
{
  if (_unitInfo == 0)
    return new ClassArray(0);
  RClassArray ca = new ClassArray(0);
  const ::acdk::lang::dmi::NamedScopedMetaInfo* ci = _unitInfo->_firstChild;
  while (ci)
  {
    if (ci->isClazzInfo() == true)
      ca->append(Class::getSingeltonClass(reinterpret_cast<const acdk::lang::dmi::ClazzInfo*>(ci)));
    ci = ci->_nextScopeSibling;
  }
  return ca;
}

REnumerationArray 
Unit::getEnumerations()
{
  if (_unitInfo == 0)
    return new EnumerationArray(0);
  REnumerationArray ret = new EnumerationArray(0);
  const ::acdk::lang::dmi::NamedScopedMetaInfo* ci = _unitInfo->_firstChild;
  while (ci)
  {
    if (ci->isEnumInfo() == true)
      ret->append(new Enumeration(reinterpret_cast<const acdk::lang::dmi::ClazzEnumInfo*>(ci)));
    ci = ci->_nextScopeSibling;
  }
  return ret;
}

RUnit 
Unit::getParent()
{
  return getUnit(_unitInfo->ns);
}

RUnitArray 
Unit::getChilds(bool recursive)
{
  RUnitArray ua = new UnitArray(0);
  const ::acdk::lang::dmi::NamedScopedMetaInfo* ui = _unitInfo->_firstChild;
  while (ui != 0)
  {
    if (ui->isUnitInfo() == true)
    {
      RUnit nunit = new Unit(reinterpret_cast<const ::acdk::lang::dmi::UnitInfo*>(ui));
      ua->append(nunit);
      if (recursive == true)
        ua->concat(nunit->getChilds(true));
    }
    ui = ui->_nextScopeSibling;
  }
  return ua;
}

//virtual 
RString 
Unit::toIndentifier()
{
  return toString()->replace("::", "_");
}

//static 
RUnit 
Unit::getUnit(IN(RString) unitname)
{
  const acdk::lang::dmi::MetaInfo* mi = acdk::lang::dmi::MetaInfo::findMetaInfo(unitname, acdk::lang::dmi::MiUnitInfo, false);
  if (mi == 0)
    return Nil;
  if (mi->isUnitInfo() == false)
    return Nil;
  return new Unit(reinterpret_cast<const acdk::lang::dmi::UnitInfo*>(mi));
}

//static 
RUnitArray 
Unit::getUnits()
{

  ::acdk::lang::dmi::UnitInfo* ui = ::acdk::lang::dmi::_unitInfoRoot;
  Unit unit(ui);
  return unit.getChilds(true);
}

} // reflect
} // lang
} // acdk



