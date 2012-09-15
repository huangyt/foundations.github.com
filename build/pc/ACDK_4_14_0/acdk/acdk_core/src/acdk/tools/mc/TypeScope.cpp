
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



#include "TypeScope.h"

#include <acdk/lang/Character.h>

namespace acdk {
namespace tools {
namespace mc {

//static 
bool 
TypeScope::isBasicType(IN(RString) type)
{
  if (type->equals("bool") ||
      type->equals("char")  ||
      type->equals("ucchar")  ||
      type->equals("uc2char")  ||
      type->equals("byte")  ||
      type->equals("short")  ||
      type->equals("int")  ||
      //type->equals("long")  ||
      type->equals("jlong") ||
      type->equals("float")  ||
      type->equals("double") ||
      type->equals("void") == true )
    return true;
  return false;
}

//static 
bool 
TypeScope::isObjectType(IN(RString) type)
{
  RString tmp = type;
  int pos = type->lastIndexOf(':');
  if (pos > 1 && type->length() > pos + 3 &&
    type->charAt(pos - 1) == ':') // Paranoia setting.
    tmp = type->substring(pos + 1);
  if (tmp->length() > 2 &&
    tmp->charAt(0) == 'R')
  {
    if (Character::isUpperCase(tmp->charAt(1)) == true)
      return true;
    if (tmp->endsWith("Array") == false)
      return false;
    RString st = tmp->substr(1, tmp->length() - strlen("Array"));
    if (isBasicType(st) || st->equals("long"))
      return true;
  }
  return false;
}

bool 
TypeScope::checkCompatibleType(IN(RString) str)
{
  return isBasicType(str) ||
         isObjectType(str) ||
         hasType(str) != TsUnknown;
}

TypeScopeType 
TypeScope::hasType(IN(RString) symbol)
{
  int idx = symbol->lastIndexOf("::");
  RString ns;
  RString symb = symbol;
  if (idx != -1)
  {
    symb = symbol->substr(idx + 1);
    ns = symbol->substr(0, idx - 1);
  }
  RObject obj = _types->get((RObject)symbol );
  if (obj == Nil)
    obj = _types->get((RObject)symb);
  if (obj == Nil)  
  {
    if (_parent != Nil)
      return _parent->hasType(symbol);
    return TsUnknown;
  }
  return (TypeScopeType)RInteger(obj)->intValue();  
}

RString 
TypeScope::clazzInfoExpr(IN(RString) tpname)
{
  if (tpname->equals("bool") == true) 
    return "::acdk::lang::dmi::ClazzInfo::getBoolClazz()";
  if (tpname->equals("char") == true)
    return "::acdk::lang::dmi::ClazzInfo::getCharClazz()";
  if (tpname->equals("ucchar") == true || tpname->equals("uc2char") == true)
    return "::acdk::lang::dmi::ClazzInfo::getUcCharClazz()";
  if (tpname->equals("byte") == true)
    return "::acdk::lang::dmi::ClazzInfo::getByteClazz()";
  if (tpname->equals("short") == true)
    return "::acdk::lang::dmi::ClazzInfo::getShortClazz()";
  if (tpname->equals("int") == true)
    return "::acdk::lang::dmi::ClazzInfo::getIntClazz()";
  if (tpname->equals("jlong") == true)
    return "::acdk::lang::dmi::ClazzInfo::getLongClazz()";
  if (tpname->equals("float") == true)
    return "::acdk::lang::dmi::ClazzInfo::getFloatClazz()";
  if (tpname->equals("double") == true)
    return "::acdk::lang::dmi::ClazzInfo::getDoubleClazz()";
  if (tpname->equals("void") == true)
    return "::acdk::lang::dmi::ClazzInfo::getVoidClazz()";
  RString basetpname = tpname;
  if (basetpname->lastIndexOf((RString)":") != -1)
    basetpname = basetpname->substr(basetpname->lastIndexOf(":") + 1);
  if (hasType(tpname) == TsEnum)
    return "::acdk::lang::dmi::ClazzInfo::getIntClazz()";
  if (basetpname->charAt(0) == 'R')
    return tpname + "::clazzInfo()";
  return "::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz()";
}

bool 
TypeScope::isInvalidDeclStartIdentifier(IN(RString) str)
{
  if (str->equals("friend") == true || str->equals("template") == true || str->equals("class") == true || 
      str->equals("typedef") == true || str->equals("foreign") == true)
    return true;
  return false;
}
} // namespace mc
} // namespace tools
} // namespace acdk


