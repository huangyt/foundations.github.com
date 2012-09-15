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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/AcdkStdWeakTypeDmiClient.cpp,v 1.19 2005/04/26 11:14:11 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>


#include <acdk/lang/NumberFormatException.h>

#include "AcdkStdWeakTypeDmiClient.h"
#include "DmiObject.h"

namespace acdk {
namespace lang {
namespace dmi {


int 
AcdkStdWeakTypeDmiClient::typeDistance(const ScriptVar& arg, const ClazzInfo* toType, int svcastflags)
{
  const ClazzInfo* vclazz = arg.getValueClazzInfo();
  if (vclazz == toType)
    return 0;

  if (toType->isBasicClazz() == false)
  {
    int dist = AcdkDmiClient::getTypeDistance(arg, toType);
    if (dist != -1)
      return dist;
  }
  
  RObject obj;
  if (arg.isObjectType())
    obj = arg.getObjectVar();

  if (svcastflags & SVCastUnwrapDmiObject)
  {
    if (instanceof(obj, DmiObject) == true) 
      return typeDistance(*RDmiObject(obj), toType, svcastflags);
  }
  
  if (svcastflags & SVCastWrapDmiObject && DmiObject::clazzInfo() == toType)
  {
    return 200;
  }
  
  if (toType == ClazzInfo::getBoolClazz())
  {
    
    if (svcastflags & SVCastDecodeString && arg.isStringType() == true) 
    {
      RString str = arg.getStringVar();
      if (str->equalsIgnoreCase("true") == true || str->equalsIgnoreCase("t") == true)
      {
        return 300;
      }
      if (str->equalsIgnoreCase("false") == true)
      {
        return 300;
      }
    }
    if (obj != Nil)
    {
      if (svcastflags & SVCastAutobox && instanceof(obj, Boolean) == true)
      {
        return 150;
      }
    }
    if (svcastflags & SVCastNum2Bool)
    {
      int dc = typeDistance(arg, ClazzInfo::getIntClazz(), svcastflags & ~SVCastNum2Bool);
      if (dc != -1)
        return dc + 100;
    }
  }
  else if (toType == ClazzInfo::getCharClazz() || toType == ClazzInfo::getUcCharClazz())
  {
    if (arg.isCharacterType() == true)
    {
      if (toType == ClazzInfo::getCharClazz() && vclazz == ClazzInfo::getUcCharClazz())
      {
        if (svcastflags & SVCastCheckOvervflow)
        {
          if (arg.getUcCharVar() > 127)
            return -1;
        }
      }
      return 5;
    }
    
    if (svcastflags & SVCastDecodeString && arg.isStringType() == true) 
    {
      RString str = arg.getStringVar();
      if (str->length() == 1)
      {
        int tc = typeDistance(inOf(str->charAt(0)), toType, svcastflags);
        if (tc != -1)
          return 300;
      }
    }
    if (svcastflags & SVCastAutobox && arg.isObjectType())
    {
      int tc = -1;
     if (instanceof(obj, Character) == true)
        tc = typeDistance(inOf(RCharacter(obj)->charValue()), toType, svcastflags);
     else if (instanceof(obj, UnicodeCharacter) == true)
       tc = typeDistance(inOf(RUnicodeCharacter(obj)->charValue()), toType, svcastflags);
     if (tc != -1)
       return 150;
    }
    if (svcastflags & SVCastSVCastChar2Int)
    {
      int tc = typeDistance(arg, ClazzInfo::getShortClazz(), svcastflags & ~SVCastSVCastChar2Int);
      if (tc != -1)
        return 170;
    }
  }
  else if (toType == ClazzInfo::getFloatClazz() || toType == ClazzInfo::getDoubleClazz())
  {
    if (arg.isFloatingType() == true)
    {
      if (toType == ClazzInfo::getDoubleClazz())
        return 1;
      if (svcastflags & SVCastCheckOvervflow)
      {
        if (arg.getDoubleVar() <= Float::MAX_VALUE  && arg.getDoubleVar() >= Float::MIN_VALUE)
          return 2;
      }
      else
        return 2;
    }
    if (svcastflags & SVCastDecodeString && arg.isStringType() == true) 
    {
      RString str = arg.getStringVar();
      bool tryOnly = true;
      char typeChar;
      int ignoreTrailing = 0;
      double d = Number::parseFloatNumber(str, tryOnly, typeChar, ignoreTrailing);
      if (tryOnly == true)
      {
        int tc = typeDistance(inOf(d), toType, svcastflags);
        if (tc != -1)
          return 300;
      }
    }
    if (svcastflags & SVCastAutobox && arg.isObjectType() && 
        (instanceof(obj, Float) || instanceof(obj, Double)))
    {
      int tc = typeDistance(inOf(RNumber(obj)->doubleValue()), toType, svcastflags);
      if (tc != -1)
        return 150;
    }
    if (svcastflags & SVCastInt2Float)
    {
      int dc = typeDistance(arg, ClazzInfo::getIntClazz(), svcastflags & ~SVCastInt2Float);
      if (dc != -1)
        return dc + 100;
    }
    return -1;
  }
  else if (toType == ClazzInfo::getByteClazz()
        || toType == ClazzInfo::getShortClazz() 
        || toType == ClazzInfo::getIntClazz()
        || toType == ClazzInfo::getLongClazz())
  {
    if (arg.isIntegerType() == true)
    {
      
      if (svcastflags & SVCastCheckOvervflow)
      {
        jlong lv = arg.getLongVar();
        if (toType == ClazzInfo::getByteClazz())
        {
          if (lv < Byte::MIN_VALUE || lv > Byte::MAX_VALUE)
            return -1;
        }
        else if (toType == ClazzInfo::getShortClazz())
        {
          if (lv < Short::MIN_VALUE || lv > Short::MAX_VALUE)
            return -1;
        }
        else if (toType == ClazzInfo::getIntClazz())
        {
          if (lv < Integer::MIN_VALUE || lv > Integer::MAX_VALUE)
            return -1;
        }
      }
      return 5;
    }
    else if (arg.isFloatingType() == true)
    {
      if (svcastflags & SVCastInt2Float)
      {
        int tc = typeDistance(inOf(arg.getLongVar(svcastflags)), toType, svcastflags & ~SVCastInt2Float);
        if (tc == -1)
          return -1;
        return 10;
      }
      return -1;
    }
    if (svcastflags & SVCastDecodeString && arg.isStringType() == true) 
    {
      RString str = arg.getStringVar();
      bool tryOnly = true;
      char typeChar;
      int ignoreTrailing = 0;
      Number::decodeIntegerNumber(str, tryOnly, typeChar, ignoreTrailing);
      if (tryOnly == true)
        return 300;
    }
    else if (svcastflags & SVCastAutobox && arg.isObjectType() == true)
    {
      if (instanceof(obj, Byte) ||
          instanceof(obj, Short) ||
          instanceof(obj, Integer) ||
          instanceof(obj, Long))
      {
        int tc = typeDistance(inOf(RNumber(obj)->longValue()), toType, svcastflags);
        if (tc == -1)
          return -1;
        return 150 + tc;
      }
    }
    if (svcastflags & SVCastInt2Float)
    {
      int dc = typeDistance(arg, ClazzInfo::getDoubleClazz(), svcastflags & ~SVCastInt2Float);
      if (dc != -1)
        return dc + 100;
    }
    if (svcastflags & SVCastSVCastChar2Int)
    {
      int dc = typeDistance(arg, ClazzInfo::getUcCharClazz(), svcastflags & ~SVCastSVCastChar2Int);
      if (dc != -1)
        return dc + 150;
    }
  }
  return -1;
}

int 
AcdkStdWeakTypeDmiClient::weakTypeDistance(const ScriptVar& arg, const ClazzInfo* toType)
{
  return typeDistance(arg, toType, _scriptVarCastFlags);
}

bool
AcdkStdWeakTypeDmiClient::typeCast(ScriptVar& arg, const ClazzInfo* toType, int svcastflags)
{
  //arg = arg._castScriptVar(toType, svcastflags);
  return true;
  /*
   RObject obj;
  if (arg.isObjectType())
    obj = arg.getObjectVar();
  if (decodeDmiObjects == true)
  {
    if (instanceof(obj, DmiObject) == true) 
      return weakTypeCast(*RDmiObject(obj), toType);
    if (DmiObject::clazzInfo() == toType)
    {
      RDmiObject wrapped = new DmiObject(arg);
      arg.reset(); // has to reset, becuase if out Object it result in recursive pointer
      arg = inOf(wrapped);
      return true;
    }
  }
  if (toType == ClazzInfo::getBoolClazz())
  {
    if (decodeStrings == true && arg.isStringType() == true) {
      RString str = arg.getStringVar();
      if (str->equalsIgnoreCase("true") == true || str->equalsIgnoreCase("t") == true || str->equalsIgnoreCase("1") == true)
      {
        arg = true;
        return true;
      }
      if (str->equalsIgnoreCase("false") == true || str->equalsIgnoreCase("0") == true)
      {
        arg = false;
        return true;
      }
    }
    if (decodeObjects == true && arg.isObject() && instanceof(obj, Boolean))
    {
      arg = RBoolean(obj)->booleanValue();
      return true;
    }
    if (AcdkDmiClient::typeDistance(arg, ClazzInfo::getIntClazz()) >= 0)
    {
      int ival = arg.getIntVar();
      if (ival == 0)
        arg = false;
      else
        arg = true;
      return true;
    }
  }
  if (toType == ClazzInfo::getFloatClazz() || toType == ClazzInfo::getDoubleClazz())
  {
    if (decodeStrings == true && arg.isStringType() == true) {
      RString str = arg.getStringVar();
      try {
        double d = Double::parseDouble(str);
        arg = d;
        return true;
      } catch (RNumberFormatException nf) {
        return false;
      }
    }
    if (decodeObjects == true && arg.isObject() &&
        (instanceof(obj, Float) || instanceof(obj, Double)))
    {
      arg = RNumber(obj)->doubleValue();
      return true;
    }
    return false;
  }
  if (  toType == ClazzInfo::getCharClazz() || toType == ClazzInfo::getByteClazz()
      || toType == ClazzInfo::getShortClazz() || toType == ClazzInfo::getIntClazz()
       || toType == ClazzInfo::getLongClazz())
  {
    if (decodeStrings == true && arg.isStringType() == true)
    {
      RString str = arg.getStringVar();
      try {
        RLong tl = Long::decode(str); // BC6 compat
        arg = tl->longValue();
        return true;
      } catch (RNumberFormatException nf) {
        return false;
      }

    }

    if (decodeObjects == true && arg.isObject())
    {
      if (instanceof(obj, Character) ||
          instanceof(obj, Byte) ||
          instanceof(obj, Short) ||
          instanceof(obj, Integer) ||
          instanceof(obj, Long))
      {
        if (instanceof(obj, Character) == true)
          arg = RCharacter(obj)->charValue();
        else
          arg = RNumber(obj)->longValue();
        return true;
      }
    }
    return false;
  }
  return false;
  */
}

bool
AcdkStdWeakTypeDmiClient::weakTypeCast(ScriptVar& arg, const ClazzInfo* toType)
{
 return typeCast(arg, toType, _scriptVarCastFlags);
}

//virtual
int
AcdkStdWeakTypeDmiClient::typeDistance(const ScriptVar& arg, const ClazzInfo* toType)
{
  return typeDistance(arg, toType, _scriptVarCastFlags);
}

//virtual 
void 
AcdkStdWeakTypeDmiClient::castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType)
{
  int dist = AcdkDmiClient::typeDistance(value, toType);
  if (dist != -1)
    return;
  typeCast(value, toType, _scriptVarCastFlags);
}

//static 
AcdkStdWeakTypeDmiClient AcdkStdWeakTypeDmiClient::_client;

} // dmi
} // lang
} // acdk



