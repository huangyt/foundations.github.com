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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ScriptVar2.cpp,v 1.17 2005/04/19 10:28:40 kommer Exp $



#include <acdk.h>
#include "../Boolean.h"
#include "../Character.h"
#include "../Number.h"
#include "../Byte.h"
#include "../Short.h"
#include "../Integer.h"
#include "../Long.h"
#include "../Float.h"
#include "../Double.h"
#include "../Exception.h"
#include "../RuntimeException.h"
#include "../DmiTypeConversionException.h"

namespace acdk {
namespace lang {
namespace dmi {

using namespace ::acdk::lang;



inline ScriptVar castNumberRight(jlong value)
{
  if (value > Integer::MIN_VALUE && value < Integer::MAX_VALUE)
    return int(value);
  return value;
  /*
  return erg < Byte::MIN_VALUE || erg > Byte::MAX_VALUE;
    return erg < Short::MIN_VALUE || erg > Short::MAX_VALUE;
    return erg < Integer::MIN_VALUE || erg > Integer::MAX_VALUE;
  */
}


bool 
ScriptVar::isOnlyFloating(short castFlags) const
{
  if (isFloatingType() == true)
    return true;
  if (isIntegerType() == true)
    return false;
  if (isFloating(castFlags) == true && isInteger(castFlags) == false)
    return true;
  return false;
}

//arithmetic operators
ScriptVar
ScriptVar::addition(const ScriptVar& other, short castFlags) const
{
  if (isNumber(castFlags) == false || other.isNumber(castFlags) == false)
    THROW1(DmiTypeConversionException, "Cannot add " + getTypeInfo() + " with " + other.getTypeInfo());
  
  if (isOnlyFloating(castFlags) || other.isOnlyFloating(castFlags) == true)
    return getDoubleVar(castFlags) + other.getDoubleVar(castFlags);
  
  return castNumberRight(getLongVar(castFlags) + other.getLongVar(castFlags));
}

ScriptVar
ScriptVar::subtraction(const ScriptVar& other, short castFlags) const
{
  if (isNumber(castFlags) == false || other.isNumber(castFlags) == false)
    THROW1(DmiTypeConversionException, "Cannot subtract " + getTypeInfo() + " with " + other.getTypeInfo());
  if (isOnlyFloating(castFlags) || other.isOnlyFloating(castFlags) == true)
    return getDoubleVar(castFlags) - other.getDoubleVar(castFlags);
  return castNumberRight(getLongVar(castFlags) - other.getLongVar(castFlags));
  /*
  if (isInteger(castFlags) == true)
  {
    if (other.isInteger(castFlags) == true)
      return castNumberRight(getLongVar(castFlags) - other.getLongVar(castFlags));
    if (other.isFloating(castFlags) == true)
      return getDoubleVar(castFlags) - other.getDoubleVar(castFlags);
  } else if (isFloating(castFlags) == true) {
    if (other.isNumber(castFlags) == true)
      return getDoubleVar(castFlags) - other.getDoubleVar(castFlags);
  }
  THROW1(DmiTypeConversionException, "Cannot subtract " + getTypeInfo() + " with " + other.getTypeInfo());
  return ScriptVar(); // never reached
  */
}

ScriptVar
ScriptVar::multiply(const ScriptVar& other, short castFlags) const
{
  if (isNumber(castFlags) == false || other.isNumber(castFlags) == false)
    THROW1(DmiTypeConversionException, "Cannot multiply " + getTypeInfo() + " with " + other.getTypeInfo());
  if (isOnlyFloating(castFlags) || other.isOnlyFloating(castFlags) == true)
    return getDoubleVar(castFlags) * other.getDoubleVar(castFlags);
  return castNumberRight(getLongVar(castFlags) * other.getLongVar(castFlags));
  /*
  if (isInteger(castFlags) == true)
  {
    if (other.isInteger(castFlags) == true)
      return castNumberRight(getLongVar(castFlags) * other.getLongVar(castFlags));
    if (other.isFloating(castFlags) == true)
      return getDoubleVar(castFlags) * other.getDoubleVar(castFlags);
  } else if (isFloating(castFlags) == true) {
    if (other.isNumber(castFlags) == true)
      return getDoubleVar(castFlags) * other.getDoubleVar(castFlags);
  }
  THROW1(DmiTypeConversionException, "Cannot multiply " + getTypeInfo() + " with " + other.getTypeInfo());
  return ScriptVar(); // never reached
  */
}

ScriptVar
ScriptVar::divide(const ScriptVar& other, short castFlags) const
{
  if (isNumber(castFlags) == false || other.isNumber(castFlags) == false)
    THROW1(DmiTypeConversionException, "Cannot divide " + getTypeInfo() + " with " + other.getTypeInfo());
  if (isOnlyFloating(castFlags) || other.isOnlyFloating(castFlags) == true)
  {
    double d = other.getDoubleVar(castFlags);
    if (d == 0.0)
      THROW1(DmiTypeConversionException, RString("cannot divide by zero"));
    return getDoubleVar(castFlags) / d;
  }
  jlong l = other.getLongVar(castFlags);
  if (l == 0)
    THROW1(DmiTypeConversionException, RString("cannot divide by zero"));
  return getLongVar(castFlags) / l;
  /*
  if (isInteger(castFlags) == true)
  {
    if (other.isInteger(castFlags) == true)
    {
      jlong jl = other.getLongVar(castFlags);
      if (jl == 0)
        THROW1(DmiTypeConversionException, RString("cannot divide by zero"));
      return double(getLongVar(castFlags)) / jl;
    }
    if (other.isFloating(castFlags) == true)
    {
      double d = other.getDoubleVar(castFlags);
      if (d == 0)
        THROW1(DmiTypeConversionException, RString("cannot divide by zero"));
      return getDoubleVar(castFlags) / d;
    }
  } else if (isFloating(castFlags) == true) {
    if (other.isNumber(castFlags) == true)
    {
      double d = other.getDoubleVar(castFlags);
      if (d == 0)
        THROW1(DmiTypeConversionException, RString("cannot divide by zero"));
      return getDoubleVar(castFlags) / d;
    }
  }
  THROW1(DmiTypeConversionException, "Cannot divide " + getTypeInfo() + " with " + other.getTypeInfo());
  return ScriptVar(); // never reached
  */
}




ScriptVar
ScriptVar::modulo(const ScriptVar& other, short castFlags) const
{
  if (isInteger(castFlags) == false || other.isInteger(castFlags) == false)
    THROW1(DmiTypeConversionException, "Invalid types for modulo: " + getTypeInfo() + " % " + other.getTypeInfo());
  
  switch(type)
  {
  case ByteType:
  case ByteRefType: return getByteVar() % other.getLongVar();
  case ShortType:
  case ShortRefType: return getShortVar() % other.getLongVar();
  case IntType:
  case IntRefType: return getIntVar() % other.getLongVar();
  case LongType:
  case LongRefType: return castNumberRight(getLongVar() % other.getLongVar());
 default:
      return getLongVar(castFlags) % other.getLongVar(castFlags);
    break;
  }
  THROW1(DmiTypeConversionException, "Invalid types for modulo: " + getTypeInfo() + " % " + other.getTypeInfo());
  return ScriptVar();
}

ScriptVar
ScriptVar::equal(const ScriptVar& other, short castFlags) const
{
  if (isOnlyFloating(castFlags) || other.isOnlyFloating(castFlags))
    return getDoubleVar(castFlags) == other.getDoubleVar(castFlags);
  if (isBoolType() == true && other.isBoolType() == true)
    return getBoolVar() == other.getBoolVar(); 
  if (isIntegerType() == true && other.isIntegerType() == true)
    return getLongVar() == other.getLongVar();
  if (isFloatingType() == true && other.isFloatingType() == true)
    return getDoubleVar() == other.getDoubleVar();
  if (isCharacterType() == true && other.isCharacterType() == true)
    return getUcCharVar() == other.getUcCharVar();

  if (isInteger(castFlags) == true)
  {
    if (other.isInteger(castFlags) == true)
      return getLongVar(castFlags) == other.getLongVar(castFlags);
    if (other.isFloating(castFlags) == true)
      return getDoubleVar(castFlags) == other.getDoubleVar(castFlags);
  } 
  else if (isFloating(castFlags) == true) 
  {
    if (other.isNumber(castFlags) == true)
      return getDoubleVar(castFlags) == other.getDoubleVar(castFlags);
  }

  if (isBoolean(castFlags) == true && other.isBoolean(castFlags) == true)
    return getBoolVar(castFlags) == other.getBoolVar(castFlags);
  
  if (isCharacter(castFlags) == true && other.isCharacter(castFlags) == true)
    return getUcCharVar(castFlags) == other.getUcCharVar(castFlags);

  if (isObject(castFlags) == true && other.isObject(castFlags) == true)
  {
    RObject o1 = getObjectVar(castFlags);
    RObject o2 = other.getObjectVar(castFlags);
    if (o1 == Nil && o2 == Nil)
      return true;
    if (o1 == Nil)
      return false;
    return o1->equals(o2);
  }
  return false;
}

ScriptVar 
ScriptVar::same(const ScriptVar& other, short castFlags) const
{
  if (isObjectType())
  {
    if (other.isObjectType())
      return getObjectVar() == other.getObjectVar();
    return false;
  }
  return equal(other);
}

bool
ScriptVar::isTrue(short castFlags) const
{
  return getBoolVar(castFlags);
}

ScriptVar
ScriptVar::not_equal(const ScriptVar& other, short castFlags) const
{
  return equal(other, castFlags).logical_not();
}

ScriptVar
ScriptVar::logical_and(const ScriptVar& other, short castFlags) const
{
  return ScriptVar(bool(isTrue(castFlags) && other.isTrue(castFlags)));
}

ScriptVar
ScriptVar::logical_or(const ScriptVar& other, short castFlags) const
{
  return ScriptVar(bool(isTrue(castFlags) || other.isTrue(castFlags)));
}

ScriptVar
ScriptVar::logical_xor(const ScriptVar& other, short castFlags) const
{
  bool f = isTrue(castFlags);
  bool s = other.isTrue(castFlags);
  return ScriptVar(bool(f != s));
}

ScriptVar
ScriptVar::greater_than(const ScriptVar& other, short castFlags) const
{
  return equal(other).logical_not().logical_and(less_than(other).logical_not());
}

ScriptVar
ScriptVar::greater_or_equal(const ScriptVar& other, short castFlags) const
{
  return less_than(other, castFlags).logical_not();
}

ScriptVar
ScriptVar::less_than(const ScriptVar& other, short castFlags) const
{
  if (isOnlyFloating(castFlags) || other.isOnlyFloating(castFlags))
    return getDoubleVar(castFlags) < other.getDoubleVar(castFlags);
  if (isInteger(castFlags) && other.isInteger(castFlags))
    return getLongVar(castFlags) < other.getLongVar(castFlags);
  /*
  if (isInteger(castFlags) == true)
  {
    if (other.isInteger(castFlags) == true)
      return getLongVar(castFlags) < other.getLongVar(castFlags);
    if (other.isFloating(castFlags) == true)
      return getDoubleVar(castFlags) < other.getDoubleVar(castFlags);
  } else if (isFloating(castFlags) == true) {
    if (other.isNumber(castFlags) == true)
      return getDoubleVar(castFlags) < other.getDoubleVar(castFlags);
  }
  */
  THROW1(DmiTypeConversionException, "Invalid types for less_than: " + getTypeInfo() + " % " + other.getTypeInfo());
  return false;
}

ScriptVar
ScriptVar::less_or_equal(const ScriptVar& other, short castFlags) const
{
  return less_than(other, castFlags).logical_or(equal(other, castFlags));
}

ScriptVar
ScriptVar::logical_not(short castFlags) const
{
  return getBoolVar(castFlags) == false;
}

ScriptVar 
ScriptVar::binary_and(const ScriptVar& other, short castFlags) const
{
  if (isInteger(castFlags) == false || other.isInteger(castFlags) == false)
    THROW1(DmiTypeConversionException, "binary and " + getTypeInfo() + " with " + other.getTypeInfo());
  if (isLongType() || other.isLongType())
    return getLongVar(castFlags) & other.getLongVar(castFlags);
  if (isIntType() || other.isIntType())
    return getIntVar(castFlags) & other.getIntVar(castFlags);
  if (isShortType() || other.isShortType())
    return getShortVar(castFlags) & other.getShortVar(castFlags);
  if (isByteType() || other.isByteType())
    return getByteVar(castFlags) & other.getByteVar(castFlags);
  jlong i1 = getLongVar(castFlags);
  jlong i2 = other.getLongVar(castFlags);
  return i1 & i2;
}

ScriptVar 
ScriptVar::binary_or(const ScriptVar& other, short castFlags) const
{
  //if (isInteger() == false || other.isInteger() == false)
  //  THROW1(DmiTypeConversionException, "binary or " + getTypeInfo() + " with " + other.getTypeInfo());
  if (isLongType() || other.isLongType())
    return getLongVar(castFlags) | other.getLongVar(castFlags);
  if (isIntType() || other.isIntType())
    return getIntVar(castFlags) | other.getIntVar(castFlags);
  if (isShortType() || other.isShortType())
    return getShortVar(castFlags) | other.getShortVar(castFlags);
  jlong i1 = getLongVar(castFlags);
  jlong i2 = other.getLongVar(castFlags);
  return i1 | i2;
}

ScriptVar 
ScriptVar::binary_xor(const ScriptVar& other, short castFlags) const
{
  if (isInteger(castFlags) == false || other.isInteger(castFlags) == false)
    THROW1(DmiTypeConversionException, "binary xor " + getTypeInfo() + " with " + other.getTypeInfo());
  if (isLongType() || other.isLongType())
    return getLongVar(castFlags) ^ other.getLongVar(castFlags);
  if (isIntType() || other.isIntType())
    return getIntVar(castFlags) ^ other.getIntVar(castFlags);
  if (isShortType() || other.isShortType())
    return getShortVar(castFlags) ^ other.getShortVar(castFlags);
  jlong i1 = getLongVar(castFlags);
  jlong i2 = other.getLongVar(castFlags);
  return i1 ^ i2;
}

ScriptVar 
ScriptVar::binary_leftshift(const ScriptVar& other, short castFlags) const
{
  if (isInteger(castFlags) == false || other.isInteger(castFlags) == false)
    THROW1(DmiTypeConversionException, "binary leftshift " + getTypeInfo() + " with " + other.getTypeInfo());
  if (isLongType() || other.isLongType())
    return getLongVar(castFlags) << other.getLongVar(castFlags);
  if (isIntType() || other.isIntType())
    return getIntVar(castFlags) << other.getIntVar(castFlags);
  if (isShortType() || other.isShortType())
    return getShortVar(castFlags) << other.getShortVar(castFlags);
  jlong i1 = getLongVar(castFlags);
  jlong i2 = other.getLongVar(castFlags);
  return i1 << i2;
}

bool isOnlyFloating(const ScriptVar& sv, short castFlags);

ScriptVar
ScriptVar::negation(short castFlags) const
{
  switch (type)
  {
  case ByteType: 
  case ByteRefType: return acdk::lang::inOf((short)-getShortVar()); // not byte, because value will be 255
  case ShortType:
  case ShortRefType: return acdk::lang::inOf((short)-getShortVar());
  case IntType: 
  case IntRefType: return acdk::lang::inOf((int)-getIntVar());
  case LongType:
  case LongRefType: return acdk::lang::inOf((jlong)-getLongVar());
  case FloatType: 
  case FloatRefType: return acdk::lang::inOf((float)-getFloatVar());
  case DoubleType: 
  case DoubleRefType: return acdk::lang::inOf((double)-getDoubleVar());
  default:
    if (isOnlyFloating(castFlags) == true)
      return -getDoubleVar(castFlags);
    if (isInteger(castFlags) == true)
      return -getLongVar(castFlags);

    THROW1(DmiTypeConversionException, "cannot negate non number type: " + getTypeInfo());
    break;
  } 
  return ScriptVar();

}

ScriptVar 
ScriptVar::binary_rightshift(const ScriptVar& other, short castFlags) const
{
  if (isInteger(castFlags) == false || other.isInteger(castFlags) == false)
    THROW1(DmiTypeConversionException, "binary rightshift " + getTypeInfo() + " with " + other.getTypeInfo());
  ScriptVar erg;
  ScriptVar ot = *this;
  if (getLongVar() < 0)
    ot = negation();

  if (type == LongRefType || type == LongType)
    erg = ot.getLongVar() >> other.getByteVar() ;
  if (type == IntRefType || type == IntType)
    erg = ot.getIntVar() >> other.getByteVar();
  if (type == ShortRefType || type == ShortType)
    erg =  ot.getShortVar() >> other.getByteVar();
  else
    erg = ot.getByteVar() >> other.getByteVar();
  if (getLongVar() < 0)
    return erg.negation();
  return erg;
}

ScriptVar 
ScriptVar::binary_rightshift_unsigned(const ScriptVar& other, short castFlags) const
{
  if (isInteger(castFlags) == false || other.isInteger(castFlags) == false)
    THROW1(DmiTypeConversionException, "binary rightshift " + getTypeInfo() + " with " + other.getTypeInfo());
  if (isLongType())
  {
    jlong ret = getLongVar() >> other.getByteVar(castFlags);
    if (ret < 0)
      return -1;
    return ret;
  }
  if (isIntType())
    return int( ((unsigned int)getIntVar()) >> other.getByteVar(castFlags));
  
  if (isShortType())
    return short(((unsigned short)getShortVar()) >> other.getByteVar(castFlags));
  return getLongVar(castFlags) >> other.getByteVar(castFlags);
}

ScriptVar 
ScriptVar::binary_not(short castFlags) const
{
  if (isLongType())
    return ~getLongVar();
  if (isIntType())
    return ~getIntVar();
  if (isShortType())
    return ~getShortVar();
  return ~getIntVar(castFlags);
}




} // dmi
} // lang
} // acdk
