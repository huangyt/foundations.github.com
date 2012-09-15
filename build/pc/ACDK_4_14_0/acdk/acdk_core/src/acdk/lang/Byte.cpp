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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Byte.cpp,v 1.18 2005/03/24 16:58:00 kommer Exp $

#include <acdk.h>

#include "Byte.h"
#include "Integer.h"

namespace acdk {
namespace lang {

//static 
byte Byte::MAX_VALUE = 0xFF;
//static 
byte Byte::MIN_VALUE = 0;

Byte::Byte(IN(RString) str) THROWS1(RNumberFormatException)
: Number(),
  value(0)
{
  value = parseByte(str);
}

//virtual 
int 
Byte::compareTo(IN(RByte) o)
{
  return shortValue() - o->shortValue();
}

//virtual 
int 
Byte::compareTo(IN(RObject) o) //throw(RClassCastException, RThrowable)
{
  if (instanceof(o, RByte) == false)
    THROW0(ClassCastException);
  return compareTo(RByte(o));
}

//virtual 
bool 
Byte::equals(IN(RObject) o)
{
  if (instanceof(o, RByte) == false)    
    return false;  
  return compareTo(RByte(o)) == 0;
}

//virtual 
short 
Byte::shortValue()
{
  return short(value);
}

//virtual 
int 
Byte::intValue()
{
  return int(value);
}

//virtual 
jlong 
Byte::longValue()
{
  return jlong(value);
}

//virtual 
float 
Byte::floatValue()
{
  return float(value);
}

//virtual 
double 
Byte::doubleValue()
{
  return double(value);
}

//virtual 
RString 
Byte::toString()
{
  return String::valueOf(int(value));
}

//static 
RByte 
Byte::decode(IN(RString) str) THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 'c';
  int ignoreTrailing = 0;
  return new Byte(char(decodeIntegerNumber(str, tryOnly, typeChar, ignoreTrailing)));
}

//static 
char 
Byte::parseByte(IN(RString) str, int radix) THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 'c';
  int ignoreTrailing = 0;
  return parseIntegerNumber(str, radix, tryOnly, typeChar, ignoreTrailing);
}

//static 
RByte 
Byte::valueOf(IN(RString) str, int radix) THROWS1(RNumberFormatException)
{
  return new Byte(parseByte(str, radix));
}

//static 
RClass 
Byte::getTYPE() 
{
  return Class::getSingeltonClass(dmi::ClazzInfo::getByteClazz());
}


} // lang
} // acdk
