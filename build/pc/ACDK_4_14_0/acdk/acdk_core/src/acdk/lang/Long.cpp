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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Long.cpp,v 1.20 2005/03/24 16:58:00 kommer Exp $



#include <acdk.h>

#include "Long.h"
#include "Integer.h"
#include "Character.h"

#include "NumberFormatException.h"
#include "UnsupportedOperationException.h"
#include <ctype.h>
#include <stdlib.h>

namespace acdk {
namespace lang {

jlong Long::MAX_VALUE = JLONG_CONSTANT(0xFFFFFFFFFFFFFFFF);
jlong Long::MIN_VALUE = JLONG_CONSTANT(0x0000000000000000);
                        


Long::Long(IN(RString) str) THROWS1(RNumberFormatException)
: Number(),
  value(0)
{
  if (str == Nil || str->length() == 0)
    THROW1(NumberFormatException, new String("empty string"));
  char typeChar = 'l';
  bool tryOnly = false;
  int ignoreaTrailing = 0;
  value = Number::decodeIntegerNumber(str, tryOnly, typeChar, ignoreaTrailing);
}

//virtual 
bool 
Long::equals(IN(RObject) obj) 
{
  return obj != Nil && 
          instanceof(obj, Long) != false &&
          RLong(obj)->longValue() == longValue(); 
}

int 
Long::hashCode(jlong value)
{
  if (value <= MAX_VALUE && value >= MIN_VALUE)
    return (int)value;
  return int(value) + 31 * (value >> 32);
}

//static 
RLong 
Long::decode(IN(RString) str) THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 'l';
  int ignoreaTrailing = 0;
  return new Long(decodeIntegerNumber(str, tryOnly, typeChar, ignoreaTrailing));
}

//static 
RString 
Long::toString(int i, int radix) 
{
  return Integer::toString(i, radix);
}

//static 
RString 
Long::toString(jlong i, int radix) 
{
  return String::_jltoa(i, radix);
}

//static 
jlong 
Long::parseLong(IN(RString) str, int radix) THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 'l';
  int ignoreTrailing = 0;
  return parseIntegerNumber(str, radix, tryOnly, typeChar, ignoreTrailing);
}

//static 
RLong 
Long::valueOf(IN(RString) str, int radix) THROWS1(RNumberFormatException)
{
  return new Long(parseLong(str, radix));
}

//static 
RClass 
Long::getTYPE() 
{ 
  return Class::getSingeltonClass(dmi::ClazzInfo::getLongClazz());
}


} // lang
} // acdk
