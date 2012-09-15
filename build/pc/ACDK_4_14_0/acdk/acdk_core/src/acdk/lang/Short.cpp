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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Short.cpp,v 1.15 2005/03/24 16:58:00 kommer Exp $


#include <acdk.h>

#include "Short.h"
#include "Character.h"
#include "Math.h"
#include "StringBuffer.h"
#include "StringIndexOutOfBoundsException.h"
#include "NumberFormatException.h"

namespace acdk {
namespace lang {

short Short::MAX_VALUE = 32767;
short Short::MIN_VALUE = -32768;


Short::Short(IN(RString) str) THROWS1(RNumberFormatException)
: Number(),
  value(0)
{
  value = parseShort(str, 10);
}

Short::Short(short s)
: Number(),
  value(s)
{
}

//static 
short 
Short::parseShort(IN(RString) str, int radix) THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 's';
  int ignoreTrailing = 0;
  return parseIntegerNumber(str, radix, tryOnly, typeChar, ignoreTrailing);
}

//static 
RShort 
Short::decode(IN(RString) str) THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 's';
  int ignoreaTrailing = 0;
  return new Short(decodeIntegerNumber(str, tryOnly, typeChar, ignoreaTrailing));
}

//virtual 
int 
Short::compareTo(IN(RObject) o)
{
  return compareTo(RShort(o));
}

//virtual 
int 
Short::compareTo(IN(RShort) anotherShort)
{
  return shortValue() - anotherShort->shortValue();
}


//virtual 
RString 
Short::toString()
{
  return toString(value);
}
 
//static 
RString 
Short::toString(short s, int radix)
{
  
  char digits[] = 
    { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
      'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
      'u', 'v', 'w', 'x', 'y', 'z' };
  RStringBuffer sb = new StringBuffer();
  bool negative = (s < 0);
  do {
    sb->append(digits[Math::abs(s % radix)]);
  } while ((s /= radix) != 0);
  if (negative) 
    sb->append('-');
  return sb->reverse()->toString();
}
 
//static 
RShort 
Short::valueOf(IN(RString) s, int radix) THROWS1(RNumberFormatException)
{
  return new Short(parseShort(s, radix));
}

/* old code
//static 
short 
Short::_parseShort(IN(RString) s, int radix, bool decode, bool& tryOnly)  THROWS1(RNumberFormatException)
{
  if (s == Nil || s->length() == 0)
    THROW1(NumberFormatException, "string null or empty");
  if (radix < Character::MIN_RADIX || radix > Character::MAX_RADIX)
    THROW1(NumberFormatException, "radix outside of range: " + radix);
  bool isNegative = false;
  int i = 0;
  String::iterator it = s->begin();
  String::iterator end = s->end();

  if (*it == '-') {
    if (s->length() == 1)
    {
      if (tryOnly == false)
        THROW1(NumberFormatException, "negative sign without value");
      tryOnly = false;
      return 0;
    }
    isNegative = true;
    ++it;
  }
  if (decode == true && isNegative == false) 
  {
    if (*it == '0') 
    {
      try {
        if (Character::toUpperCase(*(it + 1)) == 'X') 
        {
          it += 2;
          radix = 16;
          if (it >= end) 
          {
            if (tryOnly == false)
              THROW1(NumberFormatException, "string empty");
            tryOnly = false;
            return 0;
          }
        } else
          radix = 8;
      } catch (RStringIndexOutOfBoundsException ex) { 
      }
    } 
    else 
    {
      if (*it == '#') 
      {
        ++it;
        radix = 16;
        if (it >= end) 
        {
          if (tryOnly == false)
            THROW1(NumberFormatException, "string empty");
          tryOnly = false;
          return 0;
        }
      } else {
        radix = 10;
      }
    }
  }
  short cutoff = (short) (MAX_VALUE / radix);
  short cutlim = (short) (MAX_VALUE % radix);
  short result = 0;
  
  while (it < end) 
  {
    
    int c = Character::asciiDigit(*it, radix);
    ++it;
    if (c == -1) 
    {
      if (tryOnly == false)
        THROW1(NumberFormatException, RString("char at index ") + String::valueOf(i) + " is not of specified radix");
      tryOnly = false;
      return 0;
    }
      
    if (result > cutoff || (result == cutoff && c > cutlim)) 
    {
      if (isNegative == true) {
        result *= radix;
        result += c;
        if (result == MIN_VALUE && it == end) 
          return MIN_VALUE;
      }
      if (tryOnly == false)
        THROW1(NumberFormatException, "overflow");
      tryOnly = false;
      return -1;
    }
    result *= radix;
    result += c;
  }
  return (short) ((isNegative) ? -result : result);
}
*/

//static 
RClass 
Short::getTYPE() 
{ 
  return Class::getSingeltonClass(dmi::ClazzInfo::getShortClazz());
}

} // lang
} // acdk


