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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Number.cpp,v 1.25 2005/04/25 22:25:39 kommer Exp $

#include "Number.h"
#include "Integer.h"
#include "Byte.h"
#include "Short.h"
#include "Long.h"
#include "Float.h"
#include "Double.h"
#include "Character.h"
#include <acdk/util/ResourceBundle.h>

/*
#include <errno.h>
#include <stdlib.h>
*/
namespace acdk {
namespace lang {

static
char getTypeChar(char c)
{
  switch (c)
  {
  case 's':
  case 'S':
    return 's';
  case 'i':
  case 'I':
    return 'i';
  case 'l':
  case 'L':
    return 'l';
  case 'f':
  case 'F':
    return 'f';
  case 'd':
  case 'D':
    return 'd';
  default:
    return 0;
  }
}

static
char getTypeChar(String::iterator it, String::iterator& end)
{
  bool isHex = false;
  if (end - it > 2)
  {
    if (*it == '0' && (*(it + 1) == 'x' || *(it + 1) == 'X'))
      isHex = true;
  }
  String::iterator it2 = end - 1;
  char c = getTypeChar(*it2);
  if (c != 0)
  {
    if (isHex && (c == 'd' || c == 'f'))
      return 0;
    --end;
  }
  return c;
}

/**
  returns true if overflow 
*/
static
bool checkOverflow(jlong erg, char typeChar)
{
  if (typeChar == 0)
    return false;
  typeChar = Character::toLowerCase(typeChar);
  switch(typeChar)
  {
  case 'b':
    return erg < Byte::MIN_VALUE || erg > Byte::MAX_VALUE;
  case 's':
    return erg < Short::MIN_VALUE || erg > Short::MAX_VALUE;
  case 'i':
    return erg < Integer::MIN_VALUE || erg > Integer::MAX_VALUE;
  case 'l':
    return false;
  case 'd':
  case 'f':
    return false; // ???
  default:
    return true; // unknown
  }

}


bool 
_parseBasicInt(String::iterator& it, String::iterator end, OUT(jlong) ret)
{
  bool readSome = false;
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isDigit(*it) == false)
      return readSome;
    ret *= 10;
    ret += UnicodeCharacter::decVal(*it);
    readSome = true;
  }
  return readSome;
}
bool
_parseSignedInt(String::iterator& it, String::iterator end, OUT(jlong) ret)
{
  if (*it == '+')
    return _parseBasicInt(++it, end, ret);
  if (*it == '-')
  {
    bool b = _parseBasicInt(++it, end, ret);
    if (b == false)
      return false;
    ret = -ret;
    return true;
  }
  return _parseBasicInt(it, end, ret);
}


static
jlong 
_parseIntegerNumber(IN(RString) str, String::iterator& it, String::iterator end, int radix, INOUT(bool) tryOnly, INOUT(char) typeChar)
{
  
  const char* XS = "0123456789abcdefghijklmnopqrstuvwxyz";
  bool neg = false;
  jlong res = 0;
  if (it >= end)
  {
    if (tryOnly == false)
      THROW1(NumberFormatException, new String("empty string"));
    tryOnly = false;
    return 0;
  }
  
  if ((radix < 0) || (radix > 36))
  {
    if (tryOnly == false)
      THROW1(NumberFormatException, RString("radix ") + Integer::toString(radix) + " is out of range (0 ... 36)");
    tryOnly = false;
    return 0;
  }
  
  if (*it == '-')
  {
    neg = true;
    ++it;
  }
  else if (*it == '+')
    ++it;

  if (radix == 10)
  {
    res = 0;
    bool readSome = _parseSignedInt(it, end, res);
    if (readSome == false)
    {
      if (tryOnly == false)
        THROW1(NumberFormatException, "canot parse to integer: " + str);
      tryOnly = false;
      return 0;
    }
  }
  else
  {
    for (int i = 0; it != end; ++it, i++)
    {
      const char* s;
      if (*it > 0x80)
        break;

      char ch = Character::toLowerCase((char)*it);
      int idx;
      if (((s = strchr(XS, ch)) == NULL) || ((idx = s - XS) >= radix))
      {
        break;
        /*
        if (tryOnly == false)
          THROW1(NumberFormatException, RString("char `") + Character::toString(ch) + "' at index " + Integer::toString(i) + " in \"" + str + "\" is invalid for radix " + Integer::toString(radix));
        tryOnly = false;
        return 0;
        */
      }
      jlong tmp = res * radix + idx;
      if (tmp < res)
      {
        if (tryOnly == false)
          THROW1(NumberFormatException, RString("\"") + str + "\" forces an overflow for radix " + Integer::toString(radix));
        tryOnly = false;
        return 0;
      }
      res = tmp;
    }
  }
  if (it != end)
  {
    char setTypeChar = getTypeChar(*it);
    if (setTypeChar != 0)
    {
      typeChar = setTypeChar;
      ++it;
    }
  }
  if (neg == true)
    return res;
  return res;

/* old code
  int len = end - it;
  res = 0;
  const char* s = 0;
  int idx = 0;
  for (int i = 0; i < len; i++) 
  {
    char ch = ::tolower(*it);
    
    if (i == 0) 
    {
      if (ch == '+')
        continue;
      if (ch == '-') 
      {
        neg = true;
        continue;
      }
    }
    jlong tmp = 0;
    if (radix == 10)
      tmp = UnicodeCharacter::decval(*it);
    
     if (((s = strchr(XS, ch)) == NULL) || ((idx = s - XS) >= radix))
    {
      if (tryOnly == false)
        THROW1(NumberFormatException, RString("char `") + Character::toString(ch) + "' at index " + Integer::toString(i) + " in \"" + str + "\" is invalid for radix " + Integer::toString(radix));
      tryOnly = false;
      return 0;
    }
    jlong tmp = res * radix + idx;
    if (tmp < res)
    {
      if (tryOnly == false)
        THROW1(NumberFormatException, RString("\"") + str + "\" forces an overflow for radix " + Integer::toString(radix));
      tryOnly = false;
      return 0;
    }
    res = tmp;
    ++it;
    
  }
  if (neg == true) 
  {
    jlong tmp = -res;
    if (tmp > res)
    {
      if (tryOnly == false)
        THROW1(NumberFormatException, RString("\"") + str + "\" forces a negative overflow for radix " + Integer::toString(radix));
      tryOnly = false;
      return 0;
    }
    res = tmp;
  }
  return res;
  */
}



//static 
jlong 
Number::decodeIntegerNumber(IN(RString) str, INOUT(bool) tryOnly, INOUT(char) typeChar, INOUT(int) ignoreaTrailing, bool ignoreLeadingWs) THROWS1(RNumberFormatException)
{
  int len;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  String::iterator numBegin = it;
  bool wastryOnly = tryOnly;
  if (ignoreLeadingWs == true)
  {
    for (; it != end; ++it)
    {
      if (UnicodeCharacter::isWhitespace(*it) == false)
        break;
    }
    numBegin = it;    
  }

  if (numBegin == end)
  {
    if (tryOnly == false)
      THROW1(NumberFormatException, new String("empty string"));
    tryOnly = false;
    return 0;
  }
  
  //char isTypeChar = getTypeChar(it, end);
  len = end - it;
  jlong res;

  

  int idx = 0;
  bool neg = false;
  int radix = 10;

  char ch = *it;
  if ((ch == '-') || (ch == '+')) 
  {
    if (len < 2)
    {
      if (tryOnly == false)
        THROW1(NumberFormatException, RString("short string: ") + str);
      tryOnly = false;
      return 0;
    }
    if (ch == '-')
      neg = true;
    ++it;
    ch = *it;
    len--;
  }
  if (ch == '#') 
  {
    if (len < 2)
    {
      if (tryOnly == false)
        THROW1(NumberFormatException, RString("short string: ") + str);
      tryOnly = false;
      return 0;
    }
    radix = 16;
    ++it;
    ch = *it;
    len--;
  }
  if (ch == '0') 
  {
    if (len == 1) 
      return 0;
    ++it;
    ch = *it;
    len--;
    if ((ch == 'x') || (ch == 'X')) 
    {
      if (len < 2)
      {
        if (tryOnly == false)
          THROW1(NumberFormatException, RString("short string: ") + str);
        tryOnly = false;
        return 0;
      }
      radix = 16;
      ++it;
      ch = *it;
      len--;
    } 
    else 
    {
      radix = 8;
    }
  }
  res = _parseIntegerNumber(str, it, end, radix, tryOnly, typeChar);
  if (wastryOnly == true && tryOnly == false)
    return res;

  if (neg == true)
    res = -res;
  
  if (ignoreaTrailing == 0)
  {
    if (it != end)
    {
      if (wastryOnly == true)
      {
        tryOnly = false;
        return res;
      }
      else
        THROW1(NumberFormatException, RString("integer has trailing unparseable characters: ") + str);
    }
  } else
    ignoreaTrailing = it - str->begin();
  return res;
}




//static 
jlong 
Number::parseIntegerNumber(IN(RString) str, int radix, INOUT(bool) tryOnly, OUT(char) typeChar, 
                           INOUT(int) ignoreTrailing, bool ignoreLeadingSpaces) THROWS1(RNumberFormatException)
{
  String::iterator it = str->begin();
  String::iterator begin = it;
  String::iterator end = str->end();
  String::iterator numBegin = it;

  if (ignoreLeadingSpaces == true)
  {
    for (; it != end; ++it)
    {
      if (UnicodeCharacter::isWhitespace(*it) == false)
        break;
    }
    numBegin = it;
  }
  
  bool wasTryOnly = tryOnly;
  tryOnly = true;
  char isTypeChar = 0;
  jlong res = _parseIntegerNumber(str, it, end, radix, tryOnly, isTypeChar);
  if (wasTryOnly == true && tryOnly == false)
    return res;
  
  if (checkOverflow(res, isTypeChar) == true || checkOverflow(res, typeChar) == true)
  {
    if (wasTryOnly == false)
      THROW1(NumberFormatException, RString("parsing overflow: ") + str);
    tryOnly = false;
    return -1;
  }
  return res;
}


  
double 
_parseFloatNumber(String::iterator& it, String::iterator end, ucchar fractionSep, ucchar exp)
{
  ucchar expLower = UnicodeCharacter::toLowerCase(exp);
  ucchar expUpper = UnicodeCharacter::toUpperCase(exp);
  String::iterator begin = it;
  bool hasFirst = true;
  bool isSigned = false;
  if (*it == '+')
    ++it;
  else if (*it == '-')
  {
    isSigned = true;
    ++it;
  }
  String::iterator sit = it;
  jlong lval = 0;
  bool re = _parseBasicInt(it, end, lval);
  double ret = (double)lval;
  if (sit == it || re == false)
    hasFirst = false;

  int s = -1;
  bool hasFraction = false;
  static double fraction[] = { 
    1e-1,  1e-2,  1e-3,  1e-4,  1e-5,  1e-6,  1e-7,  1e-8,  1e-9,
    1e-10, 1e-11, 1e-12, 1e-13, 1e-14, 1e-15, 1e-16, 1e-17, 1e-18,  1e-19,
    1e-20, 1e-21, 1e-22, 1e-23, 1e-24, 1e-25, 1e-26, 1e-27, 1e-28,  1e-29,
    1e-30, 1e-31, 1e-32, 1e-33, 1e-34, 1e-35, 1e-36, 1e-37, 1e-38,  1e-39,
    1e-40, 1e-41, 1e-42, 1e-43, 1e-44, 1e-45, 1e-46, 1e-47, 1e-48,  1e-49,
    1e-50, 1e-51, 1e-52, 1e-53, 1e-54, 1e-55, 1e-56, 1e-57, 1e-58,  1e-59
  };
  
  if (it != end && *it == fractionSep)
  {
    ++it;
    sit = it;
    //double fr = 0.10000000000000000; looping and fr *= 0.1 does not work correctly
#if defined(__GNUC__)
# define USE_FRACTION_CALC1
#endif
#if defined(USE_FRACTION_CALC1)
    int fractionCount = 0;
    double fracterg = 0;
    for (int idx = 0; it != end && idx < 59; ++it, ++idx)
    {
      if (UnicodeCharacter::isDigit(*it) == false)
        break;
      fracterg = fracterg * 10.0 + UnicodeCharacter::decVal(*it);
      ++fractionCount;
    }
    //fracterg *= fraction[fractionCount]; 
    // this way seem to be compatible with gcc:
    for (int i = 0; i < fractionCount; ++i)
      fracterg /= 10.0;
    ret += fracterg;
#else //defined(USE_FRACTION_CALC1)
    // this way to calculate the same result as the Microsoft compilers
    for (int idx = 0; it != end && idx < 59; ++it, ++idx)
    {
      if (UnicodeCharacter::isDigit(*it) == false)
        break;
      hasFraction = true;
      ret += fraction[idx] * UnicodeCharacter::decVal(*it);
    }
#endif //defined(USE_FRACTION_CALC1)
  }
  else if (hasFirst == false)
  {
    it = begin;
    return 0;
  }

  int exponent = 0;
  bool hasExponent = false;
  if (it != end && (*it == expLower || *it == expUpper || *it == exp))
  {
    hasExponent = true;
    
    bool isNegExp = false;
    ++it;
    if (it == end)
    {
      it = begin;
      return ret;
    }
    if (*it == '+')
      ++it;
    else if (*it == '-')
    {
      ++it;
      isNegExp = true;
    }
    String::iterator sit = it;
    jlong tval = 0;
    bool br = _parseBasicInt(it, end, tval);
    if (it == sit || br == false)
      return ret;
    int exp  = tval;
    
    static int MaxExponent = 511;	
    if (exp > MaxExponent)
    {
      if (isNegExp == true)
        ret = 0.0;
      else
        ret = Long::MAX_VALUE;
      goto done;
		}
    double expAjust = 1.0;
    while (exp >= 100)
    {
        expAjust *= 1.0e100;
        exp -= 100;
		}  
    static double pote10th[] = {  1.0, 1e10, 1e20, 1e30, 1e40, 1e50, 1e60, 1e70, 1e80, 1e90 };
    static double pote1th[] = { 1.0, 10.0, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9 };
    expAjust *= pote10th[exp / 10] * pote1th[exp % 10];

    if (isNegExp == true)
      expAjust = 1.0 / expAjust;
    ret = ret * expAjust;
  }
done:
  if (isSigned == true)
    return -ret;
  return ret;
}

//static 
void 
Number::getFractionAndExponentSignFromLocale(IN(acdk::util::RLocale) locale, OUT(ucchar) fraction, OUT(ucchar) exponent)
{
  if (locale == Nil)
  {
    fraction = '.';
    exponent = 'e';
  }
  else
  {
     acdk::util::RResourceBundle resb =  acdk::util::ResourceBundle::getBundle("acdk.locale.LocaleInfo", locale);
    fraction = resb->getString("decimalSeparator")->charAt(0);
    exponent = resb->getString("exponential")->charAt(0);
  }
}

//static 
double 
Number::parseFloatNumber(IN(RString) str, INOUT(bool) tryOnly, INOUT(char) typeChar, INOUT(int) ignoreTrailing,
                         bool ignoreLeadingSpaces, IN(acdk::util::RLocale) locale) THROWS1(RNumberFormatException)
{
  ucchar fractionSep = '.';
  ucchar expChar = 'e';
  getFractionAndExponentSignFromLocale(locale, fractionSep, expChar);
  
  String::iterator sit = str->begin();
  String::iterator send = str->end();
  String::iterator startOfNumber = sit;
  if (ignoreLeadingSpaces == true)
  {
    for (; sit != send; ++sit)
    {
      if (UnicodeCharacter::isWhitespace(*sit) == false)
        break;
    }
    startOfNumber = sit;
  }
  double ret = _parseFloatNumber(sit, send, fractionSep, expChar);
  char setTypeChar = 0;
  if (sit != startOfNumber && sit != send)
  {
    setTypeChar = getTypeChar(*sit);
    if (setTypeChar != 0)
    {
      typeChar = setTypeChar;
      ++sit;
    }
  }
  bool failed = false;
  if (startOfNumber == sit)
    failed = true;
  else if (ignoreTrailing == 0 && sit != send)
    failed = true;
  else if (ignoreTrailing != 0)
    ignoreTrailing = sit - str->begin();

  if (failed == true)
  {
    if (tryOnly == true)
    {
      tryOnly = false;
      return 0;
    }
    THROW1(NumberFormatException, "Cannot parse floating number: " + str);
  }
  return ret;

/* old implementation
  RString s = str->convert(CCUtf8);
  const char* begin = (const char*)str->byte_begin();
  const char* end = (const char*)str->byte_end();
  char setTypeChar = getTypeChar(*(end - 1));
  if (setTypeChar != 0)
    --end;
  char* pend = (char* )end;
  double erg = strtod(begin, &pend);
  if (pend != end)
  {
    if (tryOnly == true)
    {
      tryOnly = false;
      return 0;
    }
    if (errno == ERANGE)
      THROW1(NumberFormatException, "Overflow on parse floating number: " + str);
    THROW1(NumberFormatException, "Cannot parse floating number: " + str);
  }
  if (setTypeChar != 0)
    typeChar = setTypeChar;
  return erg;  
  */
}

char Number::getSmallestTypeChar(jlong number)
{
  if (number >= Byte::MIN_VALUE && number <= Byte::MAX_VALUE)
    return 'b';
  if (number >= Short::MIN_VALUE && number <= Short::MAX_VALUE)
    return 's';
  if (number >= Integer::MIN_VALUE && number <= Integer::MAX_VALUE)
    return 'i';
  return 'l';
}

char Number::getSmallestTypeChar(double number)
{
  float t = (float)number;
  double td = t;
  if (td == number)
    return 'f';
  return 'd';
}

RNumber
Number::getNumber(jlong value, char typeChar) THROWS1(RNumberFormatException)
{
  if (typeChar == 0)
    typeChar = Number::getSmallestTypeChar(value);
  switch(typeChar)
  {
  case 'b':
    return new Byte(value);
  case 's':
    return new Short(value);
  case 'i':
    return new Integer(value);
  case 'd':
    return new Double(value);
  case 'f':
    return new Float(value);
  default:
    return new Long(value);
  }
}

RNumber
Number::getNumber(double value, char typeChar) THROWS1(RNumberFormatException)
{
  if (typeChar == 0)
    typeChar = Number::getSmallestTypeChar(value);
  switch(typeChar)
  {
  case 'f':
    return new Float(float(value));
  default:
    return new Double(value);
  }
}


RNumber
Number::decodeToNumber(IN(RString) str, bool tryDecodeOnly, bool ignoreLeadingWs, IN(acdk::util::RLocale) locale) THROWS1(RNumberFormatException)
{
  bool tryOnly = true;
  char typeChar = 0;
  int ignoreTrailing = 0;
  jlong erg = Number::decodeIntegerNumber(str, tryOnly, typeChar, ignoreTrailing, ignoreLeadingWs);
  if (tryOnly == true)
    return Number::getNumber(erg, typeChar);
  tryOnly = true;
  
  double derg  = Number::parseFloatNumber(str, tryOnly, typeChar, ignoreTrailing, ignoreLeadingWs, locale);
  if (tryOnly == true)
    return Number::getNumber(derg, typeChar);
  if (tryDecodeOnly == true)
    return Nil;
  THROW1(NumberFormatException, "Cannot parse to number: " + str);
  return Nil;
}

RNumber
Number::parseToIntegerNumber(IN(RString) str, int radix, bool tryDecodeOnly, bool ignoreLeadingSpaces) THROWS1(RNumberFormatException)
{
  bool tryOnly = true;
  char typeChar = 0;
  int ignoreTrailing = 0;
  jlong erg = Number::parseIntegerNumber(str, radix, tryOnly, typeChar, ignoreTrailing, ignoreLeadingSpaces);
  if (tryOnly == true)
    return Number::getNumber(erg, typeChar);
  if (tryDecodeOnly == true)
    return Nil;
  THROW1(NumberFormatException, "Cannot parse to number: " + str);
  return Nil;
}
//virtual 
dmi::ScriptVar 
Number::toScriptVar()
{
  THROW1(UnsupportedOperationException, "Number::toScriptVar() has to be implemented by derived classes");
  return dmi::ScriptVar();
}

} // lang
} // acdk

