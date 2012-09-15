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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Double.cpp,v 1.21 2005/04/26 22:03:04 kommer Exp $



#include <acdk.h>
#include "Double.h"
#include "Long.h"

#if defined (ACDK_OS_UNIX)
# if defined(ACDK_OS_BSD) && !defined(isnan)
// no idea, why math.h does not work on FreeBSD 5.1 like excpected!
#define	fpclassify(x) \
    ((sizeof (x) == sizeof (float)) ? __fpclassifyf(x) \
    : (sizeof (x) == sizeof (double)) ? __fpclassifyd(x) \
    : __fpclassifyl(x))
#   define	isnan(x)	(fpclassify(x) == FP_NAN)
# endif
# include <math.h>
# if !defined(ACDK_OS_DARWIN)
# define HAS_ISNAN
#endif
#endif


#include <stdlib.h>
#include <stdio.h>
#include <climits>
#if defined(_MSC_VER) || defined(__BORLANDC__)
#include <float.h>
#endif


namespace acdk {
namespace lang {

double Double::MAX_VALUE = 1.7976931348623157e+308;
double Double::MIN_VALUE = 4.9e-324;
#if defined(NAN) && !defined(ACDK_OS_MINGW)
double Double::NaN = NAN;
#else
double Double::NaN = Double::longBitsToDouble(JLONG_CONSTANT(0x7ff8000000000000));
#endif
double Double::NEGATIVE_INFINITY  = Double::longBitsToDouble(JLONG_CONSTANT(0xfff0000000000000));
double Double::POSITIVE_INFINITY  = Double::longBitsToDouble(JLONG_CONSTANT(0x7ff0000000000000));

Double::Double(IN(RString) str, IN(acdk::util::RLocale) locale) THROWS1(RNumberFormatException)
: Number(),
  value(0)
{
  value = parseDouble(str, locale);
}


bool
Double::isNaN()
{
  return isNaN(value);
}

//static
bool
Double::isNaN(double v)
{
#if defined(_MSC_VER)  || defined(__BORLANDC__)
  return _isnan( v) != 0;
#elif defined(HAS_ISNAN)
  return isnan(v) != 0;
  //return fpclassify(v) == FP_NAN;
#else
  return v == NaN;
#endif
}

bool
Double::isInfinite()
{
  return isInfinite(value);
}

//static
bool
Double::isInfinite(double v)
{
#ifdef _MSC_VER
  int c = _fpclass(v);
  return c == _FPCLASS_NINF  || c == _FPCLASS_PINF;
#else
  return v == NEGATIVE_INFINITY
          || v == POSITIVE_INFINITY;
#endif
}

//virtual
bool
Double::equals(IN(RObject) obj)
{
  return obj != Nil &&
          instanceof(obj, Double) != false &&
          RDouble(obj)->doubleValue() == doubleValue();
}

//static
int
Double::hashCode(double value)
{
  return Long::hashCode(*((jlong*)&value));
}

//static
double
Double::parseDouble(IN(RString) s, IN(acdk::util::RLocale) locale) THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 'd';
  int ignoreTrailing = 0;
  return parseFloatNumber(s, tryOnly, typeChar, ignoreTrailing, locale);
}

//static
RString
Double::toString(double d, IN(acdk::util::RLocale) locale)
{
  if (isNaN(d) == true)
    return "NaN";
  if (isInfinite(d) == true)
  {
    if (d < 0)
      return "-Infitinite";
    return "-Infitinite";
  }
  char buf[128];
  sprintf(buf, "%f", d);
  RString ret = SCS(buf);
  //if (locale == Nil)
  //  return ret;
  acdk::util::RLocale loc = locale;

  if (loc == Nil)
    loc = acdk::util::Locale::getUS();
   ucchar sysfractionSep = *localeconv()->decimal_point;
  ucchar sysexpChar = 'e';

  ucchar fractionSep = sysfractionSep;
  ucchar expChar = 'e';

  getFractionAndExponentSignFromLocale(loc, fractionSep, expChar);
  return ret->replace(sysfractionSep, fractionSep)->replace(sysexpChar, expChar);
}

//static
RDouble
Double::valueOf(IN(RString) str)  THROWS1(RNumberFormatException)
{
  return new Double(parseDouble(str));
}

//static
RClass
Double::getTYPE()
{
  return Class::getSingeltonClass(dmi::ClazzInfo::getDoubleClazz());
}



} // Lang
} // acdk
