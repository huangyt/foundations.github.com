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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Float.cpp,v 1.18 2005/04/04 16:06:06 kommer Exp $



#include <acdk.h>

#include <climits>
#include <stdio.h>

#include "Float.h"
#include "NumberFormatException.h"

namespace acdk {
namespace lang {

float Float::MAX_VALUE = 1.4e-45f;
float Float::MIN_VALUE  = 3.40282346638528860e+38f; //3.4028235e+38f; 

Float::Float(IN(RString) str, IN(acdk::util::RLocale) locale) THROWS1(RNumberFormatException)
: Number(), 
  value(0) 
{ 
  value = parseFloat(str, locale);
}

//virtual 
bool 
Float::equals(IN(RObject) obj) 
{
  return obj != Nil && 
          instanceof(obj, Float) != false && 
          RFloat(obj)->floatValue() == floatValue(); 
}


//static 
RString 
Float::toString(float i, IN(acdk::util::RLocale) locale)
{
  char buffer[1024];
  sprintf(buffer, "%f", i);
  RString ret = SCS(buffer);
  
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
float 
Float::parseFloat(IN(RString) str, IN(acdk::util::RLocale) locale)  THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 'f';
  int ignoreTrailing = 0;
  return parseFloatNumber(str, tryOnly, typeChar, ignoreTrailing);
}

//static 
RFloat 
Float::valueOf(IN(RString) str) THROWS1(RNumberFormatException)
{
  return new Float(parseFloat(str));
}

//static 
RClass 
Float::getTYPE() 
{ 
  return Class::getSingeltonClass(dmi::ClazzInfo::getFloatClazz());
}



} // lang
} // acdk
