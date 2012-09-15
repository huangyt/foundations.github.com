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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/NumberFormat.cpp,v 1.9 2005/03/08 18:49:55 kommer Exp $


#include "NumberFormat.h"
#include "FieldPosition.h"
#include "ParsePosition.h"
#include "ParseException.h"

#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/ObjectArrayImpl.h>

#include <acdk/lang/Boolean.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Integer.h>

namespace acdk {
namespace text {

using namespace acdk::lang;
using namespace acdk::util;

int NumberFormat::INTEGER_FIELD = 0;
int NumberFormat::FRACTION_FIELD = 1;

NumberFormat::NumberFormat() 
  : Format()/*,
    Cloneable(),
    Serializable()*/
{
}

NumberFormat::~NumberFormat() 
{
}

//statis
RNumberFormat
NumberFormat::getNumberInstance(IN(RLocale) locale)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//static 
RNumberFormat 
NumberFormat::getCurrencyInstance(IN(RLocale) locale)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//static 
RNumberFormat 
NumberFormat::getPercentInstance(IN(RLocale) locale)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//static 
RObjectArrayImpl<RLocale> 
NumberFormat::getAvailableLocales()
{
  RObjectArrayImpl<RLocale> result = new ObjectArrayImpl<RLocale>(1);
  result[0] = Locale::getENGLISH();
  return result;
}

RString 
NumberFormat::format(jlong number) 
{
  RStringBuffer sb = new StringBuffer("");
  format(number, sb, new FieldPosition(INTEGER_FIELD));
  return sb->toString();
}

RString 
NumberFormat::format(double number)
{
  RStringBuffer sb = new StringBuffer("");
  format(number, sb, new FieldPosition(FRACTION_FIELD));
  return sb->toString();
}

RNumber
NumberFormat::parse(IN(RString) str)
{
  RParsePosition pp = new ParsePosition(0);
  RNumber n = parse(str, pp);
  if (pp->getIndex() == 0)
    throw new ParseException("Unable to parse string into Number", 0);
  return n ;
}

int
NumberFormat::hashCode()
{
  return serialized_hashCode(false);
}

bool
NumberFormat::equals(IN(RObject) obj)
{
  return serialized_equals(obj, false);
}

} // text
} // acdk
