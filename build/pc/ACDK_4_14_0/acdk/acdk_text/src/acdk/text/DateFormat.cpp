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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/DateFormat.cpp,v 1.15 2005/03/19 21:29:27 kommer Exp $





#include "DateFormat.h"
#include "NumberFormat.h"
#include "FieldPosition.h"
#include "ParsePosition.h"
#include "ParseException.h"
#include "SimpleDateFormat.h"
#include "DateFormatSymbols.h"
#include <acdk/lang/Number.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Byte.h>
#include <acdk/util/Date.h>
#include <acdk/util/ResourceBundle.h>
#include <acdk/util/MissingResourceException.h>
#include <acdk/lang/StringBuffer.h>

#include <acdk/lang/IllegalArgumentException.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace text {

/* not used any longer
int DateFormat::ERA_FIELD = 0;
int DateFormat::YEAR_FIELD = 1;
int DateFormat::MONTH_FIELD = 2;
int DateFormat::DATE_FIELD = 3;
int DateFormat::HOUR_OF_DAY1_FIELD = 4;
int DateFormat::HOUR_OF_DAY0_FIELD = 5;
int DateFormat::MINUTE_FIELD = 6;
int DateFormat::SECOND_FIELD = 7;
int DateFormat::MILLISECOND_FIELD = 8;
int DateFormat::DAY_OF_WEEK_FIELD = 9;
int DateFormat::DAY_OF_YEAR_FIELD = 10;
int DateFormat::DAY_OF_WEEK_IN_MONTH_FIELD = 11;
int DateFormat::WEEK_OF_YEAR_FIELD = 12;
int DateFormat::WEEK_OF_MONTH_FIELD = 13;
int DateFormat::AM_PM_FIELD = 14;
int DateFormat::HOUR1_FIELD = 15;
int DateFormat::HOUR0_FIELD = 16;
int DateFormat::TIMEZONE_FIELD = 17;
int DateFormat::FULL = 0;
int DateFormat::LONG = 1;
int DateFormat::MEDIUM = 2;
int DateFormat::SHORT = 3;
int DateFormat::DEFAULT = 4;
*/

using namespace acdk::lang;
USING_CLASS(::acdk::util::, Date);
USING_CLASS(::acdk::util::, ResourceBundle);


DateFormat::DateFormat() 
  : Format(),
    _calendar(Nil),
    _numberFormat(Nil),
    _lenient(true)
{
}

DateFormat::~DateFormat() 
{
}

////////////////////////////////////////////////////////////////////////////

RStringBuffer
DateFormat::format(IN(RObject) obj, IN(RStringBuffer) toAppendTo,
                   IN(RFieldPosition) fieldPosition)
{
  if (instanceof(obj, RDate) == true)
    return format(RDate(obj), toAppendTo, fieldPosition);
  else if (instanceof(obj, RNumber))
    return format(RDate(new Date((RNumber(obj)->longValue()))),
                  toAppendTo, fieldPosition);
  else 
    THROW1(IllegalArgumentException,"Cannot format given Object as a Date");
  return Nil;
}

RDate
DateFormat::parse(IN(RString) text)
{
  ParsePosition pos(0);
  RDate result = parse(text, &pos);
  return result;
}

// private static
RDateFormat 
DateFormat::get(FormatStyle timeStyle, FormatStyle dateStyle, IN(RLocale) loc)
{
  
  try {
    RResourceBundle bundle = ResourceBundle::getBundle("acdk/locale/LocaleInfo", loc);
    
    RString datefmt = Nil;
    switch (dateStyle) {
    case NoneFormatStyle : datefmt = Nil; break;
    case FullFormatStyle : datefmt = bundle->getString("fullDateFormat"); break;
    case LongFormatStyle : datefmt = bundle->getString("longDateFormat"); break;
    case MediumFormatStyle : datefmt = bundle->getString("mediumDateFormat"); break;
    case ShortFormatStyle : datefmt = bundle->getString("shortDateFormat"); break;
    case DefaultFormatStyle: datefmt = bundle->getString("mediumDateFormat"); break;
    }
    RString timefmt = Nil;
    switch (timeStyle) 
    {
    case NoneFormatStyle : timefmt = Nil; break;
    case FullFormatStyle : timefmt = bundle->getString("fullTimeFormat"); break;
    case LongFormatStyle : timefmt = bundle->getString("longTimeFormat"); break;
    case MediumFormatStyle : timefmt = bundle->getString("mediumTimeFormat"); break;
    case ShortFormatStyle : timefmt = bundle->getString("shortTimeFormat"); break;
    case DefaultFormatStyle: timefmt = bundle->getString("mediumTimeFormat"); break;
    }

    if (datefmt != Nil && timefmt != Nil)
      return new SimpleDateFormat(datefmt + " " + timefmt, new DateFormatSymbols(loc));
    if (datefmt != Nil)
      return new SimpleDateFormat(datefmt, new DateFormatSymbols(loc));
    if (timefmt != Nil)
      return new SimpleDateFormat(timefmt, new DateFormatSymbols(loc));
    return new SimpleDateFormat("M/d/yy h:mm a");
  } catch(::acdk::util::RMissingResourceException e) {
    return new SimpleDateFormat("M/d/yy h:mm a", new DateFormatSymbols(loc));
  }
  return Nil;
}

bool
DateFormat::equals(IN(RObject) obj)
{
  if (this == obj.impl()) 
    return true;
  if (instanceof(obj, DateFormat) == false)
    return false;
  // ### implement DateFormat::equals 
  return false;  
}

int
DateFormat::hashCode()
{
  SYNCTHIS();
  int result = 0;
  //result = 31 * result + (_calendar == Nil ? 0 : _calendar->hashCode());
  result = 31 * result + (_numberFormat == Nil ? 0 : _numberFormat->hashCode());
  result = 31 * result + RBoolean(new Boolean(_lenient))->hashCode();
  return result;  
}

RString 
DateFormat::format(IN(RDate) date)
{
  FieldPosition fp(0);
  StringBuffer sb(30);
  return format(date, &sb, SR(FieldPosition, fp))->toString();
}
    
} // text
} // acdk
