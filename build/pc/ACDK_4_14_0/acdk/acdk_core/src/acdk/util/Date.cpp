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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Date.cpp,v 1.17 2005/03/08 12:45:45 kommer Exp $


#include <acdk.h>
#include <acdk/util/Date.h>
#include <acdk/util/SysDate.h>
#include <acdk/util/GregorianCalendar.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {


Date::Date()
  : Object(),
    _date(0)
{
  _date = SysDate().getTime();
}

Date::Date(jlong date)
  : Object(),
    _date(date)
{
}

/*
Date::Date(int sec, int usec)
  : Object(),
    _date(jlong(1000) * sec + usec / jlong(1000))
{
}
*/

Date::Date(IN(RString) s) // ### Implement me 
  : Object(),
    _date(0)
{
  THROW0(UnsupportedOperationException);
}

Date::Date(int year, int month, int day)
  : Object(),
    _date(0)
{
  _date = GregorianCalendar(year, month, day).getTimeInMillis();
}

Date::Date(int year, int month, int day, int hour, int min)
  : Object(),
    _date(0)
{
  _date = GregorianCalendar(year, month, day, hour, min).getTimeInMillis();
}
 
Date::Date(int year, int month, int day, int hour, int min, int sec)
  : Object(),
    _date(0)
{
  _date = GregorianCalendar(year, month, day, hour, min, sec).getTimeInMillis();
}

// #### remove this. it has very low performance
int Date::getYear()
{
  GregorianCalendar gc;
  gc.setTimeInMillis(_date);
  return gc.get(Calendar::YEAR);
}

//virtual 
int 
Date::compareTo(IN(RObject) o)
{
  if (instanceof(o, Date) == false)
    THROW1(ClassCastException, "Date::compareTo(RObject o)");
  return getTime() - RDate(o)->getTime() > 0 ? -1 : getTime() - RDate(o)->getTime() < 0 ? 1 : 0;
}

//virtual 
bool 
Date::equals(IN(RObject) obj)
{
  if (instanceOf(obj, Date) == false)
    return false;
  return getTime() == RDate(obj)->getTime();
}

//virtual 
RString 
Date::toString()
{
  
  RCalendar _cld = Calendar::getInstance();
  Calendar& cld = *_cld;
  cld.setTimeInMillis(getTime());
  StringBuffer sb(100);
  
  sb.append(cld.get(Calendar::YEAR));
  sb.append("-");
  int t = cld.get(Calendar::MONTH) + 1;
  if (t < 10)
    sb.append("0");
  sb.append(t);
  sb.append("-");
  t = cld.get(Calendar::DAY_OF_MONTH);
  if (t < 10)
    sb.append("0");
  sb.append(t);
  
  sb.append("T");
  t = cld.get(Calendar::HOUR);
  if (t < 10)
    sb.append("0");
  sb.append(t);
  sb.append(":");
  t = cld.get(Calendar::MINUTE);
  if (t < 10)
    sb.append("0");
  sb.append(t);
  sb.append(":");
  t = cld.get(Calendar::SECOND);
  if (t < 10)
    sb.append("0");
  sb.append(t);
  
  sb.append(":");
  sb.append(cld.get(Calendar::MILLISECOND));

  return new String(sb.toString());
}

RString 
Date::getTimeStamp()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}


//static 
jlong 
Date::getTickCount()
{
  return SysDate().getTime();
}

} // util
} // acdk

