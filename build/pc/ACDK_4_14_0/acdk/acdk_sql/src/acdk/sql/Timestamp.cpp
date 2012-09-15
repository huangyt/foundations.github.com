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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Timestamp.cpp,v 1.9 2005/02/05 10:45:31 kommer Exp $



#include "Timestamp.h"
#if !defined(ACDK_MINI)

#include <acdk/lang/System.h>
#include <acdk/text/ParsePosition.h>
#include <acdk/text/DateFormatSymbols.h>

namespace acdk {
namespace sql {

using namespace acdk::lang;
using namespace acdk::io;
using namespace acdk::util;

acdk::text::RSimpleDateFormat Timestamp::__parse_sdf; //=  new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");

//static 
acdk::text::RSimpleDateFormat 
Timestamp::parse_sdf()
{
  if (__parse_sdf != Nil)
    return __parse_sdf;
  __parse_sdf  = new acdk::text::SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
  System::registerStaticReference(__parse_sdf);
  return __parse_sdf;
}

acdk::text::RSimpleDateFormat Timestamp::__format_sdf;// = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
//static 
acdk::text::RSimpleDateFormat 
Timestamp::format_sdf()
{
  if (__format_sdf != Nil)
    return __format_sdf;
  __format_sdf  = new acdk::text::SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
  System::registerStaticReference(__format_sdf);
  return __format_sdf;
}

  

//static 
RTimestamp
Timestamp::valueOf(INP(RString) str)
{
  try {
    acdk::text::ParsePosition ppos(0);
    acdk::util::RDate d = (acdk::util::RDate)parse_sdf()->parseObject(str, SR_FQ(acdk::text::, ParsePosition, ppos));
    return new Timestamp(d->getTime());
  } catch(RException e) {
      return Nil;
  }
}

Timestamp::Timestamp(int year, int month, int day, int hour, int minute, int second, int nanos)
: Date(year, month, day, hour, minute, second),
  _nanos(nanos)
{
}

Timestamp::Timestamp(jlong date)
: Date(date),
  _nanos(0)
{
}

RString 
Timestamp::toString()
{
  return format_sdf()->format(this) + "." + getNanos();
}


bool 
Timestamp::before(Timestamp& ts)
{
  if (ts.getTime() > getTime())
    return true;
  if (ts.getNanos() > getNanos())
    return true;
  return false;
}


bool 
Timestamp::after(Timestamp& ts)
{
  if (ts.getTime() < getTime())
    return true;
  if (ts.getNanos() < getNanos())
    return true;
  return false;
}

bool 
Timestamp::equals(INP(RObject) obj)
{
  if (obj == Nil)
    return false;
  if (instanceof(obj, Timestamp) == false)
    return false;
  return equals((RTimestamp)obj);
}

bool 
Timestamp::equals(Timestamp& ts)
{
  if (ts.getTime() != getTime())
    return false;
  if (ts.getNanos() != getNanos())
    return false;
  return true;
}

} // sql
} // acdk

#endif //#if !defined(ACDK_MINI)
