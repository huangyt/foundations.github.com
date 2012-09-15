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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Time.cpp,v 1.12 2005/02/05 10:45:31 kommer Exp $

#include <acdk.h>
#if !defined(ACDK_MINI)

#include "sql.h"
#include <acdk/lang/System.h>

#include <acdk/text/text.h>
#include <acdk/text/SimpleDateFormat.h>
#include <acdk/text/DateFormatSymbols.h>

#include "Time.h"

namespace acdk {
namespace sql {

using namespace acdk::lang;
//using namespace acdk::io;
//using namespace acdk::util;

acdk::text::RSimpleDateFormat Time::__sdf;
using acdk::util::Date;

//static 
acdk::text::RSimpleDateFormat 
Time::getSimpleDateFormat()
{
  if (__sdf != Nil)
    return __sdf;
  __sdf = new acdk::text::SimpleDateFormat("HH:mm:ss");
  System::registerStaticReference(__sdf);
  return __sdf;
}

  

//static 
RTime
Time::valueOf(INP(RString) str)
{
  try {
    acdk::util::RDate d = (acdk::util::RDate)getSimpleDateFormat()->parse(str);
    return new Time(d->getTime());
  } catch(RException e) {
      return Nil;
  }
}


Time::Time(int hour, int minute, int second)
: Date(System::currentTimeMillis())
{
  //setHours(hour);
  //setMinutes(minute);
  //setSeconds(second);
}

Time::Time(jlong date)
: Date(date)
{
}

RString
Time::toString()
{
  return getSimpleDateFormat()->format(this);
}


} // sql
} // acdk

#endif //#if !defined(ACDK_MINI)
