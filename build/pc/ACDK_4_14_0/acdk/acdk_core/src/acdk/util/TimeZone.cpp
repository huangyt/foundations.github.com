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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TimeZone.cpp,v 1.10 2005/03/08 12:45:46 kommer Exp $


#include <acdk.h>

#include "TimeZone.h"
#include "SimpleTimeZone.h"
#include <acdk/lang/System.h>

#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

RStringArrayArray TimeZone::_zoneAlliases;
RTimeZoneArray TimeZone::_timeZone ;

int TimeZone::SHORT = 0;
int TimeZone::LONG = 1;
RTimeZone TimeZone::_defaulZone = Nil;

//static
RStringArrayArray
TimeZone::get_zoneAlliases() 
{
  if (_zoneAlliases == Nil) {
    _zoneAlliases = new StringArrayArray(2);
    _zoneAlliases[0] = new StringArray(2);
    _zoneAlliases[1] = new StringArray(2);
    _zoneAlliases[0][0] = new String("UTC");
    _zoneAlliases[0][1] = new String("GMT");
    _zoneAlliases[1][0] = new String("MET");
    _zoneAlliases[1][1] = new String("CET");

    System::registerStaticReference(_zoneAlliases);
  }
  return _zoneAlliases;
}

RTimeZoneArray
TimeZone::get_timeZone()
{
  if (_timeZone == Nil) {
    _timeZone = new ObjectArrayImpl<RTimeZone> (56);
    _timeZone[0] = new SimpleTimeZone( 0, "UTC" , 
                           Calendar::APRIL  ,  1, Calendar::SUNDAY, 2000*3600,
                           Calendar::OCTOBER, -1, Calendar::SUNDAY, 2000*3600);
    int i = 0;
    for(i = 1; i <=12; i++) {
      _timeZone[i] = new SimpleTimeZone( i * 1000 * 3600, "UTC+" + String::valueOf(i) + ":00" , 
                           Calendar::APRIL  ,  1, Calendar::SUNDAY, 2000*3600,
                           Calendar::OCTOBER, -1, Calendar::SUNDAY, 2000*3600);
      _timeZone[12 + i] = new SimpleTimeZone( -i * 1000 * 3600, "UTC-" + String::valueOf(i) + ":00" , 
                           Calendar::APRIL  ,  1, Calendar::SUNDAY, 2000*3600,
                           Calendar::OCTOBER, -1, Calendar::SUNDAY, 2000*3600);
      _timeZone[24 + i] = new SimpleTimeZone( i * 1000 * 3600, "GMT+" + String::valueOf(i) + ":00" , 
                           Calendar::APRIL  ,  1, Calendar::SUNDAY, 2000*3600,
                           Calendar::OCTOBER, -1, Calendar::SUNDAY, 2000*3600);
      _timeZone[36 + i] = new SimpleTimeZone( -i * 1000 * 3600, "GMT-" + String::valueOf(i) + ":00" , 
                           Calendar::APRIL  ,  1, Calendar::SUNDAY, 2000*3600,
                           Calendar::OCTOBER, -1, Calendar::SUNDAY, 2000*3600);
    }
    _timeZone[49] = new SimpleTimeZone( 0, "GMT" , 
                           Calendar::APRIL  ,  1, Calendar::SUNDAY, 2000*3600,
                           Calendar::OCTOBER, -1, Calendar::SUNDAY, 2000*3600);
    _timeZone[50] = new SimpleTimeZone( -1000 * 3600, "CVT" , 
                           Calendar::APRIL    , -1, Calendar::FRIDAY, 1000*3600,
                           Calendar::SEPTEMBER, -1, Calendar::FRIDAY, 3000*3600);
    _timeZone[51] = new SimpleTimeZone( 1000 * 3600, "WAT" , 
                           Calendar::APRIL    , -1, Calendar::FRIDAY, 1000*3600,
                           Calendar::SEPTEMBER, -1, Calendar::FRIDAY, 3000*3600);
    _timeZone[52] = new SimpleTimeZone( 1000 * 3600, "MET" , 
                           Calendar::APRIL  ,  1, Calendar::SUNDAY, 2000*3600,
                           Calendar::OCTOBER, -1, Calendar::SUNDAY, 2000*3600);
    _timeZone[53] = new SimpleTimeZone( 1000 * 3600, "CET" , 
                           Calendar::APRIL    , -1, Calendar::FRIDAY, 1000*3600,
                           Calendar::SEPTEMBER, -1, Calendar::FRIDAY, 3000*3600);
    _timeZone[54] = new SimpleTimeZone( 2000 * 3600, "CAT" , 
                           Calendar::APRIL    , -1, Calendar::FRIDAY, 1000*3600,
                           Calendar::SEPTEMBER, -1, Calendar::FRIDAY, 3000*3600);
    _timeZone[55] = new SimpleTimeZone( 3000 * 3600, "EAT" , 
                           Calendar::APRIL    , -1, Calendar::FRIDAY, 1000*3600,
                           Calendar::SEPTEMBER, -1, Calendar::FRIDAY, 3000*3600);
    System::registerStaticReference(_timeZone);
  }
  return _timeZone;
}


TimeZone::TimeZone() 
: _id(Nil)
{
}

TimeZone::TimeZone(IN(RString) id) 
: _id(id)
{
}

TimeZone::~TimeZone() 
{
}

//static
RTimeZone
TimeZone::getDefault()
{
  if (_defaulZone == Nil) {
    _defaulZone = getTimeZone("GMT+1:00");
    System::registerStaticReference(_defaulZone);
  }
  return _defaulZone;
}

RString 
TimeZone::getDisplayName()
{
  return getDisplayName(false, LONG, Locale::getDefault());
}
  
RString 
TimeZone::getDisplayName(IN(RLocale) locale)
{
  return getDisplayName(false, LONG, locale);
}

RString 
TimeZone::getDisplayName(bool b, int st)
{
  return getDisplayName(b, st, Locale::getDefault());
}

RString 
TimeZone::getDisplayName(bool b, int st, IN(RLocale) locale)
{
  int offset = getRawOffset();
  
  int hours = offset / (1000*60*60);
  int minutes = (offset / (1000*60)) % 60;
  RString reStr = new String("GMT");
  if (hours > 9)
    reStr = reStr + hours;
  else 
    reStr = reStr + RString("0") + hours;

  if (minutes > 9)
    reStr = reStr + minutes;
  else 
    reStr = reStr + RString("0") + minutes;
  return reStr;
}

//static 
RTimeZone 
TimeZone::getTimeZone(IN(RString) id)
{
  RTimeZoneArray tza = get_timeZone();
  for (int i = 0; i < tza->length(); i++) {
    if (tza[i]->getID()->equals(id) == true)
      return tza[i];
  }
  return Nil;
}

//static 
RStringArray
TimeZone::getAvailableIDs(int rawOffset)
{
  RTimeZoneArray timeZone = get_timeZone();
  int count = 0;
  int i;
  for (i = 0; i < timeZone->length(); i++) 
    if (timeZone[i]->getRawOffset() == rawOffset)
      count++;
  RStringArray result = new StringArray(count);
  int idx = 0;
  for (i = 0; i < timeZone->length(); i++)
    if (timeZone[i]->getRawOffset() == rawOffset)
      result[idx++] = timeZone[i]->getID();
  return result;
}

//static 
RStringArray
TimeZone::getAvailableIDs()
{
  RStringArray result = new StringArray(_timeZone->length());
  for (int i = 0; i < _timeZone->length(); i++)
    result[i] = _timeZone[i]->getID();
  return result;
}

RObject
TimeZone::clone(sys::Allocator* alc)
{
  THROW0(UnsupportedOperationException);
  return Nil;  
}


} // util
} // acdk
