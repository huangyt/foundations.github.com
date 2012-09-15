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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SimpleTimeZone.h,v 1.13 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_SimpleTimeZone_h
#define acdk_util_SimpleTimeZone_h

#include <acdk.h>
#include "Calendar.h"
#include "TimeZone.h"
#include "Date.h"

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;



ACDK_DECL_CLASS(SimpleTimeZone);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SimpleTimeZone
: extends acdk::util::TimeZone
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SimpleTimeZone)
public:
  SimpleTimeZone();
  
  SimpleTimeZone(int rOs, IN(RString) id);

  SimpleTimeZone(int rOs, IN(RString) id, 
                 int startMonth, int startDayOfWeekInMonth, 
                 int startDayOfWeek, int startTime,
                 int endMonth, int endDayOfWeekInMonth,
                 int endDayOfWeek, int endTime);
  
  SimpleTimeZone(int rOs, IN(RString) id, 
                 int startMonth, int startDayOfWeekInMonth, 
                 int startDayOfWeek, int startTime,
                 int endMonth, int endDayOfWeekInMonth,
                 int endDayOfWeek, int endTime, int dstSavings);

  ~SimpleTimeZone();

  void setStartYear(int year) 
  {
    _startYear = year;
    _useDayLight = true;
  }

  void setStartRule(int month, int day, int dayOfWeek, int time);
  
  void setEndRule(int month, int day, int dayOfWeek, int time);
  
  int getOffset(int era, int year, int month, 
                int day, int dayOfWeek, int millis);

  int getRawOffset() 
  {
    return _rawOffset;
  }

  void getRawOffset(int rawOffset) 
  {
    _rawOffset = rawOffset;
  }

  bool useDaylightTime() 
  {
    return _useDayLight;
  }

  bool inDaylightTime(IN(RDate) date);

  RString getDisplayName(bool dst, int style, IN(RLocale) );
  
  foreign int hashCode();
  
  foreign bool equals(IN(RObject) obj);
 

private:
  int getDaysInMonth(int month, int year);

  bool isBefore(int year, int month, int day, int dayOfWeek, 
                int millis, int mode, int otherMonth, int otherDay,
                int otherDayOfWeek, int otherMillis);
protected:

private:
  int _rawOffset;
  bool _useDayLight;
  int _dstSavings ;
  int _startYear;
  int _startMode;
  int _startMonth;
  int _startDay;
  int _startDayOfWeek;
  int _startTime;
  int _endMonth;
  int _endMode;
  int _endDay;
  int _endDayOfWeek;
  int _endTime;
  static const int DOM_MODE;
  static const int DOW_IN_MONTH_MODE; 
  static const int DOW_GE_DOM_MODE;
  static const int DOW_LE_DOM_MODE; 
};


} // util
} // acdk

#endif //acdk_util_SimpleTimeZone_h

