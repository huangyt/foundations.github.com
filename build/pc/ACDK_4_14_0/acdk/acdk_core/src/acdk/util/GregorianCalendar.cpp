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
/* 
    This file contains a C++ port of GNU Classpath project (http://www.gnu.org/software/classpath/classpath.html)
    with following copyright statement:
  
 * java.util.GregorianCalendar: part of the Java Class Libraries project.
 * Copyright (C) 1998 Jochen Hoenicke
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
*/
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/GregorianCalendar.cpp,v 1.12 2005/03/08 12:45:45 kommer Exp $



#include "GregorianCalendar.h"
#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/IllegalArgumentException.h>

namespace acdk {
namespace util {

int GregorianCalendar::BC = 0;
int GregorianCalendar::AD = 1;
const jlong GregorianCalendar::DEFAULT_GREGORIANCUTOVER =
                            (24 * 60 * 60 * jlong(1000)) *
                            (((1582 * (365 * 4 + 1)) / 4 + 
                             (OCTOBER * (31 + 30 + 31 + 30 + 31) - 9) / 5 + 5) -
                             ((1970 * (365 * 4 + 1)) / 4 + 1 - 13));


const int GregorianCalendar::_minimum[]     = { BC, 1, JANUARY, 0, 1, 1, 1,
                                                SUNDAY, 1, AM, 1, 0, 0, 0, 0,
                                                -(12 * 60 * 60 * 1000), 0};
const int GregorianCalendar::_maximum[]     = { AD, 5000000, DECEMBER, 53, 5, 31, 366,
                                                SATURDAY, 5, PM, 12, 23, 59, 59, 999, 
                                                (12 * 60 * 60 * 1000), (12 * 60 * 60 * 1000) };
const int GregorianCalendar::_greatestMin[] = { BC, 1, JANUARY, 1, 1, 1, 1,
                                                SUNDAY, 1, AM, 1, 0, 0, 0, 0,
                                                -(12 * 60 * 60 * 1000), 0};
const int GregorianCalendar::_leastMax[]    = { AD, 5000000, DECEMBER, 52, 5, 28, 365,
                                                SATURDAY, 4, PM, 12, 23, 59, 59, 999, 
                                                (12 * 60 * 60 * 1000), (12 * 60 * 60 * 1000) };
const int GregorianCalendar::_tfDefault[]   = { AD, 1970, JANUARY, 1, 1, 1, 1,
                                                THURSDAY, 1, AM, 0, 0, 0, 0, 0,
                                                0, 0};

GregorianCalendar::GregorianCalendar() 
  : Calendar()
{
  _gregorianCutover = DEFAULT_GREGORIANCUTOVER;
}

GregorianCalendar::GregorianCalendar(IN(RTimeZone) zone)
  : Calendar(zone, Locale::getDefault())
{
  _gregorianCutover = DEFAULT_GREGORIANCUTOVER;
}
 
GregorianCalendar::GregorianCalendar(IN(RLocale) locale)
  : Calendar(TimeZone::getDefault(), locale)
{
  _gregorianCutover = DEFAULT_GREGORIANCUTOVER;
}

GregorianCalendar::GregorianCalendar(IN(RTimeZone) zone, IN(RLocale) locale)
  : Calendar(zone, locale)
{
  _gregorianCutover = DEFAULT_GREGORIANCUTOVER;
}

GregorianCalendar::GregorianCalendar(int year, int month, int day)
  : Calendar()
{
  set(year, month, day);
  //add(MILLISECOND, -_fields[ZONE_OFFSET]);
  _gregorianCutover = DEFAULT_GREGORIANCUTOVER;
}
 
GregorianCalendar::GregorianCalendar(int year, int month, int day, 
                                     int hour, int minute)
  : Calendar()
{
  set(year, month, day, hour, minute);
  //add(MILLISECOND, -_fields[ZONE_OFFSET]);
  _gregorianCutover = DEFAULT_GREGORIANCUTOVER;
}
 
GregorianCalendar::GregorianCalendar(int year, int month, int day, 
                                     int hour, int minute, int second)
  : Calendar()
{
  set(year, month, day, hour, minute, second);
  //add(MILLISECOND, -_fields[ZONE_OFFSET]);
  _gregorianCutover = DEFAULT_GREGORIANCUTOVER;
}

GregorianCalendar::~GregorianCalendar() 
{
}


bool 
GregorianCalendar::isLeapYear(int year)
{
  
  if ((year & 3) != 0)
    return false;
  
  int julianDay = (((year-1) * (365*4+1)) >> 2) + (31+29 - 
    (((1970-1) * (365*4+1)) / 4 + 1 - 13));
  
  if (julianDay * (24 * 60 * 60 * 1000L) < _gregorianCutover)
    return true;
  
  return ((year % 100) != 0 || (year % 400) == 0);
/* old  
  if ((year & 3) != 0)
    return false;

  int julianDay = (((year - 1) * (365 * 4 + 1)) >> 2) + (31 + 29 - 
                  (((1970 - 1) * (365 * 4 + 1)) / 4 + 1 - 13));
  if (julianDay * (24 * 60 * 60 * jlong(1000)) < _gregorianCutover)
    return true;

  return ((year % 100) != 0 || (year % 400) == 0);
  */
}
 
bool 
GregorianCalendar::equals(IN(RObject) obj)
{
  THROW0(UnsupportedOperationException);
  return false;
}

void 
GregorianCalendar::add(int field, int amount)
{
  do {
    if (field == YEAR) {
      complete();
      _fields[YEAR] += amount;
      _isTimeSet = false;
      break;
    }
    if (field == MONTH) {
      complete();
      _fields[YEAR] += (_fields[MONTH] + amount) / 12;
      _fields[MONTH] = (_fields[MONTH] + amount) % 12;
      if (_fields[MONTH] < 0) {
        _fields[MONTH] += 12;
        _fields[YEAR]--;
      }
      break;
    }
    if (field == DAY_OF_MONTH || field == DAY_OF_YEAR || field == DAY_OF_WEEK) {
      if (!_isTimeSet) {
        computeTime();
      }
      _time += amount * (24 * 60 * 60 * jlong(1000));
      _areFieldsSet = false;
      break;
    }
    if (field == WEEK_OF_YEAR || field == WEEK_OF_MONTH || field == DAY_OF_WEEK_IN_MONTH) {
      if (!_isTimeSet) {
        computeTime();
      }
      _time += amount * (7 * 24 * 60 * 60 * jlong(1000));
      _areFieldsSet = false;
      break;
    }
    if (field == HOUR || field == HOUR_OF_DAY) {
      if (!_isTimeSet) {
        computeTime();
      }
      _time += amount * (60 * 60 * jlong(1000));
      _areFieldsSet = false;
      break;
    }
    if (field == MINUTE) {
      if (!_isTimeSet) {
        computeTime();
      }
      _time += amount * (60 * jlong(1000));
      _areFieldsSet = false;
      break;
    }
    if (field == SECOND) {
      if (!_isTimeSet) {
        computeTime();
      }
      _time += amount * jlong(1000);
      _areFieldsSet = false;
      break;
    }
    if (field == MILLISECOND) {
      if (!_isTimeSet) {
        computeTime();
      }
      _time += amount;
      _areFieldsSet = false;
      break;
    }
    if (field == ZONE_OFFSET) {
      complete();
      _fields[ZONE_OFFSET] += amount;
      _time -= amount;
      break;
    }
    if (field == DST_OFFSET) {
      complete();
      _fields[DST_OFFSET] += amount;
      _isTimeSet = false;
      break;
    }
    // default:
    THROW0(IllegalArgumentException);
  } while (false);
}

void 
GregorianCalendar::roll(int field, bool up)
{
  complete();
  if (up == true){
    if (_fields[field] < getActualMaximum(field))
      _fields[field]++;
    else
      _fields[field] = getActualMinimum(field);
  } else {
    if (_fields[field] > getActualMinimum(field))
      _fields[field]--;
    else
      _fields[field] = getActualMaximum(field);
  }
  _isTimeSet = false;
}

int 
GregorianCalendar::getMinimum(int field)
{
  if (field < 0 || field >= FIELD_COUNT)
    THROW0(IllegalArgumentException);
  return _minimum[field];
}
 
int 
GregorianCalendar::getMaximum(int field)
{
  if (field < 0 || field >= FIELD_COUNT)
    THROW0(IllegalArgumentException);
  return _maximum[field];
}

int 
GregorianCalendar::getGreatestMinimum(int field)
{
  if (field < 0 || field >= FIELD_COUNT)
    THROW0(IllegalArgumentException);
  return _greatestMin[field];
}
 
int 
GregorianCalendar::getLeastMaximum(int field)
{
  if (field < 0 || field >= FIELD_COUNT)
    THROW0(IllegalArgumentException);
  return _leastMax[field];
}
 
int 
GregorianCalendar::getActualMinimum(int field)
{
  if (field == WEEK_OF_YEAR) {
    int min = getMinimalDaysInFirstWeek();
    if (min == 0)
      return 1;
    complete();
    int weekday = getWeekDay(_fields[YEAR], min);
    if ((7 + weekday - getFirstDayOfWeek()) % 7 >= min - 1)
      return 1;
    return 0;
  }
  return _minimum[field];
}
 
int 
GregorianCalendar::getActualMaximum(int field)
{
  if (field == WEEK_OF_YEAR) {
    int lastDay = getActualMaximum(DAY_OF_YEAR);
    int weekday = getWeekDay(_fields[YEAR], lastDay);
    int week = (lastDay + 6 - (7 + weekday - getFirstDayOfWeek()) % 7) / 7;
    int minimalDays = getMinimalDaysInFirstWeek();
    int firstWeekday = getWeekDay(_fields[YEAR], minimalDays);
    if (minimalDays - (7 + firstWeekday - getFirstDayOfWeek()) % 7 < 1)
      return week + 1;
  }
  if (field == DAY_OF_MONTH) {
    complete();
    int month = _fields[MONTH];
    if (month == FEBRUARY) {
      int year = _fields[ERA] == AD ? _fields[YEAR] : 1 - _fields[YEAR];
      return  isLeapYear(year) ? 366 : 365;
    } else if (month < AUGUST)
        return  31 - (month & 1);
      else
        return  30 + (month & 1);
    }
    if (field == DAY_OF_YEAR) {
      complete();
      int year = _fields[ERA] == AD ? _fields[YEAR] : 1 - _fields[YEAR];
      return  isLeapYear(year) ? 366 : 365;
    }
    if (field == DAY_OF_WEEK_IN_MONTH) {
      int daysInMonth = getActualMaximum(DAY_OF_MONTH);
      return  (daysInMonth - (_fields[DAY_OF_MONTH] - 1) % 7 + 6) / 7;
    }
    if (field == WEEK_OF_MONTH) {
      int daysInMonth = getActualMaximum(DAY_OF_MONTH);
      int weekday = (daysInMonth - _fields[DAY_OF_MONTH] + _fields[DAY_OF_WEEK] - SUNDAY) % 7 + SUNDAY;
      return  (daysInMonth + 6 - (7+weekday-getFirstDayOfWeek()) % 7) /7;
    }

  // default is ...
  return _maximum[field];
}

//private
int 
GregorianCalendar::getLinearDay(int year, int dayOfYear, bool gregorian)
{
  int julianDay = ((year * (365 * 4 + 1)) >> 2) + dayOfYear - 
                  ((1970 * (365 * 4 + 1)) / 4 + 1 - 13);

  if (gregorian) {
    int gregOffset = (year / 400) - (year / 100) + 2;
    if (isLeapYear(year) && dayOfYear < 31 + 29)
      --gregOffset;
    julianDay += gregOffset;
  }
  return julianDay;

}

//private
jlong 
GregorianCalendar::getLinearTime(int year, int dayOfYear, int millis)
{
  int julianDay = ((year * (365 * 4 + 1)) >> 2) + dayOfYear - 
                  ((1970 * (365 * 4 + 1)) / 4 + 1 - 13);
  jlong time = julianDay * (24 * 60 * 60 * jlong(1000)) + millis;

  if (time >= _gregorianCutover)
  {
    int gregOffset = (year / 400) - (year / 100) + 2;
	  if (isLeapYear(year) && dayOfYear < 31 + 29)
	    --gregOffset;
	  time += gregOffset * (24 * 60 * 60 * 1000L);
  }
  return time;
}
 
//private
int 
GregorianCalendar::getWeekDay(int year, int dayOfYear)
{
  int day = (int) (getLinearTime(year, dayOfYear, 0) / (24 * 60 * 60 * jlong(1000)));

  int weekday = (day + THURSDAY) % 7;
  if (weekday <= 0)
    weekday += 7;
  return weekday;
}

//private
void 
GregorianCalendar::calculateDay(int day, bool gregorian)
{
  
  int weekday = (day + THURSDAY) % 7;
  if (weekday <= 0)
    weekday += 7;
  _fields[DAY_OF_WEEK] = weekday;
  int year = 1970 + (gregorian 
                     ? ((day - 100) * 400) / (365 * 400 + 100 - 4 + 1)
                     : ((day - 100) *   4) / (365 * 4 + 1));
  if (day >= 0)
    year++;

  int firstDayOfYear = getLinearDay(year , 1, gregorian);
  if (day < firstDayOfYear) {
    year--;
    firstDayOfYear = getLinearDay(year, 1, gregorian);
  }

  day -= firstDayOfYear - 1;

  _fields[DAY_OF_YEAR] = day;
  if (year <= 0) {
    _fields[ERA] = BC;
    _fields[YEAR] = 1-year;
  } else {
    _fields[ERA] = AD;
    _fields[YEAR] = year;
  }
  int leapday = isLeapYear(year) ? 1 : 0;
  if (day <= 31 + 28 + leapday) {
    _fields[MONTH] = day / 32;	// 31->JANUARY, 32->FEBRUARY
    _fields[DAY_OF_MONTH] = day - 31 * _fields[MONTH];
  } else {
    
    int scaledDay = (day - leapday) * 5 + 8;
    _fields[MONTH] = scaledDay / (31 + 30 + 31 + 30 + 31);
    _fields[DAY_OF_MONTH] = (scaledDay % (31 + 30 + 31 + 30 + 31)) / 5 + 1;
  }

}
  
//protected
void 
GregorianCalendar::computeTime()
{
  int era  = _isSet[ERA]  ? _fields[ERA] : AD;
  int year = _isSet[YEAR] ? _fields[YEAR] : 1970;
  if (era == BC)
    year = 1-year;
  int dayOfYear;
  int daysInWeeks = 0;
  do {
    if (_isSet[MONTH]) {
      if (_fields[MONTH] > FEBRUARY) {
        dayOfYear = (_fields[MONTH] * (31 + 30 + 31 + 30 + 31) - 9) / 5;
        if (isLeapYear(year))
          dayOfYear++;
      } else
        dayOfYear = 31 * _fields[MONTH];
                
      if (_isSet[DAY_OF_MONTH]) {
        dayOfYear += _fields[DAY_OF_MONTH];
        break;
      }
      
      if (_isSet[WEEK_OF_MONTH] && _isSet[DAY_OF_WEEK]) {
        int weekday = getWeekDay(year, ++dayOfYear);
        daysInWeeks = _fields[DAY_OF_WEEK] - weekday;
        daysInWeeks += 7 * (_fields[WEEK_OF_MONTH]
                         + (_fields[DAY_OF_WEEK] < getFirstDayOfWeek() ? 0 : -1)
                         + (weekday  < getFirstDayOfWeek() ? -1 : 0));
        break;
      }

      if (_isSet[DAY_OF_WEEK] && _isSet[DAY_OF_WEEK_IN_MONTH]) {
        int weekday = getWeekDay(year, ++dayOfYear);
        daysInWeeks = _fields[DAY_OF_WEEK] - weekday;
        daysInWeeks += 7 * (_fields[DAY_OF_WEEK_IN_MONTH] 
                         + (_fields[DAY_OF_WEEK] < weekday ? 0 : -1));
        break;
      }
    }  // if (_isSet[MONTH])

    if (_isSet[DAY_OF_YEAR]) {
      dayOfYear = _fields[DAY_OF_YEAR];
      break;
    }

    if (_isSet[DAY_OF_WEEK] && _isSet[WEEK_OF_YEAR]) {
      dayOfYear = getMinimalDaysInFirstWeek();
      int weekday = getWeekDay(year, dayOfYear);
      daysInWeeks = _fields[DAY_OF_WEEK] - weekday;
      daysInWeeks += 7 * (_fields[WEEK_OF_YEAR]
                     + (_fields[DAY_OF_WEEK] < getFirstDayOfWeek() ? 0 : -1)
                     + (weekday  < getFirstDayOfWeek() ? -1 : 0));
      break;
    }

    // the default value for dayOfYear is ...
    dayOfYear = 1;
  } while (false);
  

  int hour = _isSet[HOUR_OF_DAY] ? _fields[HOUR_OF_DAY]
                                 : _isSet[HOUR] && _isSet[AM_PM] 
                                    ? _fields[AM_PM] * 12 + (_fields[HOUR] % 12)
                                  : 0;
  int minute = _isSet[MINUTE] ? _fields[MINUTE] : 0;
  int second = _isSet[SECOND] ? _fields[SECOND] : 0;
  int millis = _isSet[MILLISECOND] ? _fields[MILLISECOND] : 0;
  int millisInDay;

  if (isLenient()) {
    jlong allMillis = (((hour * jlong(60)) + minute) * jlong(60) + second) * jlong(1000) + millis;
    dayOfYear += allMillis / (24 * 60 * 60 * jlong(1000));
    millisInDay = (int) (allMillis % (24 * 60 * 60 * jlong(1000)));
  } else {
    if (hour < 0 || hour >= 24 || minute < 0 || minute > 59
                 || second < 0 || second > 59 || millis < 0 || millis >= 1000)
      THROW0(IllegalArgumentException);
    millisInDay = (((hour * jlong(60)) + minute) * jlong(60) + second) * jlong(1000)  + millis;
  }
  _time = getLinearTime (year, dayOfYear, millisInDay);
  _time += daysInWeeks * (24 * 60 * 60 * jlong(1000));

  RTimeZone zone = getTimeZone();
  int rawOffset = _isSet[ZONE_OFFSET] ? _fields[ZONE_OFFSET] : getTimeZone()->getRawOffset();
  int month = (dayOfYear * 5 + 3) / (31 + 30 + 31 + 30 + 31);
  int day = (6 + (dayOfYear * 5 + 3) % (31 + 30 + 31 + 30 + 31)) / 5;
  int weekday = ((int) (_time / (24 * 60 * 60 * jlong(1000))) + THURSDAY) % 7;

  if (weekday <= 0)
    weekday += 7;
  int dstOffset = _isSet[DST_OFFSET]
                  ? _fields[DST_OFFSET] 
                  : (zone->getOffset((year < 0) ? BC : AD, 
                                    (year < 0) ? 1-year : year, 
                                     month, day, weekday, millisInDay)
                     - zone->getRawOffset());
  _time -= rawOffset + dstOffset;
  _isTimeSet = true;
}
  
//protected
void 
GregorianCalendar::computeFields()
{
  bool gregorian = (_time >= _gregorianCutover);
  RTimeZone zone = getTimeZone();
  _fields[ZONE_OFFSET] = zone->getRawOffset();
  jlong localTime = _time + _fields[ZONE_OFFSET];

  int day = (int) (localTime / (24 * 60 * 60 * 1000));
  int millisInDay = (int) (localTime % (24 * 60 * 60 * 1000));
  if (millisInDay < 0) {
    millisInDay += (24 * 60 * 60 * 1000);
    day--;
  }

  calculateDay(day, gregorian);
  _fields[DST_OFFSET] =  zone->getOffset(_fields[ERA], _fields[YEAR], _fields[MONTH], 
                                         _fields[DAY_OF_MONTH], _fields[DAY_OF_WEEK], 
                                         millisInDay) - _fields[ZONE_OFFSET];

  millisInDay += _fields[DST_OFFSET];
  if (millisInDay >= 24 * 60 * 60 * 1000) {
    millisInDay -= 24 * 60 * 60 * 1000;
    calculateDay(++day, gregorian);
  }

  _fields[DAY_OF_WEEK_IN_MONTH] = (_fields[DAY_OF_MONTH] + 6) / 7;

  int relativeWeekday = (7 + _fields[DAY_OF_WEEK] - getFirstDayOfWeek()) % 7;
  _fields[WEEK_OF_MONTH] = (_fields[DAY_OF_MONTH] - relativeWeekday + 6) / 7;
  int weekOfYear = (_fields[DAY_OF_YEAR] - relativeWeekday + 6) / 7;
  int minDays = getMinimalDaysInFirstWeek();
  int firstWeekday = (7 + getWeekDay(_fields[YEAR], minDays) - getFirstDayOfWeek()) % 7;
  if (minDays - firstWeekday < 1)
    weekOfYear++;
  _fields[WEEK_OF_YEAR] = weekOfYear;
        
  int hourOfDay = millisInDay / (60 * 60 * 1000);
  _fields[AM_PM] = (hourOfDay < 12) ? AM : PM;
  int hour = hourOfDay % 12;
  _fields[HOUR] = (hour == 0) ? 12 : hour;
  _fields[HOUR_OF_DAY] = hourOfDay;
  millisInDay %= (60 * 60 * 1000);
  _fields[MINUTE] = millisInDay / (60 * 1000);
  millisInDay %= (60 * 1000);
  _fields[SECOND] = millisInDay / (1000);
  _fields[MILLISECOND] = millisInDay % 1000;
     
  _areFieldsSet = _isSet[ERA] = _isSet[YEAR] = _isSet[MONTH] =
                  _isSet[WEEK_OF_YEAR] = _isSet[WEEK_OF_MONTH] = 
                  _isSet[DAY_OF_MONTH] = _isSet[DAY_OF_YEAR] = _isSet[DAY_OF_WEEK] = 
                  _isSet[DAY_OF_WEEK_IN_MONTH] = _isSet[AM_PM] = _isSet[HOUR] = 
                  _isSet[HOUR_OF_DAY] = _isSet[MINUTE] = _isSet[SECOND] = 
                  _isSet[MILLISECOND] = _isSet[ZONE_OFFSET] = _isSet[DST_OFFSET] = 
                  true;
}


} // util
} // acdk
