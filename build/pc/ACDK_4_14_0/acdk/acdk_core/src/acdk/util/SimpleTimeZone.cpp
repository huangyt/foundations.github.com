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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SimpleTimeZone.cpp,v 1.12 2005/03/08 12:45:46 kommer Exp $


#include <acdk.h>
#include "SimpleTimeZone.h"
#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/Integer.h>
#include <acdk/util/GregorianCalendar.h>

namespace acdk {
namespace util {

const int SimpleTimeZone::DOM_MODE = 1;
const int SimpleTimeZone::DOW_IN_MONTH_MODE = 2;
const int SimpleTimeZone::DOW_GE_DOM_MODE = 3;
const int SimpleTimeZone::DOW_LE_DOM_MODE= 4;

SimpleTimeZone::SimpleTimeZone() 
  : TimeZone(),
    _dstSavings(60*60*1000)
{
}

SimpleTimeZone::SimpleTimeZone(int rOs, IN(RString) id)
  : TimeZone(id),
    _rawOffset(rOs),
    _useDayLight(false),
    _dstSavings(60*60*1000),
    _startYear(0)
{
}

SimpleTimeZone::SimpleTimeZone(int rOs, IN(RString) id, 
                               int startMonth, int startDayOfWeekInMonth, 
                               int startDayOfWeek, int startTime,
                               int endMonth, int endDayOfWeekInMonth,
                               int endDayOfWeek, int endTime)
  : TimeZone(id),
    _rawOffset(rOs),
    _useDayLight(false),
    _dstSavings(60*60*1000)
{
  setStartRule(startMonth, startDayOfWeekInMonth, 
               startDayOfWeek, startTime);
  setEndRule(endMonth, endDayOfWeekInMonth, 
             endDayOfWeek, endTime);

  if (endDayOfWeek == 0)
    _endMode = DOM_MODE;
  else if (endDayOfWeek > 0)
    _endMode = DOW_IN_MONTH_MODE;
  else if (endDayOfWeek < 0) {
    endDayOfWeek = -endDayOfWeek;
    if (_endDay < 0){
      _endDay = -_endDay;
      _endMode = DOW_LE_DOM_MODE;
    } else
      _endMode = DOW_GE_DOM_MODE;
  }
  
  _endMonth =  endMonth;
  _endDay = endDayOfWeekInMonth;
  _endDayOfWeek = endDayOfWeek;
  _endTime = endTime;
  _startYear = 0;

}

SimpleTimeZone::SimpleTimeZone(int rOs, IN(RString) id, 
                               int startMonth, int startDayOfWeekInMonth, 
                               int startDayOfWeek, int startTime,
                               int endMonth, int endDayOfWeekInMonth,
                               int endDayOfWeek, int endTime,
                               int dstSavings)
  : TimeZone(id),
    _rawOffset(rOs),
    _useDayLight(false),
    _dstSavings(dstSavings)
{
  setStartRule(startMonth, startDayOfWeekInMonth, 
               startDayOfWeek, startTime);
  setEndRule(endMonth, endDayOfWeekInMonth, 
             endDayOfWeek, endTime);

  if (endDayOfWeek == 0)
    _endMode = DOM_MODE;
  else if (endDayOfWeek > 0)
    _endMode = DOW_IN_MONTH_MODE;
  else if (endDayOfWeek < 0) {
    endDayOfWeek = -endDayOfWeek;
    if (_endDay < 0){
      _endDay = -_endDay;
      _endMode = DOW_LE_DOM_MODE;
    } else
      _endMode = DOW_GE_DOM_MODE;
  }
  
  _endMonth =  endMonth;
  _endDay = endDayOfWeekInMonth;
  _endDayOfWeek = endDayOfWeek;
  _endTime = endTime;
  _startYear = 0;
  
}

SimpleTimeZone::~SimpleTimeZone() 
{
}

void
SimpleTimeZone::setStartRule(int month, int day, int dayOfWeek, int time)
{
  if (dayOfWeek == 0)
    _startMode = DOM_MODE;
  else if (dayOfWeek > 0)
    _startMode = DOW_IN_MONTH_MODE;
  else if (dayOfWeek < 0) {
    dayOfWeek = -dayOfWeek;
    if (day < 0) {
      day = -day;
      _startMode = DOW_LE_DOM_MODE;
    } else 
      _startMode = DOW_GE_DOM_MODE;
  }
  
  _startMonth = month;
  _startDay = day;
  _startDayOfWeek = dayOfWeek;
  _startTime = time;
  _useDayLight = true;
}

void
SimpleTimeZone::setEndRule(int month, int day, int dayOfWeek, int time)
{
  if (dayOfWeek == 0)
    _endMode = DOM_MODE;
  else if (dayOfWeek > 0)
    _endMode = DOW_IN_MONTH_MODE;
  else if (dayOfWeek < 0) {
    dayOfWeek = -dayOfWeek;
    if (day < 0) {
      day = -day;
      _endMode = DOW_LE_DOM_MODE;
    } else 
      _endMode = DOW_GE_DOM_MODE;
  }
  
  _endMonth = month;
  _endDay = day;
  _endDayOfWeek = dayOfWeek;
  _endTime = time;
  _useDayLight = true;
  
}

int 
SimpleTimeZone::getOffset(int era, int year, int month, 
                          int day, int dayOfWeek, int millis)
{
  
  bool daylightSavings;
  if (!_useDayLight || era < GregorianCalendar::AD || era < _startYear)
    daylightSavings = false;
  else if (_startMonth < _endMonth) {
    daylightSavings = !isBefore(year, month, day, dayOfWeek, millis,
                                _startMode, _startMonth, _startDay, _startDayOfWeek, _startTime)
                    && isBefore(year, month, day, dayOfWeek, millis,
                                _endMode, _endMonth, _endDay, _endDayOfWeek, _endTime);
  } else {
    daylightSavings = !isBefore(year, month, day, dayOfWeek, millis,
                                _startMode, _startMonth, _startDay, _startDayOfWeek, _startTime)
                    || isBefore(year, month, day, dayOfWeek, millis,
                                _endMode, _endMonth, _endDay, _endDayOfWeek, _endTime);
  }
  return _rawOffset + (daylightSavings ? _dstSavings : 0);
}

//private
int 
SimpleTimeZone::getDaysInMonth(int month, int year)
{
  if (month == Calendar::FEBRUARY){
    if (year&3 == 0 && (year % 100 != 0 || year % 400 == 0))
      return 29;
    else 
      return 28;
  } else if (month < Calendar::AUGUST)
    return 31 - (month & 1);
  else
    return 30 + (month & 1);
}

//private
bool
SimpleTimeZone::isBefore(int calYear, int calMonth, int calDay, int calDayOfWeek, 
                         int calMillis, int mode, int month, int day,
                         int dayOfWeek, int millis)
{
  
  if (calMonth != month)
    return calMonth < month;

  int week = 0;
  if (mode == DOM_MODE) {
      if (calDay != day)
        return calDay < day;
  } else if (mode ==  DOW_IN_MONTH_MODE ) {
      calDay += (dayOfWeek - calDayOfWeek);
      if (day < 0)
        calDay -= getDaysInMonth(calMonth, calYear)+7;
      else
        calDay += 6;
      week = calDay / 7;
      if (week != day)
        return week < day;
      if (calDayOfWeek != dayOfWeek)
        return calDayOfWeek < dayOfWeek;
  } else if (mode == DOW_LE_DOM_MODE) {
      day -= 6;
  } else if (mode == DOW_GE_DOM_MODE) {
      calDay -= (calDayOfWeek < dayOfWeek ? 7 : 0)  + calDayOfWeek - dayOfWeek;
      if (calDay < day)
        return true;
      if (calDayOfWeek != dayOfWeek || calDay >= day+7)
        return false;
  }
  return (calMillis < millis);
}


RString
SimpleTimeZone::getDisplayName(bool dst, int style, IN(RLocale) locale)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

int 
SimpleTimeZone::hashCode()
{
  SYNCTHIS();
  int result = 0;
  result = 31 * result + Integer(_rawOffset).hashCode();
  result = 31 * result + Integer(_useDayLight).hashCode();
  result = 31 * result + Integer(_dstSavings).hashCode();
  result = 31 * result + Integer(_startYear).hashCode();
  result = 31 * result + Integer(_startMode).hashCode();
  result = 31 * result + Integer(_startMonth).hashCode();
  result = 31 * result + Integer(_startDay).hashCode();
  result = 31 * result + Integer(_startDayOfWeek).hashCode();
  result = 31 * result + Integer(_startTime).hashCode();
  result = 31 * result + Integer(_endMonth).hashCode();
  result = 31 * result + Integer(_endDay).hashCode();
  result = 31 * result + Integer(_endDayOfWeek).hashCode();
  result = 31 * result + Integer(_endTime).hashCode();
  return result;
}

bool
SimpleTimeZone::equals(IN(RObject) obj)
{
  SYNCTHIS();
  THROW0(UnsupportedOperationException);
  return false;
}

bool 
SimpleTimeZone::inDaylightTime(IN(RDate) date) 
{
  RCalendar cal = Calendar::getInstance((RTimeZone)this);
  cal->setTime(date);
  return cal->get(Calendar::DST_OFFSET) != 0 ;
}

// ##
/*
void 
SimpleTimeZone::writeObject(RObjectOutputWrite output)
{
  THROW0(UnsupportedOperationException);
}

void 
SimpleTimeZone::readObject(RObjectInputWrite input)
{
  THROW0(UnsupportedOperationException);
}
*/

} // Util
} // acdk
