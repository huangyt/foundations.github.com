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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Calendar.cpp,v 1.17 2005/03/12 10:54:08 kommer Exp $


#include <acdk.h>
#include "Calendar.h"
#include "GregorianCalendar.h"

#include <acdk/lang/System.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Integer.h>

#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

int Calendar::ERA = 0;
int Calendar::YEAR = 1;
int Calendar::MONTH = 2;
int Calendar::WEEK_OF_YEAR = 3;
int Calendar::WEEK_OF_MONTH = 4;
int Calendar::DATE = 5;
int Calendar::DAY_OF_MONTH = 5;
int Calendar::DAY_OF_YEAR = 6;
int Calendar::DAY_OF_WEEK = 7;
int Calendar::DAY_OF_WEEK_IN_MONTH = 8;
int Calendar::AM_PM = 9;
int Calendar::HOUR = 10;
int Calendar::HOUR_OF_DAY = 11;
int Calendar::MINUTE = 12 ;
int Calendar::SECOND = 13;
int Calendar::MILLISECOND = 14;
int Calendar::ZONE_OFFSET = 15;
int Calendar::DST_OFFSET = 16;
int Calendar::FIELD_COUNT = 17;

int Calendar::SUNDAY = 1;
int Calendar::MONDAY = 2;
int Calendar::TUESDAY = 3;
int Calendar::WEDNESDAY = 4;
int Calendar::THURSDAY = 5;
int Calendar::FRIDAY = 6;
int Calendar::SATURDAY = 7;
int Calendar::JANUARY = 0;
int Calendar::FEBRUARY = 1;
int Calendar::MARCH = 2;
int Calendar::APRIL = 3;
int Calendar::MAY = 4;
int Calendar::JUNE = 5;
int Calendar::JULY = 6;
int Calendar::AUGUST = 7;
int Calendar::SEPTEMBER = 8;
int Calendar::OCTOBER = 9;
int Calendar::NOVEMBER = 10;
int Calendar::DECEMBER = 11;
int Calendar::UNDECIMBER = 12;
int Calendar::AM = 0;
int Calendar::PM = 1;
int Calendar::UNSET = 0;
int Calendar::INTERNALLY_SET = 1;
int Calendar::MINIMUM_USER_STAMP = 2;

RHashtable Calendar::_cachedLocaleData = Nil;

Calendar::Calendar() 
  : _fields(new (allocator()) intArray(FIELD_COUNT)),
    _isSet(new (allocator()) boolArray(FIELD_COUNT)),
    _time(0),
    _isTimeSet(false),
    _areFieldsSet(false),
    _areAllFieldsSet(false),
    _stamp(new (allocator()) intArray(FIELD_COUNT)),
    _lenient(false),
    _zone(TimeZone::getDefault()),
    _firstDayOfWeek(SUNDAY),
    _minimalDaysInFirstWeek(1),
    _nextStamp(UNSET)

{
  for (int i = 0; i < FIELD_COUNT; ++i)
  {
    _isSet[i] = false;
    _fields[i] = UNSET;
    _stamp[i] = UNSET;
  }
}

Calendar::Calendar(IN(RTimeZone) zone, IN(RLocale) locale) 
  : _fields(new (allocator()) intArray(FIELD_COUNT)),
    _isSet(new (allocator())  boolArray(FIELD_COUNT)),
    _time(0),
    _isTimeSet(false),
    _areFieldsSet(false),
    _areAllFieldsSet(false),
    _stamp(new (allocator()) intArray(FIELD_COUNT)),
    _lenient(false),
    _zone(zone),
    _firstDayOfWeek(SUNDAY),   //### vllt von locale abhaengig
    _minimalDaysInFirstWeek(1),
    _nextStamp(UNSET)
{
  for (int i = 0; i < FIELD_COUNT; ++i)
  {
    _isSet[i] = false;
    _fields[i] = UNSET;
    _stamp[i] = UNSET;
  }
}

Calendar::~Calendar() 
{
}

//static
RHashtable
Calendar::get_cachedLocaleData()
{
  if (_cachedLocaleData == Nil) {
    _cachedLocaleData = new Hashtable(3);
    System::registerStaticReference(_cachedLocaleData);
  }
  return _cachedLocaleData ;
}

//static 
RCalendar 
Calendar::getInstance() 
{ 
  SYNCCLASS();
  return new GregorianCalendar();
}

//static 
RCalendar 
Calendar::getInstance(IN(RTimeZone) zone) 
{
  SYNCCLASS();
  return new GregorianCalendar(zone, Locale::getDefault());
}
  
//static 
RCalendar 
Calendar::getInstance(IN(RLocale) locale) 
{
  SYNCCLASS();
  return new GregorianCalendar(TimeZone::getDefault(), locale);
}
  
//static 
RCalendar 
Calendar::getInstance(IN(RTimeZone) zone, IN(RLocale) locale) 
{
  SYNCCLASS();
  return new GregorianCalendar(zone, locale);
}

//static 
RLocaleArray
Calendar::getAvailableLocales()
{
  SYNCCLASS();
  RLocaleArray reVal = new LocaleArray(2);
  reVal[0] = Locale::getENGLISH();
  reVal[1] = Locale::getGERMAN();
  return reVal;
  //return DateFormat::getAvailableLocales();
}


void
Calendar::set(int field, int value)
{
  /*if (_areFieldsSet == false)
    computeFields();
    */
  _isTimeSet = false;
  _fields[field] = value;
  _isSet[field] = true;
  _stamp[field] = _nextStamp++;
}

void
Calendar::set(int year, int month, int date)
{
  set(YEAR, year + 1900);
  set(MONTH, month );
  set(DATE, date);
}

void 
Calendar::set(int year, int month, int date, int hour, int minute)
{
  set(year, month, date);
  _fields[HOUR_OF_DAY] = hour;
  _fields[MINUTE] = minute;
  _isSet[HOUR_OF_DAY] = true;
  _isSet[MINUTE] = true;
}

void 
Calendar::set(int year, int month, int date, int hour, int minute, int second, int millisecond/*  = 0*/ )
{
  set(year, month, date, hour, minute);
  _fields[SECOND] = second;
  _isSet[SECOND] = true;
  _fields[MILLISECOND] = millisecond;
  _isSet[MILLISECOND] = true;
}

void 
Calendar::clear()
{
  _fields = new (allocator()) intArray(FIELD_COUNT);
  _stamp = new (allocator()) intArray(FIELD_COUNT);
  _areFieldsSet = false;
  _areAllFieldsSet = false;
  _isSet = new (allocator()) boolArray(FIELD_COUNT);
  _isTimeSet = false;
}
  
void 
Calendar::clear(int field)
{
  _fields[field] = 0;
  _stamp[field] = UNSET;
  _areFieldsSet = false;
  _areAllFieldsSet = false;
  _isSet[field] = false;
  _isTimeSet = false;
}
  
bool 
Calendar::isSet(int field)
{
  return _stamp[field] != UNSET;
}

bool 
Calendar::equals(IN(RObject) obj)
{
  if (RObject(this) == obj) 
    return true;
  if (instanceof(obj, RCalendar) == false)
    return false;
  
  RCalendar that = RCalendar(obj);
  if (getTimeInMillis() != that->getTimeInMillis())
    return false;
  if (getLenient() != that->getLenient())
    return false;
  if (getFirstDayOfWeek() != that->getFirstDayOfWeek())
    return false;
  if (getMinimalDaysInFirstWeek() != that->getMinimalDaysInFirstWeek())
    return false;
  if (getTimeZone()->equals(&that->getTimeZone()) == false)
    return false;
  return true;
}

int 
Calendar::hashCode()
{
  SYNCTHIS();
  int result = 0;
  result = 31 * result + _fields->hashCode();
  result = 31 * result + _isSet->hashCode();
  result = 31 * result + Long(_time).hashCode();
  result = 31 * result + Boolean(_isTimeSet).hashCode();
  result = 31 * result + Boolean(_areFieldsSet).hashCode();
  result = 31 * result + Boolean(_areAllFieldsSet).hashCode();
  result = 31 * result + _stamp->hashCode();
  result = 31 * result + Boolean(_lenient).hashCode();
  result = 31 * result + (_zone == Nil ? 0 : _zone->hashCode());
  result = 31 * result + Integer(_firstDayOfWeek).hashCode();
  result = 31 * result + Integer(_minimalDaysInFirstWeek).hashCode();
  result = 31 * result + Integer(_nextStamp).hashCode();
  return result;
}

bool 
Calendar::before(IN(RObject) obj)
{
  if (instanceof(obj,RCalendar) == false)
    return false;
  return getTimeInMillis() < (RCalendar(obj))->getTimeInMillis();
}

bool 
Calendar::after(IN(RObject) obj)
{
  if (instanceof(obj,RCalendar) == false)
    return false;
  return getTimeInMillis() > (RCalendar(obj))->getTimeInMillis();
  
}

void 
Calendar::roll(int field, int amount)
{
  while (amount > 0) {
    roll(field, true);
    amount--;
  }
  while (amount < 0) {
    roll(field, false);
    amount++;
  }
}

int 
Calendar::getActualMinimum(int field)
{
  int greatesMin = getGreatestMinimum(field);
  int min = getMinimum(field);
  
  if (greatesMin == min)
    return greatesMin;

  RCalendar other = RCalendar(this->clone());
  
  int reVal = greatesMin;

  do {
    other->set(field, greatesMin);
    if (other->get(field) != greatesMin)
      break;
    reVal = greatesMin;
    greatesMin--;
  } while (greatesMin >= greatesMin);
  return reVal;
}


int 
Calendar::getActualMaximum(int field)
{

  int leastMax = getLeastMaximum(field);
  int max = getMaximum(field);
  if (leastMax == max)
    return max;
  RCalendar other = RCalendar(this->clone());
  other->set(field, leastMax);
  for ( ; leastMax < max; leastMax++)
  {
	  other->add(field, 1);
	  if (other->get(field) != leastMax + 1)
	    break;
  }
  return leastMax;
}

RObject 
Calendar::clone(sys::Allocator* alc) 
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

RString 
Calendar::toString()
{
  RStringBuffer sb = new StringBuffer("");
  sb->append(getClass()->getName());
  sb->append("[time=");
  if (_isTimeSet == true)
    sb->append(String::valueOf(_time));
  else 
    sb->append("?");

  sb->append(", ERA = ");
  RString tstr = isSet(ERA) ? String::valueOf(_fields[ERA]) : RString("?");
  sb->append(tstr);
  sb->append(", YEAR = ");
  tstr = isSet(YEAR) ? String::valueOf(_fields[YEAR]) : RString("?");
  sb->append(tstr);
  sb->append(", MONTH = ");
  tstr = isSet(MONTH) ? String::valueOf(_fields[MONTH]) : RString("?");
  sb->append(tstr);
  sb->append(", WEEK_OF_YEAR = ");
  tstr = isSet(WEEK_OF_YEAR) ? String::valueOf(_fields[WEEK_OF_YEAR]) : RString("?");
  sb->append(tstr);
  sb->append(", WEEK_OF_MONTH = ");
  tstr = isSet(WEEK_OF_MONTH) ? String::valueOf(_fields[WEEK_OF_MONTH]) : RString("?");
  sb->append(tstr);
  sb->append(", DAY_OF_YEAR = ");
  tstr = isSet(DAY_OF_YEAR) ? String::valueOf(_fields[DAY_OF_YEAR]) : RString("?");
  sb->append(tstr);
  sb->append(", DAY_OF_MONTH = ");
  tstr = isSet(DAY_OF_MONTH) ? String::valueOf(_fields[DAY_OF_MONTH]) : RString("?");
  sb->append(tstr);
  sb->append(", DAY_OF_WEEK = ");
  tstr = isSet(DAY_OF_WEEK) ? String::valueOf(_fields[DAY_OF_WEEK]) : RString("?");
  sb->append(tstr);
  sb->append(", DAY_OF_WEEK_IN_MONTH = ");
  tstr = isSet(DAY_OF_WEEK_IN_MONTH) ? String::valueOf(_fields[DAY_OF_WEEK_IN_MONTH]) : RString("?");
  sb->append(tstr);
  sb->append(", AM_PM = ");
  tstr = isSet(AM_PM) ? String::valueOf(_fields[AM_PM]) : RString("?");
  sb->append(tstr);
  sb->append(", HOUR = ");
  tstr = isSet(HOUR) ? String::valueOf(_fields[HOUR]) : RString("?");
  sb->append(tstr);
  sb->append(", HOUR_OF_DAY = ");
  tstr = isSet(HOUR_OF_DAY) ? String::valueOf(_fields[HOUR_OF_DAY]) : RString("?");
  sb->append(tstr);
  sb->append(", MINUTE = ");
  tstr = isSet(MINUTE) ? String::valueOf(_fields[MINUTE]) : RString("?");
  sb->append(tstr);
  sb->append(", SECOND = ");
  tstr = isSet(SECOND) ? String::valueOf(_fields[SECOND]) : RString("?");
  sb->append(tstr);
  sb->append(", MILLISECOND = ");
  tstr = isSet(MILLISECOND) ? String::valueOf(_fields[MILLISECOND]) : RString("?");
  sb->append(tstr);
  sb->append(", ZONE_OFFSET = ");
  tstr =  isSet(ZONE_OFFSET) ? String::valueOf(_fields[ZONE_OFFSET]) : RString("?");
  sb->append(tstr);
  sb->append(", DST_OFFSET = ");
  tstr = isSet(DST_OFFSET) ? String::valueOf(_fields[DST_OFFSET]) : RString("?");
  sb->append(tstr);

  sb->append("]");
  return sb->toString();
}

//protected
void
Calendar::complete()
{
  if (_isTimeSet == false) 
    computeTime();
  if (_areFieldsSet == false) {
    computeFields();
    _areFieldsSet = true;
    _areAllFieldsSet = true;
  }

}

//protected
void
Calendar::setTimeInMillis(jlong millis)
{
  _isTimeSet = true;
  _time = millis;
  _areFieldsSet = false;
}


} // Util
} // acdk
