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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Calendar.h,v 1.11 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_Calendar_h
#define acdk_util_Calendar_h

//#include <acdk.h>
#include "TimeZone.h"
#include "Hashtable.h"
#include "Date.h"

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;


ACDK_DECL_CLASS(Hashtable);

ACDK_DECL_CLASS(Calendar);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:56 $
  @bug only partly implemented
*/

class ACDK_CORE_PUBLIC Calendar
: extends acdk::lang::Object,
  implements acdk::io::Serializable,
  implements acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(Calendar)
public:
  static int ERA ;
  static int YEAR;
  static int MONTH;
  static int WEEK_OF_YEAR;
  static int WEEK_OF_MONTH;
  static int DATE;
  static int DAY_OF_MONTH;
  static int DAY_OF_YEAR;
  static int DAY_OF_WEEK;
  static int DAY_OF_WEEK_IN_MONTH;
  static int AM_PM;
  static int HOUR;
  static int HOUR_OF_DAY;
  static int MINUTE;
  static int SECOND;
  static int MILLISECOND;
  static int ZONE_OFFSET;
  static int DST_OFFSET;
  static int FIELD_COUNT;
  static int SUNDAY;
  static int MONDAY;
  static int TUESDAY;
  static int WEDNESDAY;
  static int THURSDAY;
  static int FRIDAY;
  static int SATURDAY;
  static int JANUARY;
  static int FEBRUARY;
  static int MARCH;
  static int APRIL;
  static int MAY;
  static int JUNE;
  static int JULY;
  static int AUGUST;
  static int SEPTEMBER;
  static int OCTOBER;
  static int NOVEMBER;
  static int DECEMBER;
  static int UNDECIMBER;
  static int AM;
  static int PM;

private:
  //void writeObject(RObjectOutputWrite stream);
  //void readObject(RObjectInputWrite stream);
  
protected:
  RintArray _fields;
  RboolArray _isSet;
  jlong _time;
  bool _isTimeSet;
  bool _areFieldsSet;
  bool _areAllFieldsSet;
private:
  transient RintArray _stamp;
  bool _lenient;
  RTimeZone _zone;
  int _firstDayOfWeek;
  int _minimalDaysInFirstWeek;
  static RHashtable  _cachedLocaleData;
  // Special values of _stamp
  static int UNSET;
  static int INTERNALLY_SET;
  static int MINIMUM_USER_STAMP;
  int _nextStamp;

public:
  /**
   *
   */
  virtual ~Calendar();

  static RHashtable get_cachedLocaleData();
  
  static RCalendar getInstance(); 

  static RCalendar getInstance(IN(RTimeZone) zone);
  
  static RCalendar getInstance(IN(RLocale) locale);
  
  static RCalendar getInstance(IN(RTimeZone) zone, IN(RLocale) locale);
  
  static RLocaleArray getAvailableLocales();

  RDate getTime() 
  {
    return new Date(getTimeInMillis());
  }
  
  void setTime(IN(RDate) date) 
  {
    setTimeInMillis(date->getTime());
  }
  
  int get(int field) 
  {
    complete();
    return _fields[field];
  }
  
  void set(int field, int value);

  void set(int year, int month, int date);

  void set(int year, int month, int date, int hour, int minute);

  void set(int year, int month, int date, int hour, int minute, int second, int millisecond = 0);

  void clear();
  
  void clear(int field);
  
  bool isSet(int field);

  bool equals(IN(RObject) obj);

  int hashCode();

  bool before(IN(RObject) obj);

  bool after(IN(RObject) obj);
  
  virtual void add(int field, int amount) = 0;
  
  virtual void roll(int field, bool up) = 0;

  void roll(int field, int amount);

  void setTimeZone(RTimeZone zone) {
    _zone = zone;
  }

  RTimeZone getTimeZone() {
    return _zone;
  }

  void setLenient(bool lenient) {
    _lenient = lenient;
  }

  bool getLenient() {
    return _lenient;
  }

  void setFirstDayOfWeek(int day) {
    _firstDayOfWeek = day;
  }
  
  int getFirstDayOfWeek() {
    return _firstDayOfWeek;
  }
  
  void setMinimalDaysInFirstWeek(int value) {
    _minimalDaysInFirstWeek = value;
  }

  int getMinimalDaysInFirstWeek() {
    return _minimalDaysInFirstWeek;
  }

  virtual int getMinimum(int field) = 0;
  
  virtual int getMaximum(int max) = 0;

  virtual int getGreatestMinimum(int field) = 0;

  virtual int getLeastMaximum(int field) = 0;

  int getActualMinimum(int field);

  int getActualMaximum(int field);

  bool isLenient() {
    return _lenient;
  }

  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc);

  RString toString();

protected:
  /**
   *
   */
  Calendar();
  Calendar(IN(RTimeZone) zone, IN(RLocale) locale);
public:
  virtual void computeTime() = 0;
  virtual void computeFields() = 0;

  jlong getTimeInMillis() 
  {
    if (_isTimeSet == false) {
      computeTime();
      if (_lenient == true || _areAllFieldsSet == false)
        _areFieldsSet = false;
    }
    return _time;
  }

  void setTimeInMillis(jlong millis);

  int internalGet(int field) 
  {
    return _fields[field];
  }

  void internalSet(int field, int value) 
  {
    _fields[field] = value;
  }

  void complete();


  
};


} // util
} // acdk

#endif //acdk_util_Calendar_h

