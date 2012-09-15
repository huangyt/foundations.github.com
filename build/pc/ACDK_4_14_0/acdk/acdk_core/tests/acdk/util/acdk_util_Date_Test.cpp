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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/acdk_util_Date_Test.cpp,v 1.12 2005/02/05 10:45:09 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/util/Date.h>
#include <acdk/util/SysDate.h>
#include <acdk/util/GregorianCalendar.h>
#include <acdk/util/TimeZone.h>
#include <acdk/util/SimpleTimeZone.h>


#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace util {

using namespace ::acdk::lang;
using namespace ::acdk::util;


  
BEGIN_DECLARE_TEST( Date_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( DateCalendar)
END_DECLARE_TEST( Date_Test  )

BEGIN_DEFINE_TEST( Date_Test )
  ADD_TEST( Date_Test, standard ) 
  ADD_TEST( Date_Test, DateCalendar ) 
END_DEFINE_TEST( Date_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);



struct CalendarTest
{
  int year;
  int month;
  int mday;
  int hour;
  int minute;
  int second;
  int msec;
  int week_of_year;
  int week_of_month;
  int day_of_year;
  int day_of_week;
  CalendarTest()
  : year(0)
  , month(0)
  , mday(0)
  , hour(0)
  , minute(0)
  , second(0)
  , msec(0)
  , week_of_year(-1)
  , week_of_month(-1)
  , day_of_year(-1)
  , day_of_week(-1)
  {
  }
  CalendarTest(int y, int m, int md, int h = 0, int min = 0, int sec = 0, int ms = 0
              , int woy = -1, int wom = -1, int doy = -1, int dow = -1)
  : year(y)
  , month(m)
  , mday(md)
  , hour(h)
  , minute(m)
  , second(sec)
  , msec(ms)
  , week_of_year(woy)
  , week_of_month(wom)
  , day_of_year(doy)
  , day_of_week(dow)
  {
  }
};

RString getField(Calendar& cal, int field)
{
  if (cal.isSet(field) == true)
    return RString("") + cal.get(field);
  else
    return "<NOT SET>";
}

void dumpCalendar(PrintWriter& out, Calendar& calendar)
{
  out.println(RString("\n\nTime:              ") + calendar.getTime()->getTime());
  out.println(RString("ZONE-ID:              ") + calendar.getTimeZone()->getID());
  out.println(RString("ERA:                  ") + getField(calendar, Calendar::ERA));
  out.println(RString("YEAR:                 ") + calendar.get(Calendar::YEAR));
  out.println(RString("MONTH:                ") + getField(calendar, Calendar::MONTH));
  out.println(RString("WEEK_OF_YEAR:         ") + getField(calendar, Calendar::WEEK_OF_YEAR));
  out.println(RString("WEEK_OF_MONTH:        ") + getField(calendar, Calendar::WEEK_OF_MONTH));
  out.println(RString("DATE:                 ") + getField(calendar, Calendar::DATE));
  out.println(RString("DAY_OF_MONTH:         ") + getField(calendar, Calendar::DAY_OF_MONTH));
  out.println(RString("DAY_OF_YEAR:          ") + getField(calendar, Calendar::DAY_OF_YEAR));
  out.println(RString("DAY_OF_WEEK:          ") + getField(calendar, Calendar::DAY_OF_WEEK));
  out.println(RString("DAY_OF_WEEK_IN_MONTH: ")
                    + getField(calendar, Calendar::DAY_OF_WEEK_IN_MONTH));
  out.println(RString("AM_PM:                ") + getField(calendar, Calendar::AM_PM));
  out.println(RString("HOUR:                 ") + getField(calendar, Calendar::HOUR));
  out.println(RString("HOUR_OF_DAY:          ") + getField(calendar, Calendar::HOUR_OF_DAY));
  out.println(RString("MINUTE:               ") + getField(calendar, Calendar::MINUTE));
  out.println(RString("SECOND:               ") + getField(calendar, Calendar::SECOND));
  out.println(RString("MILLISECOND:          ") + getField(calendar, Calendar::MILLISECOND));
  out.println(RString("ZONE_OFFSET:          ")
                    + (calendar.get(Calendar::ZONE_OFFSET)/(60*60*1000)));
  out.println(RString("DST_OFFSET:           ")
                    + (calendar.get(Calendar::DST_OFFSET)/(60*60*1000)));
}


#define TEST_CALENDAR_FIELD(soll, field) \
if (soll != -1) { \
  int ist = gc.get(field); \
  if (ist != soll) { \
    System::out->println(RString("Field not ok: ") + #field + \
              ". SOLL=[" + soll + "]; IST=[" + ist + "]"); \
  } \
}

void testCalendar(const CalendarTest& ct)
{
  Date d(ct.year - 1900, ct.month - 1, ct.mday, ct.hour, ct.minute, ct.second);
  GregorianCalendar gc;
  //GregorianCalendar gc(ct.year - 1900, ct.month, ct.mday, ct.hour, ct.minute, ct.second);
  GregorianCalendar gc2(ct.year - 1900, ct.month - 1, ct.mday, ct.hour, ct.minute, ct.second);
  gc.setTime(&d);
  System::out->println("Date.toString(): " + d.toString());
  jlong t1 = d.getTime();
  jlong t2 = gc.getTime()->getTime();
  jlong t2a = gc.getTimeInMillis();
  jlong t3 = gc2.getTime()->getTime();
  jlong t3a = gc2.getTimeInMillis();

  dumpCalendar(*System::out, gc);
  //dumpCalendar(&gc2);
  testAssert(gc.get(Calendar::YEAR) == ct.year);
  testAssert(gc.get(Calendar::MONTH) + 1 == ct.month);
  testAssert(gc.get(Calendar::DAY_OF_MONTH) == ct.mday);
  
  TEST_CALENDAR_FIELD(ct.week_of_year, Calendar::WEEK_OF_YEAR)
  TEST_CALENDAR_FIELD(ct.week_of_month, Calendar::WEEK_OF_MONTH)
  TEST_CALENDAR_FIELD(ct.day_of_year, Calendar::DAY_OF_YEAR)
  TEST_CALENDAR_FIELD(ct.day_of_week, Calendar::DAY_OF_WEEK)
  TEST_CALENDAR_FIELD(ct.hour, Calendar::HOUR_OF_DAY)
  TEST_CALENDAR_FIELD(ct.minute, Calendar::MINUTE)
  TEST_CALENDAR_FIELD(ct.second, Calendar::SECOND)
  TEST_CALENDAR_FIELD(ct.msec, Calendar::MILLISECOND)

}

//static
void
Date_Test::standard()
{
  // Outlook says it is tuesday
  //              year  m  md  h  min sec ms kw wm  yd   wd
  CalendarTest ct(1966, 9, 20, 5, 30, 12, 0, 39, 3, 293, 3);
  testCalendar(ct);
  testCalendar(CalendarTest(1970, 1, 1));
  testCalendar(CalendarTest(2001, 7, 14, 17, 42, 59));
  testCalendar(CalendarTest(1970, 1, 1, 0, 0, 0));
  testCalendar(CalendarTest(1870, 1, 12));
  testCalendar(CalendarTest(1345, 4, 1));
  
  

  /*
  try {
    System::out->println("DateTest");
    RStringArray ids = TimeZone::getAvailableIDs(-11 * 60 * 60 * 1000);
    RTimeZone pdt = TimeZone::getTimeZone(ids[0]);
    RCalendar calendar = new GregorianCalendar(pdt);
    RDate trialTime = new Date();
    System::out->println(RString("Date: ") + SysDate().getTimeStamp());

    calendar->setTime(trialTime);

    //System::out->println(typeid(trialTime).name);   

    // print out a bunch of interesting things
    System::out->println(RString("ZONE-ID:              ") + calendar->getTimeZone()->getID());
    System::out->println(RString("ERA:                  ") + calendar->get(Calendar::ERA));
    System::out->println(RString("YEAR:                 ") + calendar->get(Calendar::YEAR));
    System::out->println(RString("MONTH:                ") + calendar->get(Calendar::MONTH));
    System::out->println(RString("WEEK_OF_YEAR:         ") + calendar->get(Calendar::WEEK_OF_YEAR));
    System::out->println(RString("WEEK_OF_MONTH:        ") + calendar->get(Calendar::WEEK_OF_MONTH));
    System::out->println(RString("DATE:                 ") + calendar->get(Calendar::DATE));
    System::out->println(RString("DAY_OF_MONTH:         ") + calendar->get(Calendar::DAY_OF_MONTH));
    System::out->println(RString("DAY_OF_YEAR:          ") + calendar->get(Calendar::DAY_OF_YEAR));
    System::out->println(RString("DAY_OF_WEEK:          ") + calendar->get(Calendar::DAY_OF_WEEK));
    System::out->println(RString("DAY_OF_WEEK_IN_MONTH: ")
                    + calendar->get(Calendar::DAY_OF_WEEK_IN_MONTH));
    System::out->println(RString("AM_PM:                ") + calendar->get(Calendar::AM_PM));
    System::out->println(RString("HOUR:                 ") + calendar->get(Calendar::HOUR));
    System::out->println(RString("HOUR_OF_DAY:          ") + calendar->get(Calendar::HOUR_OF_DAY));
    System::out->println(RString("MINUTE:               ") + calendar->get(Calendar::MINUTE));
    System::out->println(RString("SECOND:               ") + calendar->get(Calendar::SECOND));
    System::out->println(RString("MILLISECOND:          ") + calendar->get(Calendar::MILLISECOND));
    System::out->println(RString("ZONE_OFFSET:          ")
                    + (calendar->get(Calendar::ZONE_OFFSET)/(60*60*1000)));
    System::out->println(RString("DST_OFFSET:           ")
                    + (calendar->get(Calendar::DST_OFFSET)/(60*60*1000)));
                  
  } catch (RException ex) {
    ex->printStackTrace();
    System::out->println(ex->getMessage());
    testAssert(false);
  }
    */
}


void
Date_Test::DateCalendar()
{
  Date date(1966 - 1900, 9 - 1, 20);
  GregorianCalendar gc;
  gc.setTime(&date);
  testAssert(gc.get(Calendar::YEAR) == 1966);
  testAssert(gc.get(Calendar::MONTH) == 8);
  testAssert(gc.get(Calendar::DAY_OF_MONTH) == 20);

}

} // namespace util
} //namespace acdk 
} //namespace tests

