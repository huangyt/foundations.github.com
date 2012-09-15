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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SimpleCalendar.cpp,v 1.7 2005/02/05 10:45:06 kommer Exp $



#include "SimpleCalendar.h"

namespace acdk {
namespace util {



namespace {

const int MinYear = -4799;
const int DaysPerYear        = 365;
const int DaysPerLeapYear    = 366;


static const int MonthDaysTable[ 2 ][ 12 ] =
  {
    { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 },
    { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 }
  };

const long day_of_year_offset = 31738;

enum Month 
{
  January = 1,
  February = 2,
  March = 3,
  April = 4,
  May = 5,
  June = 6,
  July = 7,
  August = 8,
  September = 9,
  October = 10,
  November = 11,
  December = 12
};

} // anon namespace


bool 
SimpleCalendar::isLeapYear(int year)
{
  if (year < 0)
    ++year;
  return (year % 100) ? (year % 4 == 0) : (year % 400 == 0);
}

bool 
SimpleCalendar::equals(IN(RObject) obj)
{
  if (instanceof(obj, SimpleCalendar) == false)
    return false;
  RSimpleCalendar rs(obj);
  return getJulianDay() == rs->getJulianDay() 
      && getMillisecondsOfDay() == rs->getMillisecondsOfDay();
}

//static 
int 
SimpleCalendar::monthDayCount(int year, int month)
{
   
  int ergday;
  bool isleap = isLeapYear(year);
  switch ( month ) {
    case February:
      ergday =  28;
      if (isleap == true)
         ergday += 1;
      break;
    case April:
    case June:
    case September:
    case November:
      ergday = 30;
      break;
    default:
      ergday = 31;
  }
  return ergday;
}



//static 
int 
SimpleCalendar::toJulianDay(int year, int month, int day)
{
  
  
  int day_of_year = day - 1 ;
  // Adjust for BC and normalize
  
  long tyear = year + ( year < 0 ) - MinYear;

  if ( tyear ) {
    day_of_year += ( tyear / 400 ) * 146097L; // + 365L;
    tyear %= 400;

    day_of_year += ( tyear / 100 ) * 36524L;
    tyear %= 100;

    // days in 4 years
    day_of_year += ( tyear / 4 ) * 1461L;
    tyear %= 4;

    // remaining years
    day_of_year += tyear * 365L;
  }
  
   
  day_of_year += MonthDaysTable[(isLeapYear( year ) ? 1 : 0)][month - 1];

  return day_of_year - day_of_year_offset;
}

void 
SimpleCalendar::fromJulianDay( int jday
                        , int* yp
                        , int* mp
                        , int* dp)
{
   jday += day_of_year_offset;
   int year = 0L;

   if (jday >= DaysPerYear) {
      // Days in 400 years.
      year += 400 * ( jday / 146097L );
      jday %= 146097L;
      
      
      int rest_years = jday / 36524L;
      
      if ( rest_years > 3 )
         rest_years = 3;
      year += 100L * rest_years;
      jday -= 36524L * rest_years;
      
      
      year += 4L * ( jday / 1461L );
      jday %= 1461L;
      
      rest_years = ( jday / 365L );
      
      if ( rest_years > 3 )
         rest_years = 3;
      
      year += rest_years;
      jday -= DaysPerYear * rest_years;
   }

  // Adjust year.
  year += MinYear;

  if ( year <= 0 )
    year--;

  // Find month.
  int month = 1;
  while ( true ) 
  {
    int d = monthDayCount( month, year );

    if (jday < d)
      break;

    jday -= d;
    month++;
  }
  if ( mp )
    *mp = month;

  if ( dp )
    *dp =  jday;

  if ( yp )
    *yp = year;
}

} // util
} // acdk
