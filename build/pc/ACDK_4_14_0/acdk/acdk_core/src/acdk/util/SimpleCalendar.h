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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SimpleCalendar.h,v 1.10 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_SimpleCalendar_h
#define acdk_util_SimpleCalendar_h

#include <acdk.h>
#include "Calendar.h"

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;


ACDK_DECL_CLASS(SimpleCalendar);

/**
  A more simple callendar
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:06 $
  @bug only partly implemneted
*/

class ACDK_CORE_PUBLIC SimpleCalendar
: extends acdk::util::Calendar
, implements acdk::io::Serializable
{
  //ACDK_WITH_METAINFO(SimpleCalendar)
private:
  int _JulianDay;
  int _millisecondsOfDay;
public:
  SimpleCalendar();
  SimpleCalendar(int year, int month, int day);
  SimpleCalendar(int year, int month, int day, int hour, int minute);
  SimpleCalendar(int year, int month, int day, 
                    int hour, int minute, int second, int ms = 0);

  ~SimpleCalendar() { }
  int getJulianDay() { return _JulianDay; }
  int getMillisecondsOfDay() { return _millisecondsOfDay; }
  
  static bool isLeapYear(int year);
  foreign bool equals(IN(RObject) obj);
  /* not supported
  foreign void add(int field, int amount);
  foreign void roll(int field, bool up);
  foreign int getMinimum(int field);
  foreign int getMaximum(int field);
  foreign int getGreatestMinimum(int field);
  foreign int getLeastMaximum(int field);
  foreign int getActualMinimum(int field);
  foreign int getActualMaximum(int field);
  */
  static int toJulianDay(int year, int month, int day);
  foreign static void fromJulianDay( int jday, int* yp, int* mp, int* dp);
  static int monthDayCount(int year, int month);
private:
  


};


} // util
} // acdk

#endif //acdk_util_SimpleCalendar_h

