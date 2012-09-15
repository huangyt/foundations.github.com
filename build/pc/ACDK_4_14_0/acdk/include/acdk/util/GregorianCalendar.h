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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/GregorianCalendar.h,v 1.12 2005/02/07 11:40:52 kommer Exp $

#ifndef acdk_util_GregorianCalendar_h
#define acdk_util_GregorianCalendar_h

#include <acdk.h>
#include "Calendar.h"

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;


ACDK_DECL_CLASS(GregorianCalendar);

/**
  This is a port from ClassPath GregorianCalendar

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
  
  @version $Revision: 1.12 $
  @date $Date: 2005/02/07 11:40:52 $
  @bug only partly implemented
       Only the fields YEAR, MONTH and DAY_OF_MONTH works
*/

class ACDK_CORE_PUBLIC GregorianCalendar
: extends acdk::util::Calendar
{
  ACDK_WITH_METAINFO(GregorianCalendar)
private:
  static const jlong DEFAULT_GREGORIANCUTOVER;
  static const int _minimum[];
  static const int _maximum[];
  static const int _greatestMin[];
  static const int _leastMax[];
  // Defaultvalues to complete time fields
  static const int _tfDefault[];
  jlong _gregorianCutover;
public:
  static int BC;
  static int AD;
  GregorianCalendar();
  GregorianCalendar(IN(RTimeZone) zone);
  GregorianCalendar(IN(RLocale) locale);
  GregorianCalendar(IN(RTimeZone) zone, IN(RLocale) locale);
  GregorianCalendar(int year, int month, int day);
  GregorianCalendar(int year, int month, int day, int hour, int minute);
  GregorianCalendar(int year, int month, int day, 
                    int hour, int minute, int second);

  ~GregorianCalendar();

  void setGregorianChange(IN(RDate) date) 
  {
    _gregorianCutover = date->getTime();
  }
  
  RDate getGregorianChange(IN(RDate) date) 
  {
    return new Date(_gregorianCutover);
  }
  
  bool isLeapYear(int year);
  foreign bool equals(IN(RObject) obj);
  foreign void add(int field, int amount);
  foreign void roll(int field, bool up);
  foreign int getMinimum(int field);
  foreign int getMaximum(int field);
  foreign int getGreatestMinimum(int field);
  foreign int getLeastMaximum(int field);
  foreign int getActualMinimum(int field);
  foreign int getActualMaximum(int field);

private:
  int getLinearDay(int year, int dayOfYear, bool gregorian);
  jlong getLinearTime(int year, int dayOfYear, int millis); 
  int getWeekDay(int year, int dayOfYear);
  void calculateDay(int day, bool gregorian);
  
protected:
  void computeTime();
  void computeFields();
 

};


} // util
} // acdk

#endif //acdk_util_GregorianCalendar_h

