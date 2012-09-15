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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Time.h,v 1.10 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_Time_h
#define acdk_sql_Time_h

#include "sql.h"

#if !defined(ACDK_MINI)

#include <acdk/text/SimpleDateFormat.h>

namespace acdk {
namespace sql {


ACDK_DECL_CLASS(Time);
/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/04/08 10:53:21 $
  
*/
class ACDK_SQL_PUBLIC Time
: extends acdk::util::Date
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Time)
private:
  static acdk::text::RSimpleDateFormat __sdf;
  static acdk::text::RSimpleDateFormat getSimpleDateFormat();
public:
  
  
  /**
    This method returns a new instance of this class by parsing a
    date in JDBC format into a Java date.
  
    @param str The string to parse.
  
    @return The resulting <code>java.sql.Time</code> value. 
  */
  static RTime valueOf(INP(RString) str);
  
  
  
  /**
    This method initializes a new instance of this class with the
    specified year, month, and day.
  
    @param hour The hour for this Time (0-23)
    @param minute The minute for this time (0-59)
    @param second The second for this time (0-59)
  
    @deprecated
  */
  
  Time(int hour, int minute, int second);
  
  /**
    This method initializes a new instance of this class with the
    specified time value representing the number of seconds since 
    Jan 1, 1970 at 12:00 midnight GMT.
  
    @param time The time value to intialize this <code>Time</code> to.
  */
  Time(jlong date);
  
  
  
  /**
    This method returns this date in JDBC format.
  
    @return This date as a string.
  */
  RString toString();
  
};
    

} // sql
} // acdk
#endif //!defined(ACDK_MINI)

#endif //acdk_sql_Time_h

