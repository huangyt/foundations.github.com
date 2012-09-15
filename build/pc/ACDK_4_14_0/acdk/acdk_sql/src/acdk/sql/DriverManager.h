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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/DriverManager.h,v 1.14 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_DriverManager_h
#define acdk_sql_DriverManager_h

#include "Driver.h"
#include <acdk/util/Properties.h>

#if !defined(ACDK_MINI)
#include <acdk/util/Vector.h>
#else
#include <acdk/util/TIterator.h>
#endif


namespace acdk {
namespace sql {


#if defined(ACDK_MINI)
ACDK_DECL_ITERATOR(Driver, RDriver);
#endif

ACDK_DECL_INTERFACE(DriverManager);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.14 $
  @date $Date: 2005/04/08 10:53:21 $
  
*/

class ACDK_SQL_PUBLIC DriverManager
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DriverManager)
protected:
  static acdk::io::RPrintWriter _logWriter;
  static int _loginTimeOut;
#if !defined(ACDK_MINI)
  static acdk::util::RVector _drivers;

public:
  static acdk::util::RVector drivers();
#else
  foreign static OUTP(RDriverArray) drivers();
#endif //!defined(ACDK_MINI)
public:
  static int getLoginTimeout() { return _loginTimeOut; }
  static void setLoginTimeout(int loginTimeOut) 
  { 
    _loginTimeOut = loginTimeOut; 
  }
  static acdk::io::RPrintWriter getLogWriter()
  {
    return _logWriter;
  }
  static void setLogWriter(acdk::io::RPrintWriter logWriter)
  {
    _logWriter = logWriter;
  }
#if !defined(ACDK_MINI)
  static void println(INP(RString) str);
#endif

  static void registerDriver(INP(RDriver) driver);
  static void deregisterDriver(INP(RDriver) driver);
#if defined(ACDK_MINI)
  /**
    Not supported in ACDK_MINI
  */
  foreign static RDriverIterator getDrivers();
#else
  static acdk::util::RIterator getDrivers();
#endif
  static RDriver getDriver(INP(RString) url) THROWS1(RSQLException);
  static RConnection getConnection(INP(RString) url) THROWS1(RSQLException);
  static RConnection getConnection(INP(RString) url, INP(RString) user, INP(RString) password) THROWS1(RSQLException);
  static RConnection getConnection(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RSQLException);
};          


} // sql
} // acdk

#endif //acdk_sql_DriverManager_h

