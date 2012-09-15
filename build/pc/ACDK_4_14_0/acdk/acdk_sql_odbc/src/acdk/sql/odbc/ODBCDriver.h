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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCDriver.h,v 1.6 2005/02/05 10:45:31 kommer Exp $

#ifndef acdk_sqlodbc_Driver_h
#define acdk_sqlodbc_Driver_h

#include "odbc.h"
#include "ODBCHandle.h"

#include <acdk/sql/Driver.h>
#include <acdk/sql/DriverManager.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;
using namespace acdk::sql;

ACDK_DECL_CLASS(ODBCDriver);

/**
  jdbc like driver for ODBC data sources
*/
class ACDK_SQL_ODBC_PUBLIC ODBCDriver
: extends Object
, implements ::acdk::sql::Driver
{
  ACDK_WITH_METAINFO(ODBCDriver)
public:
  ODBCDriver();
  
  ~ODBCDriver();
  static RObject create_instance() { return new ODBCDriver(); }  // needs by acdk for loading this mdbc-driver
  virtual int getMajorVersionNumber() { return majorVersion; }
  virtual int getMinorVersion() { return minorVersion; }
  virtual bool jdbcCompliant() { return false; }
  virtual RDriverPropertyInfoArray getPropertyInfo(INP(RString) url, INP(acdk::util::RProperties) properties) 
      THROWS1(RSQLException) { THROW0(UnsupportedOperationException); return Nil; }

    // acceptsURL should only throw an exception if the db-access fails, but it has nothing
    // to do with any db-access, neither checks the availability nor accessrights of a certain
    // database, but checks if it is the right driver for the given url.

  virtual bool acceptsURL(INP(RString) url) THROWS1(acdk::lang::RException)
  { 
    return ((url->length() >= 10) && ((url->startsWith("j") == true) 
					  || (url->startsWith("m") == true)) && (url->startsWith("dbc:odbc:", 1) == true)); 
  }
  virtual RConnection connect(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RSQLException);

    // additional methods 

  virtual RODBCHandle _getODBCHandle() { return _envh; }
  virtual ::acdk::sql::RSQLWarning getWarnings() { return (_envh == Nil) ? ::acdk::sql::RSQLWarning(Nil) : _envh->_getWarnings(); }

private:
  
  const int majorVersion;
  const int minorVersion;
  RODBCHandle _envh;
};

} // odbc
} // sql
} // acdk

#endif //acdk_sqlodbc_Driver_h

