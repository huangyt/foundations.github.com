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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCDriver.cpp,v 1.7 2005/02/05 10:45:31 kommer Exp $

#include "ODBCDriver.h"
#include "ODBCConnection.h"
#include "ODBCResultSetMetaData.h"

#include <acdk/lang/Integer.h>
#include <acdk/lang/NumberFormatException.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

ODBCDriver::ODBCDriver() 
: majorVersion(0), minorVersion(1), _envh(Nil)
{
  ACDK_SAFE_CONSTRUCTOR();

  // we may alloc _envh here, but if this fails, we should throw an exception,
  // which may not be expected within a constructor and may duce to memory-leaks.
  ::acdk::sql::DriverManager::registerDriver(this);
}

ODBCDriver::~ODBCDriver()
{
  //recursiv call !::acdk::sql::DriverManager::deregisterDriver(this);
}


RConnection
ODBCDriver::connect(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RSQLException)
{
  //SQLRETURN err;
  RODBCConnection conn;
  int logintimeout, pool;
  
  if (acceptsURL(url) == false) // maybe the caller is lazy
    return Nil;

  if (properties != Nil) 
  {
    RString key, res;
    key = new String("LoginTimeout");
    res = properties->getProperty(key);
    if (res != Nil) {
      try {
        logintimeout = ::acdk::lang::Integer::decode(res);
      } catch (NumberFormatException e) {
        // the caller might not catch NumberFormatException, but SQLException.
        THROW1(SQLException, "invalid Property-value for \"LoginTimeout\"");
      }      
    }
    key = Nil;
    key = new String("ConnectionPooling");
    res = properties->getProperty(key);
    if (res != Nil) {
#if ODBCVER >= 0x0300
      try {
        pool = ::acdk::lang::Integer::decode(res);
      } catch (NumberFormatException e) {
        RString tmp;
        tmp = new String("ONE_PER_DRIVER");
        if (res->equals(tmp) == true) {
          pool = SQL_CP_ONE_PER_DRIVER;
        } else {
          tmp = Nil;
          tmp = new String("ONE_PER_HENV");
          if (res->equals(tmp) == true) {
            pool = SQL_CP_ONE_PER_HENV;
          } else {
            tmp = Nil;
            tmp = new String("OFF");
            if (res->equals(tmp) == true) {
              pool = SQL_CP_OFF;
            } else {
              THROW1(SQLException, "invalid Property-value for \"ConnectionPooling\"");
            }
          }
        }
      }
#else // ODBCVER
    RString msg = RString("unsupported Property-value \"") + key + "\" for ODBC-version " + ::acdk::lang::Integer::toString(ODBCVER, 16);
    THROW1(SQLException, msg);
#endif // ODBCVER
    }
    key = Nil;
  } else {
    logintimeout = 0;
#if ODBCVER >= 0x0300
    pool = SQL_CP_DEFAULT;
#else // ODBCVER
    pool = 0;
#endif // ODBCVER
  }

  // for an ODBC-connect we need first a handle via:
  // SQLRETURN SQLAllocHandle(SQLSMALLINT HandleType, SQLHANDLE InputHandle, SQLHANDLE *OutputHandlePtr);
  // for the real connect, we need SQL_HANDLE_DBC as HandleType which needs SQL_HANDLE_ENV as InputHandle.
  // so we allocate this before the needed handle with an InputHandle of SQL_NULL_HANDLE.
  
  if (_envh == Nil) {
#if ODBCVER >= 0x0300
    /*
     * maybe this is wrong and has to be disabled, but the documentation sais, that this is needed
     * for reusing pending connections, but maybe this interpretation is simply wrong.
     */
    SQLRETURN sqlret = ::SQLSetEnvAttr(0, SQL_ATTR_CONNECTION_POOLING, (SQLPOINTER)pool, SQL_IS_UINTEGER);
    ACDK_NLOGP("sql", SysDebug, "SQLSetEnvAttr(0, SQL_ATTR_CONNECTION_POOLING, (SQLPOINTER)pool, SQL_IS_UINTEGER)", LOG_NPV(pool, pool) << LOG_NPV(ret, sqlret));
#endif // ODBCVER

    _envh = new ODBCHandle(SQL_HANDLE_ENV);
    
    _envh->init(Nil);    
  }
  conn = new ODBCConnection(this);
  
  return &conn->init(url, properties);
}

} // odbc
} // sql
} // acdk

