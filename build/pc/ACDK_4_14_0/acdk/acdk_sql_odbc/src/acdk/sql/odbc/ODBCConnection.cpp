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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCConnection.cpp,v 1.6 2005/03/08 18:55:37 kommer Exp $

#include "ODBCConnection.h"
#include "ODBCDriver.h"
#include "ODBCResultSetMetaData.h"
#include "ODBCPreparedStatement.h"
#include "ODBCCallableStatement.h"

#include <acdk/util/Properties.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/NumberFormatException.h>
#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace sql {
namespace odbc {

ODBCConnection::ODBCConnection(INP(RODBCDriver) driver) 
: _driver(driver)
, _dbch(Nil)
, _prop(Nil)
, _url(Nil)
, _loginTimeout(0)
, _connectionTimeout(0)
, _queryTimeout(0)
, _opened(false)
, _readOnly(false)
, _autoCommit(true)
, _asyncCalls(false)
, _traceCalls(false)
, _maxRows(-1)
, _maxLength(-1)
, _scanEscapes(true)
, _scrollableCursors(false) 
{ 
}

ODBCConnection::~ODBCConnection() 
{ 
  ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCConnection::~ODBCConnection");
  close(); 
}

RDatabaseMetaData 
ODBCConnection::getMetaData()
{
  return new ODBCDatabaseMetaData(this); 
}

// same problem as with ODBCDriver: the interface Connection doesn't have a method to open a connection,
// but the open of a DB may fail and we shouldn't throw an exception within the constructor. the alternative 
// is an init-function, which has to be called immediately after the creation of the new object
 
RODBCConnection
ODBCConnection::init(INP(RString) url, INP(acdk::util::RProperties) prop)
{
  RString dsn, uid, pwd;
  int stop, equl, strt = 10;

  // a jdbc-url for sqlodbc looks like: "jdbc:odbc:datasource", where datasource may contain additional host/port-
  // info or attributes: [//host[:port]/]databasename[/attrib[/...]]
  // currently I've no idea, how to connect to an outside odbc-driver, which hasn't an alias on current host, the
  // attribs will be checked for user=... and password=..., which will be deleted but used internally, any other
  // slash will be turned into a ';' and then given to the underlaying odbc-drivermanager.
  // Note: we don't use net::parseURL(), because we have to these additional tasks on the url. 
  
  if (_dbch != Nil)
    return this;
  if (_driver->acceptsURL(url) == false)
    return Nil;
  _dbch = new ODBCHandle(SQL_HANDLE_DBC);
    
  _dbch->init(_driver->_getODBCHandle());

  
  if (url->length() == strt) {
    // empty datasource, use ODBC default source
    dsn = new String("DEFAULT");
  } else {
    if (url->startsWith("//", strt) == true)
      strt += 2;
    if ((stop = url->indexOf('/', strt)) == -1)
      dsn = new String(url->substring(strt));
    else {
      RStringBuffer sb = new StringBuffer;
      sb->append(url->substring(strt, stop));
      do {
        strt = stop + 1;
        if (url->length() <= strt) {
          break;
        }
        if ((stop = url->indexOf('/', strt)) == -1)
          stop = url->length();
        equl = url->indexOf('=', strt);
        if ((equl >= 0) && (equl < stop)) {
          RString key = url->substring(strt, equl);
          if (key->equalsIgnoreCase("user") == true) {
            uid = new String(url->substring(equl + 1, stop));
          } else if (key->equalsIgnoreCase("password") == true) {
            pwd = new String(url->substring(equl + 1, stop));
          } else {
            sb->append(';');
            sb->append(url->substring(strt, stop));
          }
        } else {
          sb->append(';');
          sb->append(url->substring(strt, stop));
        }
      } while (stop < url->length());
      dsn = new String(sb);
    }
  }
  if (uid == Nil)
    uid = new String();
  if (pwd == Nil)
    pwd = new String();
#if defined(ACDK_ODBC_DEBUG)
  ::acdk::lang::System::err->println(RString("INFO: parsed url \"") + url + "\" into DSN=\"" + dsn + "\", UID=\"" + uid + "\" PWD=\"" + pwd + "\".");
#endif
#if !defined(ACDK_MINI)
  RString ndsn = dsn->convertToNative();
  RString nuid = uid->convertToNative();
  RString npwd = pwd->convertToNative();
#else
  RString ndsn = dsn;
  RString nuid = uid;
  RString npwd = pwd;
#endif
  callSQL6(_dbch, SQLConnect, (ODBC_NATIVE_CHAR*)ndsn->native_c_str(), ndsn->length(), 
                              (ODBC_NATIVE_CHAR*)nuid->native_c_str(), nuid->length(), 
                              (ODBC_NATIVE_CHAR*)npwd->native_c_str(), npwd->length());
  _url = url;
  _prop = prop;

  /* maybe these will fail */
  _parseProperties(prop);
  //_setSQLFlags(); // this WILL fail (at least on mysql)

  return this;
}

#define _parseOneProp(__name, __var, __method, __exception, __defval) \
  key = new String(__name); \
  res = prop->getProperty(key); \
  if (res != Nil) { \
    try { \
      __var = __method(res); \
    } catch (__exception e) { \
      /* the caller might not catch ???Exception, but SQLException. */ \
      RString _errmsg_ = RString("invalid Property-value for \"") \
       + __name + "\""; \
      THROW1(SQLException, _errmsg_); \
    } \
  } else { \
    __var = __defval; \
  }

#define _unsupportedProp(__name) \
  key = new String(__name); \
  res = prop->getProperty(key); \
  if (res != Nil) { \
    RString _errmsg_ = RString("unsupported Property-value \"") \
     + __name + "\" for ODBC-version " + ::acdk::lang::Integer::toString(ODBCVER, 16); \
    THROW1(SQLException, _errmsg_); \
  }

  // currently statement-relevant key/values are: (statement-interface doesn't have own properties, here we set default values)
  //
  // SQL_ATTR_CONCURRENCY: SQLUINTEGER: SQL_CONCUR_READ_ONLY (default), SQL_CONCUR_LOCK, SQL_CONCUR_ROWVER, SQL_CONCUR_VALUES 
  // SQL_ATTR_CURSOR_SCROLLABLE: SQLUINTEGER: SQL_NONSCROLLABLE (default), SQL_SCROLLABLE 
  // SQL_ATTR_CURSOR_SENSITIVITY: SQLUINTEGER: SQL_UNSPECIFIED (default), SQL_INSENSITIVE, SQL_SENSITIVE
  // SQL_ATTR_CURSOR_TYPE: SQLUINTEGER: SQL_CURSOR_FORWARD_ONLY (default), SQL_CURSOR_STATIC, SQL_CURSOR_KEYSET_DRIVEN, SQL_CURSOR_DYNAMIC
  // SQL_ATTR_MAX_LENGTH: SQLUINTEGER: 0 (default)
  // SQL_ATTR_MAX_ROWS: SQLUINTEGER: 0 (default)
  // SQL_ATTR_NOSCAN: SQLUINTEGER: SQL_NOSCAN_OFF (default), SQL_NOSCAN_ON
  // SQL_ATTR_QUERY_TIMEOUT: SQLUINTEGER: 0 (default)

void
ODBCConnection::_parseProperties(INP(acdk::util::RProperties) prop)
{
  RString key, res;
  
  if (prop == Nil)
    return;

/* choose your part: macro-calls or many code... */

  _parseOneProp("LoginTimeout", _loginTimeout, ::acdk::lang::Integer::decode, NumberFormatException, 0);
#if ODBCVER >= 0x0300
  _parseOneProp("ConnectionTimeout", _connectionTimeout, ::acdk::lang::Integer::decode, NumberFormatException, 0);
#else
  _unsupportedProp("ConnectionTimeout");
#endif
  _parseOneProp("ReadOnly", _readOnly, ::acdk::lang::Boolean::getBoolean, Exception, false);
  _parseOneProp("AutoCommit", _autoCommit, ::acdk::lang::Boolean::getBoolean, Exception, true);
#if ODBCVER >= 0x0300
  _parseOneProp("AsyncCalls", _asyncCalls, ::acdk::lang::Boolean::getBoolean, Exception, false);
#else
  _unsupportedProp("AsyncCalls");
#endif
  _parseOneProp("TraceCalls", _traceCalls, ::acdk::lang::Boolean::getBoolean, Exception, false);

  _parseOneProp("QueryTimeout", _queryTimeout, ::acdk::lang::Integer::decode, NumberFormatException, 0);
  _parseOneProp("MaxLength", _maxLength, ::acdk::lang::Integer::decode, NumberFormatException, 0);
  _parseOneProp("MaxRows", _maxRows, ::acdk::lang::Integer::decode, NumberFormatException, 0);
  _parseOneProp("ScanEscapes", _scanEscapes, ::acdk::lang::Boolean::getBoolean, Exception, true);
  _parseOneProp("ScrollableCursors", _scrollableCursors, ::acdk::lang::Boolean::getBoolean, Exception, false);

}

void
ODBCConnection::_setSQLFlags()
{
  if (_dbch == Nil)
    return;
    
  // currently connection-relevant key/values are:
  //
  //  SQL_ATTR_ACCESS_MODE:        SQLUINTEGER: SQL_MODE_READ_ONLY, SQL_MODE_READ_WRITE (default)
  //  SQL_ATTR_ASYNC_ENABLE:       SQLUINTEGER: SQL_ASYNC_ENABLE_OFF (default), SQL_ASYNC_ENABLE_ON
  //  SQL_ATTR_AUTOCOMMIT:         SQLUINTEGER: SQL_AUTOCOMMIT_OFF, SQL_AUTOCOMMIT_ON (default)
  //  SQL_ATTR_CONNECTION_TIMEOUT: SQLUINTEGER: 0 (default) 
  //  SQL_ATTR_LOGIN_TIMEOUT:      SQLUINTEGER: 0 (default)
  //  SQL_ATTR_TRACE:              SQLUINTEGER: SQL_OPT_TRACE_OFF (default), SQL_OPT_TRACE_ON

  // maybe we shouldn't set those values, which are default, or we might get unneeded errors
  _dbch->_setSQLFlag(SQL_ATTR_ACCESS_MODE, (_readOnly == true)? SQL_MODE_READ_ONLY : SQL_MODE_READ_WRITE);
#if ODBCVER >= 0x0300
  _dbch->_setSQLFlag(SQL_ATTR_ASYNC_ENABLE, (_asyncCalls == false)? SQL_ASYNC_ENABLE_OFF : SQL_ASYNC_ENABLE_ON);
#endif // ODBCVER
  _dbch->_setSQLFlag(SQL_ATTR_AUTOCOMMIT, (_autoCommit == false)? SQL_AUTOCOMMIT_OFF : SQL_AUTOCOMMIT_ON);
#if ODBCVER >= 0x0300
  _dbch->_setSQLFlag(SQL_ATTR_CONNECTION_TIMEOUT, _connectionTimeout);
#endif // ODBCVER
  _dbch->_setSQLFlag(SQL_ATTR_LOGIN_TIMEOUT, _loginTimeout);
  _dbch->_setSQLFlag(SQL_ATTR_TRACE, (_traceCalls == false)? SQL_OPT_TRACE_OFF : SQL_OPT_TRACE_ON);
}

void
ODBCConnection::_setSQLFlag(SQLUINTEGER key, SQLUINTEGER val)
{
  if (_dbch == Nil)
    return; // maybe we should throw an exception.
  _dbch->_setSQLFlag(key, val);
}

void 
ODBCConnection::commit() 
{ 
  if (_autoCommit == true) 
    return; 
  THROW0(UnsupportedOperationException); 
  _dbch->_chkSQLrcode(SQLEndTran(_dbch->_getSQLHType(), _dbch->_getSQLHandle(), SQL_COMMIT), "SQLEndTran", __FILE__, __LINE__);
  //callSQL1(_dbch, SQLEndTran, SQL_COMMIT);

}

RStatement
ODBCConnection::createStatement()
{
  RODBCStatement stmt = new ODBCStatement(this);
  
  stmt->init(_prop);

#if 0
  /* maybe we shouldn't set those values, which are default, or we might get unneeded errors */
  stmt->setEscapeProcessing(_scanEscapes);
#if ODBCVER >= 0x0300
  stmt->setScrollableCursor(_scrollableCursors);
#endif // ODBCVER
  stmt->setQueryTimeout(_queryTimeout);
  stmt->setMaxFieldSize(_maxLength);
  stmt->setMaxRows(_maxRows);
#endif // 0
  return &stmt;
}

RPreparedStatement 
ODBCConnection::prepareStatement(INP(RString) sql)
{
  RODBCPreparedStatement stmt =  new ODBCPreparedStatement(this, sql);
  stmt->init(_prop);
#if 0

#if ODBCVER >= 0x0300
  stmt->setScrollableCursor(_scrollableCursors);
#endif // ODBCVER
  stmt->setQueryTimeout(_queryTimeout);
  stmt->setMaxFieldSize(_maxLength);
  stmt->setMaxRows(_maxRows);
#endif  
  return &stmt;
}

RCallableStatement 
ODBCConnection::prepareCall(INP(RString) sql)
{
  RODBCCallableStatement stmt = new ODBCCallableStatement(this, sql);
  stmt->init(_prop);
  return &stmt;
}

} // odbc
} // sql
} // acdk
