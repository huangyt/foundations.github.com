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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCStatement.cpp,v 1.6 2005/03/08 18:55:38 kommer Exp $


#include "ODBCStatement.h"
#include "ODBCResultSet.h"
#include "ODBCHandle.h"
#include "ODBCConnection.h"
#include "ODBCDriver.h"
#include "ODBCResultSetMetaData.h"

#include <acdk/lang/Integer.h>
#include <acdk/util/Properties.h>

#if !defined(ACDK_MINI)
#include <acdk/lang/Thread.h>
#endif
#include <acdk/lang/System.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

ODBCStatement::ODBCStatement(INP(RODBCConnection) conn) 
: _stmth(Nil)
, _conn(conn)
//, _rset(Nil)
, _sql(Nil)
, _queryTimeout(0)
, _maxLength(-1)
, _maxRows(-1)
, _scanEscapes(true)
, _scrollableCursors(false)
, _direction(FETCH_UNKNOWN) 
{
}

ODBCStatement::~ODBCStatement() 
{ 
  ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCStatement::~ODBCStatement");
  close(); 
}
 
//virtual
RODBCStatement
ODBCStatement::init(INP(::acdk::util::RProperties) prop)
{
  if (_stmth != Nil)
    return this;

  _stmth = new ODBCHandle(SQL_HANDLE_STMT);

  _stmth->init(_conn->_getODBCHandle());

  /*
   * while we had many things to do for initializing driver and connection, statement is only a skeleton object
   * created by RConnection::CreateStatement, which is filled just before execution with more data...
   */
  return this;
}

//virtual
void
ODBCStatement::deinit()
{
  if (_stmth != Nil) 
  {
    _stmth->deinit();
    _stmth = Nil;
  }
  _conn = Nil;
}

//virtual
void
ODBCStatement::setScrollableCursor(bool enable)
{
#if ODBCVER >= 0x0300
  _scrollableCursors = enable;
  if (_stmth == Nil)
    return;
  _stmth->_setSQLFlag(SQL_ATTR_CURSOR_SCROLLABLE, (_scrollableCursors == true)? SQL_SCROLLABLE : SQL_NONSCROLLABLE);
#else // ODBCVER
  RString msg = RString("can't change ScrollableCursor-mode in odbc-version ") + ::acdk::lang::Integer::toString(ODBCVER, 16);
  ::acdk::sql::RSQLException ex = new SQLException(msg);
  if (_stmth != Nil) {
    _stmth->_addException(ex);
    SQLTHROW(_stmth->_getExceptions());
  } else {
    SQLTHROW(ex);
  }
#endif // ODBCVER
}

//virtual
bool
ODBCStatement::getScrollableCursor()
{
#if ODBCVER >= 0x0300
  return _scrollableCursors;
#else // ODBCVER
  RString msg = RString("can't examine ScrollableCursor-mode in odbc-version ") + ::acdk::lang::Integer::toString(ODBCVER, 16);
  ::acdk::sql::RSQLException ex = new SQLException(msg);
  if (_stmth != Nil) {
    _stmth->_addException(ex);
    SQLTHROW(_stmth->_getExceptions());
  } else {
    SQLTHROW(ex);
  }
  return false; // keep gcc quiet
#endif // ODBCVER
}

//virtual
RResultSet
ODBCStatement::getResultSet()
{
  return new ODBCResultSet(this, _direction);
}

//virtual
RConnection
ODBCStatement::getConnection()
{
  return &_conn; //can't be inline, or gcc will fail.
}

//virtual
void
ODBCStatement::close()
{
  if (_stmth != Nil) 
  {
    _stmth->_clearWarnings();
    _stmth->_clearExceptions();
  }
  deinit();
}

//virtual
bool
ODBCStatement::execute(INP(RString) sql)
{
  SQLSMALLINT ccnt = 0;
  
  if (_stmth == Nil) 
    THROW1(SQLException, "invalid Statement-object");
  
  _sql = Nil;
#if defined(ACDK_ODBC_DEBUG)
  System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: executing \"" + sql + "\"");
#endif
  RString nsql = ODBC_STR2NSTR(sql);
  ODBC_NATIVE_CHAR* tptr = ODBC_STR2NCSRT(nsql);
  int len = nsql->length();
  callSQL2(_stmth, SQLExecDirect, tptr, len);

  _sql = sql;

  //_rset = new ODBCResultSet(this, _direction);  
  /*
   * SQLNumResultCols() is the only possibility to determine, if
   * any resultset is available after execution. the column-count
   * will be >= 1 if there's at least one ResultSet.
   */
  callSQL1(_stmth, SQLNumResultCols, &ccnt);
  
  return (ccnt > 0)? true : false;
}

//virtual
RResultSet
ODBCStatement::executeQuery(INP(RString) sql)
{
  
  if (execute(sql) != true) {
    RString msg = RString("no resultset for query \"") + sql + "\"";
    ::acdk::sql::RSQLException ex = new SQLException(msg);
    if (_stmth != Nil) {
      _stmth->_addException(ex);
      SQLTHROW(_stmth->_getExceptions());
    } else {
      SQLTHROW(ex);
    }
  }
  return getResultSet();
}

//virtual
int
ODBCStatement::executeUpdate(INP(RString) sql)
{
  SQLINTEGER rcnt = 0;

  execute(sql);
  
  callSQL1(_stmth, SQLRowCount, &rcnt);
  
  return rcnt;
}

//virtual
void
ODBCStatement::setFetchDirection(int direction)
{
  switch (direction) {
    case FETCH_UNKNOWN:
    case FETCH_REVERSE:
    case FETCH_FORWARD:
      _direction = (FetchDirection)direction;
      break;
    default:
      RString msg = RString("invalid value for direction: ") + ::acdk::lang::Integer::toString(direction);
      ::acdk::sql::RSQLException ex = new SQLException(msg);
      if (_stmth != Nil) {
        _stmth->_addException(ex);
        SQLTHROW(_stmth->_getExceptions());
      } else {
        SQLTHROW(ex);
      }
      break;
  }
}

} // odbc
} // sql
} // acdk
