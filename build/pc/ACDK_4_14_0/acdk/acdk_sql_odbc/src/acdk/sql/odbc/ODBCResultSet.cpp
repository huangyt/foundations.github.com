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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCResultSet.cpp,v 1.7 2005/03/08 18:55:38 kommer Exp $

#include "ODBCResultSet.h"
#include "ODBCConnection.h"
#include "ODBCDriver.h"
#include "ODBCResultSetMetaData.h"

#include <acdk/lang/Integer.h>
#include <acdk/util/Properties.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

ODBCResultSet::~ODBCResultSet()
{
  ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCResultSet::~ODBCResultSet");
   close();
}

ODBCResultSet::ODBCResultSet(INP(RODBCStatement) stmt, int direction)
: _stmt(stmt), __hndl(Nil), _direction(direction), _numrowsValid(false), _cursorpos(0), _fetched(false), _valid(false), _colDesc(Nil)
{
  //_rsmd = new ODBCResultSetMetaData(this);
}

RODBCHandle
ODBCResultSet::_getODBCHandle() THROWS1(RSQLException)
{
  if (__hndl == Nil) {
    __hndl = (_stmt != Nil)? _stmt->_getODBCHandle() : RODBCHandle(Nil);
    if (__hndl == Nil) {
      THROW1(SQLException, "no handle");
    }
  }
  return __hndl;
}

//virtual
RStatement
ODBCResultSet::getStatement()
{
  return &_stmt;
}

//virtual
RResultSetMetaData
ODBCResultSet::getMetaData()
{
  return new ODBCResultSetMetaData(this);
}

/*
 * SQLRETURN SQLSetPos(SQLHSTMT StatementHandle, SQLUSMALLINT RowNumber, SQLUSMALLINT Operation, SQLUSMALLINT LockType);
 */
 
//virtual
bool
ODBCResultSet::absolute(int row)
{
  RODBCHandle hndl = _getODBCHandle();
  if (row == 0) {  // exception per definition
    ::acdk::sql::RSQLException ex = new SQLException("row can't be 0");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  if (row == _cursorpos) {
    return true;
  } else if (row == (_cursorpos + 1)) {
    return next();
  }
  callSQL3(hndl, SQLSetPos, row, SQL_POSITION, SQL_LOCK_NO_CHANGE);
  if ((callSQL0(hndl, SQLFetch)) != SQL_NO_DATA) {  // anything else is already handled.
    _cursorpos = row; // not ok, if row is negative...
    return true;
  }  
#if 0
  RString msg = RString("cursorpositioning is not allowed");
  ::acdk::sql::RSQLException ex = new SQLException(msg);
  hndl->_addException(ex);
  SQLTHROW(hndl->_getExceptions());
#endif // 0
  return false;
}

//virtual
void
ODBCResultSet::setFetchDirection(int direction)
{
  switch (direction) {
  case acdk::sql::odbc::FETCH_UNKNOWN:
#ifdef REVERSE_ROW_ACCESS_IS_ALLOWED
    case FETCH_REVERSE:
#endif // REVERSE_ROW_ACCESS_IS_ALLOWED
    case acdk::sql::odbc::FETCH_FORWARD:
      _direction = direction;
      break;
    default:
      RODBCHandle hndl = _getODBCHandle();
      RString msg = RString("invalid value for direction: ") + ::acdk::lang::Integer::toString(direction);
      ::acdk::sql::RSQLException ex = new SQLException(msg);
      if (hndl != Nil) {
        hndl->_addException(ex);
        SQLTHROW(hndl->_getExceptions());
      } else {
        SQLTHROW(ex);
      }
      break;
  }
}

//virtual
::acdk::sql::RSQLWarning
ODBCResultSet::getWarnings()
{
  RODBCHandle hndl = _getODBCHandle();
  return hndl->_getWarnings();
}

//virtual
void
ODBCResultSet::deleteRow()
{
  RODBCHandle hndl = _getODBCHandle();
  if ((_cursorpos >= 1) && (_cursorpos <= _numrows))
    callSQL3(hndl, SQLSetPos, _cursorpos, SQL_DELETE, SQL_LOCK_NO_CHANGE);
}

//virtual
void
ODBCResultSet::updateRow()
{
  RODBCHandle hndl = _getODBCHandle();
  if ((_cursorpos >= 1) && (_cursorpos <= _numrows))
    callSQL3(hndl, SQLSetPos, _cursorpos, SQL_UPDATE, SQL_LOCK_NO_CHANGE);
}

//virtual
void
ODBCResultSet::refreshRow()
{
  RODBCHandle hndl = _getODBCHandle();
  if ((_cursorpos >= 1) && (_cursorpos <= _numrows))
    callSQL3(hndl, SQLSetPos, _cursorpos, SQL_REFRESH, SQL_LOCK_NO_CHANGE);
}

//virtual
void
ODBCResultSet::close()
{
#if ODBCVER >= 0x0300
  RODBCHandle hndl = _getODBCHandle();
  callSQL0(hndl, SQLCloseCursor);
#endif
  _colDesc = Nil;
  //_rsmd = Nil;
  _fetched = false;
  _numrows = 0;
  _cursorpos = 0;
}

//virtual
int
ODBCResultSet::_getColumnCount() THROWS1(RSQLException)
{
  if (_colDesc == Nil) 
  {
    RODBCHandle hndl = _getODBCHandle();
#if defined(ACDK_OS_WIN32)
    int ccnt;
	// win32 accepts 0 as dummy-column, unixODBC wants a valid column...
    RODBCColumn cd = new ODBCColumn(_getODBCHandle(), 1);
    ccnt = cd->getColumnCount();
#else // defined(ACDK_OS_WIN32)
       // but MyODBC as well as oci for unixODBC support SQLNumResultCols.
    SQLSMALLINT ccnt;
    callSQL1(hndl, SQLNumResultCols, &ccnt);
#endif // defined(ACDK_OS_WIN32)
    if (ccnt < 1) 
    {
      ::acdk::sql::RSQLException ex = new SQLException("column-count unavailable");
      hndl->_addException(ex);
      SQLTHROW(hndl->_getExceptions());
    }
    _colDesc = new ODBCColumnArray(ccnt);
  }
  return _colDesc->length();
}

void
ODBCResultSet::_chkindex(int index) THROWS1(RSQLException)
{
  int ccnt = _getColumnCount();
  bool wasCurPos0 = _cursorpos == 0;
  if (_cursorpos == 0) 
  {
    if (first() == false) 
    {
      RODBCHandle hndl = _getODBCHandle();
      RString msg = RString("no data at all ");
      ::acdk::sql::RSQLException ex = new SQLException(msg);
      hndl->_addException(ex);
      SQLTHROW(hndl->_getExceptions());
      return;
    }
  }
  if ((index < 1) || (index > ccnt)) 
  {
    RODBCHandle hndl = _getODBCHandle();
    RString msg = RString("index ") + ::acdk::lang::Integer::toString(index) + " is out of bounds";
    ::acdk::sql::RSQLException ex = new SQLException(msg);
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  if (_colDesc[index - 1] == Nil) 
  {
    RODBCColumn cd = new ODBCColumn(_getODBCHandle(), index);
    _colDesc->set(index - 1, cd);
  }
  if (wasCurPos0 == true)
    _fetched = true;
}

int 
ODBCResultSet::_getColumnIndex(INP(RString) colname) THROWS1(::acdk::sql::RSQLException)
{
  ACDK_NLOG("acdk.sql.odbc", Debug, SBSTR("column count: " << _colDesc->length()));
  for (int i = 0; i < _colDesc->length(); ++i)
  {
    _chkindex(i + 1);
    RString realcolname = _colDesc[i]->getColumnName();
    ACDK_NLOG("acdk.sql.odbc", Debug, SBSTR("column name: " << realcolname));
    if (realcolname->equalsIgnoreCase(colname) == true)
      return i + 1;
  }
  THROW1(SQLException, "Columnname: " + colname + " does not exists");
  return Nil;
}

/*
 * SQLRETURN SQLFetchScroll(SQLHSTMT StatementHandle, SQLSMALLINT FetchOrientation, SQLINTEGER FetchOffset);
 */
 
//virtual
bool
ODBCResultSet::next() THROWS1(RSQLException)
{
  if (_fetched == true)
  {
    _fetched = false;
    return true;
  }
  RODBCHandle hndl = _getODBCHandle();
  
  if (_colDesc != Nil) 
  {
    for (int n = 0; n < _colDesc->length(); n++)
      if (_colDesc[n] != Nil)
        _colDesc[n]->_zeroData();
  }

#if 0 // ODBCVER >= 0x0300  // win32 oracle-odbc only supports FETCH_NEXT!
  callSQL2(hndl, SQLFetchScroll, SQL_FETCH_ABSOLUTE, _cursorpos)
#else // ODBCVER
  
  if ((callSQL0(hndl, SQLFetch)) == SQL_NO_DATA) 
    return false;

  _getColumnCount();  // this allocates a new _colDesc-Object.
  _cursorpos++;
#endif
  return true;
}

//virtual
bool
ODBCResultSet::first() THROWS1(RSQLException)
{
  switch (_cursorpos) {
    case 0:
      return next();
      break;
    case 1:
      return true;
      break;
    default:
      return absolute(1);
      break;
  }
}

//virtual
void
ODBCResultSet::_getNumRows()
{
  // how to do this in a compatible way?
  _numrows = 0x7fffffff;
  _numrowsValid = true;
}

} // odbc
} // sql
} // acdk
