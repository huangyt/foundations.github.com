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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCStatement.h,v 1.6 2005/02/05 10:45:32 kommer Exp $

#ifndef acdk_sqlodbc_Statement_h
#define acdk_sqlodbc_Statement_h
#include <acdk.h>
#include <acdk/util/Properties.h>

#include "odbc.h"
#include "ODBCHandle.h"
#include "ODBCResultSet.h"

#include <acdk/sql/Statement.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

enum FetchDirection 
{ 
  FETCH_REVERSE = -1, 
  FETCH_UNKNOWN = 0, 
  FETCH_FORWARD = 1 
};
ACDK_DEF_LIB_ENUM(ACDK_SQL_ODBC_PUBLIC, FetchDirection);

ACDK_DECL_CLASS(ODBCStatement);

class ACDK_SQL_ODBC_PUBLIC ODBCStatement
: extends Object
, implements ::acdk::sql::Statement
{
  ACDK_WITH_METAINFO(ODBCStatement)
public:
  /*
   * non-JDK
   */
   
  virtual void setScrollableCursor(bool enable);
  virtual bool getScrollableCursor();

  virtual RODBCHandle _getODBCHandle() { return _stmth; }

  ODBCStatement(INP(RODBCConnection) conn);
  ~ODBCStatement();
  virtual RODBCStatement init(INP(acdk::util::RProperties) prop);
  void deinit();
  
protected:
  RODBCHandle _stmth;
  RODBCConnection _conn;  // back-reference
  //RResultSet _rset;
  RString _sql;
  int _queryTimeout;
  int _maxLength;
  int _maxRows;
  bool _scanEscapes;
  bool _scrollableCursors;
  FetchDirection _direction;
public:
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Adds a SQL command to the current batch of commmands for the statement. 
  */
  virtual void addBatch(INP(RString) sql) { THROW0(UnsupportedOperationException); }
    
  /**
    API: JDK
    JDKDOC: Cancels this Statement object if both the DBMS and driver support aborting an SQL statement. 
  */
  virtual void cancel() { THROW0(UnsupportedOperationException); }
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Makes the set of commands in the current batch empty. 
  */
  virtual void clearBatch() { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: Clears all the warnings reported on this Statement object. 
  */
  virtual void clearWarnings() { if (_stmth != Nil) _stmth->_clearWarnings(); }

  /**
    API: JDK
    JDKDOC: Releases this Statement object's database and JDBC resources immediately instead of waiting for this to happen when it is automatically closed. 
  */
  virtual void close();
          
  
  /**
    API: JDK
    JDKDOC: Executes a SQL statement that may return multiple results. 
  */
  virtual bool execute(INP(RString) sql);
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Submits a batch of commands to the database for execution. 
  */
  virtual RintArray executeBatch() { THROW0(UnsupportedOperationException); return Nil; return Nil; }
          
  /**
    API: JDK
    JDKDOC: Executes a SQL statement that returns a single ResultSet. 
  */
  virtual RResultSet executeQuery(INP(RString) sql);
          
  /**
    API: JDK
    JDKDOC: Executes an SQL INSERT, UPDATE or DELETE statement. 
  */
  virtual int executeUpdate(INP(RString) sql);
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the Connection object that produced this Statement object. 
  */
  virtual RConnection getConnection();
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Retrieves the direction for fetching rows from database tables that is the default for result sets generated from this Statement object. 
  */
  virtual int getFetchDirection() { return _direction; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Retrieves the number of result set rows that is the default fetch size for result sets generated from this Statement object. 
  */
  virtual int getFetchSize() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Returns the maximum number of bytes allowed for any column value. 
  */
  virtual int getMaxFieldSize() { return _maxLength; }
          
  /**
    API: JDK
    JDKDOC: Retrieves the maximum number of rows that a ResultSet can contain. 
  */
  virtual int getMaxRows() { return _maxRows; }
          
  /**
    API: JDK
    JDKDOC: Moves to a Statement's next result. 
  */
  virtual bool getMoreResults() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Retrieves the number of seconds the driver will wait for a Statement to execute. 
  */
  virtual int getQueryTimeout() { return _queryTimeout; }
          
  /**
    API: JDK
    JDKDOC: Returns the current result as a ResultSet object. 
  */
  virtual RResultSet getResultSet();
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Retrieves the result set concurrency. 
  */
  virtual int getResultSetConcurrency() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Determine the result set type. 
  */
  virtual int getResultSetType() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Returns the current result as an update count; if the result is a ResultSet or there are no more results, -1 is returned. 
  */
  virtual int getUpdateCount() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Retrieves the first warning reported by calls on this Statement. 
  */
  virtual ::acdk::sql::RSQLWarning getWarnings() { return (_stmth == Nil) ?  ::acdk::sql::RSQLWarning(Nil) : _stmth->_getWarnings(); }
          
  /**
    API: JDK
    JDKDOC: Defines the SQL cursor name that will be used by subsequent Statement execute methods. 
  */
  virtual void setCursorName(INP(RString) name) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: Sets escape processing on or off. 
  */
  virtual void setEscapeProcessing(bool enable)
  {
    _scanEscapes = enable;
    if (_stmth == Nil) return;
    _stmth->_setSQLFlag(SQL_ATTR_NOSCAN, (_scanEscapes == true)? SQL_NOSCAN_OFF : SQL_NOSCAN_ON);
  }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gives the driver a hint as to the direction in which the rows in a result set will be processed. 
  */
  virtual void setFetchDirection(int direction);
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gives the JDBC driver a hint as to the number of rows that should be fetched from the database when more rows are needed. 
  */
  virtual void setFetchSize(int rows) { THROW0(UnsupportedOperationException); /* maybe implementable via SQL_ATTR_ROW_ARRAY_SIZE */ }
          
  /**
    API: JDK
    JDKDOC: Sets the limit for the maximum number of bytes in a column to the given number of bytes. 
  */
  virtual void setMaxFieldSize(int max) { _maxLength = max; if (_stmth == Nil) return; _stmth->_setSQLFlag(SQL_ATTR_MAX_LENGTH, _maxLength); }
          
  /**
    API: JDK
    JDKDOC: Sets the limit for the maximum number of rows that any ResultSet can contain to the given number. 
  */
  virtual void setMaxRows(int max) { _maxRows = max; if (_stmth == Nil) return; _stmth->_setSQLFlag(SQL_ATTR_MAX_ROWS, _maxRows); }
          
  /**
    API: JDK
    JDKDOC: Sets the number of seconds the driver will wait for a Statement to execute to the given number of seconds. 
  */
  virtual void setQueryTimeout(int seconds)
  {
    _queryTimeout = seconds;
    if (_stmth == Nil) return;
    _stmth->_setSQLFlag(SQL_ATTR_QUERY_TIMEOUT, _queryTimeout);
  }
};


} // odbc
} // sql
} // acdk

#endif //acdk_sqlodbc_Statement_h

