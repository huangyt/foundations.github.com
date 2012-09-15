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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteStatement.h,v 1.5 2005/04/13 15:38:05 kommer Exp $

#ifndef acdk_sql_sqlite_LiteStatement_h
#define acdk_sql_sqlite_LiteStatement_h

#include "Config.h"
#include <acdk.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/ResultSet.h>
#include "LiteConnection.h"

struct sqlite3;

namespace acdk {
namespace sql {
namespace sqlite {


ACDK_DECL_CLASS(LiteStatement);

/**
  Statement for a SQLite database.
  Not all methods are implemented.
*/
class ACDK_SQL_SQLITE_PUBLIC LiteStatement
: extends acdk::lang::Object
, implements acdk::sql::Statement
{
  ACDK_WITH_METAINFO(LiteStatement)
protected:
  RLiteConnection _con;
  int _updateCount;
  RResultSet _rset;
public:
  LiteStatement(IN(RLiteConnection) con) 
  : _con(con) 
  , _updateCount(0)
  {}
  /// currently not supported
  virtual void addBatch(INP(RString) sql)
  {
    THROW1(SQLException, "Unsupported");
  }
  virtual void cancel();
  virtual void clearBatch();
  /// currently not supported
  virtual void clearWarnings();
  virtual void close();
  virtual bool execute(INP(RString) sql);
  virtual RintArray executeBatch();
  virtual RResultSet executeQuery(INP(RString) sql);
  virtual int executeUpdate(INP(RString) sql);
  virtual RConnection getConnection() { return &_con; }
  /// always return ResultSet::FETCH_UNKNOWN
  virtual int getFetchDirection() { return ResultSet::FETCH_UNKNOWN; }
  /// always return 1
  virtual int getFetchSize() { return 1; }
  /// always return 0
  virtual int getMaxFieldSize() { return 0; }
  /// always return 0
  virtual int getMaxRows() { return 0; }
  /// always return false
  virtual bool getMoreResults()
  {
    return false; 
  }
  virtual int getQueryTimeout();
  virtual RResultSet getResultSet();
  virtual int getResultSetConcurrency();
  virtual int getResultSetType();
  virtual int getUpdateCount();
  virtual RSQLWarning getWarnings();
  virtual void setCursorName(INP(RString) name);
  virtual void setEscapeProcessing(bool enable);
  /// sqlite supports only forward direction
  virtual void setFetchDirection(int direction); 
  /// will be ignored
  virtual void setFetchSize(int rows);
  /// will be ignored
  virtual void setMaxFieldSize(int max);
  /// will be ignored
  virtual void setMaxRows(int max);
  virtual void setQueryTimeout(int seconds);
};

} // sqlite
} // sql 
} // acdk

#endif //acdk_sql_sqlite_LiteResultSetMetaData_h
