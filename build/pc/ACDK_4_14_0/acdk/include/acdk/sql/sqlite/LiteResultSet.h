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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteResultSet.h,v 1.6 2005/04/13 15:38:05 kommer Exp $

#ifndef acdk_sql_sqlite_LiteResultSet_h
#define acdk_sql_sqlite_LiteResultSet_h

#include "Config.h"
#include <acdk.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/ResultSet.h>
#include "LiteConnection.h"
#include "LiteStatement.h"
#include "LiteResultSetMetaData.h"



namespace acdk {
namespace sql {
namespace sqlite {


ACDK_DECL_CLASS(LiteResultSet);

/**
  A result set from a query to SQLite database.
  Modifications of a ResultSet (all update methods) are not supported
*/
class ACDK_SQL_SQLITE_PUBLIC LiteResultSet
: extends acdk::lang::Object
, implements acdk::sql::ResultSet
{
  ACDK_WITH_METAINFO(LiteResultSet)
protected:
  RLiteConnection _con;
  RLiteStatement _stm;
  RLiteTable _table;
  int _updateCount;
public:
  LiteResultSet(IN(RLiteConnection) con, IN(RLiteStatement) stm, IN(RLiteTable) table) 
  : _con(con) 
  , _stm(stm)
  , _table(table)
  , _updateCount(0)
  {}
  
  virtual bool absolute(int row)
  {
    return _table->seek(row);
  }
  virtual void afterLast()
  {
    _table->seek(-2);
  }
  virtual void beforeFirst()
  {
     _table->seek(-1);
  }
  virtual void cancelRowUpdates()
  {
    THROW1(SQLException, "Unsupported");
  }
  virtual void clearWarnings()
  {
    THROW1(SQLException, "Unsupported");
  }
  virtual void close()
  {
    _table = Nil;
  }
  virtual void deleteRow()
  {
    THROW1(SQLException, "Unsupported");
  }
  /**
    return index starting with 1
  */
  virtual int findColumn(INP(RString) columnName)
  {
    return _table->getColumnByName(columnName) + 1;
  }
  virtual bool first()
  {
    return _table->seek(0);
  }
  /// currently not supported
  virtual RArray getArray(int i)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual RArray getArray(INP(RString) colName)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  virtual RBlob getBlob(int i) { return new StandardMemBlob(_table->getBlob(i - 1)); }
  virtual RBlob getBlob(IN(RString) colName) { return getBlob(findColumn(colName)); }

  /// currently not supported
  virtual acdk::io::RReader getAsciiStream(int columnIndex)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual acdk::io::RReader getAsciiStream(INP(RString) columnName)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual acdk::io::RReader getBinaryStream(int columnIndex)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual acdk::io::RReader getBinaryStream(INP(RString) columnName)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }

  virtual bool getBoolean(int columnIndex)
  {
    return _table->getBoolean(columnIndex - 1);
  }
  virtual bool getBoolean(INP(RString) columnName)
  {
    return getBoolean(findColumn(columnName));
  }
  virtual byte getByte(int columnIndex)
  {
    return _table->getByte(columnIndex - 1);
  }
  virtual byte getByte(INP(RString) columnName)
  {
    return getByte(findColumn(columnName));
  }
  /// currently not supported
  virtual RbyteArray getBytes(int columnIndex)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual RbyteArray getBytes(INP(RString) columnName)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual acdk::io::RReader getCharacterStream(int columnIndex)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual acdk::io::RReader getCharacterStream(INP(RString) columnName)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  virtual int getConcurrency()
  {
    return CONCUR_READ_ONLY;
  }
  /// currently not supported, return Nil
  virtual RString getCursorName() { return Nil; }
  /// currently not supported
  virtual acdk::util::RDate getDate(int columnIndex)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual acdk::util::RDate getDate(INP(RString) columnName)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  virtual double getDouble(int columnIndex) { return _table->getDouble(columnIndex - 1); }
  virtual double getDouble(INP(RString) columnName) { return getDouble(findColumn(columnName)); }
  virtual int getFetchDirection() { return FETCH_FORWARD; }
  virtual int getFetchSize() { return 1; }
  virtual float getFloat(int columnIndex)
  {
    return _table->getFloat(columnIndex - 1); 
  }
  virtual float getFloat(INP(RString) columnName) { return getFloat(findColumn(columnName)); }
  virtual int getInt(int columnIndex) { return _table->getInt(columnIndex - 1); }
  virtual int getInt(INP(RString) columnName) { return getInt(findColumn(columnName)); }
  virtual jlong getLong(int columnIndex) { return _table->getLong(columnIndex - 1); }
  virtual jlong getLong(INP(RString) columnName) { return getLong(findColumn(columnName)); }
  virtual RResultSetMetaData getMetaData()
  {
    return new LiteResultSetMetaData(_table);
  }
  /// currently not supported
  virtual RObject getObject(int columnIndex);
  /// currently not supported
  virtual RObject getObject(int i, INP(acdk::util::RMap) map);
  /// currently not supported
  virtual RObject getObject(INP(RString) columnName) { return getObject(findColumn(columnName)); }
  /// currently not supported
  virtual RObject getObject(INP(RString) colName, INP(acdk::util::RMap) map) { return getObject(findColumn(colName), map); }
  /// return the internal row index
  virtual int getRow() { return _table->curRow(); }
  virtual short getShort(int columnIndex) { return _table->getShort(columnIndex - 1); }
  virtual short getShort(INP(RString) columnName) { return getShort(findColumn(columnName)); }
  virtual RStatement getStatement() { return &_stm; }
  virtual RString getString(int columnIndex) { return _table->getString(columnIndex - 1); }
  virtual RString getString(INP(RString) columnName) { return getString(findColumn(columnName)); }
  /// currently not supported
  virtual RTime getTime(int columnIndex)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual RTime getTime(INP(RString) columnName)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual RTimestamp getTimestamp(int columnIndex)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual RTimestamp getTimestamp(INP(RString) columnName)
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }

  virtual int getType() { return TYPE_SCROLL_INSENSITIVE; }
  /// currently not supported
  virtual RSQLWarning getWarnings()
  {
    THROW1(SQLException, "Unsupported");
    return Nil;
  }
  /// currently not supported
  virtual void insertRow()
  {
    THROW1(SQLException, "Unsupported");
  }
  /// currently not supported
  virtual bool isAfterLast()
  {
    THROW1(SQLException, "Unsupported");
    return false;
  }

  virtual bool isBeforeFirst()
  {
    return _table->curRow() == -1;
  }
  virtual bool isFirst()
  {
    return _table->curRow() == 0;
  }
  /// currently not supported
  virtual bool isLast()
  {
    THROW1(SQLException, "Unsupported");
    return false;
  }
  virtual bool last()
  {
    return _table->seek(-2);
  }
  /// does nothing
  virtual void moveToCurrentRow()
  {
    // does nothing
  }
  /// currently not supported
  virtual void moveToInsertRow()
  {
    THROW1(SQLException, "Unsupported");
  }
  virtual bool next()
  {
    return _table->next();
  }
  virtual bool previous()
  {
    return _table->seek(_table->curRow() - 1);
  }
  /// does nothing
  virtual void refreshRow()
  {
    // does nothing
  }
  virtual bool relative(int rows)
  {
    return _table->seek(_table->curRow() + rows);
  }
  /// currently not supported
  virtual bool rowDeleted()
  {
    THROW1(SQLException, "Unsupported");
    return false;
  }
  /// currently not supported
  virtual bool rowInserted()
  {
    THROW1(SQLException, "Unsupported");
    return false;
  }
  /// currently not supported
  virtual bool rowUpdated()
  {
    THROW1(SQLException, "Unsupported");
    return false;
  }
  /// currently not supported
  virtual void setFetchDirection(int direction)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// call will be ignored
  virtual void setFetchSize(int rows)
  {
    // ignored
  }
  /// currently not supported
  virtual void updateAsciiStream(int columnIndex, INP(acdk::io::RReader) x, int length)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateAsciiStream(INP(RString) columnName, INP(acdk::io::RReader) x, int length)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateBinaryStream(int columnIndex, INP(acdk::io::RReader) x, int length)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateBinaryStream(INP(RString) columnName, INP(acdk::io::RReader) x, int length)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateBoolean(int columnIndex, bool x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateBoolean(INP(RString) columnName, bool x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateByte(int columnIndex, byte x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateByte(INP(RString) columnName, byte x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateBytes(int columnIndex, INP(RbyteArray) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateBytes(INP(RString) columnName, INP(RbyteArray) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateCharacterStream(int columnIndex, INP(acdk::io::RReader) x, int length)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateCharacterStream(INP(RString) columnName, INP(acdk::io::RReader) reader, int length)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateDate(int columnIndex, INP(acdk::util::RDate) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateDate(INP(RString) columnName, INP(acdk::util::RDate) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateDouble(int columnIndex, double x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateDouble(INP(RString) columnName, double x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateFloat(int columnIndex, float x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateFloat(INP(RString) columnName, float x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateInt(int columnIndex, int x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateInt(INP(RString) columnName, int x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateLong(int columnIndex, jlong x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateLong(INP(RString) columnName, jlong x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateNull(int columnIndex)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateNull(INP(RString) columnName)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateObject(int columnIndex, INP(RObject) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateObject(int columnIndex, INP(RObject) x, int scale)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateObject(INP(RString) columnName, INP(RObject) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateObject(INP(RString) columnName, INP(RObject) x, int scale)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateRow()
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateShort(int columnIndex, short x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateShort(INP(RString) columnName, short x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateString(int columnIndex, INP(RString) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateString(INP(RString) columnName, INP(RString) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateTime(int columnIndex, INP(RTime) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateTime(INP(RString) columnName, INP(RTime) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateTimestamp(int columnIndex, INP(RTimestamp) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// not supported
  virtual void updateTimestamp(INP(RString) columnName, INP(RTimestamp) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  virtual bool wasNull()
  {
    return _table == Nil;
  }
};

} // sqlite
} // sql 
} // acdk

#endif //acdk_sql_sqlite_LiteResultSetMetaData_h
