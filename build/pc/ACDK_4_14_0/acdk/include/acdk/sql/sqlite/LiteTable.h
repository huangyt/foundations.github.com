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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteTable.h,v 1.10 2005/04/13 15:38:05 kommer Exp $

#ifndef acdk_sql_sqlite_LiteTable_h
#define acdk_sql_sqlite_LiteTable_h

#include "Config.h"
#include <acdk.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/ResultSet.h>
#include <acdk/sql/ResultSetMetaData.h>
#include <acdk/lang/dmi/DmiDelegate.h>
#include <acdk/lang/UnsupportedOperationException.h>

#include "LiteConnection.h"

struct sqlite3;
struct sqlite3_stmt;

namespace acdk {
namespace sql {
namespace sqlite {


#define STR2LITESTR(str) str->convert(CCUtf8)
#define STR2LITESTR16(str) str->convert(CCUcs2)
#define LITESTR2STR(cptr) new String(cptr, CCUtf8 | NormalSST)

/**
  storage types of SQLite
*/
enum LiteColType
{
  SqlLiteInteger = 1,
  SqlLiteFloat = 2,
  SqlLiteText = 3,
  SqlLiteBlob = 4,
  SqlLiteNull = 5

};


ACDK_DECL_CLASS(LiteMemTable);
ACDK_DECL_CLASS(LiteTable);

ACDK_DECL_CLASS(LiteDb);

/**
  thin wrapper to the SQLite3 database
*/
class ACDK_SQL_SQLITE_PUBLIC LiteDb
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(LiteDb)
protected:
  foreign sqlite3* _con; 
public:
  static RLiteDb openDb(IN(RString) fileName);
  foreign LiteDb(sqlite3* con);
  //void reset();
  bool isClosed() { return _con != 0; }
  void closeDb();
  /**
    execute a sql command on a LiteDB
    The complete result set will be hold in memory
  */
  RLiteMemTable execDirect(IN(RString) sql);
  /**
    executes a SQL command
  */
  void execute(IN(RString) sql);
  /**
    executes a SQL command an calles the delegate for each record set
    The arguments passed to the delegate are:
    - LiteDb __db
    - all fields as String of one record.
    - The delete should return an int. if the return value is != 0 
      no more records will be fetched.
    See also samples in gw_ref[acdk_sql_sqlite_man].
  */
  void execute(IN(RString) sql, IN(acdk::lang::dmi::RDmiDelegate) delegate);
  /**
    execute update, return the number of changes
  */
  int  executeUpdate(IN(RString) cmd);
  /**
    creates a prepared statement
  */
  RLiteTable prepareStatement(IN(RString) sql);
  
  /**
    this creates a function can be used inside an SQL Statement.
    It is similar to stored procedure, but the function itself is 
    not stored within the database, but provided by the application
  */
  void createSqlFunction(IN(RString) function, int args, IN(acdk::lang::dmi::RDmiDelegate) delegate);
  /** 
    internal helper to check calls to the C api 
  */
  int checkResult(int result)
  {
    return checkResult(_con, result);
  }
  /** only call this if a prior call failed */
  void checkLastResult()
  {
    checkLastResult(_con);
  }
  foreign sqlite3* getConPtr() { return _con; }
  /** internal helper to check calls to the C api */
  foreign static int checkResult(sqlite3* con, int result);
  /** internal helper to check calls to the C api */
  foreign static void checkLastResult(sqlite3* con);
  /** sqlite3_changes */
  int changes();
  /** return the version string of the sqlite3 c library */
  static RString getVersion();
  /** internal helper */
  void _checkOpenDb();
};



/**
  Wrapper for the C Api of SQLite.
  Represents a result from a prepared statment
  In most cases this class will not be used in user code
  Different to JDBC, internal indeces for columns starts with 0
*/
class ACDK_SQL_SQLITE_PUBLIC LiteTable
: extends acdk::lang::Object
, implements acdk::util::Iterator
{
  ACDK_WITH_METAINFO(LiteTable)
public:
  RLiteDb _db;
  foreign sqlite3_stmt* _stm;
  int _currow;
  bool _rowDelivered;
  bool _hasRow;
  /** how many rows -1 means doesn't know */
  int _rowCount;
  foreign LiteTable(IN(RLiteDb) db, sqlite3_stmt* stm);
  foreign ~LiteTable();

  /** 
    for Iterator interface 
  */
  bool hasNext();
  /** 
    for Iterator interface
    @return this
  */
  RObject next();
  /** 
    for Iterator interface
    return this
  */
  virtual RObject element() { return this; }
  /**
    will throw UnsupportedOperationException
  */
  virtual void remove();
  bool getNext();
  void clearParameters();
  RLiteDb getDb() { return _db; }
  /** current row */
  int curRow() { return _currow; }
  RString getColName(int col);
  void reset();
  int colCount();
  int getColumnByName(IN(RString) name);
  bool getBoolean(int col);
  byte getByte(int col);
  short getShort(int col);
  int getInt(int col);
  jlong getLong(int col);
  float getFloat(int col);
  double getDouble(int col);
  RString getString(int col);
  RFlexByteBuffer getBlob(int col);
  /**
    -1 before first
    0 first
    1 - n absolute
    -2 last
    -3 after last
  */
  bool seek(int rowCount);
  /**
    returns update to last row index
  */
  int seekEnd();
  /**
    returns one of LiteColType
  */
  int getLiteType(int col);
  int getSqlType(int col);
  RString getSQLTypeName(int col);
  RString getColumnClassName(int col);

  /** bind index starts with 1 */
  void bindInt(int col, int val);
  void bindDouble(int col, double val);
  void bindLong(int col, jlong val);
  void bindText(int col, IN(RString) value);
  void bindNull(int col);
  void bindBlob(int col, IN(RReadByteBuffer) buffer);
  /** for Prepared statements */
  int getParameterCount();
  RString getParameterName(int col);
  int getParamterIndexByName(IN(RString) name);
  void checkLastResult()
  {
    LiteDb::checkLastResult(_db->getConPtr());
  }
  int checkResult(int res)
  {
    return LiteDb::checkResult(_db->getConPtr(), res);
  }
};

ACDK_DECL_CLASS(LiteMemTable);

/**
  Different to LiteTable LiteMemTable reads
  the complete result set from a query into memory
  All fields of a record will represented by a string
*/
class ACDK_SQL_SQLITE_PUBLIC LiteMemTable
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(LiteMemTable)
protected:
  foreign char **_table;
  int _cols;
  int _rows;
public:
  foreign LiteMemTable(char** table, int cols, int rows)
    : _table(table)
    , _cols(cols)
    , _rows(rows)
  {
  }
  foreign ~LiteMemTable();
  int columnCount() { return _cols; }
  int rowCount() { return _rows; }
  /**
    return the names of the columns 
  */
  RStringArray getColumnNames();
  /** idx starts with 1 */
  RString getColumnName(int col) 
  {
    col = _checkColIdx(col);
    return LITESTR2STR(_table[col]);
  }
  /** row starts with 0 and row starts with 1 */
  RString getField(int row, int col)
  {
    col = _checkColIdx(col);
    row = _checkRowIdx(row);
    return LITESTR2STR(_table[(row * _cols) + col]);
  }
  /** 
    return the maximum needed characters need to display all rows of a col 
    This also includes the column names
  */
  int getMaxColWidth(int col);
  RStringArray getRow(int row)
  {
    row = _checkRowIdx(row);
    RStringArray ra = new StringArray(_cols);
    for (int i = 0; i < _cols; ++i)
      ra[i] = LITESTR2STR(_table[(row * _cols) + i]);
    return ra;
  }
  /**
    returns LiteMemTableIterator
  */
  acdk::util::RIterator iterator();
  /**
    prints a table as ascii:
    <pre>
intcol|stringcol
-----------------
1     |first    
2     |second   
3     |third    
</pre>
*/
  void printTable(IN(acdk::io::RPrintWriter) out);
protected:
  foreign int _checkColIdx(int idx)
  {
    if (idx < 0  || idx > _cols)
      THROW1(SQLException, SBSTR("No column at index: " << idx));
    return idx - 1;
  }
  foreign int _checkRowIdx(int idx)
  {
    if (idx < 0 || idx > _rows)
      THROW1(SQLException, SBSTR("No row at index: " << idx));
    return idx + 1;
  }
};

ACDK_DECL_CLASS(LiteMemTableIterator);

/**
  Iterator adapter for a LiteMemTable
*/
class ACDK_SQL_SQLITE_PUBLIC LiteMemTableIterator
: extends acdk::lang::Object
, implements acdk::util::Iterator
{
  ACDK_WITH_METAINFO(LiteMemTableIterator)
protected:
  RLiteMemTable _table;
  int _row;
public:
  LiteMemTableIterator(IN(RLiteMemTable) table)
  : _table(table)
  , _row(-1)
  {
  }

  RString getColumnName(int col) 
  {
    return _table->getColumnName(col); 
  }
  RString getField(int col) 
  {
    return _table->getField(_row, col);
  }
  /**
    returns an iterator to a string array containing the fields of current row
  */
  acdk::util::RIterator iterator() 
  {
    return _table->getRow(_row)->iterator();
  }
  virtual bool hasNext()
  {
    if (_table->rowCount() == _row + 1)
      return false;
    return true;
  }
  /** 
    @return this
  */
  virtual RObject next()
  {
    ++_row;
    return this;
  }
  virtual RObject element()
  {
    return this;
  }
  virtual void remove() 
  {
    THROW1(UnsupportedOperationException, "LiteMemTableIterator::remove()");
  }

};

inline
acdk::util::RIterator 
LiteMemTable::iterator()
{
  return new LiteMemTableIterator(this);
}




} // sqlite
} // sql 
} // acdk

#endif //acdk_sql_sqlite_LiteTable_h
