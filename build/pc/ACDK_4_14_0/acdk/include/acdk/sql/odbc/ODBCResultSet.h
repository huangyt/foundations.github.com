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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCResultSet.h,v 1.7 2005/04/06 19:03:45 kommer Exp $

#ifndef acdk_sqlodbc_ResultSet_h
#define acdk_sqlodbc_ResultSet_h

#include "odbc.h"
#include "ODBCStatement.h"
#include "ODBCColumn.h"

#include <acdk/sql/ResultSet.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

ACDK_DECL_CLASS(ODBCResultSet);

class ACDK_SQL_ODBC_PUBLIC ODBCResultSet
: extends Object
, implements ::acdk::sql::ResultSet
{
  ACDK_WITH_METAINFO(ODBCResultSet)
public:
   ~ODBCResultSet();
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the given row number in the result set. 
  */
  virtual bool absolute(int row);
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the end of the result set, just after the last row. 
  */
  virtual void afterLast() { absolute(_numrows + 1); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the front of the result set, just before the first row. 
  */
  virtual void beforeFirst() { absolute(-_numrows - 1); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Cancels the updates made to a row. 
  */
  virtual void cancelRowUpdates() { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: After this call getWarnings returns null until a new warning is reported for this ResultSet. 
  */
  virtual void clearWarnings() { if (_getODBCHandle() != Nil) _getODBCHandle()->_clearWarnings(); }
          
  /**
    API: JDK
    JDKDOC: Releases this ResultSet object's database and JDBC resources immediately instead of waiting for this to happen when it is automatically closed. 
  */
  virtual void close();
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Deletes the current row from the result set and the underlying database. 
  */
  virtual void deleteRow();
          
  /**
    API: JDK
    JDKDOC: Maps the given Resultset column name to its ResultSet column index. 
  */
  virtual int findColumn(INP(RString) columnName) { return _getColumnIndex(columnName); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the first row in the result set. 
  */
  virtual bool first() THROWS1(::acdk::sql::RSQLException);
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets an SQL ARRAY value from the current row of this ResultSet object. 
  */
  virtual RArray getArray(int i) { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets an SQL ARRAY value in the current row of this ResultSet object. 
  */
  virtual RArray getArray(INP(RString) colName) { return getArray(findColumn(colName)); }
          
  /**
    API: JDK modified
    JDKDOC: Gets the value of a column in the current row as a stream of ASCII characters. 
  */
  virtual acdk::io::RReader getAsciiStream(int columnIndex) { THROW0(UnsupportedOperationException);return Nil;  }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a stream of ASCII characters. 
  */
  virtual acdk::io::RReader getAsciiStream(INP(RString) columnName) { return getAsciiStream(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets the value of a column in the current row as a java.math.BigDecimal object with full precision. 
  */
  //## not supported: virtual RBigDecimal getBigDecimal(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Deprecated.   
  */
  //## not supported: virtual RBigDecimal getBigDecimal(int columnIndex, int scale) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets the value of a column in the current row as a java.math.BigDecimal object with full precision. 
  */
  //## not supported: virtual RBigDecimal getBigDecimal(RString columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: Deprecated.   
  */
  //## not supported: virtual RBigDecimal getBigDecimal(RString columnName, int scale) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a stream of uninterpreted bytes. 
  */
  virtual acdk::io::RReader getBinaryStream(int columnIndex) { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a stream of uninterpreted bytes. 
  */
  virtual acdk::io::RReader getBinaryStream(INP(RString) columnName) { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets a BLOB value in the current row of this ResultSet object. 
  */
  virtual RBlob getBlob(int columnIndex) { return _getColumn(columnIndex)->getBlob(); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets a BLOB value in the current row of this ResultSet object. 
  */
  virtual RBlob getBlob(INP(RString) columnName) { return getBlob(findColumn(columnName));  }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java boolean. 
  */
  virtual bool getBoolean(int columnIndex) { return _getColumn(columnIndex)->getBoolean();  }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java boolean.
  */
  virtual bool getBoolean(INP(RString) columnName) { return getBoolean(findColumn(columnName)); }
           
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java byte. 
  */
  virtual byte getByte(int columnIndex) { return _getColumn(columnIndex)->getByte(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java byte. 
  */
  virtual byte getByte(INP(RString) columnName) { return getByte(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java byte array. 
  */
  virtual RbyteArray getBytes(int columnIndex) { return _getColumn(columnIndex)->getBytes(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java byte array. 
  */
  virtual RbyteArray getBytes(INP(RString) columnName) { return getBytes(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets the value of a column in the current row as a java.io.RReader. 
  */
  virtual acdk::io::RReader getCharacterStream(int columnIndex) { THROW0(UnsupportedOperationException); return Nil; }
            
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets the value of a column in the current row as a java.io.RReader. 
  */
  virtual acdk::io::RReader getCharacterStream(INP(RString) columnName) { return getCharacterStream(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets a CLOB value in the current row of this ResultSet object. 
  */
  //## not supported: virtual RClob getClob(int i) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets a CLOB value in the current row of this ResultSet object. 
  */
  //## not supported: virtual RClob getClob(RString colName) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the concurrency mode of this result set. 
  */
  virtual int getConcurrency() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Gets the name of the SQL cursor used by this ResultSet. 
  */
  virtual RString getCursorName() { THROW0(UnsupportedOperationException); return Nil; }
#if !defined(ACDK_MINI)
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RDate object. 
  */
  virtual acdk::util::RDate getDate(int columnIndex) { return _getColumn(columnIndex)->getDate(); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets the value of a column in the current row as a java.sql.RDate object. 
  */
  //## not supported: virtual RDate getDate(int columnIndex, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RDate object. 
  */
  virtual acdk::util::RDate getDate(INP(RString) columnName) { return getDate(findColumn(columnName)); }
#endif //!defined(ACDK_MINI)
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RDate object. 
  */
  //## not supported: virtual RDate getDate(RString columnName, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java double. 
  */
  virtual double getDouble(int columnIndex) { return _getColumn(columnIndex)->getDouble(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java double. 
  */
  virtual double getDouble(INP(RString) columnName) { return getDouble(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the fetch direction for this result set. 
  */
  virtual int getFetchDirection() { return _direction; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the fetch size for this result set. 
  */
  virtual int getFetchSize() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java float. 
  */
  virtual float getFloat(int columnIndex) { return _getColumn(columnIndex)->getFloat(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java float. 
  */
  virtual float getFloat(INP(RString) columnName) { return getFloat(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java int. 
  */
  virtual int getInt(int columnIndex) { return _getColumn(columnIndex)->getInt(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java int. 
  */
  virtual int getInt(INP(RString) columnName) { return getInt(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java jlong. 
  */
  virtual jlong getLong(int columnIndex) { return _getColumn(columnIndex)->getLong(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java jlong. 
  */
  virtual jlong getLong(INP(RString) columnName) { return getLong(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: Retrieves the number, types and properties of a ResultSet's columns. 
  */
  virtual RResultSetMetaData getMetaData();
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java object. 
  */
  virtual RObject getObject(int columnIndex) { return _getColumn(columnIndex)->getObject(); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the value of a column in the current row as a Java object. 
  */
  virtual RObject getObject(int i, INP(acdk::util::RMap) map) { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java object. 
  */
  virtual RObject getObject(INP(RString) columnName) { return getObject(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the value in the specified column as a Java object. 
  */
  virtual RObject getObject(INP(RString) columnName, INP(acdk::util::RMap) map) { return getObject(findColumn(columnName), map); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets a REF(<structured-type>) column value from the current row. 
  */
  //## not supported: virtual RRef getRef(int i) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets a REF(<structured-type>) columnvalue from the current row.  
  */
  //## not supported: virtual RRef getRef(RString colName) = 0;
           
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Retrieves the current row number. 
  */
  virtual int getRow() { return _cursorpos; }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java short. 
  */
  virtual short getShort(int columnIndex) { return _getColumn(columnIndex)->getShort(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java short. 
  */
  virtual short getShort(INP(RString) columnName) { return getShort(findColumn(columnName)); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the Statement that produced this ResultSet object.
  */
  virtual RStatement getStatement();
           
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java RString. 
  */
  virtual RString getString(int columnIndex) { return _getColumn(columnIndex)->getString(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java RString. 
  */
  virtual RString getString(INP(RString) columnName) { return getString(findColumn(columnName)); }
#if !defined(ACDK_MINI)          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RTime object. 
  */
  virtual RTime getTime(int columnIndex) { return _getColumn(columnIndex)->getTime(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RTime object. 
  */
  //## not supported: virtual RTime getTime(int columnIndex, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RTime object. 
  */
  virtual RTime getTime(INP(RString) columnName) { return getTime(findColumn(columnName)); }

  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RTime object. 
  */
  //## not supported: virtual RTime getTime(RString columnName, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.Timestamp object. 
  */
  virtual RTimestamp getTimestamp(int columnIndex) { return _getColumn(columnIndex)->getTimestamp(); }
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.Timestamp object. 
  */
  //## not supported: virtual RTimestamp getTimestamp(int columnIndex, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.Timestamp object. 
  */
  virtual RTimestamp getTimestamp(INP(RString) columnName) { return getTimestamp(findColumn(columnName)); }
#endif //!defined(ACDK_MINI)                
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.Timestamp object. 
  */
  //## not supported: virtual RTimestamp getTimestamp(RString columnName, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the type of this result set. 
  */
  virtual int getType() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: Deprecated.   
  */
  //## not supported: virtual RReader getUnicodeStream(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Deprecated.   
  */
  //## not supported: virtual RReader getUnicodeStream(RString columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: The first warning reported by calls on this ResultSet is returned. 
  */
  virtual ::acdk::sql::RSQLWarning getWarnings();
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Inserts the contents of the insert row into the result set and the database. 
  */
  virtual void insertRow() { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the cursor is after the last row in the result set. 
  */
  virtual bool isAfterLast() { return (_cursorpos > _numrows)? true : false; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the cursor is before the first row in the result set. 
  */
  virtual bool isBeforeFirst() { return (_cursorpos < 1)? true : false; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the cursor is on the first row of the result set. 
  */
  virtual bool isFirst() { return (_cursorpos == 1)? true : false; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the cursor is on the last row of the result set. 
  */
  virtual bool isLast() { return (_cursorpos == _numrows)? true : false; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the last row in the result set. 
  */
  virtual bool last() { return absolute(_numrows); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the remembered cursor position, usually the current row. 
  */
  virtual void moveToCurrentRow() { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the insert row. 
  */
  virtual void moveToInsertRow() { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: Moves the cursor down one row from its current position. 
  */
  virtual bool next() THROWS1(::acdk::sql::RSQLException);
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the previous row in the result set. 
  */
  virtual bool previous() { return relative( (_direction == FETCH_REVERSE)? 1 : -1 ); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Refreshes the current row with its most recent value in the database. 
  */
  virtual void refreshRow();
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor a relative number of rows, either positive or negative. 
  */
  virtual bool relative(int rows) { return (_cursorpos > -rows)? absolute(_cursorpos + rows) : absolute(-_numrows - 1); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether a row has been deleted. 
  */
  virtual bool rowDeleted() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the current row has had an insertion. 
  */
  virtual bool rowInserted() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the current row has been updated. 
  */
  virtual bool rowUpdated() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gives a hint as to the direction in which the rows in this result set will be processed. 
  */
  virtual void setFetchDirection(int direction);
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gives the JDBC driver a hint as to the number of rows that should be fetched from the database when more rows are needed for this result set. 
  */
  virtual void setFetchSize(int rows) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an ascii stream value. 
  */
  virtual void updateAsciiStream(int columnIndex, INP(acdk::io::RReader) x, int length) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an ascii stream value. 
  */
  virtual void updateAsciiStream(INP(RString) columnName, INP(acdk::io::RReader) x, int length) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a BigDecimal value. 
  */
  //## not supported: virtual void updateBigDecimal(int columnIndex, RBigDecimal x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a BigDecimal value. 
  */
  //## not supported: virtual void updateBigDecimal(RString columnName, RBigDecimal x) = 0;
          
  /**
    API: JDK modified
    JDKDOC: JDBC 2.0 Updates a column with a binary stream value. 
  */
  virtual void updateBinaryStream(int columnIndex, INP(acdk::io::RReader) x, int length) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a binary stream value. 
  */
  virtual void updateBinaryStream(INP(RString) columnName, INP(acdk::io::RReader) x, int length) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a boolean value. 
  */
  virtual void updateBoolean(int columnIndex, bool x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a boolean value. 
  */
  virtual void updateBoolean(INP(RString) columnName, bool x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a byte value. 
  */
  virtual void updateByte(int columnIndex, byte x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a byte value. 
  */
  virtual void updateByte(INP(RString) columnName, byte x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a byte array value. 
  */
  virtual void updateBytes(int columnIndex, INP(RbyteArray) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a byte array value. 
  */
  virtual void updateBytes(INP(RString) columnName, INP(RbyteArray) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a character stream value. 
  */
  virtual void updateCharacterStream(int columnIndex, INP(acdk::io::RReader) x, int length) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a character stream value. 
  */
  virtual void updateCharacterStream(INP(RString) columnName, INP(acdk::io::RReader) reader, int length) { THROW0(UnsupportedOperationException); }
#if !defined(ACDK_MINI)                  
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RDate value. 
  */
  virtual void updateDate(int columnIndex, INP(acdk::util::RDate) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RDate value. 
  */
  virtual void updateDate(INP(RString) columnName, INP(acdk::util::RDate) x) { THROW0(UnsupportedOperationException); }
#endif         
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a Double value. 
  */
  virtual void updateDouble(int columnIndex, double x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a double value. 
  */
  virtual void updateDouble(INP(RString) columnName, double x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a float value. 
  */
  virtual void updateFloat(int columnIndex, float x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a float value. 
  */
  virtual void updateFloat(INP(RString) columnName, float x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an integer value. 
  */
  virtual void updateInt(int columnIndex, int x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an integer value. 
  */
  virtual void updateInt(INP(RString) columnName, int x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a jlong value. 
  */
  virtual void updateLong(int columnIndex, jlong x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a jlong value. 
  */
  virtual void updateLong(INP(RString) columnName, jlong x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Give a nullable column a null value. 
  */
  virtual void updateNull(int columnIndex) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a null value. 
  */
  virtual void updateNull(INP(RString) columnName) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an RObject value. 
  */
  virtual void updateObject(int columnIndex, INP(RObject) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an RObject value. 
  */
  virtual void updateObject(int columnIndex, INP(RObject) x, int scale) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an RObject value. 
  */
  virtual void updateObject(INP(RString) columnName, INP(RObject) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an RObject value. 
  */
  virtual void updateObject(INP(RString) columnName, INP(RObject) x, int scale) { THROW0(UnsupportedOperationException); }
                    
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a short value. 
  */
  virtual void updateShort(int columnIndex, short x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a short value. 
  */
  virtual void updateShort(INP(RString) columnName, short x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RString value. 
  */
  virtual void updateString(int columnIndex, INP(RString) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RString value. 
  */
  virtual void updateString(INP(RString) columnName, INP(RString) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RTime value. 
  */
  virtual void updateTime(int columnIndex, INP(RTime) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RTime value. 
  */
  virtual void updateTime(INP(RString) columnName, INP(RTime) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a Timestamp value. 
  */
  virtual void updateTimestamp(int columnIndex, INP(RTimestamp) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a Timestamp value. 
  */
  virtual void updateTimestamp(INP(RString) columnName, INP(RTimestamp) x) { THROW0(UnsupportedOperationException); }
          
  /**
    API: JDK
    JDKDOC: 
  */
  virtual bool wasNull() { return (_getColumnCount() == 0)? true : false; }

  void updateRow();

  ODBCResultSet(INP(RODBCStatement) stmt, int direction);
  virtual int _getColumnCount() THROWS1(::acdk::sql::RSQLException);
  virtual RODBCColumn _getColumn(int index) THROWS1(::acdk::sql::RSQLException) 
  { 
    _chkindex(index); 
    return _colDesc[index - 1]; 
  }
  int _getColumnIndex(INP(RString) colname) THROWS1(::acdk::sql::RSQLException);
  
private:
  RODBCStatement _stmt;  
  /**
    cached from Statement
  */
  RODBCHandle __hndl; 
  int _direction;
  int _numrows;
  bool _numrowsValid;
  /**
    we need an internal representation of the cursor-position
  */
  int _cursorpos;  
  bool _fetched; // true if row at _cursorpos is fetched
  bool _valid; // true if _cursorpos points to a existant row.
  RODBCColumnArray _colDesc;
  //int _skipNextCount;
  //cyclic reference RODBCResultSetMetaData _rsmd;
  
  virtual void _chkindex(int index) THROWS1(::acdk::sql::RSQLException);
  virtual void _getNumRows();
public:
  virtual RODBCHandle _getODBCHandle() THROWS1(::acdk::sql::RSQLException);
};
    


} // odbc
} // sql
} // acdk

#endif //acdk_sqlodbc_ResultSet_h

