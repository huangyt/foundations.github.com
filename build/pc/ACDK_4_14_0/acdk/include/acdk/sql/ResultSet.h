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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/ResultSet.h,v 1.17 2005/04/15 10:25:56 kommer Exp $

#ifndef acdk_sql_ResultSet_h
#define acdk_sql_ResultSet_h

#include <acdk.h>
#if !defined(ACDK_MINI)
#include <acdk/util/Date.h>
#include <acdk/util/Map.h>
#include "Time.h"
#else
#include <acdk/io/Reader.h>
#endif

#include <acdk/util/Map.h>
#include "Blob.h"

namespace acdk {
namespace sql {


ACDK_DECL_INTERFACE(ResultSet);

/** 
  API: JDBC 2.0, extended

  @author Roger Rene Kommer
  @version $Revision: 1.17 $
  @date $Date: 2005/04/15 10:25:56 $
*/
class ACDK_SQL_PUBLIC ResultSet
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ResultSet)
public:
  /**
    API: JDK
    JDKDOC: JDBC 2.0 The concurrency mode for a ResultSet object that may NOT be updated. 
  */
  static int CONCUR_READ_ONLY;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 The concurrency mode for a ResultSet object that may be updated. 
  */
  static int CONCUR_UPDATABLE;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 The rows in a result set will be processed in a forward direction; first-to-last. 
  */
  static int FETCH_FORWARD;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 The rows in a result set will be processed in a reverse direction; last-to-first. 
  */
  static int FETCH_REVERSE;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 The order in which rows in a result set will be processed is unknown. 
  */
  static int FETCH_UNKNOWN;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 The type for a ResultSet object whose cursor may move only forward. 
  */
  static int TYPE_FORWARD_ONLY;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 The type for a ResultSet object that is scrollable but generally not sensitive to changes made by others. 
  */
  static int TYPE_SCROLL_INSENSITIVE;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 The type for a ResultSet object that is scrollable and generally sensitive to changes made by others. 
  */
  static int TYPE_SCROLL_SENSITIVE;
          
  /** 
    gives a forward iterator wrapper to this ResultSet
  */
  acdk::util::RIterator iterator();
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the given row number in the result set. 
  */
  virtual bool absolute(int row) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the end of the result set, just after the last row. 
  */
  virtual void afterLast() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the front of the result set, just before the first row. 
  */
  virtual void beforeFirst() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Cancels the updates made to a row. 
  */
  virtual void cancelRowUpdates() = 0;
          
  /**
    API: JDK
    JDKDOC: After this call getWarnings returns null until a new warning is reported for this ResultSet. 
  */
  virtual void clearWarnings() = 0;
          
  /**
    API: JDK
    JDKDOC: Releases this ResultSet object's database and JDBC resources immediately instead of waiting for this to happen when it is automatically closed. 
  */
  virtual void close() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Deletes the current row from the result set and the underlying database. 
  */
  virtual void deleteRow() = 0;
          
  /**
    API: JDK
    JDKDOC: Maps the given Resultset column name to its ResultSet column index. 
  */
  virtual int findColumn(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the first row in the result set. 
  */
  virtual bool first() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets an SQL ARRAY value from the current row of this ResultSet object. 
  */
  virtual RArray getArray(int i) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets an SQL ARRAY value in the current row of this ResultSet object. 
  */
  virtual RArray getArray(INP(RString) colName) = 0;
          
  /**
    API: JDK modified
    JDKDOC: Gets the value of a column in the current row as a stream of ASCII characters. 
  */
  virtual acdk::io::RReader getAsciiStream(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a stream of ASCII characters. 
  */
  virtual acdk::io::RReader getAsciiStream(INP(RString) columnName) = 0;
          
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
  virtual acdk::io::RReader getBinaryStream(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a stream of uninterpreted bytes. 
  */
  virtual acdk::io::RReader getBinaryStream(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets a BLOB value in the current row of this ResultSet object. 
  */
  virtual RBlob getBlob(int i) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets a BLOB value in the current row of this ResultSet object. 
  */
  virtual RBlob getBlob(IN(RString) colName) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java boolean. 
  */
  virtual bool getBoolean(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java boolean.
  */
  virtual bool getBoolean(INP(RString) columnName) = 0;
           
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java byte. 
  */
  virtual byte getByte(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java byte. 
  */
  virtual byte getByte(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java byte array. 
  */
  virtual RbyteArray getBytes(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java byte array. 
  */
  virtual RbyteArray getBytes(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets the value of a column in the current row as a java.io.RReader. 
  */
  virtual acdk::io::RReader getCharacterStream(int columnIndex) = 0;
            
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets the value of a column in the current row as a java.io.RReader. 
  */
  virtual acdk::io::RReader getCharacterStream(INP(RString) columnName) = 0;
          
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
  virtual int getConcurrency() = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the name of the SQL cursor used by this ResultSet. 
  */
  virtual RString getCursorName() = 0;
#if !defined(ACDK_MINI)          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RDate object. 
  */
  virtual acdk::util::RDate getDate(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gets the value of a column in the current row as a java.sql.RDate object. 
  */
  //## not supported: virtual RDate getDate(int columnIndex, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RDate object. 
  */
  virtual acdk::util::RDate getDate(INP(RString) columnName) = 0;
#endif //#if !defined(ACDK_MINI)
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RDate object. 
  */
  //## not supported: virtual RDate getDate(RString columnName, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java double. 
  */
  virtual double getDouble(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java double. 
  */
  virtual double getDouble(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the fetch direction for this result set. 
  */
  virtual int getFetchDirection() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the fetch size for this result set. 
  */
  virtual int getFetchSize() = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java float. 
  */
  virtual float getFloat(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java float. 
  */
  virtual float getFloat(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java int. 
  */
  virtual int getInt(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java int. 
  */
  virtual int getInt(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java jlong. 
  */
  virtual jlong getLong(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java jlong. 
  */
  virtual jlong getLong(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: Retrieves the number, types and properties of a ResultSet's columns. 
  */
  virtual RResultSetMetaData getMetaData() = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java object. 
  */
  virtual RObject getObject(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the value of a column in the current row as a Java object. 
  */
  virtual RObject getObject(int i, INP(acdk::util::RMap) map) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java object. 
  */
  virtual RObject getObject(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the value in the specified column as a Java object. 
  */
  virtual RObject getObject(INP(RString) colName, INP(acdk::util::RMap) map) = 0;
          
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
  virtual int getRow() = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java short. 
  */
  virtual short getShort(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java short. 
  */
  virtual short getShort(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the Statement that produced this ResultSet object.
  */
  virtual RStatement getStatement() = 0;
           
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java RString. 
  */
  virtual RString getString(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a Java RString. 
  */
  virtual RString getString(INP(RString) columnName) = 0;
#if !defined(ACDK_MINI)          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RTime object. 
  */
  virtual RTime getTime(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RTime object. 
  */
  //## not supported: virtual RTime getTime(int columnIndex, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RTime object. 
  */
  virtual RTime getTime(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.RTime object. 
  */
  //## not supported: virtual RTime getTime(RString columnName, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.Timestamp object. 
  */
  virtual RTimestamp getTimestamp(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.Timestamp object. 
  */
  //## not supported: virtual RTimestamp getTimestamp(int columnIndex, RCalendar cal) = 0;
          
  /**
    API: JDK
    JDKDOC: Gets the value of a column in the current row as a java.sql.Timestamp object. 
  */
  virtual RTimestamp getTimestamp(INP(RString) columnName) = 0;
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
  virtual int getType() = 0;
          
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
  virtual RSQLWarning getWarnings() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Inserts the contents of the insert row into the result set and the database. 
  */
  virtual void insertRow() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the cursor is after the last row in the result set. 
  */
  virtual bool isAfterLast() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the cursor is before the first row in the result set. 
  */
  virtual bool isBeforeFirst() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the cursor is on the first row of the result set. 
  */
  virtual bool isFirst() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the cursor is on the last row of the result set. 
  */
  virtual bool isLast() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the last row in the result set. 
  */
  virtual bool last() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the remembered cursor position, usually the current row. 
  */
  virtual void moveToCurrentRow() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the insert row. 
  */
  virtual void moveToInsertRow() = 0;
          
  /**
    API: JDK
    JDKDOC: Moves the cursor down one row from its current position. 
  */
  virtual bool next() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor to the previous row in the result set. 
  */
  virtual bool previous() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Refreshes the current row with its most recent value in the database. 
  */
  virtual void refreshRow() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Moves the cursor a relative number of rows, either positive or negative. 
  */
  virtual bool relative(int rows) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether a row has been deleted. 
  */
  virtual bool rowDeleted() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the current row has had an insertion. 
  */
  virtual bool rowInserted() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Indicates whether the current row has been updated. 
  */
  virtual bool rowUpdated() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gives a hint as to the direction in which the rows in this result set will be processed. 
  */
  virtual void setFetchDirection(int direction) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gives the JDBC driver a hint as to the number of rows that should be fetched from the database when more rows are needed for this result set. 
  */
  virtual void setFetchSize(int rows) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an ascii stream value. 
  */
  virtual void updateAsciiStream(int columnIndex, INP(acdk::io::RReader) x, int length) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an ascii stream value. 
  */
  virtual void updateAsciiStream(INP(RString) columnName, INP(acdk::io::RReader) x, int length) = 0;
          
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
  virtual void updateBinaryStream(int columnIndex, INP(acdk::io::RReader) x, int length) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a binary stream value. 
  */
  virtual void updateBinaryStream(INP(RString) columnName, INP(acdk::io::RReader) x, int length) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a boolean value. 
  */
  virtual void updateBoolean(int columnIndex, bool x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a boolean value. 
  */
  virtual void updateBoolean(INP(RString) columnName, bool x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a byte value. 
  */
  virtual void updateByte(int columnIndex, byte x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a byte value. 
  */
  virtual void updateByte(INP(RString) columnName, byte x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a byte array value. 
  */
  virtual void updateBytes(int columnIndex, INP(RbyteArray) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a byte array value. 
  */
  virtual void updateBytes(INP(RString) columnName, INP(RbyteArray) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a character stream value. 
  */
  virtual void updateCharacterStream(int columnIndex, INP(acdk::io::RReader) x, int length) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a character stream value. 
  */
  virtual void updateCharacterStream(INP(RString) columnName, INP(acdk::io::RReader) reader, int length) = 0;
#if !defined(ACDK_MINI)          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RDate value. 
  */
  virtual void updateDate(int columnIndex, INP(acdk::util::RDate) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RDate value. 
  */
  virtual void updateDate(INP(RString) columnName, INP(acdk::util::RDate) x) = 0;
#endif //!defined(ACDK_MINI)
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a Double value. 
  */
  virtual void updateDouble(int columnIndex, double x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a double value. 
  */
  virtual void updateDouble(INP(RString) columnName, double x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a float value. 
  */
  virtual void updateFloat(int columnIndex, float x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a float value. 
  */
  virtual void updateFloat(INP(RString) columnName, float x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an integer value. 
  */
  virtual void updateInt(int columnIndex, int x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an integer value. 
  */
  virtual void updateInt(INP(RString) columnName, int x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a jlong value. 
  */
  virtual void updateLong(int columnIndex, jlong x) =0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a jlong value. 
  */
  virtual void updateLong(INP(RString) columnName, jlong x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Give a nullable column a null value. 
  */
  virtual void updateNull(int columnIndex) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a null value. 
  */
  virtual void updateNull(INP(RString) columnName) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an RObject value. 
  */
  virtual void updateObject(int columnIndex, INP(RObject) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an RObject value. 
  */
  virtual void updateObject(int columnIndex, INP(RObject) x, int scale) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an RObject value. 
  */
  virtual void updateObject(INP(RString) columnName, INP(RObject) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with an RObject value. 
  */
  virtual void updateObject(INP(RString) columnName, INP(RObject) x, int scale) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates the underlying database with the new contents of the current row. 
  */
  virtual void updateRow() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a short value. 
  */
  virtual void updateShort(int columnIndex, short x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a short value. 
  */
  virtual void updateShort(INP(RString) columnName, short x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RString value. 
  */
  virtual void updateString(int columnIndex, INP(RString) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RString value. 
  */
  virtual void updateString(INP(RString) columnName, INP(RString) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RTime value. 
  */
  virtual void updateTime(int columnIndex, INP(RTime) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a RTime value. 
  */
  virtual void updateTime(INP(RString) columnName, INP(RTime) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a Timestamp value. 
  */
  virtual void updateTimestamp(int columnIndex, INP(RTimestamp) x) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Updates a column with a Timestamp value. 
  */
  virtual void updateTimestamp(INP(RString) columnName, INP(RTimestamp) x) = 0;
          
  /**
    API: JDK
    JDKDOC: 
  */
  virtual bool wasNull() = 0;
};
    
ACDK_DECL_CLASS(ResultSetIterator);
/**
  Iterator wrapper for a ResultSet

*/
class ACDK_SQL_PUBLIC ResultSetIterator
: extends acdk::lang::Object
, implements acdk::util::Iterator
{
  ACDK_WITH_METAINFO(ResultSetIterator)
protected:
  RResultSet _rset;
public:
  ResultSetIterator(IN(RResultSet) rset)
    : _rset(rset)
  {
  }
  virtual bool hasNext() 
  {
    return  _rset->isLast();
  }
  /**
    return the ResultSet
  */
  virtual RObject next()
  {
    if (_rset->next() == false)
      return Nil;
    return (RObject)_rset;
  }
  /**
    return the ResultSet
  */
  virtual RObject element()
  {
    return (RObject)_rset;
  }
  virtual void remove()
  {
    _rset->deleteRow();
  }
};

inline
acdk::util::RIterator 
ResultSet::iterator()
{
  return new ResultSetIterator(this);
}


} // sql
} // acdk

#endif //acdk_sql_ResultSet_h

