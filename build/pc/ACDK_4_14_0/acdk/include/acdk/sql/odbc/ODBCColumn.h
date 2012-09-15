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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCColumn.h,v 1.9 2005/04/06 19:03:45 kommer Exp $

#ifndef acdk_sqlodbc_Column_h
#define acdk_sqlodbc_Column_h

#include "odbc.h"
#include <acdk/lang/UnsupportedOperationException.h>

#include <acdk/sql/Types.h>
#include <acdk/sql/Blob.h>
#include <acdk/sql/ResultSetMetaData.h>

// following includes are needed for VC to compile odbc_clazzinfo.cpp
#include <acdk/lang/Number.h>
#include <acdk/util/Map.h>
//#include <acdk/util/Date.h>
#include <acdk/sql/Time.h>
#include <acdk/sql/Timestamp.h>
#include <acdk/sql/DriverPropertyInfo.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;


enum AttributeFlag 
{
    AF_invalid = 0,
    AF_valid = 1,
    AF_failed = 2
};
ACDK_DEF_LIB_ENUM(ACDK_SQL_ODBC_PUBLIC, AttributeFlag);

enum AttributeID 
{
    AI_autoIncrement = SQL_COLUMN_AUTO_INCREMENT,
    AI_caseSensitive = SQL_COLUMN_CASE_SENSITIVE,
    AI_displaySize = SQL_COLUMN_DISPLAY_SIZE,
    AI_fixedPrecScale = SQL_COLUMN_MONEY,
    AI_nullable = SQL_COLUMN_NULLABLE,
    /* hopefully length contains the right value before odbc3.x */
    AI_octetLength = SQL_COLUMN_LENGTH, 
    AI_precision = SQL_COLUMN_PRECISION,
    AI_scale = SQL_COLUMN_SCALE,
    AI_searchable = SQL_COLUMN_SEARCHABLE,
    AI_columnName = SQL_COLUMN_NAME,
    AI_catalogName = SQL_COLUMN_QUALIFIER_NAME, // SQL_DESC_CATALOG_NAME
    AI_columnLabel = SQL_COLUMN_LABEL,
    AI_schemaName = SQL_COLUMN_OWNER_NAME, // SQL_DESC_SCHEMA_NAME
    AI_tableName = SQL_COLUMN_TABLE_NAME,
    AI_columnType = SQL_COLUMN_TYPE,
    AI_columnTypeName = SQL_COLUMN_TYPE_NAME,
    AI_unsigned = SQL_COLUMN_UNSIGNED,
    AI_writable = SQL_COLUMN_UPDATABLE
};
ACDK_DEF_LIB_ENUM(ACDK_SQL_ODBC_PUBLIC, AttributeID);

ACDK_DECL_CLASS(ODBCColumn);

class ACDK_SQL_ODBC_PUBLIC ODBCColumn
: extends Object
{
  ACDK_WITH_METAINFO(ODBCColumn)

  
private:
  //cyclic refernence RODBCResultSet _rset;
  RODBCHandle __hndl; // cached from ResultSet
  int _col;
  
  RObject _obj;

  virtual bool _getIntegerAttribute(int attrID, int& value);
  virtual bool _getStringAttribute(int attrID, RString& value);
  virtual bool _getBooleanAttribute(int attrID, bool& value);
  virtual bool _getAttribute(AttributeID attrID);
  virtual RObject _getData();
  virtual RNumber _getNumber();
  void _chkAttr(AttributeID attrID);
  void _chkNull();

// any *Flag-var is used to determine if the value is set by an ODBC-call or
// is set to a reasonable default (eg. UNKNOWN). it'll be set to false, if
// JDK have no equivalent value for the ODBC-value defined.
// the String-vars doesn't need this, because they'll be equal Nil in these cases.
// to use Integer//Boolean-Objects for the flags may be a bit overdosed...

  bool _autoIncrement;
  AttributeFlag _autoIncrementFlag;
  //RString _baseColumnName;
  //RString _baseTableName;
  bool _caseSensitive;
  AttributeFlag _caseSensitiveFlag;
  RString _catalogName;
  AttributeFlag _catalogNameFlag;
  //RInteger _conciseType;
  //RInteger _columnCount;
  int _displaySize;
  AttributeFlag _displaySizeFlag;
  bool _fixedPrecScale; // true for monetary value and similar
  AttributeFlag _fixedPrecScaleFlag;
  RString _columnLabel;
  AttributeFlag _columnLabelFlag;
  //RInteger _length; // maximum for fixed-length, actual for variable-length
  //RString _literalPrefix;
  //RString _literalSuffix;
  //RString _localeTypeName;
  RString _columnName;
  AttributeFlag _columnNameFlag;
  ::acdk::sql::Nullable _nullable;
  AttributeFlag _nullableFlag;
  //RInteger _precRadix;
  int _octetLength;  // not available via JDK-API, but used internally
  AttributeFlag _octetLengthFlag;
  int _precision;
  AttributeFlag _precisionFlag;
  int _scale;
  AttributeFlag _scaleFlag;
  RString _schemaName;
  AttributeFlag _schemaNameFlag;
  bool _searchable;
  AttributeFlag _searchableFlag;
  RString _tableName;
  AttributeFlag _tableNameFlag;
  ::acdk::sql::SQLType _columnType;
  AttributeFlag _columnTypeFlag;
  RString _columnTypeName;
  AttributeFlag _columnTypeNameFlag;
  //RInteger _named;
  bool _unsigned;
  AttributeFlag _unsignedFlag;
  bool _writable; // via UPDATABLE
  AttributeFlag _writableFlag;
  bool _wasNull;
  
  virtual RODBCHandle _getODBCHandle() THROWS1(::acdk::sql::RSQLException);
  ODBCColumn() {}
public:
  ODBCColumn(INP(RODBCHandle) handle, int col);

  ~ODBCColumn();

  virtual void _zeroData()
  {
    _obj = Nil; 
    _autoIncrementFlag = AF_invalid;
    _caseSensitiveFlag = AF_invalid;
    _catalogNameFlag = AF_invalid;
    _displaySizeFlag = AF_invalid;
    _fixedPrecScaleFlag = AF_invalid;
    _columnLabelFlag = AF_invalid;
    _columnNameFlag = AF_invalid;
    _nullableFlag = AF_invalid;
    _octetLengthFlag = AF_invalid;
    _precisionFlag = AF_invalid;
    _scaleFlag = AF_invalid;
    _schemaNameFlag = AF_invalid;
    _searchableFlag = AF_invalid;
    _tableNameFlag = AF_invalid;
    _columnTypeFlag = AF_invalid;
    _columnTypeNameFlag = AF_invalid;
    _unsignedFlag = AF_invalid;
    _writableFlag = AF_invalid;
  }
  virtual RNumber getNumber();

// from ResultSet:

  virtual RArray getArray() { THROW0(UnsupportedOperationException); return Nil; }
  virtual acdk::io::RReader getAsciiStream() { THROW0(UnsupportedOperationException); return Nil; }
  //## not supported: virtual RBigDecimal getBigDecimal() = 0;
  virtual acdk::io::RReader getBinaryStream() { THROW0(UnsupportedOperationException); return Nil; }
  virtual RBlob getBlob();
  virtual bool getBoolean();
  virtual byte getByte();
  virtual RbyteArray getBytes();
  virtual acdk::io::RReader getCharacterStream() { THROW0(UnsupportedOperationException); return Nil; }
  //## not supported: virtual RClob getClob() = 0;
  virtual int getConcurrency() { THROW0(UnsupportedOperationException); return Nil; }
  virtual RString getCursorName() { THROW0(UnsupportedOperationException); return Nil; }
#if !defined(ACDK_MINI)
  virtual acdk::util::RDate getDate();
#endif //!defined(ACDK_MINI)
  //## not supported: virtual RDate getDate(RCalendar cal) = 0;
  virtual double getDouble();
  virtual float getFloat();
  virtual int getInt();
  virtual jlong getLong();
  virtual RObject getObject() { if (_obj == Nil) _getData(); if (_wasNull == true) return Nil; return _obj; }
  virtual RObject getObject(INP(acdk::util::RMap) map) { THROW0(UnsupportedOperationException); return Nil; }
  //## not supported: virtual RRef getRef() = 0;
  virtual short getShort();
  virtual RString getString();
#if !defined(ACDK_MINI)
  virtual RTime getTime();

  //## not supported: virtual RTime getTime(RCalendar cal) = 0;
  virtual RTimestamp getTimestamp();
#endif //!defined(ACDK_MINI)
  //## not supported: virtual RTimestamp getTimestamp(RCalendar cal) = 0;
  virtual void updateAsciiStream(INP(acdk::io::RReader) x, int length) { THROW0(UnsupportedOperationException); }
  //## not supported: virtual void updateBigDecimal(RBigDecimal x) = 0;
  virtual void updateBinaryStream(INP(acdk::io::RReader) x, int length) { THROW0(UnsupportedOperationException);  }
  virtual void updateBoolean(bool x) { THROW0(UnsupportedOperationException);  }
  virtual void updateByte(byte x) { THROW0(UnsupportedOperationException); }
  virtual void updateBytes(INP(RbyteArray) x) { THROW0(UnsupportedOperationException); }
  virtual void updateCharacterStream(INP(acdk::io::RReader) reader, int length) { THROW0(UnsupportedOperationException);  }
#if !defined(ACDK_MINI)
  virtual void updateDate(INP(acdk::util::RDate) x) { THROW0(UnsupportedOperationException); }
#endif
  virtual void updateDouble(double x) { THROW0(UnsupportedOperationException); }
  virtual void updateFloat(float x) { THROW0(UnsupportedOperationException);  }
  virtual void updateInt(int x) { THROW0(UnsupportedOperationException);  }
  virtual void updateLong(jlong x) { THROW0(UnsupportedOperationException); }
  virtual void updateNull() { THROW0(UnsupportedOperationException); }
  virtual void updateObject(INP(RObject) x, int scale) { THROW0(UnsupportedOperationException);  }
  virtual void updateShort(short x) { THROW0(UnsupportedOperationException);  }
  virtual void updateString(INP(RString) x) { THROW0(UnsupportedOperationException);  }
#if !defined(ACDK_MINI)
  virtual void updateTime(INP(RTime) x) { THROW0(UnsupportedOperationException); }
  virtual void updateTimestamp(INP(RTimestamp) x) { THROW0(UnsupportedOperationException); }
#endif //!defined(ACDK_MINI)
  virtual bool wasNull() { return _wasNull; }

// from ResultSetMetaData:
  virtual bool isAutoIncrement()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_autoIncrement); return _autoIncrement; }
  virtual bool isCaseSensitive()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_caseSensitive); return _caseSensitive; }
  virtual bool isSearchable()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_searchable); return _searchable; }
  virtual bool isCurrency()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_fixedPrecScale); return _fixedPrecScale; }
  virtual int isNullable()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_nullable); return _nullable; }
  virtual bool isSigned()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_unsigned); return !(_unsigned); }
  virtual int getColumnDisplaySize()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_displaySize); return _displaySize; }
  virtual RString getColumnLabel()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_columnLabel); return _columnLabel; }
  virtual RString getColumnName()  THROWS1(::acdk::sql::RSQLException);
  virtual RString getSchemaName()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_schemaName); return _schemaName; }
  virtual int getPrecision()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_precision); return _precision; }
  virtual int getScale()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_scale); return _scale; }
  virtual RString getTableName()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_tableName); return _tableName; }
  virtual RString getCatalogName()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_catalogName); return _catalogName; }
  virtual ::acdk::sql::SQLType getColumnType()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_columnType); return _columnType; }
  virtual RString getColumnTypeName()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_columnTypeName); return _columnTypeName; }
  virtual bool isReadOnly()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_writable); return !(_writable); }
  virtual bool isWritable()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_writable); return _writable; }
  virtual bool isDefinitelyWritable()  THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual RString getColumnClassName()  THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual int getColumnCount();
   // merely for debugging-purposes
  virtual int getLength()  THROWS1(::acdk::sql::RSQLException) { _chkAttr(AI_octetLength); return _octetLength; }
};

} // odbc
} // sql
} // acdk

#endif // acdk_sqlodbc_Column_h
