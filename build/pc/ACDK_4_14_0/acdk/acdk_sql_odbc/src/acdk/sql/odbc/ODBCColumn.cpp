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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCColumn.cpp,v 1.17 2005/04/06 19:03:44 kommer Exp $

#include "ODBCColumn.h"
#include "ODBCResultSet.h"
#include "ODBCResultSetMetaData.h"
#include "ODBCStatement.h"
#include "ODBCHandle.h"
#include "ODBCConnection.h"
#include "ODBCDriver.h"

#include <acdk/sql/SQLException.h>
#include <acdk/sql/SQLWarning.h>

#include <acdk/lang/Number.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Boolean.h>
#if !defined(ACDK_MINI)        
#include <acdk/util/Date.h>
#include <acdk/sql/Time.h>
#include <acdk/sql/Timestamp.h>
#endif
#include <acdk/util/Properties.h>
#if !defined(ACDK_MINI)
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Thread.h>
#endif //!defined(ACDK_MINI)
#include <acdk/sql/Types.h>
#include <acdk/util/logging/Log.h>

#include <acdk/lang/System.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;


ODBCColumn::ODBCColumn(INP(RODBCHandle) handle, int col) 
: __hndl(handle)
, _col(col)
, _obj(Nil),
   _autoIncrementFlag(AF_invalid), _caseSensitiveFlag(AF_invalid), _catalogNameFlag(AF_invalid),
   _displaySizeFlag(AF_invalid), _fixedPrecScaleFlag(AF_invalid), _columnLabelFlag(AF_invalid),
   _columnNameFlag(AF_invalid), _nullableFlag(AF_invalid), _octetLengthFlag(AF_invalid),
   _precisionFlag(AF_invalid), _scaleFlag(AF_invalid), _schemaNameFlag(AF_invalid),
   _searchableFlag(AF_invalid), _tableNameFlag(AF_invalid), _columnTypeFlag(AF_invalid),
   _columnTypeNameFlag(AF_invalid), _unsignedFlag(AF_invalid), _writableFlag(AF_invalid) 
{
}

ODBCColumn::~ODBCColumn()
{
  ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCColumn::~ODBCColumn");
}

RString 
ODBCColumn::getColumnName()  THROWS1(::acdk::sql::RSQLException) 
{ 
  _chkAttr(AI_columnName); 
  return _columnName; 
}

RODBCHandle
ODBCColumn::_getODBCHandle() THROWS1(RSQLException)
{
  return __hndl; 
  /*
  if (__hndl == Nil) 
  {
    __hndl = (_rset != Nil) ? _rset->_getODBCHandle() : RODBCHandle(Nil);
    if (__hndl == Nil) {
      THROW1(SQLException, "no handle");
    }
  }
  return __hndl;
  */
}

void
ODBCColumn::_chkNull()
{
  RODBCHandle hndl = _getODBCHandle();
  if (_wasNull == true) {
    // no possibility to return a base value which represents SQLNULL
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException("object was null.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
}

// less information is available via:
//
// SQLRETURN SQLDescribeCol(SQLHSTMT StatementHandle, SQLSMALLINT ColumnNumber, SQLCHAR//ColumnName, SQLSMALLINT BufferLength,
//                          SQLSMALLINT//NameLengthPtr, SQLSMALLINT//DataTypePtr, SQLUINTEGER//ColumnSizePtr, SQLSMALLINT//DecimalDigitsPtr,
//                          SQLSMALLINT//NullablePtr);

// but we use this one:
//
// SQLRETURN SQLColAttribute(SQLHSTMT StatementHandle, SQLUSMALLINT ColumnNumber, SQLUSMALLINT FieldIdentifier, SQLPOINTER CharacterAttributePtr,
//                           SQLSMALLINT BufferLength, SQLSMALLINT//StringLengthPtr, SQLPOINTER NumericAttributePtr);
// FieldIdentifier can be one of:
//  SQL_DESC_AUTO_UNIQUE_VALUE  SQL_TRUE, SQL_FALSE
//  SQL_DESC_BASE_COLUMN_NAME   The base column name for the result set column.
//  SQL_DESC_BASE_TABLE_NAME    The name of the base table that contains the column.
//  SQL_DESC_CASE_SENSITIVE     SQL_TRUE, SQL_FALSE
//  SQL_DESC_CATALOG_NAME       The catalog of the table that contains the column.
//  SQL_DESC_CONCISE_TYPE       The concise data type.
//  SQL_DESC_COUNT              The number of columns available in the result set.
//  SQL_DESC_DISPLAY_SIZE       Maximum number of characters required to display data from the column.
//  SQL_DESC_FIXED_PREC_SCALE   SQL_TRUE, SQL_FALSE
//  SQL_DESC_LABEL              The column label or title.
//  SQL_DESC_LENGTH             A numeric value that is either the maximum or actual character length of a character string or binary data type. 
//  SQL_DESC_LITERAL_PREFIX     This VARCHAR(128) record field contains the character or characters that the driver recognizes as a prefix for a literal of this data type.
//  SQL_DESC_LITERAL_SUFFIX     This VARCHAR(128) record field contains the character or characters that the driver recognizes as a suffix for a literal of this data type.
//  SQL_DESC_LOCAL_TYPE_NAME    This VARCHAR(128) record field contains any localized (native language) name for the data type that may be different from the regular name of the data type.
//  SQL_DESC_NAME               The column alias, if it applies. If the column alias does not apply, the column name is returned.
//  SQL_DESC_NULLABLE           SQL_NULLABLE, SQL_NO_NULLS, SQL_NULLABLE_UNKNOWN
//  SQL_DESC_NUM_PREC_RADIX     2 for SQL_DESC_PRECISION in bits, 10 for SQL_DESC_PRECISION in digits
//  SQL_DESC_OCTET_LENGTH       The length, in bytes, of a character string or binary data type.
//  SQL_DESC_PRECISION          A numeric value that for a numeric data type denotes the applicable precision.
//  SQL_DESC_SCALE              For DECIMAL and NUMERIC data types, this is the defined scale. It is undefined for all other data types.
//  SQL_DESC_SCHEMA_NAME        The schema of the table that contains the column
//  SQL_DESC_SEARCHABLE         SQL_PRED_NONE, SQL_PRED_CHAR, SQL_PRED_BASIC, SQL_PRED_SEARCHABLE
//  SQL_DESC_TABLE_NAME         The name of the table that contains the column
//  SQL_DESC_TYPE               A numeric value that specifies the SQL data type
//  SQL_DESC_TYPE_NAME          Data source dependent data type name
//  SQL_DESC_UNNAMED            SQL_NAMED, SQL_UNNAMED 
//  SQL_DESC_UNSIGNED           SQL_TRUE, SQL_FALSE
//  SQL_DESC_UPDATABLE          SQL_ATTR_READONLY, SQL_ATTR_WRITE, SQL_ATTR_READWRITE_UNKNOWN

// before ODBCVER 0x300 SQLColAttribute didn't exist, instead SQLColAttributes did.
// the argument-types are similar (transparently mappable), but the attribute-IDs
// changed completely...
// unfortunately not every flag has a similar name and some mappings are not clear.
// Note: all defines below are only for internal use and should never be moved into a header. 

#if ODBCVER < 0x300

# define SQLColAttribute SQLColAttributes

# define SQL_PRED_NONE        SQL_UNSEARCHABLE
# define SQL_PRED_CHAR        SQL_LIKE_ONLY
# define SQL_PRED_BASIC       SQL_ALL_EXCEPT_LIKE
# define SQL_PRED_SEARCHABLE  SQL_SEARCHABLE


// following types have similar SQL_COLUMN_* defines, but may return other data:

// SQL_DESC_COUNT                  1001
// SQL_DESC_TYPE                   1002
// SQL_DESC_LENGTH                 1003
// SQL_DESC_PRECISION              1005
// SQL_DESC_SCALE                  1006
// SQL_DESC_NULLABLE               1008
// SQL_DESC_NAME                   1011

// SQL_COLUMN_COUNT                0
// SQL_COLUMN_NAME                 1
// SQL_COLUMN_TYPE                 2
// SQL_COLUMN_LENGTH               3
// SQL_COLUMN_PRECISION            4
// SQL_COLUMN_SCALE                5
// SQL_COLUMN_NULLABLE             7


// win32-includes of VC has following direct-mappings:

//  SQL_DESC_AUTO_UNIQUE_VALUE	->	SQL_COLUMN_AUTO_INCREMENT
//  SQL_DESC_CATALOG_NAME	->	SQL_COLUMN_QUALIFIER_NAME
//  SQL_DESC_CONCISE_TYPE	->	SQL_COLUMN_TYPE
//  SQL_DESC_FIXED_PREC_SCALE	->	SQL_COLUMN_MONEY
//  SQL_DESC_SCHEMA_NAME	->	SQL_COLUMN_OWNER_NAME

#endif // ODBCVER

// known JDBC types:
//    BigInt, Binary, Bit, Char, Date, Time, TimeStamp, Decimal, Double, Float, Integer,
//    LongVarBinary, LongVarChar, Numeric, Real, SmallInt, TinyInt, VarBinary, VarChar

#define MAP_SQL(s,m) \
  case s: \
    _columnType = ::acdk::sql::  m; \
    return true;
#define NOMAP_SQL(s) \
  case s: // fall through

#define MDBCTYPENAME(t) \
  case ::acdk::sql::  t: \
    _columnTypeName = SCS( #t ); \
    return true;

#define MDBC_ATTR(attr, type) \
  case AI ## attr: \
    if (attr ## Flag == AF_valid) \
      return true; \
    if (attr ## Flag == AF_failed) \
      return false; \
    if (_get ## type ## Attribute(AI ## attr, attr) == true) { \
      attr ## Flag = AF_valid; \
      return true; \
    } \
    attr ## Flag = AF_failed; \
    return false; \
    break;

#define MDBC_BOOLATTR(attr) \
  MDBC_ATTR(attr, Boolean)

#define MDBC_INTATTR(attr) \
  MDBC_ATTR(attr, Integer)

#define MDBC_STRINGATTR(attr) \
  MDBC_ATTR(attr, String)

bool
ODBCColumn::_getAttribute(AttributeID attrID)
{
  RODBCHandle hndl = _getODBCHandle();
  int tmp = 0;
  bool valid;
#if defined(ACDK_ODBC_DEBUG)
  System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getAttr(col=" + Integer::toString(_col) + ", id=" + Integer::toString(attrID) + ")");
#endif
  switch (attrID) {

    MDBC_BOOLATTR(_autoIncrement);
    //_getStringAttribute(SQL_DESC_BASE_COLUMN_NAME);
    //_getStringAttribute(SQL_DESC_BASE_TABLE_NAME);
    MDBC_BOOLATTR(_caseSensitive);
    MDBC_STRINGATTR(_catalogName);
    //_getIntegerAttribute(SQL_DESC_CONCISE_TYPE);
    //_columnCount = _getIntegerAttribute(SQL_COLUMN_COUNT);
    MDBC_INTATTR(_displaySize);
    //_getBooleanAttribute(SQL_COLUMN_MONEY);	// SQL_DESC_FIXED_PREC_SCALE
    MDBC_STRINGATTR(_columnLabel);

    MDBC_BOOLATTR(_fixedPrecScale);
    //_getStringAttribute(SQL_DESC_LITERAL_PREFIX);
    //_getStringAttribute(SQL_DESC_LITERAL_SUFFIX);
    //_getStringAttribute(SQL_DESC_LOCAL_TYPE_NAME);
    MDBC_STRINGATTR(_columnName);
    case (AI_nullable):
      if (_nullableFlag == AF_failed)
        return false;
      if (_nullableFlag == AF_valid)
        return true;
      valid = _getIntegerAttribute(attrID, tmp);
      if (valid == 1) {
        _nullableFlag = AF_valid;
        switch (tmp) {
          case SQL_NULLABLE:
            _nullable = ::acdk::sql::ColumnNullable;
            return true;
          case SQL_NO_NULLS:
            _nullable = ::acdk::sql::ColumnNoNulls;
            return true;
          case SQL_NULLABLE_UNKNOWN:
            _nullable = ::acdk::sql::ColumnNullableUnknown;
            return true;
          default:
            RString msg = RString("value ") + ::acdk::lang::Integer::toString(tmp) + " unknown for attribute(nullable)" + " in column " + Integer::toString(_col) + ".";
            hndl->_addWarning(new SQLWarning(msg));
            _nullable = ::acdk::sql::ColumnNullableUnknown;
            _nullableFlag = AF_failed;
            return false;
        }
      }
      _nullableFlag = AF_failed;
      _nullable = ::acdk::sql::ColumnNullableUnknown;
      return false;

  //_getIntegerAttribute(SQL_DESC_NUM_PREC_RADIX);
  MDBC_INTATTR(_octetLength);
  MDBC_INTATTR(_precision);
  MDBC_INTATTR(_scale);
  MDBC_STRINGATTR(_schemaName);
    case AI_searchable:
      if (_searchableFlag == AF_failed)
        return false;
      if (_searchableFlag == AF_valid)
        return true;
      valid = _getIntegerAttribute(attrID, tmp);
      if (valid != 0) 
      {
        _searchableFlag = AF_valid;
        switch (tmp) {
          case SQL_PRED_NONE: // SQL_UNSEARCHABLE
            _searchable = false;
            return true;
          case SQL_PRED_CHAR: // SQL_LIKE_ONLY
            _searchable = false; // should this be true, too?
            return true;
          case SQL_PRED_BASIC: // SQL_EXCEPT_LIKE
            _searchable = false; // should this be true, too?
            return true;
          case SQL_PRED_SEARCHABLE:
            _searchable = true;
            return true;
          default:
            RString msg = RString("value ") + ::acdk::lang::Integer::toString(tmp) + " unknown for attribute(searchable)" + " in column " + Integer::toString(_col) + ".";
            hndl->_addWarning(new SQLWarning(msg));
            _searchable = false;
            _searchableFlag = AF_failed;
            return true;
            break;
        }
      }
      _searchable = false;
      _searchableFlag = AF_failed;
      return false;

  MDBC_STRINGATTR(_tableName);
    case AI_columnType:
#if defined(ACDK_ODBC_DEBUG)
    System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getAttr() flag for type is " + Integer::toString(_columnTypeFlag) + ")");
#endif
      if (_columnTypeFlag == AF_failed)
        return false;
      if (_columnTypeFlag == AF_valid)
        return true;
      valid = _getIntegerAttribute(attrID, tmp);
#if defined(ACDK_ODBC_DEBUG)
    System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getAttr() sql returns " + (valid != 0)? "true" : "false" + Integer::toString(tmp) + ")");
#endif
      if (valid == true) {
        _columnTypeFlag = AF_valid;
        switch (tmp) {
        
// basic SQL types (direct mapped)

          // SQL_UNKNOWN_TYPE moved to end.
      
          MAP_SQL(SQL_CHAR, CharSqlType)
          MAP_SQL(SQL_NUMERIC, NumericSqlType)
          MAP_SQL(SQL_DECIMAL, DecimalSqlType)
          MAP_SQL(SQL_INTEGER, IntegerSqlType)
          MAP_SQL(SQL_SMALLINT, SmallIntSqlType)
          MAP_SQL(SQL_FLOAT, FloatSqlType)
          MAP_SQL(SQL_REAL, RealSqlType)
          MAP_SQL(SQL_DOUBLE, DoubleSqlType)
        
          MAP_SQL(SQL_VARCHAR, VarCharSqlType)

// three new types since ODBC 3.x

          MAP_SQL(SQL_TYPE_DATE, DateSqlType)
          MAP_SQL(SQL_TYPE_TIME, TimeSqlType)
          MAP_SQL(SQL_TYPE_TIMESTAMP, TimeStampSqlType)
      
// extended SQL types

// following three types should be mapped internally by 3.x-drivers to basic types SQL_TYPE_*, but sometimes they aren't 

          MAP_SQL(SQL_DATE, OldDateSqlType)
          MAP_SQL(SQL_TIME, OldTimeSqlType)
          MAP_SQL(SQL_TIMESTAMP, OldTimeStampSqlType)
      
          MAP_SQL(SQL_LONGVARCHAR, LongVarCharSqlType)
          MAP_SQL(SQL_BINARY, BinarySqlType)
          MAP_SQL(SQL_VARBINARY, VarBinarySqlType)
          MAP_SQL(SQL_LONGVARBINARY, LongVarBinarySqlType)
          MAP_SQL(SQL_BIGINT, BigIntSqlType)
          MAP_SQL(SQL_TINYINT, TinyIntSqlType)
          MAP_SQL(SQL_BIT, BitSqlType)
#if (ODBCVER >= 0x0300)
          
// following types for unicode or similar wide-chars and are equal to those without a `W´ since ODBC 3.x
#if defined(ACDK_HAS_SQL_WCHAR)
          MAP_SQL(SQL_WCHAR, WCharSqlType)
          MAP_SQL(SQL_WLONGVARCHAR, WLongVarCharSqlType)
          MAP_SQL(SQL_WVARCHAR, WVarCharSqlType)
#endif
#endif
#if (ODBCVER >= 0x0300) && defined(ACDK_OS_WIN32)
          NOMAP_SQL(SQL_GUID)
#endif // ODBCVER
          NOMAP_SQL(SQL_INTERVAL_YEAR)
          NOMAP_SQL(SQL_INTERVAL_MONTH)
          NOMAP_SQL(SQL_INTERVAL_DAY)
          NOMAP_SQL(SQL_INTERVAL_HOUR)
          NOMAP_SQL(SQL_INTERVAL_MINUTE)
          NOMAP_SQL(SQL_INTERVAL_SECOND)
          NOMAP_SQL(SQL_INTERVAL_YEAR_TO_MONTH)
          NOMAP_SQL(SQL_INTERVAL_DAY_TO_HOUR)
          NOMAP_SQL(SQL_INTERVAL_DAY_TO_MINUTE)
          NOMAP_SQL(SQL_INTERVAL_DAY_TO_SECOND)
          NOMAP_SQL(SQL_INTERVAL_HOUR_TO_MINUTE)
          NOMAP_SQL(SQL_INTERVAL_HOUR_TO_SECOND)
          NOMAP_SQL(SQL_INTERVAL_MINUTE_TO_SECOND)
          case SQL_UNKNOWN_TYPE:
          default:
            RString msg = RString("value ") + ::acdk::lang::Integer::toString(tmp) + " unknown or unhandled for attribute(type)" + " in column " + Integer::toString(_col) + ".";
            hndl->_addWarning(new SQLWarning(msg));
            _columnTypeFlag = AF_failed;
            return false;
        }
      }
      _columnType = ::acdk::sql::NullSqlType; // as defined there, but should be of type unknown!
      _columnTypeFlag = AF_failed;
      return false;

    case AI_columnTypeName:

      if (_columnTypeNameFlag == AF_failed)
        return false;
      if (_columnTypeNameFlag == AF_valid)
        return true;

      // we don't use _getStringAttribute() to get _columnTypeName, because we're not interested
      // in the name used by the driver, but the type's MDBC-Name.
      // _columnTypeName = _getStringAttribute(SQL_DESC_TYPE_NAME);

      if (_getAttribute(AI_columnType) == true) {
        _columnTypeNameFlag = AF_valid;
        switch (_columnType) 
        {
          MDBCTYPENAME(CharSqlType)
          MDBCTYPENAME(NumericSqlType)
          MDBCTYPENAME(DecimalSqlType)
          MDBCTYPENAME(IntegerSqlType)
          MDBCTYPENAME(SmallIntSqlType)
          MDBCTYPENAME(FloatSqlType)
          MDBCTYPENAME(RealSqlType)
          MDBCTYPENAME(DoubleSqlType)
          MDBCTYPENAME(VarCharSqlType)
          MDBCTYPENAME(WVarCharSqlType)
          MDBCTYPENAME(DateSqlType)
          MDBCTYPENAME(TimeSqlType)
          MDBCTYPENAME(TimeStampSqlType)
          MDBCTYPENAME(OldDateSqlType)
          MDBCTYPENAME(OldTimeSqlType)
          MDBCTYPENAME(OldTimeStampSqlType)
          MDBCTYPENAME(LongVarCharSqlType)
          MDBCTYPENAME(BinarySqlType)
          MDBCTYPENAME(VarBinarySqlType)
          MDBCTYPENAME(LongVarBinarySqlType)
          MDBCTYPENAME(BigIntSqlType)
          MDBCTYPENAME(TinyIntSqlType)
          MDBCTYPENAME(BitSqlType)
          default:
            _columnTypeNameFlag = AF_failed;
            _columnTypeName = Nil;
            RString msg = RString("invalid value for columnType (") + ::acdk::lang::Integer::toString(tmp) + ") in column " + Integer::toString(_col) + ".";
            ::acdk::sql::RSQLException ex = new SQLException(msg);
            hndl->_addException(ex);
            SQLTHROW(hndl->_getExceptions());
            return false;
        }
      }
      _columnTypeNameFlag = AF_failed;
      _columnTypeName = Nil;
      return false;
  
  //_getIntegerAttribute(SQL_DESC_UNNAMED);
  MDBC_BOOLATTR(_unsigned);

    case AI_writable:
      if (_writableFlag == AF_failed)
        return false;
      if (_writableFlag == AF_valid)
        return true;
      valid = _getIntegerAttribute(attrID, tmp);
      if (valid != 0) {
        _writableFlag = AF_valid;
        switch (tmp) {
          case SQL_ATTR_READONLY:
            _writable = false;
            break;
          case SQL_ATTR_WRITE:
            _writable = true;
            break;
          case SQL_ATTR_READWRITE_UNKNOWN:
            _writable = false;
            //_writableFlag = AF_failed;
            break;
          default:
            RString msg = RString("value ") + ::acdk::lang::Integer::toString(tmp) + " unknown for attribute(updatable)" + " in column " + Integer::toString(_col) + ".";
            hndl->_addWarning(new SQLWarning(msg));
            _writable = false;
            _writableFlag = AF_failed;
            return false;
        }
      }
      _writableFlag = AF_failed;
      _writable = false;
      return false;

    default:
      RString msg = RString("invalid AttributeID (") + ::acdk::lang::Integer::toString(attrID) + ") in column " + Integer::toString(_col) + ".";
      ::acdk::sql::RSQLException ex = new SQLException(msg);
      hndl->_addException(ex);
      SQLTHROW(hndl->_getExceptions());
  }
  return false;
}

#undef MDBC_ATTR
#undef MDBC_INTATTR
#undef MDBC_BOOLATTR
#undef MDBC_STRINGATTR
#undef MAP_SQL
#undef NOMAP_SQL
#undef MDBCTYPENAME

bool
ODBCColumn::_getStringAttribute(int attrID, RString& value)
{
  SQLRETURN ret;
  SQLCHAR res[4097] = { 0 };
  SQLSMALLINT len = 0;  // do we need this?
  RODBCHandle hndl = _getODBCHandle();

  // special handling for MS SQL
  if (attrID == AI_columnName)
  {
    SQLRETURN ret = SQLColAttribute(hndl->_getSQLHandle(), _col, attrID, res, sizeof(res), &len, 0);
    if (ret == SQL_SUCCESS)
    {
      value = ODBCSTR2STR(res, len);
      return true;
    }
    ret = SQLColAttribute(hndl->_getSQLHandle(), _col, AI_columnLabel, res, sizeof(res), &len, 0);
    if (ret == SQL_SUCCESS)
    {
      value = ODBCSTR2STR(res, len);
      return true;
    }
    // give up and let default call throw the exception
  }
  ret = callSQL6(hndl, SQLColAttribute, _col, attrID, res, sizeof(res), &len, 0);
  if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
    return false;
  value = ODBCSTR2STR(res, len);
  return true;
}

bool
ODBCColumn::_getBooleanAttribute(int attrID, bool& value)
{
  SQLRETURN ret;
  SQLINTEGER res;
  RODBCHandle hndl = _getODBCHandle();
  ret = callSQL6(hndl, SQLColAttribute, _col, attrID, 0, 0, 0, &res);
  if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
    return false;
  value = (res != 0);
  return true;
}

bool
ODBCColumn::_getIntegerAttribute(int attrID, int& value)
{
  SQLRETURN ret;
  SQLINTEGER res = 0;
  RODBCHandle hndl = _getODBCHandle();
  ret = callSQL6(hndl, SQLColAttribute, _col, attrID, 0, 0, 0, &res);
  if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
    return false;
  value = res;
  return true;
}

void
ODBCColumn::_chkAttr(AttributeID attrID)
{
  RODBCHandle hndl = _getODBCHandle();
  if (_getAttribute(attrID) == false) {
    ::acdk::sql::RSQLException ex = new SQLException(RString("no value for requested item (") + Integer::toString(attrID) + ") in Column " + Integer::toString(_col) + ".");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
}

//virtual
int
ODBCColumn::getColumnCount()
{
  RODBCHandle hndl = _getODBCHandle();
  int res;

#if defined(ACDK_OS_WIN32)
  if (_getIntegerAttribute(SQL_DESC_COUNT, res) == false) {
#else
  if (_getIntegerAttribute(SQL_COLUMN_COUNT, res) == false) {
#endif
    ::acdk::sql::RSQLException ex = new SQLException("no value for column-count.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  return res;
}


#define MKDATA(__dbt, __tt, __sz, __res) \
  case ::acdk::sql::  __dbt: \
    type = __tt; \
    len = __sz; \
    res = __res; \
    break;

#define VALDATA(__dbt, __tt) \
  MKDATA(__dbt, __tt, 0, &vRes)

#define BUFDATA(__dbt, __var) \
  MKDATA(__dbt, SQL_C_CHAR, sizeof(bRes.  __var), &bRes)

#define XVARDATA(__dbt) \
  case ::acdk::sql::  __dbt: \
    _chkAttr(AI_octetLength); \
    varbuf = new byteArray(_octetLength + 1); \
    type = SQL_C_CHAR; \
    len = _octetLength + 1; \
    res = varbuf->data(); \
    break;
  
#define MK_NRESULT(__dbt, __ot, __dat) \
  case ::acdk::sql::  __dbt: \
    if (_wasNull == true) { \
      vRes.__dat = 0; \
    } \
    _obj = new __ot(vRes.__dat); \
    break;

#define MK_SRESULT(__dbt) \
  case ::acdk::sql::  __dbt: \
    if (_wasNull == true) { \
      _obj = new String(""); \
    } else { \
      _obj = (RObject)SCS(reinterpret_cast<char *>(varbuf->data())); \
    } \
    break;
#if defined(_UNICODE)
#define MK_WSRESULT(__dbt) \
  case ::acdk::sql::  __dbt: \
    if (_wasNull == true) { \
      _obj = new String(""); \
    } else { \
      _obj = (RObject)ODBCSTR2STR(varbuf->data(), varbuf->length()); \
    } \
    break;
#else
#define MK_WSRESULT(__dbt) \
  case ::acdk::sql:: __dbt: \
    if (_wasNull == true) { \
      _obj = new String(""); \
    } else { \
      _obj = new String(reinterpret_cast<char*>(varbuf->data()), NormalSST | CCAscii); \
    } \
    break;
#endif
#define MK_BRESULT(__dbt) \
  case ::acdk::sql:: __dbt: \
    if (_wasNull == true) { \
      _obj = new byteArray((const unsigned char *)(""), 0); \
    } else { \
      _obj = new byteArray(varbuf->data(), len_ind); \
    } \
    break;

// SQLRETURN SQLGetData(SQLHSTMT StatementHandle, SQLUSMALLINT ColumnNumber, SQLSMALLINT TargetType, SQLPOINTER TargetValuePtr, 
//                      SQLINTEGER BufferLength, SQLINTEGER *StrLen_or_IndPtr);

RObject
ODBCColumn::_getData()
{
  union {
    char byteval;
    short shortval;
    long longval;
    float floatval;
    double doubleval;
  } vRes;

  union {
    char jlongbuf[39];  // 20 digits (or 19 + sign) + perhaps terminating 0, but oracle uses decimal with a size of 38
    DATE_STRUCT datebuf;
    TIME_STRUCT timebuf;
    TIMESTAMP_STRUCT timestampbuf; 
#if ODBCVER >= 0x0300
    SQL_NUMERIC_STRUCT numericbuf;
#if defined(ACDK_OS_WIN32)
    SQLGUID guidbuf;
#endif // ACDK_OS_WIN32
    SQL_INTERVAL_STRUCT intervalbuf;
#endif // ODBCVER
  } bRes;
  
  RbyteArray varbuf;

  SQLPOINTER res;  
  int type, len = 0;
  SQLINTEGER len_ind;

  RODBCHandle hndl = _getODBCHandle();
  
#if defined(ACDK_ODBC_DEBUG)
  System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getData(" + Integer::toString(_col) + ")");
#endif
  _chkAttr(AI_columnType);
#if defined(ACDK_ODBC_DEBUG) 
  System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getData() type is " + Integer::toString(_columnType));
  _chkAttr(AI_columnName);
  System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getData() type is " + _columnName);
#endif

  switch (_columnType) {
    
    VALDATA(TinyIntSqlType, SQL_C_UTINYINT);
    VALDATA(BitSqlType, SQL_C_BIT);
    VALDATA(SmallIntSqlType, SQL_C_USHORT);
    VALDATA(IntegerSqlType, SQL_C_ULONG);
    VALDATA(RealSqlType, SQL_C_FLOAT);
    VALDATA(FloatSqlType, SQL_C_DOUBLE);
    VALDATA(DoubleSqlType, SQL_C_DOUBLE);
    
    BUFDATA(BigIntSqlType, jlongbuf);
    BUFDATA(DateSqlType, datebuf);
    BUFDATA(TimeSqlType, timebuf);
    BUFDATA(TimeStampSqlType, timestampbuf);
    BUFDATA(OldDateSqlType, datebuf);
    BUFDATA(OldTimeSqlType, timebuf);
    BUFDATA(OldTimeStampSqlType, timestampbuf);
//    BUFDATA(Numeric, numericbuf);
    BUFDATA(NumericSqlType, jlongbuf);
//    BUFDATA(Decimal, decimalbuf);
    BUFDATA(DecimalSqlType, jlongbuf);
    
    XVARDATA(CharSqlType);
    XVARDATA(VarCharSqlType);
    XVARDATA(LongVarCharSqlType);
#if !defined(ACDK_MINI)
    XVARDATA(WLongVarCharSqlType);
    XVARDATA(WVarCharSqlType);
    XVARDATA(WCharSqlType);
#endif //!defined(ACDK_MINI)
    XVARDATA(BinarySqlType);
    XVARDATA(VarBinarySqlType);
    XVARDATA(LongVarBinarySqlType);
    
    default:
      ::acdk::sql::RSQLException ex = new SQLException(RString("unknown or unhandled column-type (") + Integer::toString(_columnType) + ") in column " + Integer::toString(_col) + ".");
      hndl->_addException(ex);
      SQLTHROW(hndl->_getExceptions());
      break;
  }
  callSQL5(hndl, SQLGetData, _col, type, res, len, &len_ind);
  _wasNull = false;
#if defined(ACDK_ODBC_DEBUG)
  System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getData() lenInd is " + Integer::toString(len_ind));
#endif
  switch (len_ind) {
    case SQL_NTS: // 0-terminated string, just do nothing
      break;
    case SQL_NULL_DATA:
      _wasNull = true;
      break;
    case SQL_NO_TOTAL:  // should this be an exception?
      {
        ::acdk::sql::RSQLWarning wg = new SQLWarning(RString("driver can't determine number of available bytes for long data in column ") + Integer::toString(_col) + ".");
        hndl->_addWarning(wg);
      }
      return Nil;
    case SQL_DATA_AT_EXEC:  // invalid for receiving results
    case SQL_DEFAULT_PARAM: // invalid for receiving results
    case SQL_COLUMN_IGNORE: // invalid for receiving results
    default:
      if (len_ind < 0) {
        RString msg = RString("unknown or invalid length indicator for getting data (") + ::acdk::lang::Integer::toString(len_ind) + ") in column " + Integer::toString(_col) + ".";
        ::acdk::sql::RSQLException ex = new SQLException(msg);
        hndl->_addException(ex);
        SQLTHROW(hndl->_getExceptions());
        return Nil;
      }
      // maybe we should check here, if len_ind equals len for all char[]
      break;
  }
  
  switch (_columnType) {
    
    MK_NRESULT(TinyIntSqlType, ::acdk::lang::Byte, byteval)
    MK_NRESULT(BitSqlType, ::acdk::lang::Byte, byteval)
    MK_NRESULT(SmallIntSqlType, ::acdk::lang::Short, shortval)
    MK_NRESULT(IntegerSqlType, ::acdk::lang::Integer, longval)
    MK_NRESULT(RealSqlType, ::acdk::lang::Float, floatval)
    MK_NRESULT(FloatSqlType, ::acdk::lang::Double, doubleval)
    MK_NRESULT(DoubleSqlType, ::acdk::lang::Double, doubleval)
    
//  (Numeric, ::acdk::math::BigDecimal)
//  (Decimal, ::acdk::math::BigDecimal)
    case ::acdk::sql::NumericSqlType:
    case ::acdk::sql::DecimalSqlType:
    case ::acdk::sql::BigIntSqlType:
      if (_wasNull == true) {
        _obj = new Long(jlong(0));
      } else {
        try {
          ::acdk::lang::RString tmp = SCS(bRes.jlongbuf);
	          tmp = tmp->replace(',', '.');
	          if (tmp->indexOf('.') >= 0)
              _obj = new ::acdk::lang::Double(::acdk::lang::Double::parseDouble(tmp));
	          else
            _obj = new ::acdk::lang::Long(::acdk::lang::Long::parseLong(tmp));
        } catch(RNumberFormatException) {
          RString msg = RString("value \"") + bRes.jlongbuf + "\" in column " + Integer::toString(_col) + " can't be converted into a numeric value.";
          ::acdk::sql::RSQLException ex = new SQLException(msg);
          hndl->_addException(ex);
          SQLTHROW(hndl->_getExceptions());
        }
      }
      break;
#if !defined(ACDK_MINI)
    case ::acdk::sql::DateSqlType:
    case ::acdk::sql::OldDateSqlType:
      if (_wasNull == true) {
        _obj = new ::acdk::util::Date(0, 0, 0);
      } else {
        _obj = new ::acdk::util::Date(bRes.datebuf.year, bRes.datebuf.month, bRes.datebuf.day); // deprecated constructor of Date!!!
      }
      break;

    case ::acdk::sql::TimeSqlType:
    case ::acdk::sql::OldTimeSqlType:
      if (_wasNull == true) 
      {
        _obj = new class acdk::sql::Time(0, 0, 0);
      } else {
        _obj = new class acdk::sql::Time(bRes.timebuf.hour, bRes.timebuf.minute, bRes.timebuf.second); // uses deprecated constructor of Date!!!
      }
      break;
      
    case ::acdk::sql::TimeStampSqlType:
    case ::acdk::sql::OldTimeStampSqlType:
      if (_wasNull == true) {
        _obj = new ::acdk::sql::Timestamp(0, 0, 0, 0, 0, 0, 0);
      } else {
        _obj = new ::acdk::sql::Timestamp(bRes.timestampbuf.year, bRes.timestampbuf.month, bRes.timestampbuf.day, 
                                          bRes.timestampbuf.hour, bRes.timestampbuf.minute, bRes.timestampbuf.second, 
                                          bRes.timestampbuf.fraction); // uses deprecated constructor of Date!!!
      }
      break;
#endif
    case ::acdk::sql::CharSqlType:
    case ::acdk::sql::VarCharSqlType:
    case ::acdk::sql::LongVarCharSqlType:
      if (_wasNull == true) 
      { 
        _obj = &String::emptyString(); 
      } 
      else 
      { 
        _obj = (RObject)SCS(reinterpret_cast<char *>(varbuf->data()));
      } 
      break;
#if !defined(ACDK_MINI)
    case ::acdk::sql::WCharSqlType: 
    case ::acdk::sql::WVarCharSqlType: 
    case ::acdk::sql::WLongVarCharSqlType:
      if (_wasNull == true) { 
        _obj = &String::emptyString(); 
      } else { 
        byte* d = varbuf->data();
        _obj = ODBCSTR2STR(varbuf->data(), -1); 
      } 
      break;
    //MK_WSRESULT(WVarChar)
    
#endif //!defined(ACDK_MINI)
    MK_BRESULT(BinarySqlType)
    MK_BRESULT(VarBinarySqlType)
    MK_BRESULT(LongVarBinarySqlType)

    default:
      ::acdk::sql::RSQLException ex = new SQLException(RString("unknown or unhandled column-type (") + Integer::toString(_columnType) + ") in column " + Integer::toString(_col) + ".");
      hndl->_addException(ex);
      SQLTHROW(hndl->_getExceptions());
      break;
  }
#if defined(ACDK_ODBC_DEBUG)
  if (_obj != Nil)
    System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getData() object is of type " + _obj->getName());
  else
    System::err->println(RString("SQLODBC [") + Integer::toString(Thread::currentThreadId().threadID()) + "]: getData() object is of type NIL");
#endif
  if (_obj == Nil) {
    ::acdk::sql::RSQLException ex = new SQLException(RString("can't get data for column ") + Integer::toString(_col) + ".");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  return _obj;
}

#undef XVARDATA
#undef BUFDATA
#undef VALDATA
#undef MKDATA

#undef MK_NRESULT
#undef MK_SRESULT
#undef MK_BRESULT

//virtual
RNumber
ODBCColumn::_getNumber()
{
  RNumber num;

  if (_obj == Nil)
    _getData();

  if (instanceof(_obj, Number) == false) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into an number.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  num = RNumber(_obj); // should work without an exception and should return value != Nil
  if (num == Nil) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("can't make a number out of object in column ") + Integer::toString(_col) + ".");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  return num;
}

//virtual
RNumber
ODBCColumn::getNumber()
{
  RNumber num;

  num = _getNumber();

  if (_wasNull == true)
    return Nil;
  return num;
}

//virtual
int
ODBCColumn::getInt()
{
  int res;
  
  try {
    res = _getNumber()->intValue();
  } catch (Exception nsme) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into an integer.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  //_chkNull();
  if (_wasNull == true)
    return 0;
  return res;
}

//virtual
byte
ODBCColumn::getByte()
{
  byte res;
  
  try {
    res = _getNumber()->byteValue();
  } catch (Exception nsme) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into an boolean.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  //_chkNull();
  if (_wasNull == true)
    return 0;
  return res;
}

//virtual
double
ODBCColumn::getDouble()
{
  double res;

  try {
    res = _getNumber()->doubleValue();
  } catch (Exception nsme) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into an double.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  //_chkNull();
  if (_wasNull == true)
    return 0;
  return res;
}

//virtual
float
ODBCColumn::getFloat()
{
  float res;
  
  try {
    res = _getNumber()->floatValue();
  } catch (Exception nsme) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into an float.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  //_chkNull();
  if (_wasNull == true)
    return 0;
  return res;
}

//virtual
jlong
ODBCColumn::getLong()
{
  jlong res;
  
  try {
    res = _getNumber()->intValue();
  } catch (Exception nsme) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into an long.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  //_chkNull();
  if (_wasNull == true)
    return 0;
  return res;
}

//virtual
short
ODBCColumn::getShort()
{
  short res;
  
  try {
    res = _getNumber()->intValue();
  } catch (Exception nsme) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into an short.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  //_chkNull();
  if (_wasNull == true)
    return 0;
  return res;
}

//virtual
RbyteArray
ODBCColumn::getBytes()
{
  if (_obj == Nil)
    _getData();

  if ((instanceof(_obj, charArray) == false) &&
      (instanceof(_obj, byteArray) == false) &&
      (instanceof(_obj, shortArray) == false) &&
      (instanceof(_obj, intArray) == false) &&
      (instanceof(_obj, longArray) == false) &&
      (instanceof(_obj, floatArray) == false) &&
      (instanceof(_obj, doubleArray) == false)) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into a byteArray.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  if (_wasNull == true)
    return Nil;
  return RbyteArray(_obj);
}

//virtual
bool
ODBCColumn::getBoolean()
{
  RBoolean val;
  bool res;

  if (_obj == Nil)
    _getData();

  if (instanceof(_obj, acdk::lang::Boolean) == false) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into a Boolean.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  val = acdk::lang::RBoolean(_obj); // should work without an exception and should return value != Nil
  if (val == Nil) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("can't make a boolean out of object in column ") + Integer::toString(_col) + ".");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  try {
    res = val->booleanValue(); // only defined for RBoolean ...
  } catch (Exception nsme) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("no method to get a boolean out of object in column ") + Integer::toString(_col) + ".");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  //_chkNull();
  if (_wasNull == true)
    return false;
  return res;
}

#if !defined(ACDK_MINI)
//virtual
::acdk::util::RDate
ODBCColumn::getDate()
{
  if (_obj == Nil)
    _getData();

  if (_wasNull == true)
    return Nil;
  if (instanceof(_obj, acdk::util::Date) == false) {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into a date.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  return ::acdk::util::RDate(_obj);
}
#endif //!defined(ACDK_MINI)

//virtual
RString
ODBCColumn::getString()
{
  RString res;
  
  if (_obj == Nil)
    _getData();
  
  if (_wasNull)
    return Nil;
  if (instanceof(_obj, acdk::util::String) == false) {
    try {
    res = _obj->toString();
    } catch (Exception nsme) {
      RODBCHandle hndl = _getODBCHandle();
      ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into an string.");
      hndl->_addException(ex);
      SQLTHROW(hndl->_getExceptions());
    }
  } else {
    res = ::acdk::util::RString(_obj);
  }
  return res;
}

#if !defined(ACDK_MINI)

RBlob 
ODBCColumn::getBlob()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual
RTime
ODBCColumn::getTime()
{
  if (_obj == Nil)
    _getData();
  
  if (_wasNull)
    return Nil;
  if (instanceof(_obj, ::acdk::sql::Time) == false) 
  {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into a time.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  return ::acdk::sql::RTime(_obj);
}

//virtual
RTimestamp
ODBCColumn::getTimestamp()
{
  if (_obj == Nil)
    _getData();
  
  if (_wasNull)
    return Nil;
  
  if (instanceof(_obj, acdk::sql::Timestamp) == false) 
  {
    RODBCHandle hndl = _getODBCHandle();
    ::acdk::sql::RSQLException ex = new SQLException(RString("object in column ") + Integer::toString(_col) + "can't be converted into a timestamp.");
    hndl->_addException(ex);
    SQLTHROW(hndl->_getExceptions());
  }
  return ::acdk::sql::RTimestamp(_obj);
}
#endif

} // odbc
} // sql
} // acdk

