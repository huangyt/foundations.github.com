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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCDatabaseMetaData.cpp,v 1.7 2005/02/05 10:45:31 kommer Exp $

#include "ODBCDatabaseMetaData.h"
#include "ODBCConnection.h"
#include "ODBCDriver.h"
#include "ODBCResultSetMetaData.h"

#include <acdk/util/Properties.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

RString 
ODBCDatabaseMetaData::getStringInfo(int code) THROWS1(::acdk::sql::RSQLException)
{
  const int bufsize = 255;
  SQLCHAR buffer[bufsize];
  RODBCHandle h = _conn->_getODBCHandle();
  SQLHANDLE sqlhandle = h->_getSQLHandle();
  short reallen;
  SQLRETURN ret = SQLGetInfo(sqlhandle, (SQLUSMALLINT)code, (SQLPOINTER)buffer, bufsize - 1, &reallen);
  h->_chkSQLrcode(ret, "ODBCDatabaseMetaData::getStringInfo", __FILE__, __LINE__);
  return ODBCSTR2STR(buffer, reallen);
}

short 
ODBCDatabaseMetaData::getShortInfo(int code) THROWS1(::acdk::sql::RSQLException)
{
  RODBCHandle h = _conn->_getODBCHandle();
  SQLHANDLE sqlhandle = h->_getSQLHandle();
  short returnValue;
  short reallen;
  SQLRETURN ret = SQLGetInfo(sqlhandle, (SQLUSMALLINT)code, &returnValue, sizeof(returnValue),  &reallen);
  h->_chkSQLrcode(ret, "ODBCDatabaseMetaData::getShortInfo", __FILE__, __LINE__);
  return returnValue;
}

int 
ODBCDatabaseMetaData::getIntInfo(int code) THROWS1(::acdk::sql::RSQLException)
{
  RODBCHandle h = _conn->_getODBCHandle();
  SQLHANDLE sqlhandle = h->_getSQLHandle();
  int returnValue;
  short reallen;
  SQLRETURN ret = SQLGetInfo(sqlhandle, (SQLUSMALLINT)code, &returnValue, sizeof(returnValue),  &reallen);
  h->_chkSQLrcode(ret, "ODBCDatabaseMetaData::getIntInfo", __FILE__, __LINE__);
  return returnValue;
}

RString 
ODBCDatabaseMetaData::getNumericFunctions() THROWS1(::acdk::sql::RSQLException)
{
  struct FuncMap
  {
    int code;
    const char* name;
  };
  FuncMap m[] = 
  {
  { SQL_FN_NUM_ABS, "ABS" },
  { SQL_FN_NUM_ACOS, "ACOS" },
  { SQL_FN_NUM_ASIN, "ASIN" },
  { SQL_FN_NUM_ATAN, "ATAN" },
  { SQL_FN_NUM_ATAN2, "ATAN2" },
  { SQL_FN_NUM_CEILING,	"CEILING" },
  { SQL_FN_NUM_COS, "COS" },
  { SQL_FN_NUM_COT, "COT" },
  { SQL_FN_NUM_DEGREES, "DEGREES" },
  { SQL_FN_NUM_EXP, "EXP" },
  { SQL_FN_NUM_FLOOR, "FLOOR" },
  { SQL_FN_NUM_LOG, "LOG" },
  { SQL_FN_NUM_LOG10, "LOG10" },
  { SQL_FN_NUM_MOD, "MOD" },
  { SQL_FN_NUM_PI, "PI" },
  { SQL_FN_NUM_POWER, "POWER" },
  { SQL_FN_NUM_RADIANS, "RADIANS" },
  { SQL_FN_NUM_RAND, "RAND" },
  { SQL_FN_NUM_ROUND, "ROUND" },
  { SQL_FN_NUM_SIGN, "SIGN" },
  { SQL_FN_NUM_SIN, "SIN" },
  { SQL_FN_NUM_SQRT, "SQRT" },
  { SQL_FN_NUM_TAN, "TAN" },
  { SQL_FN_NUM_TRUNCATE, "TRUNCATE" },
  { 0, 0 }
  };
  StringBuffer sb;
  int funcs = getIntInfo(SQL_NUMERIC_FUNCTIONS);
  for (int i = 0; m[i].code != 0; ++i)
  {
    if (funcs & m[i].code)
    {
      if (sb.length() > 0)
        sb << ",";
      sb << m[i].name;
    }
  }
  return sb.toString();
}


RString 
ODBCDatabaseMetaData::getStringFunctions() THROWS1(::acdk::sql::RSQLException)
{
  struct FuncMap
  {
    int code;
    const char* name;
  };
  FuncMap m[] = 
  {
    { SQL_FN_STR_BIT_LENGTH, "BIT_LENGTH" },
    { SQL_FN_STR_CHAR_LENGTH, "CHAR_LENGTH" },
    { SQL_FN_STR_CHARACTER_LENGTH, "CHARACTER_LENGTH" },
    { SQL_FN_STR_OCTET_LENGTH, "OCTET_LENGTH" },
    { SQL_FN_STR_POSITION, "POSITION" },
    { SQL_FN_STR_ASCII, "ASCII" },
    { SQL_FN_STR_CHAR, "CHAR" },
    { SQL_FN_STR_CONCAT, "CONCAT" },
    { SQL_FN_STR_DIFFERENCE, "DIFFERENCE" },
    { SQL_FN_STR_INSERT, "INSERT" },
    { SQL_FN_STR_LCASE, "LCASE" },
    { SQL_FN_STR_LEFT, "LEFT" },
    { SQL_FN_STR_LENGTH, "LENGTH" },
    { SQL_FN_STR_LOCATE, "LOCATE" },
    { SQL_FN_STR_LOCATE_2, "LOCATE_2" },
    { SQL_FN_STR_LTRIM, "LTRIM" },
    { SQL_FN_STR_REPEAT, "REPEAT" },
    { SQL_FN_STR_REPLACE, "REPLACE" },
    { SQL_FN_STR_RIGHT, "RIGHT" },
    { SQL_FN_STR_RTRIM, "RTRIM" },
    { SQL_FN_STR_SOUNDEX, "SOUNDEX" },
    { SQL_FN_STR_SPACE, "SPACE" },
    { SQL_FN_STR_SUBSTRING, "SUBSTRING" },
    { SQL_FN_STR_UCASE, "UCASE" },
    { 0, 0 }
  };
  StringBuffer sb;
  int funcs = getIntInfo(SQL_STRING_FUNCTIONS);
  for (int i = 0; m[i].code != 0; ++i)
  {
    if (funcs & m[i].code)
    {
      if (sb.length() > 0)
        sb << ",";
      sb << m[i].name;
    }
  }
  return sb.toString();
}

RString 
ODBCDatabaseMetaData::getSystemFunctions() THROWS1(::acdk::sql::RSQLException)
{
  struct FuncMap
  {
    int code;
    const char* name;
  };
  FuncMap m[] = 
  {
    { SQL_FN_SYS_DBNAME, "DBNAME" },
    { SQL_FN_SYS_IFNULL, "IFNULL" },
    { SQL_FN_SYS_USERNAME, "USERNAME" },
    { 0, 0 }
  };
  StringBuffer sb;
  int funcs = getIntInfo(SQL_SYSTEM_FUNCTIONS);
  for (int i = 0; m[i].code != 0; ++i)
  {
    if (funcs & m[i].code)
    {
      if (sb.length() > 0)
        sb << ",";
      sb << m[i].name;
    }
  }
  return sb.toString();
}


RString 
ODBCDatabaseMetaData::getDateTimeFunctions() THROWS1(::acdk::sql::RSQLException)
{
  struct FuncMap
  {
    int code;
    const char* name;
  };
  FuncMap m[] = 
  {
    { SQL_FN_TD_CURRENT_DATE, "CURRENT_DATE" },
    { SQL_FN_TD_CURRENT_TIME, "CURRENT_TIME" },
    { SQL_FN_TD_CURRENT_TIMESTAMP, "CURRENT_TIMESTAMP" },
    { SQL_FN_TD_EXTRACT, "EXTRACT" },
    { SQL_FN_TD_CURDATE, "CURDATE" },
    { SQL_FN_TD_CURTIME, "CURTIME" },
    { SQL_FN_TD_DAYNAME, "DAYNAME" },
    { SQL_FN_TD_DAYOFMONTH, "DAYOFMONTH" }, 
    { SQL_FN_TD_DAYOFWEEK, "DAYOFWEEK" },
    { SQL_FN_TD_DAYOFYEAR, "DAYOFYEAR" },
    { SQL_FN_TD_HOUR, "HOUR" },
    { SQL_FN_TD_MINUTE, "MINUTE" },
    { SQL_FN_TD_MONTH, "MONTH" },
    { SQL_FN_TD_MONTHNAME, "MONTHNAME" },
    { SQL_FN_TD_NOW, "NOW" },
    { SQL_FN_TD_QUARTER, "QUARTER" },
    { SQL_FN_TD_SECOND, "SECOND" },
    { SQL_FN_TD_TIMESTAMPADD, "TIMESTAMPADD" },
    { SQL_FN_TD_TIMESTAMPDIFF, "TIMESTAMPDIFF" },
    { SQL_FN_TD_WEEK, "WEEK" },
    { SQL_FN_TD_YEAR, "YEAR" },
    { 0, 0 }
  };
  StringBuffer sb;
  int funcs = getIntInfo(SQL_TIMEDATE_FUNCTIONS);
  for (int i = 0; m[i].code != 0; ++i)
  {
    if (funcs & m[i].code)
    {
      if (sb.length() > 0)
        sb << ",";
      sb << m[i].name;
    }
  }
  return sb.toString();
}

bool 
ODBCDatabaseMetaData::supportsConvert(int fromType, int toType) THROWS1(::acdk::sql::RSQLException) 
{ 
  THROW0(UnsupportedOperationException); 
  struct TypeToTypeMap 
  {
    int sqlType;
    int odbcType;
  };
  TypeToTypeMap inputMap[] =
  {
    { BigIntSqlType, SQL_CONVERT_BIGINT },
    { BinarySqlType, SQL_CONVERT_BINARY },
    { BitSqlType, SQL_CONVERT_BIT },
    { CharSqlType, SQL_CONVERT_CHAR },
    { DateSqlType, SQL_CONVERT_DATE },
    { DecimalSqlType, SQL_CONVERT_DECIMAL },
    { DoubleSqlType, SQL_CONVERT_DOUBLE },
    { FloatSqlType, SQL_CONVERT_FLOAT },
    { IntegerSqlType, SQL_CONVERT_INTEGER },
    { LongVarBinarySqlType, SQL_CONVERT_LONGVARBINARY },
    { LongVarCharSqlType, SQL_CONVERT_LONGVARCHAR },
    { NumericSqlType, SQL_CONVERT_NUMERIC },
    { RealSqlType, SQL_CONVERT_REAL },
    { SmallIntSqlType, SQL_CONVERT_SMALLINT },
    { TimeSqlType, SQL_CONVERT_TIME },
    { TimeStampSqlType, SQL_CONVERT_TIMESTAMP },
    { TinyIntSqlType, SQL_CONVERT_TINYINT },
    { VarBinarySqlType, SQL_CONVERT_VARBINARY },
    { VarCharSqlType, SQL_CONVERT_VARCHAR },
    { 0, 0 }
  };
  TypeToTypeMap outputMap[] =
  {
    { BigIntSqlType, SQL_CVT_BIGINT },
    { BinarySqlType, SQL_CVT_BINARY },
    { BitSqlType, SQL_CVT_BIT },
    { CharSqlType, SQL_CVT_CHAR },
    { DateSqlType, SQL_CVT_DATE },
    { DecimalSqlType, SQL_CVT_DECIMAL },
    { DoubleSqlType, SQL_CVT_DOUBLE },
    { FloatSqlType, SQL_CVT_FLOAT },
    { IntegerSqlType, SQL_CVT_INTEGER },
    { LongVarBinarySqlType, SQL_CVT_LONGVARBINARY },
    { LongVarCharSqlType, SQL_CVT_LONGVARCHAR },
    { NumericSqlType, SQL_CVT_NUMERIC },
    { RealSqlType, SQL_CVT_REAL },
    { SmallIntSqlType, SQL_CVT_SMALLINT },
    { TimeSqlType, SQL_CVT_TIME },
    { TimeStampSqlType, SQL_CVT_TIMESTAMP },
    { TinyIntSqlType, SQL_CVT_TINYINT },
    { VarBinarySqlType, SQL_CVT_VARBINARY },
    { VarCharSqlType, SQL_CVT_VARCHAR },
    { 0, 0 }
  };
  for (int i=0; inputMap[i].sqlType != 0; ++i) 
  {
    if (inputMap[i].sqlType == fromType) 
    {
      for (int j = 0; outputMap[j].sqlType != 0; ++j) 
      {
	      if(outputMap[j].sqlType == toType) 
	        return getIntInfo(inputMap[i].odbcType) & outputMap[j].odbcType;
	    }
    }	
  }
  return false;
}


int 
ODBCDatabaseMetaData::getDefaultTransactionIsolation() THROWS1(::acdk::sql::RSQLException)
{
  int val = getIntInfo(SQL_DEFAULT_TXN_ISOLATION);
  switch(val)
  {
  case SQL_TXN_READ_UNCOMMITTED:
    return Connection::TRANSACTION_READ_UNCOMMITTED;
  case SQL_TXN_READ_COMMITTED:
    return Connection::TRANSACTION_READ_COMMITTED;
  case SQL_TXN_REPEATABLE_READ:
    return Connection::TRANSACTION_REPEATABLE_READ;
  case SQL_TXN_SERIALIZABLE:
    return Connection::TRANSACTION_SERIALIZABLE;
  default:
    return Connection::TRANSACTION_NONE;
  }
}

bool 
ODBCDatabaseMetaData::supportsTransactionIsolationLevel(int level) THROWS1(::acdk::sql::RSQLException)
{
  int val = getIntInfo(SQL_TXN_ISOLATION_OPTION);
  if (level ==  Connection::TRANSACTION_READ_UNCOMMITTED)
    return val & SQL_TXN_READ_UNCOMMITTED;
  else if (level == Connection::TRANSACTION_READ_COMMITTED)
    return val & SQL_TXN_READ_COMMITTED;
  else if (level == Connection::TRANSACTION_REPEATABLE_READ)
    return val & SQL_TXN_REPEATABLE_READ;
  /*
  else if (level == Connection::TRANSACTION_SERIALIZABLE)
    return (val &  SQL_TXN_SERIALIZABLE) || (val & SQL_TXN_VERSIONING);
    */
  return false;  
}
#if defined(ACDK_HAS_SQL_WCHAR)
#define STR2ODBCSTR(str) (str) == Nil ? (ODBC_NATIVE_CHAR*)0 : (ODBC_NATIVE_CHAR*)(str)->convertToNative()->native_c_str()

#else
#define STR2ODBCSTR(str) (str) == Nil ? (ODBC_NATIVE_CHAR*)0 : (ODBC_NATIVE_CHAR*)(str)->convert(CCAscii)->c_str()
#endif
#define STRLEN(str) (str) == Nil ? 0 : str->length()

::acdk::sql::RResultSet 
ODBCDatabaseMetaData::getTables(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern) THROWS1(::acdk::sql::RSQLException)
{
  RString types = "";
  //RString types = Nil;
  //RODBCHandle h = _conn->_getODBCHandle();
  //SQLHANDLE sqlhandle = h->_getSQLHandle();
  // SQL_ALL_CATALOGS 
  // SQL_ALL_SCHEMAS 
  // SQL_ALL_TABLE_TYPES 
  RODBCStatement stm = (RODBCStatement)_conn->createStatement();
  RODBCHandle h = stm->_getODBCHandle();
  SQLHANDLE sqlhandle = h->_getSQLHandle();
  SQLRETURN  ret = SQLTables(sqlhandle, STR2ODBCSTR(catalog), STRLEN(catalog), STR2ODBCSTR(schemaPattern), STRLEN(schemaPattern),
                                        STR2ODBCSTR(namePattern), STRLEN(namePattern), STR2ODBCSTR(types), STRLEN(types));
  h->_chkSQLrcode(ret, "ODBCDatabaseMetaData::getTables", __FILE__, __LINE__);
  return new ODBCResultSet(stm, FETCH_FORWARD);
}

} // odbc
} // sql
} // acdk
