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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCPreparedStatement.cpp,v 1.7 2005/04/06 19:03:45 kommer Exp $

#include "ODBCPreparedStatement.h"

namespace acdk {
namespace sql {
namespace odbc {

using acdk::lang::dmi::ScriptVar;

struct TypeMap
{
  ScriptVar::Type acdkType;
  int cType;
  int sqlType;
};

TypeMap _typeMap[] = 
{
  { ScriptVar::BoolType, SQL_C_CHAR, SQL_CHAR },
  { ScriptVar::CharType, SQL_C_CHAR, SQL_CHAR },
  { ScriptVar::UcCharType, SQL_C_SSHORT, SQL_SMALLINT },
  { ScriptVar::ByteType, SQL_C_CHAR, SQL_CHAR },
  { ScriptVar::ShortType, SQL_C_SSHORT, SQL_SMALLINT },
  { ScriptVar::IntType, SQL_C_LONG, SQL_INTEGER },
  { ScriptVar::LongType, SQL_C_LONG, SQL_BIGINT },
  { ScriptVar::FloatType, SQL_C_FLOAT, SQL_REAL },
  { ScriptVar::DoubleType, SQL_C_DOUBLE, SQL_DOUBLE }
};

int 
_getCType(ScriptVar::Type tp)
{
  for (int i = 0; i < int(sizeof(_typeMap) / sizeof(_typeMap[0])); ++i)
  {
    if (_typeMap[i].acdkType == tp)
      return _typeMap[i].cType;
  }
  //ACDK_NLOG("acdk.sql.odbc", Error, SBSTR("cannot map ScriptVar Type to C-Type: " << (int)tp));
  return -1;
}

int
_getSQLType(ScriptVar::Type tp)
{
  for (int i = 0; i < int(sizeof(_typeMap) / sizeof(_typeMap[0])); ++i)
  {
    if (_typeMap[i].acdkType == tp)
      return _typeMap[i].sqlType;
  }
  return -1;
}

int 
_getSQLType(const ScriptVar& sv)
{
  int ret = _getSQLType(sv.type);
  if (ret != -1)
    return ret;
  RObject obj = sv.getObjectVar();
  if (instanceof(obj, String) == true)
  {
    RString s = (RString)obj;
    CharacterClass cc = s->characterClass();
    if (cc == CCAscii)
      return SQL_VARCHAR;
#if defined(ACDK_HAS_SQL_WCHAR)
    return SQL_WVARCHAR;
#else
    return SQL_VARCHAR;
#endif
  }
  if (instanceof(obj, byteArray) == true)
    return SQL_VARCHAR;
  // ### @todo handle other object types
  //ACDK_NLOG("acdk.sql.odbc", Error, SBSTR("cannot map ScriptVar Type to SQL-Type" << (int)sv.type));
  return -1;
}

ODBCParam::ODBCParam(acdk::sql::SQLType sqlType, int flags)
: _flags(flags)
, _sqlType(sqlType)
, _transferedSize(0)
{
}

ODBCParam::ODBCParam(INP(ScriptVar) sv, int flags)
: _flags(flags)
, _val(sv)
, _sqlType(_getSQLType(sv))
, _transferedSize(0)
{
}


int 
ODBCParam::getCType() const
{
  int tp = _getCType(_val.type);
  if (tp != -1)
    return tp;
  RObject obj = _val.getObjectVar();
  if (instanceof(obj, String) == true)
  {
    RString s = (RString)obj;
    CharacterClass cc = s->characterClass();
    if (cc == CCAscii)
      return SQL_C_CHAR;
#if defined(ACDK_HAS_SQL_WCHAR)
    return SQL_C_WCHAR;
#else
    return SQL_C_CHAR;
#endif 
  }
  if (instanceof(obj, byteArray) == true)
  {
#if defined(ACDK_HAS_SQL_WCHAR)
    if (getSQLType() == WVarCharSqlType || getSQLType() == WLongVarCharSqlType)
      return SQL_C_WCHAR;
#endif
    return SQL_C_CHAR;
  }
  ACDK_NLOG("acdk.sql.odbc", Error, SBSTR("cannot map ScriptVar Type to C-Type" << int(_val.type)));
  
  return SQL_C_CHAR;
}

int 
ODBCParam::getTypeSize() const 
{ 
  int ret = _val.getTypeStorageSize(); 
  if (ret != -1)
    return ret;
  RObject obj = _val.getObjectVar();
  if (instanceof(obj, String) == true)
  {
    RString s = (RString)obj;
    CharacterClass cc = s->characterClass();
    if (cc == CCAscii)
      return s->length();
    return s->length() * sizeof(ODBC_NATIVE_CHAR);
  }
  else if (instanceof(obj, byteArray) == true)
    return RbyteArray(obj)->length();
  // ### @todo handle other ODBCParam types
  return -1;
}

byte* 
ODBCParam::getValData() const
{
  int ret = _val.getTypeStorageSize(); 
  if (ret != -1)
  {
    return (byte*)const_cast<ScriptVar&>(_val).getDataPtr();
  }
  RObject obj = _val.getObjectVar();
  if (instanceof(obj, String) == true)
  {
    RString s = (RString)obj;
    CharacterClass cc = s->characterClass();
    if (cc == CCAscii)
      return (byte*)s->c_str();
    RString nstr = RString(obj)->convertToNative(); 
    const_cast<ScriptVar&>(_val) = inOf(nstr);
    return (byte*)nstr->native_c_str();
  }
  else if (instanceof(obj, byteArray) == true)
  {
    return (byte*)RbyteArray(obj)->data();
  }
  // ### @todo handle other ODBCParam types
  return 0;
}

ODBCPreparedStatement::ODBCPreparedStatement(INP(RODBCConnection) conn, INP(RString) clause)
: ODBCStatement(conn)
, _clause(clause)
, _args(new ODBCParamArray(0))
{
  
}

RODBCStatement 
ODBCPreparedStatement::init(INP(acdk::util::RProperties) prop)
{
  ODBCStatement::init(prop);
  RString nstr = ODBC_STR2NSTR(_clause);
  ODBC_NATIVE_CHAR* t = (ODBC_NATIVE_CHAR*)ODBC_STR2NCSRT(nstr);
  int len = nstr->length();
  callSQL2(_stmth, SQLPrepare, (ODBC_NATIVE_CHAR*)t, len);
  return this;
}

void 
ODBCPreparedStatement::clearParameters()
{
  THROW0(UnsupportedOperationException);
}

bool 
ODBCPreparedStatement::execute() 
{
  THROW0(UnsupportedOperationException);
  //return ODBCStatement::execute();
  return false;
}

RResultSet 
ODBCPreparedStatement::executeQuery() 
{
  callSQL0(_stmth, SQLExecute);
  return getResultSet();
}

int 
ODBCPreparedStatement::executeUpdate() 
{
  callSQL0(_stmth, SQLExecute);
  SQLINTEGER rcnt = 0;
  callSQL1(_stmth, SQLRowCount, &rcnt);
  return rcnt;
}

RResultSetMetaData 
ODBCPreparedStatement::getMetaData()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

void 
ODBCPreparedStatement::setBoolean(int parameterIndex, bool x)
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  RODBCParam p = new ODBCParam(inOf((char)(x == true ? 1 : 0)), acdk::lang::dmi::MiAiIn);
  _args[parameterIndex - 1] = p;
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_INPUT, p->getCType(), p->getSQLType(), p->getTypeSize(),
                   0, p->getValData(), p->getTypeSize(), p->transferSizePtr());
}

void 
ODBCPreparedStatement::setByte(int parameterIndex, byte x) 
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  
  RODBCParam p = new ODBCParam(inOf(x), acdk::lang::dmi::MiAiIn);
  _args[parameterIndex - 1] = p;
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_INPUT, p->getCType(), p->getSQLType(), p->getTypeSize(),
                   0, p->getValData(), p->getTypeSize(), p->transferSizePtr());
}

void 
ODBCPreparedStatement::setBytes(int parameterIndex, INP(RbyteArray) x) 
{
  THROW0(UnsupportedOperationException);
}

#if !defined(ACDK_MINI)
void 
ODBCPreparedStatement::setDate(int parameterIndex, INP(RDate) x) 
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCPreparedStatement::setDate(int parameterIndex, INP(RDate) x, INP(RCalendar) cal) 
{
  THROW0(UnsupportedOperationException);
}

#endif //!defined(ACDK_MINI)

void 
ODBCPreparedStatement::setDouble(int parameterIndex, double x) 
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  RODBCParam p = new ODBCParam(inOf(x), acdk::lang::dmi::MiAiIn);
  _args[parameterIndex - 1] = p;
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_INPUT, p->getCType(), p->getSQLType(), p->getTypeSize(),
                   0, p->getValData(), p->getTypeSize(), p->transferSizePtr());
  
}

void 
ODBCPreparedStatement::setFloat(int parameterIndex, float x) 
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  RODBCParam p = new ODBCParam(inOf(x), acdk::lang::dmi::MiAiIn);
  _args[parameterIndex - 1] = p;
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_INPUT, p->getCType(), p->getSQLType(), p->getTypeSize(),
                   0, p->getValData(), 0, p->transferSizePtr());
  
}

void 
ODBCPreparedStatement::setInt(int parameterIndex, int x) 
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  RODBCParam p = new ODBCParam(inOf(x), acdk::lang::dmi::MiAiIn);
  _args[parameterIndex - 1] = p;
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_INPUT, p->getCType(), p->getSQLType(), p->getTypeSize(),
                   0, p->getValData(), p->getTypeSize(), p->transferSizePtr());
}

void 
ODBCPreparedStatement::setLong(int parameterIndex, jlong x) 
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  RODBCParam p = new ODBCParam(inOf(x), acdk::lang::dmi::MiAiIn);
  _args[parameterIndex - 1] = p;
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_INPUT, p->getCType(), p->getSQLType(), p->getTypeSize(),
                   0, p->getValData(), p->getTypeSize(), p->transferSizePtr());
}

void 
ODBCPreparedStatement::setNull(int parameterIndex, int sqlType) 
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCPreparedStatement::setNull(int paramIndex, int sqlType, INP(RString) typeName) 
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCPreparedStatement::setObject(int parameterIndex, INP(RObject) x)
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCPreparedStatement::setObject(int parameterIndex, INP(RObject) x, int targetSqlType) 
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCPreparedStatement::setObject(int parameterIndex, INP(RObject) x, int targetSqlType, int scale) 
{
  THROW0(UnsupportedOperationException);
}

  // not supported virtual void setRef(int i, Ref x) = 0;
void 
ODBCPreparedStatement::setShort(int parameterIndex, short x) 
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  RODBCParam p = new ODBCParam(inOf(x), acdk::lang::dmi::MiAiIn);
  _args[parameterIndex - 1] = p;
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_INPUT, p->getCType(), p->getSQLType(), p->getTypeSize(),
                   0, p->getValData(), p->getTypeSize(), p->transferSizePtr());
}

void 
ODBCPreparedStatement::setString(int parameterIndex, INP(RString) x) 
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  RODBCParam p = new ODBCParam(inOf(x), acdk::lang::dmi::MiAiIn);
  _args[parameterIndex - 1] = p;
  int ctype = p->getCType();
  int sqltype = p->getSQLType();
  byte* valdata = p->getValData();
  int typesize = p->getTypeSize();
//### don't set c-char, but set unicode character
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_INPUT, ctype, sqltype, typesize,
                   0, valdata, 0/* must be 0 */, 0/* must be 0 */);
}

void 
ODBCPreparedStatement::setBlob(int parameterIndex, INP(RBlob) b)
{
  THROW0(UnsupportedOperationException);
}

#if !defined(ACDK_MINI)
void 
ODBCPreparedStatement::setTime(int parameterIndex, INP(RTime) x) 
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCPreparedStatement::setTime(int parameterIndex, INP(RTime) x, INP(RCalendar) cal) 
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCPreparedStatement::setTimestamp(int parameterIndex, INP(RTimestamp) x) 
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCPreparedStatement::setTimestamp(int parameterIndex, INP(RTimestamp) x, INP(RCalendar) cal) 
{
  THROW0(UnsupportedOperationException);
}
#endif //!defined(ACDK_MINI)
  

} // odbc
} // sql
} // acdk

