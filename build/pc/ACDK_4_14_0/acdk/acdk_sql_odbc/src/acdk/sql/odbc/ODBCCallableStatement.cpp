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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCCallableStatement.cpp,v 1.9 2005/04/06 19:03:35 kommer Exp $

#include "ODBCCallableStatement.h"

namespace acdk {
namespace sql {
namespace odbc {

ODBCCallableStatement::ODBCCallableStatement(INP(RODBCConnection) conn, INP(RString) clause)
: ODBCPreparedStatement(conn, clause)
{

}

RODBCStatement 
ODBCCallableStatement::init(INP(acdk::util::RProperties) prop)
{
  ODBCStatement::init(prop);
  RString ts = ODBC_STR2NSTR(_clause);
  //const char* t = _clause->c_str();
  int len = ts->length();
  callSQL2(_stmth, SQLPrepare, (ODBC_NATIVE_CHAR*)ODBC_STR2NCSRT(ts), len);
  return this;
}

 /*
  virtual Array getArray(int i);
  virtual BigDecimal getBigDecimal(int parameterIndex);
  */
RBlob 
ODBCCallableStatement::getBlob(int i)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

bool 
ODBCCallableStatement::getBoolean(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return false;
}

byte 
ODBCCallableStatement::getByte(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return 0;
}

RbyteArray 
ODBCCallableStatement::getBytes(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

#if !defined(ACDK_MINI)
/*
RClob 
ODBCCallableStatement::getClob(int i)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
*/

RDate 
ODBCCallableStatement::getDate(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

RDate 
ODBCCallableStatement::getDate(int parameterIndex, IN(RCalendar) cal)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

#endif
double 
ODBCCallableStatement::getDouble(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return 0;
}

float 
ODBCCallableStatement::getFloat(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return 0;
}

int 
ODBCCallableStatement::getInt(int parameterIndex)
{
  RODBCParam p = _args[parameterIndex - 1];
  return p->_val;
  //return _args[parameterIndex - 1];
}

jlong 
ODBCCallableStatement::getLong(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return 0;
}

RObject 
ODBCCallableStatement::getObject(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

RObject 
ODBCCallableStatement::getObject(int i, INP(RMap) map)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

#if !defined(ACDK_MINI)
/*
RRef 
ODBCCallableStatement::getRef(int i)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
*/
#endif //!defined(ACDK_MINI)

short 
ODBCCallableStatement::getShort(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return 0;
}

RString 
ODBCCallableStatement::getString(int parameterIndex)
{
  RODBCParam p = _args[parameterIndex - 1];
  RbyteArray ba = RbyteArray(p->_val.getObjectVar());
  if (p->getSQLType() == VarCharSqlType)
    return SCS((const char*)ba->data());
  return ODBCSTR2STR(ba->data(), p->_transferedSize);
}

#if !defined(ACDK_MINI)
RTime 
ODBCCallableStatement::getTime(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

RTime 
ODBCCallableStatement::getTime(int parameterIndex, INP(RCalendar) cal)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

RTimestamp 
ODBCCallableStatement::getTimestamp(int parameterIndex)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

RTimestamp 
ODBCCallableStatement::getTimestamp(int parameterIndex, INP(RCalendar) cal)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
#endif


void 
ODBCCallableStatement::registerOutParameter(int parameterIndex, int sqlType)
{
  if (_args->length() < parameterIndex)
    _args->resize(parameterIndex);
  RODBCParam p;
  switch(sqlType)
  {
  case IntegerSqlType:
  {
    p = new ODBCParam(int(0), acdk::lang::dmi::MiAiOut);
    p->_sqlType = sqlType;
    break;
  }
  case WVarCharSqlType:
  case VarCharSqlType:
  {
    const int maxvarcharbuffer = 4096;
    RbyteArray ba = new byteArray(maxvarcharbuffer, 0); // ### @todo use define
    p = new ODBCParam(inOf(ba), acdk::lang::dmi::MiAiOut);
    p->_sqlType = sqlType;
    break;
  }
  default:
    THROW1(SQLException, SBSTR("Cannot map sqlType to ScriptVar: " << sqlType));
  }
  _args[parameterIndex - 1] = p;
  callSQL9(_stmth, SQLBindParameter, 
                   parameterIndex, SQL_PARAM_OUTPUT, p->getCType(), p->getSQLType(), 0,
                   0, p->getValData(), p->getTypeSize(), p->transferSizePtr());
}

void 
ODBCCallableStatement::registerOutParameter(int parameterIndex, int sqlType, int scale)
{
  THROW0(UnsupportedOperationException);
}

void 
ODBCCallableStatement::registerOutParameter(int paramIndex, int sqlType, INP(RString) typeName)
{
  THROW0(UnsupportedOperationException);
}

bool 
ODBCCallableStatement::wasNull()
{
  THROW0(UnsupportedOperationException);
  return false;
}

} // odbc
} // sql
} // acdk


