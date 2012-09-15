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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCPreparedStatement.h,v 1.9 2005/04/13 14:00:10 kommer Exp $

#ifndef acdk_sql_odbc_ODBCPreparedStatement_h
#define acdk_sql_odbc_ODBCPreparedStatement_h

#include "odbc.h"
#include "ODBCStatement.h"
#include <acdk/sql/PreparedStatement.h>
#include <acdk/lang/dmi/ScriptVar.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

ACDK_DECL_CLASS(ODBCParam);



/**
  used for input and output variables
*/
class ACDK_SQL_ODBC_PUBLIC ODBCParam
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(ODBCParam)
public:
  /**
    @see acdk::lang::dmi::MetaInfoFlags
  */
  int _flags;
  acdk::lang::dmi::ScriptVar _val;
  int _sqlType;
  int _transferedSize;
  ODBCParam(acdk::sql::SQLType sqlType, int flags);
  ODBCParam(INP(acdk::lang::dmi::ScriptVar) sv, int flags);

  int getCType() const;
  int getSQLType() const { return _sqlType; }
  int getTypeSize() const;
  byte* getValData() const;
  foreign long* transferSizePtr() const { return (long*)&_transferedSize; }
};


ACDK_DECL_CLASS(ODBCPreparedStatement);

/**
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $RDate: 2003/06/19 14:37:21 $

*/
class ACDK_SQL_ODBC_PUBLIC ODBCPreparedStatement
: extends ODBCStatement
, implements PreparedStatement
{
  ACDK_WITH_METAINFO(ODBCPreparedStatement)
protected:
  RString _clause;
  RODBCParamArray _args;
public:
  ODBCPreparedStatement(INP(RODBCConnection) conn, INP(RString) clause);
  virtual RODBCStatement init(INP(acdk::util::RProperties) prop);
  virtual void addBatch() { THROW0(UnsupportedOperationException); }
  /**
    @todo implement
  */
  virtual void clearParameters();
  virtual bool execute();
  virtual RResultSet executeQuery();
  virtual int executeUpdate();
  virtual RResultSetMetaData getMetaData();
  virtual void setBoolean(int parameterIndex, bool x);
  virtual void setByte(int parameterIndex, byte x);
  virtual void setBytes(int parameterIndex, INP(RbyteArray) x);
#if !defined(ACDK_MINI)
  virtual void setDate(int parameterIndex, INP(RDate) x);
  virtual void setDate(int parameterIndex, INP(RDate) x, INP(RCalendar) cal);
#endif //!defined(ACDK_MINI)
  virtual void setDouble(int parameterIndex, double x);
  virtual void setFloat(int parameterIndex, float x);
  virtual void setInt(int parameterIndex, int x);
  virtual void setLong(int parameterIndex, jlong x);
  virtual void setNull(int parameterIndex, int sqlType);
  virtual void setNull(int paramIndex, int sqlType, INP(RString) typeName);
  virtual void setObject(int parameterIndex, INP(RObject) x);
  virtual void setObject(int parameterIndex, INP(RObject) x, int targetSqlType);
  virtual void setObject(int parameterIndex, INP(RObject) x, int targetSqlType, int scale);
  // not supported virtual void setRef(int i, Ref x);
  virtual void setShort(int parameterIndex, short x);
  virtual void setString(int parameterIndex, INP(RString) x);
  virtual void setBlob(int parameterIndex, INP(RBlob) b);
#if !defined(ACDK_MINI)
  virtual void setTime(int parameterIndex, INP(RTime) x);
  virtual void setTime(int parameterIndex, INP(RTime) x, INP(RCalendar) cal);
  virtual void setTimestamp(int parameterIndex, INP(RTimestamp) x);
  virtual void setTimestamp(int parameterIndex, INP(RTimestamp) x, INP(RCalendar) cal);
#endif //!defined(ACDK_MINI)

};

} // odbc
} // sql
} // acdk

#endif //acdk_sql_odbc_ODBCPreparedStatement_h

