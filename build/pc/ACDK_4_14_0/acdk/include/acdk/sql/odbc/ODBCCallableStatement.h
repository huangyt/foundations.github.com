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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCCallableStatement.h,v 1.7 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_ODBCCallableStatement_h
#define acdk_sql_ODBCCallableStatement_h

#include "ODBCPreparedStatement.h"
#include <acdk/sql/CallableStatement.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;
using namespace acdk::sql;

ACDK_DECL_INTERFACE(ODBCCallableStatement);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.7 $
  @date $RDate: 2003/06/19 14:37:21 $
  
*/
class ACDK_SQL_ODBC_PUBLIC ODBCCallableStatement
: extends ODBCPreparedStatement
, implements CallableStatement
{
  ACDK_WITH_METAINFO(ODBCCallableStatement)
public:
  ODBCCallableStatement(INP(RODBCConnection) conn, INP(RString) clause);
  virtual RODBCStatement init(INP(acdk::util::RProperties) prop);
  /*
  virtual Array getArray(int i);
  virtual BigDecimal getBigDecimal(int parameterIndex);
  
  */
  virtual RBlob getBlob(int i);
  virtual bool getBoolean(int parameterIndex);
  virtual byte getByte(int parameterIndex);
  virtual RbyteArray getBytes(int parameterIndex);
#if !defined(ACDK_MINI)
  //virtual Clob getClob(int i);
  virtual RDate getDate(int parameterIndex);
  virtual RDate getDate(int parameterIndex, INP(acdk::util::RCalendar) cal);
#endif
  virtual double getDouble(int parameterIndex);
  virtual float getFloat(int parameterIndex);
  virtual int getInt(int parameterIndex);
  virtual jlong getLong(int parameterIndex);
  virtual RObject getObject(int parameterIndex);
  virtual RObject getObject(int i, INP(RMap) map);
#if !defined(ACDK_MINI)
  //virtual RRef getRef(int i);
#endif //!defined(ACDK_MINI)
  virtual short getShort(int parameterIndex);
  virtual RString getString(int parameterIndex);
#if !defined(ACDK_MINI)
  virtual RTime getTime(int parameterIndex);
  virtual RTime getTime(int parameterIndex, INP(RCalendar) cal);
  virtual RTimestamp getTimestamp(int parameterIndex);
  virtual RTimestamp getTimestamp(int parameterIndex, INP(RCalendar) cal);
#endif
  virtual void registerOutParameter(int parameterIndex, int sqlType);
  virtual void registerOutParameter(int parameterIndex, int sqlType, int scale);
  virtual void registerOutParameter(int paramIndex, int sqlType, INP(RString) typeName);
  virtual bool wasNull();
};          


} // odbc
} // sql
} // acdk

#endif //acdk_sql_ODBCCallableStatement_h

