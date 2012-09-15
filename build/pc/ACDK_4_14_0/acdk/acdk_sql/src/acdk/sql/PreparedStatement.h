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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/PreparedStatement.h,v 1.7 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_PreparedStatement_h
#define acdk_sql_PreparedStatement_h

#include "Statement.h"
#if !defined(ACDK_MINI)
# include <acdk/util/Date.h>
# include <acdk/util/Calendar.h>
# include "Time.h"
#endif 
namespace acdk {
namespace sql {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(PreparedStatement);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.7 $
  @date $RDate: 2003/06/19 14:37:21 $
 
*/
class ACDK_SQL_PUBLIC PreparedStatement
: implements Statement
{
  ACDK_WITH_METAINFO(PreparedStatement)
public:
  virtual void addBatch() = 0;
  virtual void clearParameters() = 0;
  virtual bool execute() = 0;
  virtual RResultSet executeQuery() = 0;
  virtual int executeUpdate() = 0;
  virtual RResultSetMetaData getMetaData() = 0;
  virtual void setBoolean(int parameterIndex, bool x) = 0;
  virtual void setByte(int parameterIndex, byte x) = 0;
  virtual void setBytes(int parameterIndex, INP(RbyteArray) x) = 0;
#if !defined(ACDK_MINI)
  virtual void setDate(int parameterIndex, INP(acdk::util::RDate) x) = 0;
  virtual void setDate(int parameterIndex, INP(acdk::util::RDate) x, INP(acdk::util::RCalendar) cal) = 0;
#endif //!defined(ACDK_MINI)
  virtual void setDouble(int parameterIndex, double x) = 0;
  virtual void setFloat(int parameterIndex, float x) = 0;
  virtual void setInt(int parameterIndex, int x) = 0;
  virtual void setLong(int parameterIndex, jlong x) = 0;
  virtual void setNull(int parameterIndex, int sqlType) = 0;
  virtual void setNull(int paramIndex, int sqlType, INP(RString) typeName) = 0;
  virtual void setObject(int parameterIndex, INP(RObject) x) = 0;
  virtual void setObject(int parameterIndex, INP(RObject) x, int targetSqlType) = 0;
  virtual void setObject(int parameterIndex, INP(RObject) x, int targetSqlType, int scale) = 0;
  // not supported virtual void setRef(int i, Ref x) = 0;
  virtual void setShort(int parameterIndex, short x) = 0;
  virtual void setString(int parameterIndex, INP(RString) x) = 0;
  virtual void setBlob(int parameterIndex, INP(RBlob) b) = 0;
#if !defined(ACDK_MINI)
  virtual void setTime(int parameterIndex, INP(RTime) x) = 0;
  virtual void setTime(int parameterIndex, INP(RTime) x, INP(acdk::util::RCalendar) cal) = 0;
  virtual void setTimestamp(int parameterIndex, INP(RTimestamp) x) = 0;
  virtual void setTimestamp(int parameterIndex, INP(RTimestamp) x, INP(acdk::util::RCalendar) cal) = 0;
#endif //!defined(ACDK_MINI)

};          


} // sql
} // acdk

#endif //acdk_sql_PreparedStatement_h

