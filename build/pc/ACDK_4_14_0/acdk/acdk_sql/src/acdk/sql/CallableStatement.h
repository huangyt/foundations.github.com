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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/CallableStatement.h,v 1.7 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_sql_CallableStatement_h
#define acdk_sql_CallableStatement_h

#include "PreparedStatement.h"
#include <acdk/util/Date.h>
#include "Time.h"
#include "Timestamp.h"

namespace acdk {
namespace sql {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(CallableStatement);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.7 $
  @date $RDate: 2003/06/19 14:37:21 $
  
*/
class ACDK_SQL_PUBLIC CallableStatement
: implements PreparedStatement
{
  ACDK_WITH_METAINFO(CallableStatement)
public:
   /*
  virtual Array getArray(int i) = 0;
  virtual BigDecimal getBigDecimal(int parameterIndex) = 0;
  */
  virtual RBlob getBlob(int i) = 0;
  virtual bool getBoolean(int parameterIndex) = 0;
  virtual byte getByte(int parameterIndex) = 0;
  virtual RbyteArray getBytes(int parameterIndex) = 0;
#if !defined(ACDK_MINI)
  virtual acdk::util::RDate getDate(int parameterIndex) = 0;
  virtual acdk::util::RDate getDate(int parameterIndex, INP(acdk::util::RCalendar) cal) = 0;
#endif
  virtual double getDouble(int parameterIndex) = 0;
  virtual float getFloat(int parameterIndex) = 0;
  virtual int getInt(int parameterIndex) = 0;
  virtual jlong getLong(int parameterIndex) = 0;
  virtual RObject getObject(int parameterIndex) = 0;
#if !defined(ACDK_MINI)
  virtual RObject getObject(int i, INP(acdk::util::RMap) map) = 0;
//  virtual RRef getRef(int i) = 0;
#endif //!defined(ACDK_MINI)
  virtual short getShort(int parameterIndex) = 0;
  virtual RString getString(int parameterIndex) = 0;
#if !defined(ACDK_MINI)
  virtual RTime getTime(int parameterIndex) = 0;
  virtual RTime getTime(int parameterIndex, INP(acdk::util::RCalendar) cal) = 0;
  virtual RTimestamp getTimestamp(int parameterIndex) = 0;
  virtual RTimestamp getTimestamp(int parameterIndex, INP(acdk::util::RCalendar) cal) = 0;
#endif
  virtual void registerOutParameter(int parameterIndex, int sqlType) = 0;
  virtual void registerOutParameter(int parameterIndex, int sqlType, int scale) = 0;
  virtual void registerOutParameter(int paramIndex, int sqlType, INP(RString) typeName) = 0;
  virtual bool wasNull() = 0;
};          


} // sql
} // acdk

#endif //acdk_sql_CallableStatement_h

