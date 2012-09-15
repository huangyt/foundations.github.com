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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LitePreparedStatement.h,v 1.6 2005/04/13 15:38:05 kommer Exp $

#ifndef acdk_sql_sqlite_LitePreparedStatement_h
#define acdk_sql_sqlite_LitePreparedStatement_h

#include "Config.h"
#include <acdk.h>
#include <acdk/sql/PreparedStatement.h>
#include <acdk/sql/Timestamp.h>
#include "LiteStatement.h"
#include "LiteTable.h"
#include "LiteResultSetMetaData.h"

namespace acdk {
namespace sql {
namespace sqlite {


ACDK_DECL_CLASS(LitePreparedStatement);

/**
  Prepared statements for SQLite database
*/
class ACDK_SQL_SQLITE_PUBLIC LitePreparedStatement
: extends LiteStatement
, implements acdk::sql::PreparedStatement
{
  ACDK_WITH_METAINFO(LitePreparedStatement)
protected:
  RLiteTable _table;
public:
  LitePreparedStatement(IN(RLiteConnection) con, IN(RLiteTable) table) 
  : LiteStatement(con)
  , _table(table)
  {}
  virtual void addBatch()
  {
    THROW1(SQLException, "Unsupported");
  }
  virtual void clearParameters()
  {
    _table->clearParameters();
  }
  virtual bool execute();
  virtual RResultSet executeQuery();
  virtual int executeUpdate();
  virtual RResultSetMetaData getMetaData() { return new LiteResultSetMetaData(_table); }
  virtual void setBoolean(int parameterIndex, bool x)
  {
    _table->bindInt(parameterIndex, x ? 1 : 0);
  }
  virtual void setByte(int parameterIndex, byte x)
  {
    _table->bindInt(parameterIndex, x);
  }

  /// currently not supported
  virtual void setBytes(int parameterIndex, INP(RbyteArray) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// currently not supported
  virtual void setDate(int parameterIndex, INP(acdk::util::RDate) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// currently not supported
  virtual void setDate(int parameterIndex, INP(acdk::util::RDate) x, INP(acdk::util::RCalendar) cal)
  {
    THROW1(SQLException, "Unsupported");
  }
  virtual void setDouble(int parameterIndex, double x)
  {
    _table->bindDouble(parameterIndex, x);
  }
  virtual void setFloat(int parameterIndex, float x)
  {
    _table->bindDouble(parameterIndex, x);
  }

  virtual void setInt(int parameterIndex, int x)
  {
    _table->bindInt(parameterIndex, x);
  }

  virtual void setLong(int parameterIndex, jlong x)
  {
    _table->bindLong(parameterIndex, x);
  }
  virtual void setNull(int parameterIndex, int sqlType)
  {
    _table->bindNull(parameterIndex);
  }
  virtual void setNull(int paramIndex, int sqlType, INP(RString) typeName)
  {
    _table->bindNull(paramIndex);
  }

  /// currently not supported
  virtual void setObject(int parameterIndex, INP(RObject) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// currently not supported
  virtual void setObject(int parameterIndex, INP(RObject) x, int targetSqlType)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// currently not supported
  virtual void setObject(int parameterIndex, INP(RObject) x, int targetSqlType, int scale)
  {
    THROW1(SQLException, "Unsupported");
  }
  virtual void setShort(int parameterIndex, short x)
  {
    _table->bindInt(parameterIndex, x);
  }
  virtual void setString(int parameterIndex, INP(RString) x)
  {
    _table->bindText(parameterIndex, x);
  }
  virtual void setBlob(int parameterIndex, INP(RBlob) b)
  {
    _table->bindBlob(parameterIndex, b->getReadByteBuffer());
  }
  /// currently not supported
  virtual void setTime(int parameterIndex, INP(RTime) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// currently not supported
  virtual void setTime(int parameterIndex, INP(RTime) x, INP(acdk::util::RCalendar) cal)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// currently not supported
  virtual void setTimestamp(int parameterIndex, INP(RTimestamp) x)
  {
    THROW1(SQLException, "Unsupported");
  }
  /// currently not supported
  virtual void setTimestamp(int parameterIndex, INP(RTimestamp) x, INP(acdk::util::RCalendar) cal)
  {
    THROW1(SQLException, "Unsupported");
  }
};

} // sqlite
} // sql 
} // acdk

#endif //acdk_sql_sqlite_LitePreparedStatement_h
