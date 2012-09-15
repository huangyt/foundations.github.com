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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteResultSetMetaData.h,v 1.2 2005/04/05 13:28:55 kommer Exp $

#ifndef acdk_sql_sqlite_LiteResultSetMetaData_h
#define acdk_sql_sqlite_LiteResultSetMetaData_h

#include "Config.h"
#include <acdk.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/ResultSet.h>
#include <acdk/sql/ResultSetMetaData.h>

#include "LiteConnection.h"
#include "LiteTable.h"

struct sqlite3;
struct sqlite3_stmt;

namespace acdk {
namespace sql {
namespace sqlite {


ACDK_DECL_CLASS(LiteResultSetMetaData);

class ACDK_SQL_SQLITE_PUBLIC LiteResultSetMetaData
: extends acdk::lang::Object
, implements acdk::sql::ResultSetMetaData
{
  ACDK_WITH_METAINFO(LiteResultSetMetaData)
protected:
  RLiteTable _table;
public:
  LiteResultSetMetaData(IN(RLiteTable) table) 
  : _table(table) 
  {}

  virtual int getColumnCount()  THROWS1(RSQLException)
  {
    return _table->colCount();
  }
  virtual bool isAutoIncrement(int index)  THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool isCaseSensitive(int index)  THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool isSearchable(int index)  THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool isCurrency(int index)  THROWS1(RSQLException)
  {
    return false;
  }
  virtual int isNullable(int index)  THROWS1(RSQLException)
  {
    return acdk::sql::ColumnNullableUnknown;
  }
  virtual bool isSigned(int index)  THROWS1(RSQLException)
  {
    return false;
  }
  virtual int getColumnDisplaySize(int index)  THROWS1(RSQLException)
  {
    return 0;
  }
  virtual RString getColumnLabel(int index)  THROWS1(RSQLException)
  {
    return _table->getColName(index - 1);
  }
  virtual RString getColumnName(int index)  THROWS1(RSQLException)
  {
    RString s = getColumnLabel(index);
    int idx = s->indexOf('.');
    if (idx == -1)
      return s;
    return s->substr(idx + 1);
  }

  virtual RString getSchemaName(int index)  THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual int getPrecision(int index)  THROWS1(RSQLException)
  {
    return 0;
  }

  virtual int getScale(int index)  THROWS1(RSQLException)
  {
    return 0;
  }
  virtual RString getTableName(int index)  THROWS1(RSQLException)
  {
     RString s = getColumnLabel(index);
    int idx = s->indexOf('.');
    if (idx == -1)
      return Nil;
    return s->substr(0, idx);
  }
  virtual RString getCatalogName(int index)  THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual int getColumnType(int index)  THROWS1(RSQLException)
  {
    return _table->getSqlType(index - 1);
  }

  virtual RString getColumnTypeName(int index)  THROWS1(RSQLException)
  {
    return _table->getSQLTypeName(index - 1); 
  }
  virtual bool isReadOnly(int index)  THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool isWritable(int index)  THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool isDefinitelyWritable(int index)  THROWS1(RSQLException)
  {
    return true;
  }
  virtual RString getColumnClassName(int index)  THROWS1(RSQLException)
  {
    return _table->getColumnClassName(index - 1);
  }
  
};

} // sqlite
} // sql 
} // acdk

#endif //acdk_sql_sqlite_LiteResultSetMetaData_h
