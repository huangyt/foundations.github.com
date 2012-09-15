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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteConnection.h,v 1.6 2005/04/13 15:38:04 kommer Exp $

#ifndef acdk_sql_sqlite_LiteConnection_h
#define acdk_sql_sqlite_LiteConnection_h

#include "Config.h"
#include <acdk/sql/Connection.h>


#include "LiteTable.h"

struct sqlite3;

namespace acdk {
namespace sql {
namespace sqlite {

ACDK_DECL_CLASS(LiteConnection);

/**
  connection to a SQLite database
*/
class ACDK_SQL_SQLITE_PUBLIC LiteConnection
: extends acdk::lang::Object
, implements acdk::sql::Connection
{
  ACDK_WITH_METAINFO(LiteConnection)
protected:
  RLiteDb _db;
  RString _url;
  bool _readOnly;
  bool _autoCommit;
public:
  foreign LiteConnection(IN(RString) url, IN(RLiteDb) db, bool readOnly = false) 
  : _db(db)
  , _url(url)
  , _readOnly(readOnly)
  , _autoCommit(true)
  {}
  /** return the native SQLite connection pointer */
  foreign sqlite3* conPtr() { return _db->getConPtr(); }
  /** return the core SQLite wrapper */
  RLiteDb getDb() { return _db; }
  RString getURL() { return _url; }
  static RLiteConnection createConnection(IN(RString) url, IN(RString) dbFile, bool readOnly = false);
  virtual void clearWarnings();
  virtual void close();
  virtual void commit();
  virtual RStatement createStatement();
  virtual RStatement createStatement(int resultSetType, int resultSetConcurrency);
  virtual bool getAutoCommit();
  virtual RString getCatalog();
  virtual RDatabaseMetaData getMetaData();
  virtual int getTransactionIsolation();
  virtual acdk::util::RMap getTypeMap();
  virtual RSQLWarning getWarnings();
  virtual bool isClosed();
  virtual bool isReadOnly();
  virtual RString nativeSQL(INP(RString) sql);
  virtual RCallableStatement prepareCall(INP(RString) sql);
  virtual RPreparedStatement prepareStatement(INP(RString) sql);
  virtual void rollback();
  virtual void setAutoCommit(bool autoCommit);
  virtual void setCatalog(INP(RString) catalog);
  virtual void setReadOnly(bool readOnly);
  virtual void setTransactionIsolation(int level);
  virtual void setTypeMap(INP(acdk::util::RMap) map);

  void execute(IN(RString) cmd);
  int executeUpdate(IN(RString) cmd);
};

} // sqlite
} // sql 
} // acdk

#endif //acdk_sql_sqlite_LiteResultSetMetaData_h
