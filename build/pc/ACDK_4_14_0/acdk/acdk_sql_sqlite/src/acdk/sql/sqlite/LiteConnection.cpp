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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteConnection.cpp,v 1.4 2005/04/05 15:28:00 kommer Exp $


#include "LiteConnection.h"
#include "LiteStatement.h"
#include "LiteDriver.h"
#include "LitePreparedStatement.h"
#include "LiteDatabaseMetaData.h"
#include <acdk/sql/SQLException.h>
#include <acdk/sql/DriverManager.h>

namespace acdk {
namespace sql {
namespace sqlite {

namespace {
struct RegisterClass
{
  RegisterClass()
  {
    ::acdk::sql::DriverManager::registerDriver(new LiteDriver());
  }
};
RegisterClass _register;
} // anon namespace

using namespace acdk::sql;
//virtual 
RConnection 
LiteDriver::connect(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RException)
{
  RString dbFile;
  RString pref1 = "sqlite:/";
  RString pref2 = "jdbc:sqlite:/";
  if (url->startsWith(pref1) == true)
    dbFile = url->substr(pref1->length());
  else if (url->startsWith(pref2) == true)
    dbFile = url->substr(pref2->length());
  else
    THROW1(SQLException, "LiteDriver: cannot parse url: " + url);
  return &LiteConnection::createConnection(url, dbFile, false);
}

//static 
RLiteConnection 
LiteConnection::createConnection(IN(RString) url, IN(RString) dbFile, bool readOnly)
{
  RLiteDb db = LiteDb::openDb(dbFile);
  return new LiteConnection(url, db, readOnly);
}

void 
LiteConnection::clearWarnings() 
{
}

void 
LiteConnection::close()
{
  if (_db->isClosed() == true)
    return;
  try {
    rollback();
  } catch (RSQLException ex) {
  }
  _db->closeDb();
}

void 
LiteConnection::execute(IN(RString) cmd)
{
  _db->execute(cmd);
}

int 
LiteConnection::executeUpdate(IN(RString) cmd)
{
  return _db->executeUpdate(cmd);
}

void 
LiteConnection::commit()
{
  execute("COMMIT");
  
}

RStatement 
LiteConnection::createStatement()
{
  return new LiteStatement(this);
}

RStatement 
LiteConnection::createStatement(int resultSetType, int resultSetConcurrency)
{
  return new LiteStatement(this);
}

bool 
LiteConnection::getAutoCommit()
{
  return _autoCommit;
}

RString 
LiteConnection::getCatalog()
{
  return Nil;
}

RDatabaseMetaData 
LiteConnection::getMetaData()
{
  return new LiteDatabaseMetaData(this);
}

int 
LiteConnection::getTransactionIsolation()
{
  return -1;
}

acdk::util::RMap 
LiteConnection::getTypeMap()
{
  return Nil;
}

RSQLWarning 
LiteConnection::getWarnings()
{
  return Nil;
}

bool 
LiteConnection::isClosed()
{
  return _db->isClosed();
}

bool 
LiteConnection::isReadOnly()
{
  return _readOnly;
}

RString 
LiteConnection::nativeSQL(INP(RString) sql)
{
  return Nil;
}

RCallableStatement 
LiteConnection::prepareCall(INP(RString) sql)
{
  return Nil;
}

RPreparedStatement 
LiteConnection::prepareStatement(INP(RString) sql)
{
  RLiteTable tl = _db->prepareStatement(sql);
  return new LitePreparedStatement(this, tl);
}

void 
LiteConnection::rollback()
{
  execute("ROLLBACK");
}

void 
LiteConnection::setAutoCommit(bool autoCommit)
{
  _autoCommit = autoCommit;
}

void 
LiteConnection::setCatalog(INP(RString) catalog)
{

}

void 
LiteConnection::setReadOnly(bool readOnly)
{
  _readOnly = readOnly;
}

void 
LiteConnection::setTransactionIsolation(int level)
{
}

void 
LiteConnection::setTypeMap(INP(acdk::util::RMap) map)
{
}



} // sqlite
} // sql 
} // acdk

