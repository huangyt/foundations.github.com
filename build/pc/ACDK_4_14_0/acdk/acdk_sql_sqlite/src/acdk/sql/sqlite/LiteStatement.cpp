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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteStatement.cpp,v 1.3 2005/04/06 10:30:35 kommer Exp $


#include "LiteStatement.h"
#include "LiteResultSet.h"
#include "LitePreparedStatement.h"

#include "../../../sqlitesrc/sqlite3.h"

namespace acdk {
namespace sql {
namespace sqlite {

void 
LiteStatement::cancel()
{
}


void 
LiteStatement::clearBatch()
{
}

void 
LiteStatement::clearWarnings()
{
}


void 
LiteStatement::close()
{
  _con = Nil;
  
}

bool 
LiteStatement::execute(INP(RString) sql)
{
  RResultSet rset = executeQuery(sql);
  if (rset == Nil)
    return false;
  return true;
  // ### check if has any results/columns
}

RintArray 
LiteStatement::executeBatch()
{
  THROW1(SQLException, "Unsupported");
  return Nil;
}

RResultSet 
LiteStatement::executeQuery(INP(RString) sql)
{
  sqlite3_stmt* stm = 0;
  RString lsql = STR2LITESTR16(sql);
  LiteDb::checkResult(_con->conPtr(), sqlite3_prepare16(_con->conPtr(), lsql->byte_begin(), lsql->length() * 2, &stm, 0));
  _rset = new LiteResultSet(_con, this, new LiteTable(_con->getDb(), stm));
  return &_rset; 
}

int 
LiteStatement::executeUpdate(INP(RString) sql)
{
  return _updateCount = _con->executeUpdate(sql);
}

int 
LiteStatement::getQueryTimeout()
{
  return -1; /// ### todo implement me
}

RResultSet 
LiteStatement::getResultSet()
{
  return _rset;
}

int 
LiteStatement::getResultSetConcurrency()
{
  return ResultSet::CONCUR_READ_ONLY;
}

int 
LiteStatement::getResultSetType()
{
  if (_rset != Nil)
    return _rset->getType();
  return ResultSet::TYPE_FORWARD_ONLY;
}

int 
LiteStatement::getUpdateCount()
{
  return _updateCount;
}

RSQLWarning 
LiteStatement::getWarnings()
{
  THROW1(SQLException, "Unsupported");
  return Nil;
}

void 
LiteStatement::setCursorName(INP(RString) name)
{
  THROW1(SQLException, "Unsupported");
}

void 
LiteStatement::setEscapeProcessing(bool enable)
{
  //
}

void 
LiteStatement::setFetchDirection(int direction)
{
  THROW1(SQLException, "Unsupported");
}

void 
LiteStatement::setFetchSize(int rows)
{
  // ignore
}

void 
LiteStatement::setMaxFieldSize(int max)
{
  // ignore
}

void 
LiteStatement::setMaxRows(int max)
{
  // ignore
}

void 
LiteStatement::setQueryTimeout(int seconds)
{
  // ignore??
}

bool 
LitePreparedStatement::execute() 
{

  RResultSet rset = executeQuery();
  if (rset == Nil)
    return false;
  return true;
}

RResultSet 
LitePreparedStatement::executeQuery()
{
  return new LiteResultSet(_con, this, _table);
}

int 
LitePreparedStatement::executeUpdate()
{
  _table->next(); //
  return _table->getDb()->changes();
}

} // sqlite
} // sql 
} // acdk

