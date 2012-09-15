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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/tests/acdk/sql/sqlite/acdk_sql_sqlite_PreparedStatement.cpp,v 1.2 2005/04/04 23:35:29 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
//#include <acdk/lang/Thread.h>
#include <acdk/sql/sql.h>
#include <acdk/sql/DriverManager.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/Connection.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/ResultSet.h>
#include <acdk/sql/ResultSetMetaData.h>
#include <acdk/sql/PreparedStatement.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/LogManager.h>
//#include <acdk/util/logging/Win32DbgConsumer.h>

namespace tests {
namespace acdk {
namespace sql {
namespace sqlite {
  
BEGIN_DECLARE_TEST( PreparedStatement_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( PreparedStatement_Test  )

BEGIN_DEFINE_TEST( PreparedStatement_Test )
  ADD_TEST( PreparedStatement_Test, standard ) 
END_DEFINE_TEST( PreparedStatement_Test )

using namespace ::acdk::lang; 
using namespace ::acdk::sql;

RString getDbUrl();

void 
PreparedStatement_Test::standard()
{
  ::acdk::util::logging::RLogger log = ::acdk::util::logging::LogManager::getRootLogger();
#if defined(ACDK_OS_WIN32)
    //log->addConsumer(new ::acdk::util::logging::Win32DbgConsumer(new ::acdk::util::logging::StdFormatter()));
#endif
  ::acdk::util::logging::LogManager::Threshold = ::acdk::util::logging::All;
  
  RString url = getDbUrl();
  RDriver driver = DriverManager::getDriver(url);
  if (driver == Nil)
    testAssertComment(false, "Cannot load driver: " + url);
  RConnection connection = driver->connect(url, Nil);
  
  {
  // delete a table, if it already exists
    RStatement statement;
    try {
      RString sql = "DROP TABLE acdk_PreparedStatement_Test";
      RStatement statement = connection->createStatement();
      int rows = statement->executeUpdate(sql);
    } catch (RSQLException ex) { 
     
    }
  }
 
  {
    // create a table
    RString sql = "CREATE TABLE acdk_PreparedStatement_Test ( intcol int, realcol real, stringcol Varchar(100) )";
    RStatement statement = connection->createStatement();
    statement->executeUpdate(sql);
    statement = connection->createStatement();
    sql = "CREATE UNIQUE INDEX acdk_pstm_idx ON acdk_PreparedStatement_Test(intcol)";
    int rows = statement->executeUpdate(sql);
  }
  {
    for (int i = 0; i < 20; i++)
    {
      float f = i + 0.5;
      
      RString sql = SBSTR("INSERT INTO acdk_PreparedStatement_Test VALUES ( " << i << ", " << f << ", " << "'stm: " << i << "')"
                          );
      RStatement statement = connection->createStatement();
      int rows = statement->executeUpdate(sql);
    }
  }
  {
    RString sql = "SELECT intcol, realcol, stringcol FROM acdk_PreparedStatement_Test WHERE intcol < ?";
    RPreparedStatement statement = connection->prepareStatement(sql); 
    //statement->setString(1, "stm: 1");
    statement->setInt(1, 2);

    RResultSet rset = statement->executeQuery();
    RResultSetMetaData mdata = rset->getMetaData(); 
    int colcount = mdata->getColumnCount(); 
    for (int i = 1; i <= colcount; ++i)
    {
      RString label = mdata->getColumnLabel(i);
      RString name = mdata->getColumnName(i);
      RString schema = mdata->getSchemaName(i);
      int type = mdata->getColumnType(i);
      RString typname = mdata->getColumnTypeName(i);
    }
    int count = 0;
    while (rset->next() == true)
    {
      int intcol = rset->getInt(1);
      float floatcol = rset->getFloat("realcol"); 
      RString sval = rset->getString("stringcol");
      ++count;
    }
    testAssert(count == 2);
  }
  {
    RString sql = "SELECT intcol, realcol, stringcol FROM acdk_PreparedStatement_Test WHERE stringcol = ?"; 
    RPreparedStatement statement = connection->prepareStatement(sql);
    statement->setString(1, "stm: 1");
    RResultSet rset = statement->executeQuery();
    int count = 0;
    while (rset->next() == true)
    {
      int intcol = rset->getInt(1);
      float floatcol = rset->getFloat("realcol"); 
      RString sval = rset->getString("stringcol");
      ++count;
    }
    testAssert(count == 1);
  }
}


} // namespace sqlite
} // namespace sql
} // namespace acdk
} // namespace tests



