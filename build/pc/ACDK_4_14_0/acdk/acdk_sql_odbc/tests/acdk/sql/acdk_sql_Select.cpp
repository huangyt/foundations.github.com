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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/tests/acdk/sql/acdk_sql_Select.cpp,v 1.18 2005/02/20 13:56:04 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/util/Properties.h>
//#include <acdk/lang/Thread.h>
#include <acdk/sql/sql.h>
#include <acdk/sql/DriverManager.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/Connection.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/ResultSet.h>
#include <acdk/sql/ResultSetMetaData.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/Win32DbgConsumer.h>
#include <acdk/sql/odbc/ODBCHandle.h>

//int forceODBCLibToLink();

namespace tests {
namespace acdk {
namespace sql {
  
BEGIN_DECLARE_TEST( Select_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( Select_Test  )

BEGIN_DEFINE_TEST( Select_Test )
  ADD_TEST( Select_Test, standard ) 
END_DEFINE_TEST( Select_Test )

using namespace ::acdk::lang; 
using namespace ::acdk::sql;


RString getDbUrl()
{
  static RString odbcUrl = Nil;
  if (odbcUrl != Nil)
    return odbcUrl;

  ::acdk::util::RProperties props = ::acdk::util::Properties::loadProperties("tests.acdk.sql.odbc.Connection");
  RString dbName = props->getProperty("dbName");
  RString user = props->getProperty("user");
  RString pass = props->getProperty("pass");
  odbcUrl = SBSTR("jdbc:odbc:" << dbName << "/user=" << user << "/password=" << pass);
  ACDK_NLOG("tests.acdk.sql.odbc", Info, "Using ODBC connection: " + odbcUrl);
  System::out->println("Using ODBC connection: " + odbcUrl);
  return odbcUrl;
}

void Select_Test::standard()
{
  //forceODBCLibToLink();
/*
  ::acdk::util::logging::RLogger log = ::acdk::util::logging::LogManager::getRootLogger();
  log->addConsumer(new ::acdk::util::logging::Win32DbgConsumer());
  ::acdk::util::logging::LogManager::Threshold = ::acdk::util::logging::SysDebug;
*/
  // This Statement is currently needed to load the ODBC-DLL.
  // If no Class from acdk::sql::odbc is used, Class::forName will fail!
  // So let's start with a dummy line ...
    RDriver tdriver = (RDriver)Class::forName("acdk::sql::odbc::ODBCDriver")->newInstance();
    //DriverManager::registerDriver(tdriver);

  //RString url = "jdbc:odbc:acdk/user=acdk/password=acdk";
  RString url = getDbUrl();
  RDriver driver = DriverManager::getDriver(url);
  if (driver == Nil)
    testAssertComment(false, "Cannot load driver: " + url);
  RConnection connection = driver->connect(url, Nil);

  {
  // delete a table, if it already exists
    RStatement statement;
    try {
      RString sql = "DROP TABLE acdk_select_test";
      RStatement statement = connection->createStatement();
      int rows = statement->executeUpdate(sql);
    } catch (RSQLException ex) { 
     
    }
  }

  {
    // create a table
    RString sql = "CREATE TABLE acdk_select_test ( intcol int, realcol real, stringcol Varchar(100) )";
    RStatement statement = connection->createStatement();
    statement->executeUpdate(sql);
    statement = connection->createStatement();
    sql = "CREATE UNIQUE INDEX acdk_select_test_idx ON acdk_select_test(intcol)";
    int rows = statement->executeUpdate(sql);
  }
  {
    for (int i = 0; i < 2; i++)
    {
      float f = i + 0.5;
      RString sql = SBSTR("INSERT INTO acdk_select_test VALUES ( " << i << ", " << f << ", " << "'stm: " << i << "')"
                          );
      RStatement statement = connection->createStatement();
      int rows = statement->executeUpdate(sql);
    }
  }
  
  {
    RStatement statement = connection->createStatement();
    RString sql = "SELECT intcol, realcol, stringcol FROM acdk_select_test"; 
    RResultSet rset = statement->executeQuery(sql);
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
    while (rset->next() == true) 
    { 
      int intcol = rset->getInt(1);
      float floatcol = rset->getFloat("realcol");
      RString sval = rset->getString("stringcol");
    }
  }
  DriverManager::deregisterDriver(tdriver);  
}


} // namespace sql
} // namespace acdk
} // namespace tests



