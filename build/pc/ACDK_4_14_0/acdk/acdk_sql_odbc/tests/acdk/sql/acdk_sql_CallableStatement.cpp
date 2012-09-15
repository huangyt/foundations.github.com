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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/tests/acdk/sql/acdk_sql_CallableStatement.cpp,v 1.8 2005/04/25 19:20:45 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
//#include <acdk/lang/Thread.h>
#include <acdk/sql/sql.h>
#include <acdk/sql/DriverManager.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/Connection.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/Types.h>
#include <acdk/sql/ResultSet.h>
#include <acdk/sql/ResultSetMetaData.h>
#include <acdk/sql/PreparedStatement.h>
#include <acdk/sql/CallableStatement.h>
#include <acdk/sql/DatabaseMetaData.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/LogManager.h>
//#include <acdk/util/logging/Win32DbgConsumer.h>

namespace tests {
namespace acdk {
namespace sql {
  
BEGIN_DECLARE_TEST( CallableStatement_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( CallableStatement_Test  )

BEGIN_DEFINE_TEST( CallableStatement_Test )
  ADD_TEST( CallableStatement_Test, standard ) 
END_DEFINE_TEST( CallableStatement_Test )

using namespace ::acdk::lang; 
using namespace ::acdk::sql;
RString getDbUrl();


void 
CallableStatement_Test::standard()
{
  ::acdk::util::logging::RLogger log = ::acdk::util::logging::LogManager::getRootLogger();
    //log->addConsumer(new ::acdk::util::logging::Win32DbgConsumer(new ::acdk::util::logging::StdFormatter()));
  ::acdk::util::logging::LogManager::Threshold = ::acdk::util::logging::All;
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
  
  acdk::sql::RDatabaseMetaData dmd = connection->getMetaData();
  if (dmd->supportsStoredProcedures() == false ||
      dmd->getDatabaseProductName()->equals("ACCESS") == true)
    return;
  RString dbProductName = dmd->getDatabaseProductName();
  System::out->println("using database: " + dbProductName);

  {
  // delete a table, if it already exists
    try {
      RString sql = "DROP TABLE acdk_CallableStatement_Test";
      RStatement statement = connection->createStatement();
      int rows = statement->executeUpdate(sql);
    } catch (RSQLException ex) { 
    }
    // delete stored procedure
    try {
      RString sql = "DROP PROCEDURE acdk_sql_function";
      RStatement statement = connection->createStatement();
      int rows = statement->executeUpdate(sql);
    } catch (RSQLException ex) { 
    }
  }
 
  {
    // create a table
    RString sql = "CREATE TABLE acdk_CallableStatement_Test ( intcol int, realcol real, stringcol Varchar(100) )";
    RStatement statement = connection->createStatement();
    statement->executeUpdate(sql);
    statement = connection->createStatement();
    sql = "CREATE UNIQUE INDEX acdk_clstm_int ON acdk_CallableStatement_Test(intcol)";
    int rows = statement->executeUpdate(sql);
  }
  {
    for (int i = 0; i < 20; i++)
    {
      float f = i + 0.5;
      
      RString sql = SBSTR("INSERT INTO acdk_CallableStatement_Test VALUES ( " << i << ", " << f << ", " << "'stm: " << i << "')"
                          );
      RStatement statement = connection->createStatement();
      int rows = statement->executeUpdate(sql);
    }
  }
  
  {
    try {
    RString sql;
    if (dbProductName->equals("Oracle") == true)
    {
      sql = "CREATE PROCEDURE acdk_sql_function(instr in varchar2, innum in integer, outstr out varchar2, outnum out integer) AS\n"
                    "begin\n"
                    " outstr:=CONCAT(instr, ' SQL');\n"
                    " outnum:=innum + 1;\n"
                    "end;"
                    ;
    } 
    else if (dbProductName->equals("PostgreSQL") == true)
    {
      sql = "CREATE FUNCTION acdk_sql_function(varchar, integer) returns varchar as '\n"
        "DECLARE\n"
        "instr ALIAS FOR $1;\n"
        "instr innum for $2;\n"
        "begin\n"

                    " outstr:=CONCAT(instr, '' SQL'');\n"
        //" outnum:=innum + 1;\n"
                    "end;\n"
        "' LANGUAGE plpgsql;"
                    ;
    }
    else
    {
      sql = "CREATE PROCEDURE acdk_sql_function @instr varchar(20), @innum SMALLINT, @outstr varchar(20) OUTPUT, @outnum SMALLINT OUTPUT\n"
                    "AS\n"
                    "SET @outstr = @instr + ' SQL'\n"
                    //"print @outstr\n"
                    "SET @outnum = @innum + 1\n"
                    ;
    }
    RStatement statement = connection->createStatement(); 
    System::out->println("executing: " + sql);
    int rows = statement->executeUpdate(sql);
    } catch (RSQLException ex) {
      //ACDK_LOG(Error, "CREATE PROCEDURE Failed: " + ex->getMessage());
      testAssertComment(true, "CREATE PROCEDURE Failed for database system " + dbProductName + ": " + ex->getMessage());
      return ;
    }
  }
  {
    try {
    //RString sql = "{ ? = CALL acdk_sql_function ( ?, ? ) }";
      RString sql = "{ CALL acdk_sql_function ( ?, ?, ?, ?) }";
    RCallableStatement statement = connection->prepareCall(sql);
    statement->setString(1, "ACDK");  
    statement->setInt(2, 42);
    statement->registerOutParameter(3, ::acdk::sql::NativeVarCharSqlType);
    if (dbProductName->equals("PostgreSQL") != true)
      statement->registerOutParameter(4, ::acdk::sql::IntegerSqlType);  
    
    int count = statement->executeUpdate(); 
    RString serg = statement->getString(3); 
    //serg = serg->convert(CCAscii);
    System::out->println("Erg is: " + serg);
    testAssert(serg->equals("ACDK SQL") == true);
    if (dbProductName->equals("PostgreSQL") != true)
    {
      int ierg = statement->getInt(4);
      testAssert(ierg == 43);
    }
    } catch (RSQLException ex) {
      testAssertComment(false, "Failure on execute stored procedure: " +  ex->getMessage());
    }
    /*
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
    */
  }
  {
    try {
      RString sql = "{ CALL acdk_sql_function ( ?, ?, ?, ?) }";
    RCallableStatement statement = connection->prepareCall(sql);
    statement->setString(1, "ACDK");  
    statement->setInt(2, 42);
    statement->registerOutParameter(3, ::acdk::sql::VarCharSqlType); // now with normal char
    statement->registerOutParameter(4, ::acdk::sql::IntegerSqlType);  
    
    int count = statement->executeUpdate(); 
    RString serg = statement->getString(3); 
    testAssert(serg->equals("ACDK SQL") == true);
    int ierg = statement->getInt(4);
    testAssert(ierg == 43);

    } catch (RSQLException ex) {
      ACDK_NLOG("acdk.sql.odbc", Error, ex->getMessage());
    }
  }
  /*
  {
    RString sql = "SELECT intcol, realcol, stringcol FROM acdk_CallableStatement_Test WHERE stringcol = ?";
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
  */
  DriverManager::deregisterDriver(tdriver);
}


} // namespace sql
} // namespace acdk
} // namespace tests



