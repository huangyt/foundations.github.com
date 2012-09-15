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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/tools/odbctests/odbctests.cpp,v 1.6 2005/03/08 18:55:38 kommer Exp $


// ACDK-Language include section
#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/io/AbstractStorageWriter.h>
#include <acdk/io/NullWriter.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/sql/sql.h>
#include <acdk/sql/DriverManager.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/ResultSetMetaData.h>
#include <acdk/sql/odbc/ODBCResultSetMetaData.h>
#include <acdk/util/Date.h>

// Project include section
#include "odbctests.h"
#include "dbconnect.h"

// defining current namespace
namespace acdk {
namespace tools {
namespace odbctests {

using namespace acdk::sql;

// The PrintWriter for Testoutput
::acdk::io::RPrintWriter ODBCTests::_out;

// static 
bool 
ODBCTests::completeTest(RString connectString, RString testTable,
                         int numberOfTestRecords, int numberOfThreads,
                         RPrintWriter testWriter)
{ 
  
  if (numberOfTestRecords < 0){
    numberOfTestRecords = 0;
  }
  if (numberOfThreads < 0){
    numberOfThreads = 0;
  }

  ODBCTests::_out->println("[ODBCTEST]======================================================");
  ODBCTests::_out->println(RString("[ODBCTEST][START] ") + connectString);
  ODBCTests::_out->println("[ODBCTEST]======================================================");

  bool singleTestPassed = singleTest(connectString, testTable,
                                     numberOfTestRecords, testWriter);
  bool connTestPassed = multiConnTest(connectString, testTable,
                                     numberOfTestRecords, numberOfThreads, testWriter);
  bool stmtTestPassed = multiStmtTest(connectString, testTable,
                                     numberOfTestRecords, numberOfThreads, testWriter);
  bool result = singleTestPassed && connTestPassed && stmtTestPassed;

  if (result){
    ODBCTests::_out->println("[ODBCTEST]======================================================");
    ODBCTests::_out->println("[ODBCTEST] >>> ODBCTEST-TEST PASSED SUCCESSFUL <<<");
    ODBCTests::_out->println("[ODBCTEST]======================================================");
  } else {
    ODBCTests::_out->println("[ODBCTEST]======================================================");
    ODBCTests::_out->println("[ODBCTEST] >>> ODBCTEST-TEST FAILED <<<");
    ODBCTests::_out->println("[ODBCTEST]======================================================");
  }
  return result;
}

// static 
bool 
ODBCTests::singleTest(RString connectString, RString testTable,
                      int numberOfTestRecords, RPrintWriter testWriter)
{
  
  bool singlePassed = false;
  try {
    //--------------------
    // Single-Session Test
    //--------------------
    RDBConnect dbss;
    ODBCTests::_out->println("[ODBCTEST]======================================================");
    ODBCTests::_out->println("[ODBCTEST]SINGLE-SESSION-TEST (no Threads spawned/ 1 connection)");
    ODBCTests::_out->println("[ODBCTEST]======================================================");
    dbss = new DBConnect();
    dbss->init(testTable, numberOfTestRecords, Nil, testWriter);
    if (dbss->connect(connectString) == false){
      ODBCTests::_out->println("ODBC-Driver doesn't accept URL!");
    }
    dbss->run();
    singlePassed = dbss->getTestSuccessful();
  } catch (RSQLException ex) {
    do {
      ODBCTests::_out->println(ex->getMessage());
      ex = ex->getNextException();
    } while (ex != Nil);
  } catch (RException ex) {
    ODBCTests::_out->println(ex->getMessage());
  } catch (...) {
    ODBCTests::_out->println("Unknown Exception caught!");
  }

  if (singlePassed == true){
    ODBCTests::_out->println("[ODBCTEST] >>> TEST PASSED SUCCESSFUL <<<");
  } else {
    ODBCTests::_out->println("[ODBCTEST] >>> TEST FAILED <<<");
  }
  
  return singlePassed;
}

// static 
bool 
ODBCTests::multiConnTest(RString connectString, RString testTable,
                         int numberOfTestRecords, int numberOfThreads,
                         RPrintWriter testWriter)
{
  
  int i = 0;
  bool connTestPassed = false;
  RDBConnectArray db = new DBConnectArray(numberOfThreads);
  RThreadArray db_thread = new ThreadArray(numberOfThreads);

  try {
    //----------------------
    // Multi-Connection-Mode
    //----------------------
    ODBCTests::_out->println("[ODBCTEST]=====================================================");
    ODBCTests::_out->println(RString("[ODBCTEST]MULTI-CONNECTION-TEST (") +
                                     numberOfThreads + " Threads & connections spawned)");
    ODBCTests::_out->println("[ODBCTEST]=====================================================");

    // connect ...
    for (i = 0; i < numberOfThreads; i++){
      db[i] = new DBConnect();
      db[i]->init(RString(testTable) + i, numberOfTestRecords, Nil, testWriter);
      if (db[i]->connect(connectString) == false){
        ODBCTests::_out->println("ODBC-Driver doesn't accept URL!");
      }
    }

    for (i = 0; i < numberOfThreads; i++){
      db_thread[i] = new Thread((RRunnable)db[i]);
      db_thread[i]->start();
    }

    for (i = 0; i < numberOfThreads; i++){
      db_thread[i]->join();
    }

    i = 0;
    bool result = true;
    while(i < numberOfThreads)
    {
      if (db[i]->getTestSuccessful()){
        ODBCTests::_out->println(RString("[ODBCTEST] >>> THREAD") + i + " PASSED SUCCESSFUL <<<");
      } else {
        ODBCTests::_out->println(RString("[ODBCTEST] >>> THREAD") + i + " FAILED <<<");
      }
      result = result && db[i]->getTestSuccessful();
      i++;
    }
    connTestPassed = result;

  } catch (RSQLException ex) {
    do {
      ODBCTests::_out->println(ex->getMessage());
      ex = ex->getNextException();
    } while (ex != Nil);
  } catch (RException ex) {
    ODBCTests::_out->println(ex->getMessage());
  } catch (...) {
    ODBCTests::_out->println("Unknown Exception caught!");
  }

  if (connTestPassed == true){
    ODBCTests::_out->println("[ODBCTEST] >>> TEST PASSED SUCCESSFUL <<<");
  } else {
    ODBCTests::_out->println("[ODBCTEST] >>> TEST FAILED <<<");
  }
  
  return connTestPassed;
}

// static 
bool 
ODBCTests::multiStmtTest(RString connectString, RString testTable,
                         int numberOfTestRecords, int numberOfThreads,
                         RPrintWriter testWriter)
{
  
  int i = 0;
  bool stmtTestPassed = false;
  RDBConnectArray db = new DBConnectArray(numberOfThreads);
  RThreadArray db_thread = new ThreadArray(numberOfThreads);

  try {
    //---------------------------------------
    // SINGLE-Connection-MULTI-Statement-Mode
    //---------------------------------------
    ODBCTests::_out->println("[ODBCTEST]=====================================================");
    ODBCTests::_out->println(RString("[ODBCTEST]SINGLE-CONNECTION-MULTI-THREAD-TEST (") +
                                     numberOfThreads + " Threads spawned with same Connection)");
    ODBCTests::_out->println("[ODBCTEST]=====================================================");

    RConnection connection; 
    RDriver driver = DriverManager::getDriver(connectString);
    if (driver == Nil){
      ODBCTests::_out->println("ODBC-Driver doesn't accept URL!");
    }
    connection = RConnection(driver->connect(connectString, Nil));

    // connect ...
    for (i = 0; i < numberOfThreads; i++){
      db[i] = new DBConnect();
      db[i]->init(RString(testTable) + i, numberOfTestRecords, connection, testWriter);
      if (db[i]->connect(connectString) == false){
        ODBCTests::_out->println("ODBC-Driver doesn't accept URL!");
      }
    }

    for (i = 0; i < numberOfThreads; i++){
      db_thread[i] = new Thread((RRunnable)db[i]);
      db_thread[i]->start();
    }

    for (i = 0; i < numberOfThreads; i++){
      db_thread[i]->join();
    }

    i = 0;
    bool result = true;
    while(i < numberOfThreads)
    {
      if (db[i]->getTestSuccessful()){
        ODBCTests::_out->println(RString("[ODBCTEST] >>> THREAD") + i + " PASSED SUCCESSFUL <<<");
      } else {
        ODBCTests::_out->println(RString("[ODBCTEST] >>> THREAD") + i + " FAILED <<<");
      }
      result = result && db[i]->getTestSuccessful();
      i++;
    }
    stmtTestPassed = result;

  } catch (RSQLException ex) {
    do {
      ODBCTests::_out->println(ex->getMessage());
      ex = ex->getNextException();
    } while (ex != Nil);
  } catch (RException ex) {
    ODBCTests::_out->println(ex->getMessage());
  } catch (...) {
    ODBCTests::_out->println("Unknown Exception caught!");
  }

  if (stmtTestPassed == true){
    ODBCTests::_out->println("[ODBCTEST] >>> TEST PASSED SUCCESSFUL <<<");
  } else {
    ODBCTests::_out->println("[ODBCTEST] >>> TEST FAILED <<<");
  }
  
  return stmtTestPassed;
}



//static 
int 
ODBCTests::acdkmain(RStringArray args)
{
  
  // This PrintWriter is used by the underlying Tests (use for verbose Mode)
  RPrintWriter testWriter = new PrintWriter(new NullWriter());
  // RPrintWriter testWriter = System::out;
  
  // This PrintWriter is used for the Test-Summary
  ODBCTests::_out = System::out;

  ODBCTests::_out->println("[ODBCTEST][BEGIN]");
  ODBCTests::_out->println("[ODBCTEST][CLASSNAME] ::acdk::tools::odbc::ODBCTEST");

  // load ODBCDriver ...
  DBConnect::loadDriver("::acdk::sql::odbc::ODBCDriver");

  bool result = true;
  try {
    
	// Test(Connect-String, Test-Table prefix, count of test records, Counts of Threads)
    result = completeTest("jdbc:odbc:acdk/user=acdk/password=acdk", "TEMP_ACDK_ODBCTEST", 500, 2, System::out) && result;

    

    if (result == true){
      ODBCTests::_out->println("[ODBCTEST][SUMMARY] >>> ALL TESTS PASSED SUCCESSFUL <<<");
    } else {
      ODBCTests::_out->println("[ODBCTEST][SUMMARY] >>> TEST FAILED <<<");
    }
    ODBCTests::_out->println("[ODBCTEST][END]");

  } catch (RSQLException ex) {
    do {
      ODBCTests::_out->println(ex->getMessage());
      // ex->printStackTrace(System::err);
      ex = ex->getNextException();
    } while (ex != Nil);
  } catch (RException ex) {
    ODBCTests::_out->println(ex->getMessage());
  }
  return 0;
}

} // namespace odbctests
} // namespace tools
} // namespace acdk



int
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(acdk::tools::odbctests::ODBCTests::acdkmain, argc, argv, envptr);
}
