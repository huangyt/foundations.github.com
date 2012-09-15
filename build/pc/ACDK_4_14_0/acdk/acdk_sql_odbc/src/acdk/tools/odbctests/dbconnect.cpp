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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/tools/odbctests/dbconnect.cpp,v 1.10 2005/03/08 18:55:38 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/ClassNotFoundException.h>
#include <acdk/util/Date.h>
#include <acdk/sql/DriverManager.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/ResultSetMetaData.h>
#include <acdk/sql/odbc/ODBCDriver.h>

#include "dbconnect.h"

namespace acdk {
namespace tools {
namespace odbctests {

using namespace acdk::lang;
using namespace acdk::sql;


/**
 * Class DBConnect: Constructor
 **/
DBConnect::DBConnect()
{
  _status = false;
  _out = System::out;
  _sizeOfTimeStamp = 8;
  _url = Nil;
  _driver = Nil;
  _connection = Nil;
  _connectionProperties = Nil;
  _testTable = Nil;
  _numberOfTestRecords = 0;
  _testSuccessful = false;
  _starttime = 0;
}

DBConnect::~DBConnect()
{
  if (getStatus() && (_connection != Nil)){
    _connection->close();
  }
}


void
DBConnect::init(RString testTable, int numberOfTestRecords, RConnection connection,
                ::acdk::io::RPrintWriter out)
{  
  _connection = connection;
  _testTable = testTable;
  _numberOfTestRecords = numberOfTestRecords;
  _out = out;
  if (connection != Nil) {
    setStatus(true);
  } else {
    setStatus(false);
  }
}

/**
 * connect(RString url)
 * in: URL (Connection String to Database)
 * out: bool (true if connection is ok, false if error occurs)
 **/
bool DBConnect::connect(RString url)
{
  SYNCTHIS();      // Paranoia setting ;-)

  setURL(url);     // Set new URL in member var
  setStatus(false);

  try {
    System::out->println(url);
    _driver = DriverManager::getDriver(url);
    if (_driver == Nil){
      return false;
    }
    _connectionProperties = new acdk::util::Properties();
    _connection = RConnection(_driver->connect(url, Nil));
    setStatus(true); // set connection flag to valid
  } catch (RSQLException ex) {
    // can't connect to Database
    do {
      _out->println(ex->getMessage());
      //ex->printStackTrace(System::err);
      ex = ex->getNextException();
    } while (ex != Nil);
  } // catch RSQLException

  return getStatus();
}

void
DBConnect::print(RString message)
{
  jlong now = ::acdk::util::Date().getTime();
  RString time = String::valueOf((now - getStartTime()) / 1000);
  RString timestamp;

  if (time->length() > _sizeOfTimeStamp){
    timestamp = time->substring(0,_sizeOfTimeStamp);
  } else {
    RStringBuffer buf = new StringBuffer("");
    for (int i = time->length(); i < _sizeOfTimeStamp; i++){
      buf->append("0");
    }
    buf->append(time);
    timestamp = buf->toString();
  }

  _out->print(RString("[") + getTestTable() + "][" + timestamp + "]: " + message);
  _out->flush();
}

void
DBConnect::println(RString message)
{
  SYNCTHIS();
  jlong now = ::acdk::util::Date().getTime();
  RString time = String::valueOf((now - getStartTime()) / 1000);
  RString timestamp;

  if (time->length() > _sizeOfTimeStamp){
    timestamp = time->substring(0,_sizeOfTimeStamp);
  } else {
    RStringBuffer buf = new StringBuffer("");
    for (int i = time->length(); i < _sizeOfTimeStamp; i++){
      buf->append("0");
    }
    buf->append(time);
    timestamp = buf->toString();
  }

  _out->println(RString("[") + getTestTable() + "][" + timestamp + "]: " + message);
}

/**
 * execute(RString sql) - Execute SQL Statement
 * in: sql-Statement to process
 * out: -
 **/
void
DBConnect::execute(RString sql)
{
  RStatement statement = (RStatement)_connection->createStatement();
  statement->execute(sql);
  statement->close();
  //statement = Nil;
}


/**
 * executeUpdate(RString sql) - Execute SQL Statement
 * in: sql-Statement to process
 * out: -
 **/
void
DBConnect::executeUpdate(RString sql)
{
  RStatement statement = (RStatement)_connection->createStatement();
  statement->executeUpdate(sql);
  statement->close();
  //statement = Nil;
}


/**
 * executeQuery(RString sql)
 * in (sql-Statement to process
 * out: ResultSet
 **/
::acdk::sql::RResultSet
DBConnect::executeQuery(RString sql)
{
  RStatement statement
    = (RStatement)_connection->createStatement();
  return statement->executeQuery(sql);
}

//static
void
DBConnect::loadDriver(RString driverClassName)
{
  // This Statement is currently needed to load the ODBC-DLL.
  // If no Class from acdk::sql::odbc is used, Class::forName will fail!
  // So let's start with a dummy line ...
#if defined(ACDK_OS_WIN32)
  RString dummy = ::acdk::sql::odbc::ODBCDriver::GetClass()->getName();
#endif //defined(ACDK_OS_WIN32)
 
  try {
    System::out->println(RString("LOAD DRIVER: ") + driverClassName);
    RObject driver = ::acdk::lang::Class::forName(driverClassName)->newInstance();
    DriverManager::registerDriver((RDriver)driver);
  } catch (RClassNotFoundException ex) {
    System::err->println(ex->getMessage());
  }
}

// virtual
void
DBConnect::run()
{
  if ((getStatus() == true) && ( getTestTable() != Nil)){
    setStartTime(::acdk::util::Date().getTime());
    setTestSuccessful(false);
    println("Starting Test");
    println("Timestamp-Precision is Milliseconds");
    println("Connection-Autocommit is " + String::valueOf(_connection->getAutoCommit()));
    println("Connection-ReadOnly is " + String::valueOf(_connection->isReadOnly()));
    println(RString("Number of TestRecords is ") + getNumberOfTestRecords());

    int i = 0;
    // used to store the SQLStatements
    RString clause;
    // the Resultset for Queries ..
    RResultSet query;

    try {
      clause = RString("drop table ") + getTestTable();
      println(clause);
      execute(clause);
    } catch (RSQLException ex) {}

    try {
      // create testtable
      clause = RString(RString("create table ") + getTestTable() +
                       "(" +
                          "intval int," + 
                          "realval real," +
                          "stringval Varchar(100)" + 
                       ")");
      println(clause);
      execute(clause);
      clause = RString(RString("create unique index ") + getTestTable() + "_idx on " +
		       getTestTable() + "(stringval)");
      println(clause);
      execute(clause);

      // fill testtable
      println(RString("doing ") + getNumberOfTestRecords() + " inserts:");
      for(i = 0; i < getNumberOfTestRecords(); i++){
        clause = "insert into " + getTestTable() +  " values (" + i + "," + i + ",'" + i + "')";
        executeUpdate(clause);
      }
      //execute("commit");

      // lets see if everything stored / retrieved right ...
      println(RString("checking ") + getNumberOfTestRecords() + " inserts:");
      clause = "select * from " + getTestTable() +  " order by intval asc";

      RResultSet rset = executeQuery(clause);
      RResultSetMetaData mdata = rset->getMetaData();
      i = 0;
      while(rset->next())
      {
        int int_val = rset->getInt(1);
        double double_val = rset->getDouble(2);
        RString string_val = rset->getString(3);
        if ((int_val != i) || (double_val != i) || (string_val->equals(String::valueOf(i)) == false)) 
        {
          println("MISSMATCH WHILE READING RESULTSET (should be equal to rownum):");
          println(RString("ResultSet rownum: ") + i);
          println(RString("intval = ") + int_val + " (retrieved with getInt)" );
          println(RString("  ResultSet ObjectName is ") + rset->getObject(1)->getName());
          println(RString("realval = ") + double_val + " (retrieved with getDouble)");
          println(RString("  ResultSet ObjectName is ") + rset->getObject(2)->getName());
          println(RString("stringval = ") + string_val + RString(" (retrieved with getString)"));
          println(RString("  ResultSet ObjectName is ") + rset->getObject(3)->getName());
          setTestSuccessful(false);
          return;
        }
        i++;
      }
      // there should be no more or less rows in the ResultSet ...
      if (i < getNumberOfTestRecords()){
          println("LESS ROWS IN RESULTSET THAN INSERTED!");
          setTestSuccessful(false);
          return;
      } 
      //try {
      //  rset->next();
      //  println("MORE ROWS IN RESULTSET THAN INSERTED!");
      //  setTestSuccessful(false);
      //  return;
      //} catch (RSQLException ex) {}

      // update testtable
      println(RString("doing ") + getNumberOfTestRecords() + " updates:");
      for(i = 0; i < getNumberOfTestRecords(); i++){
        clause = "update " + getTestTable() + " set " +
                    "intval = " + (i + getNumberOfTestRecords()) + ", " + 
                    "realval = " + (i + getNumberOfTestRecords()) + ", " +
                    "stringval = 'new" + (i + getNumberOfTestRecords()) + "'" +
                  "where intval=" + i;
        //if ((i == 0) || (i == getNumberOfTestRecords() - 1)){
        //  println(clause);
        //}
        executeUpdate(clause);
      }
      //execute("commit");

      // delete testtable
      println(RString("doing ") + getNumberOfTestRecords() + " deletes:");
      for(i = 0; i < getNumberOfTestRecords(); i++){
        clause = "delete from " + getTestTable() + " where " +
                 "stringval = 'new" + (i + getNumberOfTestRecords()) + "'";
        //if ((i == 0) || (i == getNumberOfTestRecords() - 1)){
        //  println(clause);
        //}
        executeUpdate(clause);
      }
      //execute("commit");

      // check the remaing number of rows
      query = executeQuery(RString("select count(*) from ") + getTestTable());
      println(RString("remaing records after delete ") + query->getInt(1));
      if (query->getInt(1) != 0){
        println(">>> TEST FAILED <<<");
        return;
      }

      // drop testtable after use ..
      //clause = RString("drop table ") + getTestTable();
      //println(clause);
      //execute(clause);

      setTestSuccessful(true);
      println(">>> TEST PASSED SUCCESSFUL! <<<");

    } catch (RException ex) {
      System::out->println(ex->getMessage());
    } catch (RThrowable ex) {
      System::out->println("Throwable caught: " + ex->getMessage());
    }

  } else {
    println(" no connection has been established yet.");
  }
}

} // odbctests
} // tools
} // acdk

