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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/tools/odbctests/dbconnect.h,v 1.5 2005/02/05 10:45:32 kommer Exp $

#ifndef acdk_tools_odbctests_dbconnect_h
#define acdk_tools_odbctests_dbconnect_h

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Runnable.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/sql/sql.h>
#include <acdk/sql/Driver.h>
#include <acdk/sql/Connection.h>
#include <acdk/sql/ResultSet.h>


namespace acdk {
namespace tools {
namespace odbctests {

using namespace acdk::lang;
using namespace acdk::sql;


ACDK_DECL_CLASS(DBConnect);

/**
 * Class DBConnect: Establish a connection to a Database via ODBC
 **/
class DBConnect
  : extends acdk::lang::Object,
  implements Runnable
{
 ACDK_WITH_METAINFO(DBConnect)
  
 private:
   /** ODBC connection url */
  RString _url;
  /** set if a connection is established at present*/
  bool _status;
  /** The Driver */
  RDriver _driver;
  /** The Connection */
  RConnection _connection;
  /** The Properties Object */
  ::acdk::util::RProperties _connectionProperties;
  /** Textoutput will be passed to this PrintWriter */
  ::acdk::io::RPrintWriter _out;
  /** Tablename for Testing */
  RString _testTable;
  /** The total Number of records to be inserted by the test. */
  int _numberOfTestRecords;
  /** Start-Timestamp of Test */
  jlong _starttime;
  /** Lenght of Timestamp in output. Timestamp precision is milliseconds */
  int _sizeOfTimeStamp;
  /** indicates success of Test */
  bool _testSuccessful;

  /** Status can only be set by member-functions */
  void setStatus(bool stat) {_status = stat;}
  /** URL will only be set by connect */
  void setURL(RString url) {_url = url;}
  void setStartTime(jlong starttime) {_starttime = starttime;}
  void setTestSuccessful(bool stat) {_testSuccessful = stat;}

 public:
  
  //** Default Constructor only initialising _status with false */
  DBConnect();
  //** closes open connection */
  ~DBConnect();

  //** Init-Method */
  void init(RString testtable, int numberOfTestRecords, RConnection connection = Nil,
    ::acdk::io::RPrintWriter out = System::out);

  // get- and set-methods for member vars
  RString getURL() {return _url;}
  bool getStatus() {return _status;}
  RString getTestTable() {return _testTable;}
  void setTestTable(RString table) {_testTable = table;}
  int getNumberOfTestRecords() {return _numberOfTestRecords;}
  void setNumberOfTestRecords(int newVal) {_numberOfTestRecords = newVal;}
  jlong getStartTime() {return _starttime;}
  bool getTestSuccessful() {return _testSuccessful;}

  //** Establish connection */
  bool connect(RString url);          // establish db connection

  // Functions that are using the connection
  /** Send SQL string to Database  */
  void execute(RString sql);

  /** Send SQL string to Database for INSERT, UPDATE, DELETE */
  void executeUpdate(RString sql);

  /** Send SQL string to Database. ResultSet is returned */
  RResultSet executeQuery(RString sql);

  // Implementation of Runnable-Interface
  virtual void run();

  // load the Database-Driver
  static void loadDriver(RString driverClassName);

  // println message to _out
  void println(RString message);

  // print message to _out
  void print(RString message);
};

} // odbctests
} // tools
} // acdk


#endif // acdk_tools_odbctests_dbconnect_h
