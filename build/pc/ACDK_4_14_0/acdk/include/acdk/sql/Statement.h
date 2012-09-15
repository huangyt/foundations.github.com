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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Statement.h,v 1.12 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_Statement_h
#define acdk_sql_Statement_h

#include <acdk.h>
#include "Config.h"
#include "ResultSet.h"

namespace acdk {
namespace sql {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Statement);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.12 $
  @date $Date: 2005/04/08 10:53:21 $
  
*/
class ACDK_SQL_PUBLIC Statement
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Statement)
public:
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Adds a SQL command to the current batch of commmands for the statement. 
  */
  virtual void addBatch(INP(RString) sql) = 0;
    
  /**
    API: JDK
    JDKDOC: Cancels this Statement object if both the DBMS and driver support aborting an SQL statement. 
  */
  virtual void cancel() = 0;
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Makes the set of commands in the current batch empty. 
  */
  virtual void clearBatch() = 0;
          
  /**
    API: JDK
    JDKDOC: Clears all the warnings reported on this Statement object. 
  */
  virtual void clearWarnings() = 0;

  /**
    API: JDK
    JDKDOC: Releases this Statement object's database and JDBC resources immediately instead of waiting for this to happen when it is automatically closed. 
  */
  virtual void close() = 0;
          
  
  /**
    API: JDK
    JDKDOC: Executes a SQL statement that may return multiple results. 
  */
  virtual bool execute(INP(RString) sql) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Submits a batch of commands to the database for execution. 
  */
  virtual RintArray executeBatch() = 0;
          
  /**
    API: JDK
    JDKDOC: Executes a SQL statement that returns a single ResultSet. 
  */
  virtual RResultSet executeQuery(INP(RString) sql) = 0;
          
  /**
    API: JDK
    JDKDOC: Executes an SQL INSERT, UPDATE or DELETE statement. 
  */
  virtual int executeUpdate(INP(RString) sql) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Returns the Connection object that produced this Statement object. 
  */
  virtual RConnection getConnection() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Retrieves the direction for fetching rows from database tables that is the default for result sets generated from this Statement object. 
  */
  virtual int getFetchDirection() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Retrieves the number of result set rows that is the default fetch size for result sets generated from this Statement object. 
  */
  virtual int getFetchSize() = 0;
          
  /**
    API: JDK
    JDKDOC: Returns the maximum number of bytes allowed for any column value. 
  */
  virtual int getMaxFieldSize() = 0;
          
  /**
    API: JDK
    JDKDOC: Retrieves the maximum number of rows that a ResultSet can contain. 
  */
  virtual int getMaxRows() = 0;
          
  /**
    API: JDK
    JDKDOC: Moves to a Statement's next result. 
  */
  virtual bool getMoreResults() = 0;
          
  /**
    API: JDK
    JDKDOC: Retrieves the number of seconds the driver will wait for a Statement to execute. 
  */
  virtual int getQueryTimeout() = 0;
          
  /**
    API: JDK
    JDKDOC: Returns the current result as a ResultSet object. 
  */
  virtual RResultSet getResultSet() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Retrieves the result set concurrency. 
  */
  virtual int getResultSetConcurrency() = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Determine the result set type. 
  */
  virtual int getResultSetType() = 0;
          
  /**
    API: JDK
    JDKDOC: Returns the current result as an update count; if the result is a ResultSet or there are no more results, -1 is returned. 
  */
  virtual int getUpdateCount() = 0;
          
  /**
    API: JDK
    JDKDOC: Retrieves the first warning reported by calls on this Statement. 
  */
  virtual RSQLWarning getWarnings() = 0;
          
  /**
    API: JDK
    JDKDOC: Defines the SQL cursor name that will be used by subsequent Statement execute methods. 
  */
  virtual void setCursorName(INP(RString) name) = 0;
          
  /**
    API: JDK
    JDKDOC: Sets escape processing on or off. 
  */
  virtual void setEscapeProcessing(bool enable) = 0;
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gives the driver a hint as to the direction in which the rows in a result set will be processed. 
  */
  virtual void setFetchDirection(int direction) = 0; 
          
  /**
    API: JDK
    JDKDOC: JDBC 2.0 Gives the JDBC driver a hint as to the number of rows that should be fetched from the database when more rows are needed. 
  */
  virtual void setFetchSize(int rows) = 0;
          
  /**
    API: JDK
    JDKDOC: Sets the limit for the maximum number of bytes in a column to the given number of bytes. 
  */
  virtual void setMaxFieldSize(int max) = 0;
          
  /**
    API: JDK
    JDKDOC: Sets the limit for the maximum number of rows that any ResultSet can contain to the given number. 
  */
  virtual void setMaxRows(int max) = 0;
          
  /**
    API: JDK
    JDKDOC: Sets the number of seconds the driver will wait for a Statement to execute to the given number of seconds. 
  */
  virtual void setQueryTimeout(int seconds) = 0;
};          


} // sql
} // acdk

#endif //acdk_sql_Statement_h

