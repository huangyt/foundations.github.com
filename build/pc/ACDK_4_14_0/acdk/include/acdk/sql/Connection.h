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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Connection.h,v 1.13 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_sql_Connection_h
#define acdk_sql_Connection_h

#include <acdk.h>
#include <acdk/util/Map.h>

#include "sql.h"

namespace acdk {
namespace sql {

//using namespace acdk::lang;

ACDK_DECL_INTERFACE(Connection);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.13 $
  @date $Date: 2005/04/08 10:53:20 $
 
*/
class ACDK_SQL_PUBLIC Connection
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Connection)
public:
  /**
    JDKDOC: Indicates that transactions are not supported. 
    API: JDK */
  static int TRANSACTION_NONE;
          
  /**
    JDKDOC: Dirty reads are prevented; non-repeatable reads and phantom reads can occur. 
    API: JDK */
  static int TRANSACTION_READ_COMMITTED;
          
  /**
    JDKDOC: Dirty reads, non-repeatable reads and phantom reads can occur. 
    API: JDK */
  static int TRANSACTION_READ_UNCOMMITTED;
          
  /**
    JDKDOC: Dirty reads and non-repeatable reads are prevented; phantom reads can occur. 
    API: JDK */
  static int TRANSACTION_REPEATABLE_READ;
          
  /**
    JDKDOC: Dirty reads, non-repeatable reads and phantom reads are prevented. 
    API: JDK */
  static int TRANSACTION_SERIALIZABLE;
          

  /**
    JDKDOC: Clears all warnings reported for this Connection object. 
    API: JDK */
  virtual void clearWarnings() = 0;
          
  /**
    JDKDOC: Releases a Connection's database and JDBC resources immediately instead of waiting for them to be automatically released. 
    API: JDK */
  virtual void close() = 0;
          
  /**
    JDKDOC: Makes all changes made since the previous commit/rollback permanent and releases any database locks currently held by the Connection. 
    API: JDK */
  virtual void commit() = 0;
          
  /**
    JDKDOC: Creates a Statement object for sending SQL statements to the database. 
    API: JDK */
  virtual RStatement createStatement() = 0;
          
  /**
    JDKDOC: JDBC 2.0 Creates a Statement object that will generate ResultSet objects with the given type and concurrency. 
    API: JDK */
  virtual RStatement createStatement(int resultSetType, int resultSetConcurrency) = 0;
          
  /**
    JDKDOC: Gets the current auto-commit state. 
    API: JDK */
  virtual bool getAutoCommit() = 0;
          
  /**
    JDKDOC: Returns the Connection's current catalog name. 
    API: JDK */
  virtual RString getCatalog() = 0;
          
  /**
    JDKDOC: Gets the metadata regarding this connection's database. 
    API: JDK */
  virtual RDatabaseMetaData getMetaData() = 0;
          
  /**
    JDKDOC: Gets this Connection's current transaction isolation level. 
    API: JDK */
  virtual int getTransactionIsolation() = 0;
          
  /**
    JDKDOC: JDBC 2.0 Gets the type map object associated with this connection. 
    API: JDK */
  virtual acdk::util::RMap getTypeMap() = 0;
          
  /**
    JDKDOC: Returns the first warning reported by calls on this Connection. 
    API: JDK */
  virtual RSQLWarning getWarnings() = 0;
          
  /**
    JDKDOC: Tests to see if a Connection is closed. 
    API: JDK */
  virtual bool isClosed() = 0;
          
  /**
    JDKDOC: Tests to see if the connection is in read-only mode. 
    API: JDK */
  virtual bool isReadOnly() = 0;
          
  /**
    JDKDOC: Converts the given SQL statement into the system's native SQL grammar. 
    API: JDK */
  virtual RString nativeSQL(INP(RString) sql) = 0;
          
  /**
    JDKDOC: Creates a CallableStatement object for calling database stored procedures. 
    API: JDK */
  virtual RCallableStatement prepareCall(INP(RString) sql) = 0;
          
  /**
    JDKDOC: JDBC 2.0 Creates a CallableStatement object that will generate ResultSet objects with the given type and concurrency. 
    API: JDK */
  //## notimplemented virtual RCallableStatement prepareCall(RString sql, int resultSetType, int resultSetConcurrency) = 0;
  
  /**
    JDKDOC: Creates a PreparedStatement object for sending parameterized SQL statements to the database. 
    API: JDK */
  virtual RPreparedStatement prepareStatement(INP(RString) sql) = 0;
          
  /**
    JDKDOC: JDBC 2.0 Creates a PreparedStatement object that will generate ResultSet objects with the given type and concurrency. 
    API: JDK */
  //## notimplemented virtual RPreparedStatement prepareStatement(RString sql, int resultSetType, int resultSetConcurrency) = 0;
          
  /**
    JDKDOC: Drops all changes made since the previous commit/rollback and releases any database locks currently held by this Connection. 
    API: JDK */
  virtual void rollback() = 0;
          
  /**
    JDKDOC: Sets this connection's auto-commit mode. 
    API: JDK */
  virtual void setAutoCommit(bool autoCommit) = 0;
          
  /**
    JDKDOC: Sets a catalog name in order to select a subspace of this Connection's database in which to work. 
    API: JDK */
  virtual void setCatalog(INP(RString) catalog) = 0;
          
  /**
    JDKDOC: Puts this connection in read-only mode as a hint to enable database optimizations. 
    API: JDK */
  virtual void setReadOnly(bool readOnly) = 0;
          
  /**
    JDKDOC: Attempts to change the transaction isolation level to the one given. 
    API: JDK */
  virtual void setTransactionIsolation(int level) = 0;
          
  /**
    JDKDOC: JDBC 2.0 Installs the given type map as the type map for this connection. 
    API: JDK */
  virtual void setTypeMap(INP(acdk::util::RMap) map) = 0;
          

};
    


} // sql
} // acdk

#endif //acdk_sql_Connection_h

