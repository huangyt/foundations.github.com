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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCConnection.h,v 1.5 2005/02/05 10:45:31 kommer Exp $

#ifndef acdk_sqlodbc_Connection_h
#define acdk_sqlodbc_Connection_h

#include "odbc.h"
#include "ODBCHandle.h"
#include "ODBCDatabaseMetaData.h"

#include <acdk/sql/Connection.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;
using namespace acdk::sql;

ACDK_DECL_CLASS(ODBCConnection);

/**
  Provides a connection to ODBC data sources
  @todo The ODBC connection is not fully implemented.
        - Support more Data types in all classes. Currently only Strings and int are tested.
          Some other types are implemented, but not tested.
        - Support the set methods of the ResultSet 
        - Support CallableConnection
        - Support / test transactions

*/
class ACDK_SQL_ODBC_PUBLIC ODBCConnection
: extends Object, implements ::acdk::sql::Connection
{
  ACDK_WITH_METAINFO(ODBCConnection)
public:
  /**
    JDKDOC: Clears all warnings reported for this Connection object. 
    API: JDK */
  virtual void clearWarnings() { if (_dbch != Nil) _dbch->_clearWarnings(); }
          
  /**
    JDKDOC: Releases a Connection's database and JDBC resources immediately instead of waiting for them to be automatically released. 
    API: JDK */
  virtual void close() { if (_dbch != Nil) _dbch->deinit(); _dbch = Nil; }
          
  /**
    JDKDOC: Makes all changes made since the previous commit/rollback permanent and releases any database locks currently held by the Connection. 
    API: JDK */
  virtual void commit();
          
  /**
    JDKDOC: Creates a Statement object for sending SQL statements to the database. 
    API: JDK */
  virtual RStatement createStatement();
          
  /**
    JDKDOC: JDBC 2.0 Creates a Statement object that will generate ResultSet objects with the given type and concurrency. 
    API: JDK */
  virtual RStatement createStatement(int resultSetType, int resultSetConcurrency) { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    JDKDOC: Gets the current auto-commit state. 
    API: JDK */
  virtual bool getAutoCommit() { return _autoCommit; }
          
  /**
    JDKDOC: Returns the Connection's current catalog name. 
    API: JDK */
  virtual RString getCatalog() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    JDKDOC: Gets the metadata regarding this connection's database. 
    API: JDK */
  virtual RDatabaseMetaData getMetaData();
          
  /**
    JDKDOC: Gets this Connection's current transaction isolation level. 
    API: JDK */
  virtual int getTransactionIsolation() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    JDKDOC: JDBC 2.0 Gets the type map object associated with this connection. 
    API: JDK 
    @todo not implemented
  */
  virtual acdk::util::RMap getTypeMap() { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    JDKDOC: Returns the first warning reported by calls on this Connection. 
    API: JDK 
    @todo test
  */
  virtual ::acdk::sql::RSQLWarning getWarnings() { return (_dbch != Nil)? _dbch->_getWarnings() : ::acdk::sql::RSQLWarning(Nil); }
          
  /**
    JDKDOC: Tests to see if a Connection is closed. 
    API: JDK 
  */
  virtual bool isClosed() { return (_opened)? false:true; }
          
  /**
    JDKDOC: Tests to see if the connection is in read-only mode. 
    API: JDK */
  virtual bool isReadOnly() { return _readOnly; }
          
  /**
    JDKDOC: Converts the given SQL statement into the system's native SQL grammar. 
    API: JDK 
    @todo not implemented
  */
  virtual RString nativeSQL(INP(RString) sql) { THROW0(UnsupportedOperationException); return Nil; }
          
  /**
    JDKDOC: Creates a CallableStatement object for calling database stored procedures. 
    API: JDK */
  virtual RCallableStatement prepareCall(INP(RString) sql);
          
  /**
    JDKDOC: JDBC 2.0 Creates a CallableStatement object that will generate ResultSet objects with the given type and concurrency. 
    API: JDK */
  //## notimplemented virtual RCallableStatement prepareCall(RString sql, int resultSetType, int resultSetConcurrency) = 0;
  
  /**
    JDKDOC: Creates a PreparedStatement object for sending parameterized SQL statements to the database. 
    API: JDK 
  */
    virtual RPreparedStatement prepareStatement(INP(RString) sql);
          
  /**
    JDKDOC: JDBC 2.0 Creates a PreparedStatement object that will generate ResultSet objects with the given type and concurrency. 
    API: JDK */
  //## notimplemented virtual RPreparedStatement prepareStatement(RString sql, int resultSetType, int resultSetConcurrency) = 0;
          
  /**
    JDKDOC: Drops all changes made since the previous commit/rollback and releases any database locks currently held by this Connection. 
    API: JDK 
    @todo not implemented
  */
  virtual void rollback() { THROW0(UnsupportedOperationException); }
          
  /**
    JDKDOC: Sets this connection's auto-commit mode. 
    API: JDK 
    @todo test
  */
  virtual void setAutoCommit(bool autoCommit) { _autoCommit = autoCommit; _setSQLFlag(SQL_ATTR_AUTOCOMMIT, (_autoCommit == false)? SQL_AUTOCOMMIT_OFF : SQL_AUTOCOMMIT_ON); }
          
  /**
    JDKDOC: Sets a catalog name in order to select a subspace of this Connection's database in which to work. 
    API: JDK 
    @todo not implemented
  */
  virtual void setCatalog(INP(RString) catalog) { THROW0(UnsupportedOperationException); }
          
  /**
    JDKDOC: Puts this connection in read-only mode as a hint to enable database optimizations. 
    API: JDK */
  virtual void setReadOnly(bool readOnly) { _readOnly = readOnly; _setSQLFlag(SQL_ATTR_ACCESS_MODE, (_readOnly == true)? SQL_MODE_READ_ONLY : SQL_MODE_READ_WRITE); }
          
  /**
    JDKDOC: Attempts to change the transaction isolation level to the one given. 
    API: JDK 
    @todo not implemented
  */
  virtual void setTransactionIsolation(int level) { THROW0(UnsupportedOperationException); }
          
  /**
    JDKDOC: JDBC 2.0 Installs the given type map as the type map for this connection. 
    API: JDK 
    @todo not implemented
  */
  virtual void setTypeMap(INP(acdk::util::RMap) map) { THROW0(UnsupportedOperationException); }
        
public:
  virtual RODBCHandle _getODBCHandle() { return _dbch; }
  virtual RODBCDriver _getODBCDriver() { return _driver; }

  ODBCConnection(INP(RODBCDriver) driver);
  ~ODBCConnection();
  RODBCConnection init(INP(RString) url, INP(acdk::util::RProperties) prop);

private:
  void _setSQLFlag(SQLUINTEGER key, SQLUINTEGER val);
  void _setSQLFlags();
  void _parseProperties(INP(acdk::util::RProperties) prop);

private:
  RODBCDriver _driver;
  RODBCHandle _dbch;
  //RODBCDatabaseMetaData _dbmd;
  acdk::util::RProperties _prop;
  RString _url;
  int _loginTimeout;
  int _connectionTimeout;
  int _queryTimeout;
  bool _opened;
  bool _readOnly;
  bool _autoCommit;
  bool _asyncCalls;
  bool _traceCalls;
  int _maxRows;
  int _maxLength;
  bool _scanEscapes;
  bool _scrollableCursors;
};
    


} // odbc
} // sql
} // acdk

#endif //acdk_sqlodbc_Connection_h

