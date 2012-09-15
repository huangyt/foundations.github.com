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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCResultSetMetaData.h,v 1.4 2005/02/05 10:45:32 kommer Exp $

#ifndef acdk_sqlodbc_ResultSetMetaData_h
#define acdk_sqlodbc_ResultSetMetaData_h

#include "odbc.h"
#include "ODBCResultSet.h"

#include <acdk/sql/ResultSetMetaData.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

ACDK_DECL_CLASS(ODBCResultSetMetaData);

class ACDK_SQL_ODBC_PUBLIC ODBCResultSetMetaData
: extends Object, implements ::acdk::sql::ResultSetMetaData
{
  ACDK_WITH_METAINFO(ODBCResultSetMetaData)
public:
  ~ODBCResultSetMetaData();
/**
    This method returns the number of columns in the result set.
  
    @return The number of columns in the result set.
  
    @exception SQLException If an error occurs.
  */
  virtual int getColumnCount() THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumnCount(); }

/**
    This method test whether or not the column is an auto-increment column.
    Auto-increment columns are read-only.
  *
    @param index The index of the column to test.
  *
    @return <code>true</code> if the column is auto-increment, <code>false</code>
    otherwise.
  *
    @exception SQLException If an error occurs.
  */
  virtual bool isAutoIncrement(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isAutoIncrement(); }

/**
    This method tests whether or not a column is case sensitive in its values.
  *
    @param index The index of the column to test.
  *
    @return <code>true</code> if the column value is case sensitive,
    <code>false</code> otherwise.
  *
    @exception SQLException If an error occurs.
  */
  virtual bool isCaseSensitive(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isCaseSensitive(); }

/**
    This method tests whether not the specified column can be used in 
    a WHERE clause.
  *
    @param index The index of the column to test.
  *
    @return <code>true</code> if the column may be used in a WHERE clause,
    <code>false</code> otherwise.
  *
    @exception SQLException If an error occurs.
  */
  virtual bool isSearchable(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isSearchable(); }

/**
    This method tests whether or not the column stores a monetary value.
  *
    @param index The index of the column to test.
  *
    @return <code>true</code> if the column contains a monetary value,
    <code>false</code> otherwise.
  *
    @exception SQLException If an error occurs.
  */
  virtual bool isCurrency(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isCurrency(); }

/**
    This method returns a value indicating whether or not the specified
    column may contain a NULL value.
  *
    @param index The index of the column to test.
  *
    @return A constant indicating whether or not the column can contain NULL,
    which will be one of <code>columnNoNulls</code>,
    <code>columnNullable</code>, or <code>columnNullableUnknown</code>.
  *
    @exception SQLException If an error occurs.
  */
  virtual int isNullable(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isNullable(); }

/**
    This method tests whether or not the value of the specified column
    is signed or unsigned.
  *
    @param index The index of the column to test.
  *
    @return <code>true</code> if the column value is signed, <code>false</code>
    otherwise.
  *
    @exception SQLException If an error occurs.
  */
  virtual bool isSigned(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isSigned(); }

/**
    This method returns the maximum number of characters that can be used
    to display a value in this column.
  *
    @param index The index of the column to check.
  *
    @return The maximum number of characters that can be used to display a
    value for this column.
  *
    @exception SQLException If an error occurs.
  */
  virtual int getColumnDisplaySize(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getColumnDisplaySize(); }

/**
    This method returns a string that should be used as a caption for this
    column for user display purposes.
  *
    @param index The index of the column to check.
  *
    @return A display string for the column.
  *
    @exception SQLException If an error occurs.
  */
  virtual RString getColumnLabel(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getColumnLabel(); }

/**
    This method returns the name of the specified column.
  *
    @param index The index of the column to return the name of.
  *
    @return The name of the column.
  *
    @exception SQLException If an error occurs.
  */
  virtual RString getColumnName(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getColumnName(); }

/**
    This method returns the name of the schema that contains the specified
    column.
  *
    @param index The index of the column to check the schema name for.
  *
    @return The name of the schema that contains the column.
  *
    @exception SQLException If an error occurs.
  */
  virtual RString getSchemaName(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getSchemaName(); }

/**
    This method returns the precision of the specified column, which is the
    number of decimal digits it contains.
  *
    @param index The index of the column to check the precision on.
  *
    @return The precision of the specified column.
  *
    @exception SQLException If an error occurs.
  */
  virtual int getPrecision(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getPrecision(); }

/**
    This method returns the scale of the specified column, which is the
    number of digits to the right of the decimal point.
  *
    @param index The index column to check the scale of.
  *
    @return The scale of the column.
  *
    @exception SQLException If an error occurs.
  */
  virtual int getScale(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getScale(); }

/**
    This method returns the name of the table containing the specified
    column.
  *
    @param index The index of the column to check the table name for.
  *
    @return The name of the table containing the column.
  *
    @exception SQLException If an error occurs.
  */
  virtual RString getTableName(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getTableName(); }

/**
    This method returns the name of the catalog containing the specified
    column.
  *
    @param index The index of the column to check the catalog name for.
  *
    @return The name of the catalog containing the column.
  *
    @exception SQLException If an error occurs.
  */
  virtual RString getCatalogName(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getCatalogName(); }

/**
    This method returns the SQL type of the specified column.  This will
    be one of the constants from <code>Types</code>.
  *
    @param index The index of the column to check the SQL type of.
  *
    @return The SQL type for this column.
  *
    @exception SQLException If an error occurs.
  *
    @see Types
  */
  virtual int getColumnType(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getColumnType(); }

/**
    This method returns the name of the SQL type for this column.
  *
    @param index The index of the column to check the SQL type name for.
  *
    @return The name of the SQL type for this column.
  *
    @exception SQLException If an error occurs.
  */
  virtual RString getColumnTypeName(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getColumnTypeName(); }

/**
    This method tests whether or not the specified column is read only.
  *
    @param index The index of the column to check.
  *
    @return <code>true</code> if the column is read only, <code>false</code>
    otherwise.
  *
    @exception SQLException If an error occurs.
  */
  virtual bool isReadOnly(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isReadOnly(); }

/**
    This method tests whether or not the column may be writable.  This
    does not guarantee that a write will be successful.
  *
    @param index The index of the column to check for writability.
  *
    @return <code>true</code> if the column may be writable,
    <code>false</code> otherwise.
  *
    @exception SQLException If an error occurs.
  */
  virtual bool isWritable(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isWritable(); }

/**
    This method tests whether or not the column is writable.  This
    does guarantee that a write will be successful.
  *
    @param index The index of the column to check for writability.
  *
    @return <code>true</code> if the column is writable,
    <code>false</code> otherwise.
  *
    @exception SQLException If an error occurs.
  */
  virtual bool isDefinitelyWritable(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->isDefinitelyWritable(); }

/**
    This method returns the name of the Java class which will be used to
    create objects representing the data in this column.
  *
    @param index The index of the column to check.
  *
    @return The name of the Java class that will be used for values in
    this column.
  *
    @exception SQLException If an error occurs.
  */
  virtual RString getColumnClassName(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getColumnClassName(); }
  
   // merely for debugging-purposes
  virtual int getLength(int index)  THROWS1(::acdk::sql::RSQLException) { return _rset->_getColumn(index)->getLength(); }

  ODBCResultSetMetaData(INP(RODBCResultSet) rset);
private:
  RODBCResultSet _rset;
};          


} // odbc
} // sql
} // acdk

#endif //acdk_sqlodbc_ResultSetMetaData_h

