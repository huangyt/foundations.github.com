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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Array.h,v 1.13 2005/04/05 23:21:54 kommer Exp $

#ifndef acdk_sql_Array_h
#define acdk_sql_Array_h

#include "sql.h"
#include <acdk/util/Date.h>
#include <acdk/util/Map.h>


namespace acdk {
namespace sql {


ACDK_DECL_INTERFACE(Array);

class ACDK_SQL_PUBLIC Array
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Array)
public:
  /**
  * This method returns the name of the SQL type of the elements in this
  * array.  This name is database specific.
  *
  * @param The name of the SQL type of the elements in this array.
  *
  * @exception SQLException If an error occurs.
  */
  virtual RString getBaseTypeName() THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns the JDBC type identifier of the elements in this
  * array.  This will be one of the values defined in the <code>Types</code>
  * class.
  *
  * @return The JDBC type of the elements in this array.
  *
  * @exception SQLException If an error occurs.
  * 
  * @see Types
  */
  virtual int getBaseType() THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns the contents of this array.  This object returned
  * will be an array of Java objects of the appropriate types.
  *
  * @return The contents of the array as an array of Java objects.
  *
  * @exception SQLException If an error occurs.
  */
  virtual RObject getArray() THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns the contents of this array.  The specified
  * <code>Map</code> will be used to override selected mappings between
  * SQL types and Java classes.
  * 
  * @param map A mapping of SQL types to Java classes.
  *
  * @return The contents of the array as an array of Java objects.
  *
  * @exception SQLException If an error occurs.
  */
  virtual RObject getArray(IN(acdk::util::RMap) map) THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns a portion of this array starting at index
  * <code>offset</code> into the array and continuing for <code>length</code>
  * elements.  Fewer than the requested number of elements will be
  * returned if the array does not contain the requested number of elements.
  * The object returned will be an array of Java objects of
  * the appropriate types.
  *
  * @param offset The offset into this array to start returning elements from.
  * @param count The requested number of elements to return.
  *
  * @return The requested portion of the array.
  *
  * @exception SQLException If an error occurs.
  */
  virtual RObject getArray(int offset, int count) THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns a portion of this array starting at index
  * <code>offset</code> into the array and continuing for <code>length</code>
  * elements.  Fewer than the requested number of elements will be
  * returned if the array does not contain the requested number of elements.
  * The object returned will be an array of Java objects.  The specified
  * <code>Map</code> will be used for overriding selected SQL type to
  * Java class mappings.
  *
  * @param offset The offset into this array to start returning elements from.
  * @param count The requested number of elements to return.
  * @param map A mapping of SQL types to Java classes.
  *
  * @return The requested portion of the array.
  *
  * @exception SQLException If an error occurs.
  */
  virtual RObject getArray(int index, int count, IN(acdk::util::RMap) map) THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns the elements in the array as a <code>ResultSet</code>.
  * Each row of the result set will have two columns.  The first will be
  * the index into the array of that row's contents.  The second will be
  * the actual value of that array element.
  *
  * @return The elements of this array as a <code>ResultSet</code>.
  *
  * @exception SQLException If an error occurs.
  *
  * @see ResultSet
  */
  virtual RResultSet getResultSet() THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns the elements in the array as a <code>ResultSet</code>.
  * Each row of the result set will have two columns.  The first will be
  * the index into the array of that row's contents.  The second will be
  * the actual value of that array element.  The specified <code>Map</code>
  * will be used to override selected default mappings of SQL types to
  * Java classes.
  *
  * @param map A mapping of SQL types to Java classes.
  *
  * @return The elements of this array as a <code>ResultSet</code>.
  *
  * @exception SQLException If an error occurs.
  *
  * @see ResultSet
  */
  virtual RResultSet getResultSet(IN(acdk::util::RMap) map) THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns a portion of the array as a <code>ResultSet</code>.
  * The returned portion will start at index <code>offset</code> into the
  * array and up to <code>length</code> elements will be returned.
  * <p>
  * Each row of the result set will have two columns.  The first will be
  * the index into the array of that row's contents.  The second will be
  * the actual value of that array element.
  *
  * @param offset The index into the array to start returning elements from.
  * @param length The requested number of elements to return.
  *
  * @return The requested elements of this array as a <code>ResultSet</code>.
  *
  * @exception SQLException If an error occurs.
  *
  * @see ResultSet
  */
  virtual RResultSet getResultSet(int index, int count) THROWS1(RSQLException) = 0;

/*************************************************************************/

/**
  * This method returns a portion of the array as a <code>ResultSet</code>.
  * The returned portion will start at index <code>offset</code> into the
  * array and up to <code>length</code> elements will be returned.
  * <p>
  * Each row of the result set will have two columns.  The first will be
  * the index into the array of that row's contents.  The second will be
  * the actual value of that array element.  The specified <code>Map</code>
  * will be used to override selected default mappings of SQL types to
  * Java classes.
  *
  * @param offset The index into the array to start returning elements from.
  * @param length The requested number of elements to return.
  * @param map A mapping of SQL types to Java classes.
  *
  * @return The requested elements of this array as a <code>ResultSet</code>.
  *
  * @exception SQLException If an error occurs.
  *
  * @see ResultSet
  */
  virtual RResultSet getResultSet(int index, int count, IN(acdk::util::RMap) map) THROWS1(RSQLException) = 0;

};
    


} // sql
} // acdk

#endif //acdk_sql_Array_h

