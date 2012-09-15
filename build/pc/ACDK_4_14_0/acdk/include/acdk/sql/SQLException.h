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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/SQLException.h,v 1.10 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_SQLException_h
#define acdk_sql_SQLException_h

#include "sql.h"
#include <acdk/io/Serializable.h>
#include <acdk/lang/Exception.h>

namespace acdk {
namespace sql {

//using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_THROWABLE_FQ(SQLException, acdk::lang::, Exception);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/04/08 10:53:21 $
  
*/
class ACDK_SQL_PUBLIC SQLException
: extends ::acdk::lang::Exception,
  implements ::acdk::io::Serializable

{
  ACDK_WITH_METAINFO(SQLException)
private:
/**
* This is the next exception in the chain
* @serialized
  */
  RSQLException _next;
  
  /**
    This is the state of the SQL statement at the time of the error.
    @serialized
  */
  RString _sqlState;
  
  /**
    The vendor error code for this error
    @serialized
  */
  int _vendorCode;
  
public:
  /**
    This method initializes a new instance of <code>SQLException</code>
    that does not have a descriptive messages and SQL state, and which
    has a vendor error code of 0.
  */
  SQLException()
  : Exception(),
    _next(Nil),
    _sqlState(Nil),
    _vendorCode(0)
  {
  }

  /**
    This method initializes a new instance of <code>SQLException</code>
    with the specified descriptive error message.  The SQL state of this
    instance will be <code>null</code> and the vendor error code will be 0.
  
    @param message A string describing the nature of the error.
  */
  SQLException(INP(RString) message)
  : Exception(message),
    _next(Nil),
    _sqlState(Nil),
    _vendorCode(0)
  {
  }
  
  /**
    This method initializes a new instance of <code>SQLException</code>
    with the specified descriptive error message and SQL state string.
    The vendor error code of this instance will be 0.
  
    @param message A string describing the nature of the error.
    @param SQLState A string containing the SQL state of the error.
  */
  
  SQLException(INP(RString) message, INP(RString) sqlState)
  : Exception(message),
    _next(Nil),
    _sqlState(sqlState),
    _vendorCode(0)
  {
  }
  
  /**
    This method initializes a nwe instance of <code>SQLException</code>
    with the specified descriptive error message, SQL state string, and
    vendor code.
  
    @param message A string describing the nature of the error.
    @param SQLState A string containing the SQL state of the error.
    @param vendorCode The vendor error code associated with this error.
  */
  SQLException(INP(RString) message, INP(RString) sqlState, int vendorCode)
  : Exception(message),
    _next(Nil),
    _sqlState(sqlState),
    _vendorCode(vendorCode)
  {
  }
  
  /**
    This method returns the SQLState information associated with this
    error.  The value returned is a <code>String</code> which is formatted
    using the XOPEN SQL state conventions.
  
    @return The SQL state, which may be <code>null</code>.
  */
  RString getSQLState() { return _sqlState; }
  
  
  
  /**
    This method returns the vendor specific error code associated with 
    this error.
  
    @return The vendor specific error code associated with this error.
  */
  int getErrorCode() { return _vendorCode; }
  
  /**
    This method returns the exception that is chained to this object.
  
    @return The exception chained to this object, which may be 
    <code>null</code>.
  */
  RSQLException getNextException() { return _next; }
  
  
  
  
  /**
    This method adds a new exception to the end of the chain of exceptions
    that are chained to this object.
  
    @param e The exception to add to the end of the chain.
  */
  void setNextException(INP(RSQLException) e) 
  {
    RObject ex = e;
    if (ex == Nil)
      return;
    RSQLException list_entry = this;
    while (list_entry->getNextException() != Nil)
      list_entry = list_entry->getNextException();
    list_entry->_next = e;
  }
};
    


} // sql
} // acdk

#endif //acdk_sql_SQLException_h

