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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/SQLWarning.h,v 1.9 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_SQLWarning_h
#define acdk_sql_SQLWarning_h

namespace acdk {
namespace sql {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_THROWABLE(SQLWarning, SQLException);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/04/08 10:53:21 $
  
*/

class ACDK_SQL_PUBLIC SQLWarning
: extends SQLException,
  implements acdk::io::Serializable

{
  ACDK_WITH_METAINFO(SQLWarning)
 
public:
  /**
    This method initializes a new instance of <code>SQLWarning</code>
    that does not have a descriptive messages and SQL state, and which
    has a vendor error code of 0.
  */
  SQLWarning()
  : SQLException()
  {
  }

  /**
    This method initializes a new instance of <code>SQLWarning</code>
    with the specified descriptive error message.  The SQL state of this
    instance will be <code>null</code> and the vendor error code will be 0.
  
    @param message A string describing the nature of the error.
  */
  SQLWarning(INP(RString) message)
  : SQLException(message)
  {
  }
  
  /**
    This method initializes a new instance of <code>SQLWarning</code>
    with the specified descriptive error message and SQL state string.
    The vendor error code of this instance will be 0.
  
    @param message A string describing the nature of the error.
    @param SQLState A string containing the SQL state of the error.
  */
  SQLWarning(INP(RString) message, INP(RString) sqlState)
  : SQLException(message, sqlState)
  {
  }
  
  /**
    This method initializes a nwe instance of <code>SQLWarning</code>
    with the specified descriptive error message, SQL state string, and
    vendor code.
  
    @param message A string describing the nature of the error.
    @param SQLState A string containing the SQL state of the error.
    @param vendorCode The vendor error code associated with this error.
  */
  SQLWarning(INP(RString) message, INP(RString) sqlState, int vendorCode)
  : SQLException(message, sqlState, vendorCode)
  {
  }
  
  /**
  * This method returns the exception that is chained to this object.
  *
  * @return The exception chained to this object, which may be 
  * <code>null</code>.
  */
  RSQLWarning getNextWarning()
  {
    return (RSQLWarning)getNextException();
  }

  /**
     This method adds a new exception to the end of the chain of exceptions
     that are chained to this object.
    
     @param e The exception to add to the end of the chain.
  */
  void setNextWarning(INP(RSQLWarning) e)
  {
    setNextException(e);
  }

};
    


} // sql
} // acdk

#endif //acdk_sql_SQLWarning_h

