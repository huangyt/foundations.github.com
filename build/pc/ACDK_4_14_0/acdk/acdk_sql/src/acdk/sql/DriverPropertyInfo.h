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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/DriverPropertyInfo.h,v 1.9 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_DriverPropertyInfo_h
#define acdk_sql_DriverPropertyInfo_h

namespace acdk {
namespace sql {

using namespace acdk::lang;
using namespace acdk::io;
using namespace acdk::util;

ACDK_DECL_CLASS(DriverPropertyInfo);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/04/08 10:53:21 $
 
*/
class ACDK_SQL_PUBLIC DriverPropertyInfo
: implements acdk::lang::Object
{
  ACDK_WITH_METAINFO(DriverPropertyInfo)

  /**
  The name of the property.
  */
  RString _name;
  
  /**
  This is the value of the property.
  */
  RString _value;
  
  /**
  A description of the property, possibly <code>Nil</code>.
  */
  RString description;
  
  /**
  A flag indicating whether or not a value for this property is required
  in order to connect to the database.
  */
  bool required;
  
  /**
  If values are restricted to certain choices, this is the list of valid
  ones.  Otherwise it is <code>Nil</code>.
  */
  RObjectArrayImpl<RString> choices;
  
 
  /**
  This method initializes a new instance of <code>DriverPropertyInfo</code>
  with the specified name and value.  All other fields are defaulted.
  *
  @param name The name of the property.
  @param value The value to assign to the property.
  */
  DriverPropertyInfo(INP(RString) n, INP(RString) v)
  : Object(),
    _name(n),
    _value(v)
  {
  }
  
};          


} // sql
} // acdk

#endif //acdk_sql_DriverPropertyInfo_h

