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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Driver.h,v 1.14 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_Driver_h
#define acdk_sql_Driver_h

#include <acdk.h>
#include <acdk/util/Properties.h>
#include "sql.h"

namespace acdk {
namespace sql {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Driver);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.14 $
  @date $Date: 2005/04/08 10:53:21 $
 
*/
class ACDK_SQL_PUBLIC Driver
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Driver)
public:
  virtual int getMajorVersionNumber() = 0;
  virtual int getMinorVersion() = 0;
  virtual bool jdbcCompliant() = 0;
  virtual RDriverPropertyInfoArray getPropertyInfo(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RException) = 0;
  virtual bool acceptsURL(INP(RString) url) THROWS1(RException) = 0;
  virtual RConnection connect(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RException) = 0;
};          


} // sql
} // acdk

#endif //acdk_sql_Driver_h



