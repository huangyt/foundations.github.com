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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteDriver.h,v 1.4 2005/04/13 15:38:05 kommer Exp $

#ifndef acdk_sql_sqlite_LiteDriver_h
#define acdk_sql_sqlite_LiteDriver_h

#include "Config.h"
#include <acdk.h>
#include <acdk/sql/Driver.h>
#include <acdk/sql/SQLException.h>

struct sqlite3;

namespace acdk {
namespace sql {
namespace sqlite {


ACDK_DECL_CLASS(LiteDriver);

/**
  Driver class for SQLite databases
*/
class ACDK_SQL_SQLITE_PUBLIC LiteDriver
: extends acdk::lang::Object
, implements acdk::sql::Driver
{
  ACDK_WITH_METAINFO(LiteDriver)
public:
  LiteDriver() {}
  virtual int getMajorVersionNumber() { return 1; }
  virtual int getMinorVersion() { return 2; }
  virtual bool jdbcCompliant() { return false; }
  virtual RDriverPropertyInfoArray getPropertyInfo(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RException)
  {
    return Nil;
  }
  virtual bool acceptsURL(INP(RString) url) THROWS1(RException) 
  {
    return url->startsWith("sqlite:/") == true || url->startsWith("jdbc:sqlite:/")  == true;
  }
  virtual RConnection connect(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RException);
};

} // sqlite
} // sql 
} // acdk

#endif //acdk_sql_sqlite_LiteDriver_h
