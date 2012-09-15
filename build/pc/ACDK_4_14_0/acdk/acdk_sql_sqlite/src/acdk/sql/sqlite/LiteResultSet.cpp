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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteResultSet.cpp,v 1.1 2005/04/04 16:02:25 kommer Exp $


#include "LiteResultSet.h"
#include <acdk/sql/Types.h>

#include "../../../sqlitesrc/sqlite3.h"

namespace acdk {
namespace sql {
namespace sqlite {


RObject 
LiteResultSet::getObject(int columnIndex)
{
  int itype = _table->getLiteType(columnIndex);
  // TODO
  return Nil;
}

RObject 
LiteResultSet::getObject(int i, INP(acdk::util::RMap) map)
{
  // TODO
  return Nil;
}


} // sqlite
} // sql 
} // acdk

