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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteDatabaseMetaData.cpp,v 1.1 2005/04/06 10:28:35 kommer Exp $



#include "LiteDatabaseMetaData.h"


namespace acdk {
namespace sql {
namespace sqlite {

RResultSet 
LiteDatabaseMetaData::getTables(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern) THROWS1(RSQLException)
{
  StringBuffer sql(250);
  sql << "SELECT '' AS 'TABLE_CAT', "
	"'' AS 'TABLE_SCHEM', "
	"tbl_name AS 'TABLE_NAME', "
	"upper(type) AS 'TABLE_TYPE', "
	"'' AS REMARKS FROM sqlite_master ";
  if (String::isNotEmpty(namePattern) == true)
    sql  << "WHERE tbl_name like " << namePattern;
  else
    sql << "WHERE tbl_name like '%' ";
  sql << " AND (type = 'table' or type = 'view')";
  return _con->createStatement()->executeQuery(sql.toString());
}

} // sqlite
} // sql 
} // acdk

