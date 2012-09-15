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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/sql.h,v 1.9 2005/02/05 10:45:31 kommer Exp $

#ifndef acdk_sql_sql_h
#define acdk_sql_sql_h

#include <acdk.h>
#include <acdk/lang/Exception.h>
#include "Config.h"
#include "SQLConfig.h"



// defines the unit acdk/sql
ACDK_DECL_UNIT(acdk_sql)

namespace acdk {
/**
  This packag follows the JDBC standards (java.sql) of the JDK 1.2 
  Please refer to acdk::sql::odbc::ODBCConnection for Implementation notes.
*/
namespace sql {

using namespace acdk::lang;
ACDK_DECL_THROWABLE(SQLException, Exception);
ACDK_DECL_THROWABLE(SQLWarning, SQLException);

ACDK_DECL_INTERFACE(Statement);
ACDK_DECL_INTERFACE(PreparedStatement);
ACDK_DECL_CLASS(DriverPropertyInfo);
ACDK_DECL_INTERFACE(Array);
ACDK_DECL_INTERFACE(ResultSet);
ACDK_DECL_INTERFACE(Connection);
ACDK_DECL_INTERFACE(DatabaseMetaData);
ACDK_DECL_INTERFACE(ResultSetMetaData);
ACDK_DECL_INTERFACE(CallableStatement);
ACDK_DECL_CLASS(Time);
ACDK_DECL_CLASS(Timestamp);

} // sql
} // acdk
/*
#include "SQLException.h"
#include "SQLWarning.h"
#include "Types.h"
#include "Time.h"
#include "Timestamp.h"
#include "DriverPropertyInfo.h"
#include "Statement.h"
#include "Connection.h"
#include "Driver.h"
#include "DriverManager.h"
#include "Array.h"
#include "ResultSet.h"
#include "DatabaseMetaData.h"
#include "ResultSetMetaData.h"
*/

#endif //acdk_sql_sql_h

