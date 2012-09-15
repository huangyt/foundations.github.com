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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCResultSetMetaData.cpp,v 1.4 2005/02/05 10:45:32 kommer Exp $

#include "ODBCResultSetMetaData.h"
#include "ODBCResultSet.h"
#include "ODBCHandle.h"
#include "ODBCColumn.h"
#include "ODBCConnection.h"
#include "ODBCDriver.h"

#include <acdk/lang/Integer.h>
#include <acdk/util/Properties.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace ::acdk::lang;

ODBCResultSetMetaData::ODBCResultSetMetaData(INP(RODBCResultSet) rset) 
: _rset(rset) 
{ 
}

ODBCResultSetMetaData::~ODBCResultSetMetaData()
{
  ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCResultSetMetaData::~ODBCResultSetMetaData");
}

} // odbc
} // sql
} // acdk
