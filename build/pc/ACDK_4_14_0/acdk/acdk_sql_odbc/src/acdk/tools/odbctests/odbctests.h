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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/tools/odbctests/odbctests.h,v 1.4 2005/02/05 10:45:32 kommer Exp $

#ifndef acdk_tools_odbctests_odbctests_h
#define acdk_tools_odbctests_odbctests_h

#include <acdk.h>
#include <acdk/sql/sql.h>
#include <acdk/sql/ResultSetMetaData.h>


namespace acdk {
namespace tools {
namespace odbctests {

USING_CLASS(::acdk::io::,PrintWriter);

ACDK_DECL_CLASS(ODBCTests);


class ODBCTests
: public ::acdk::lang::Object
{
public:
  static ::acdk::io::RPrintWriter _out;

  static int acdkmain(RStringArray args);
  // all Tests are called
  static bool completeTest(RString connectString, RString testTable,
                           int numberOfTestRecords, int numberOfThreads,
                           RPrintWriter testWriter);
  // no Threads
  static bool singleTest(RString connectString, RString testTable,
                           int numberOfTestRecords,
                           RPrintWriter testWriter);
  // 1 Connection per Thread
  static bool multiConnTest(RString connectString, RString testTable,
                           int numberOfTestRecords, int numberOfThreads,
                           RPrintWriter testWriter);
  // 1 Connection for all Threads
  static bool multiStmtTest(RString connectString, RString testTable,
                           int numberOfTestRecords, int numberOfThreads,
                           RPrintWriter testWriter);
};

} // namespace odbctests
} // namespace tools
} // namespace acdk

#endif // acdk_tools_odbctests_odbctests_h
