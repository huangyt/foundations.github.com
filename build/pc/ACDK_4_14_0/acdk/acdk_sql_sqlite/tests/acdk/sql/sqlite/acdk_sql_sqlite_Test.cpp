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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/tests/acdk/sql/sqlite/acdk_sql_sqlite_Test.cpp,v 1.2 2005/04/06 10:30:35 kommer Exp $


#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/tools/aunit/CfgScriptTestSuite.h>

#include <acdk/lang/System.h>

using namespace acdk::lang;
using namespace acdk::tools::aunit;

TestRunnerStaticAdder scriptTests(new CfgScriptTestSuite("$(ACDKHOME)/acdk_sql_sqlite/cfg/csf/tests/acdk/sql/sqlite", true));


ACDK_TEST_MAIN


