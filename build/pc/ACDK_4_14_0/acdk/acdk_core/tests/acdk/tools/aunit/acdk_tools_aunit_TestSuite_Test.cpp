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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/tools/aunit/acdk_tools_aunit_TestSuite_Test.cpp,v 1.5 2005/02/05 10:45:09 kommer Exp $



//#include <tools/testunit/TestRunner.h>
#include "../../../../src/acdk/tools/aunit/TestRunner.h"
#include <acdk/lang/System.h>

BEGIN_DECLARE_TEST( TestTestUnit )
   DECLARE_TEST( standard )
   DECLARE_TEST( standard2 )
END_DECLARE_TEST( TestTestUnit )

BEGIN_DEFINE_TEST( TestTestUnit )
   ADD_TEST( TestTestUnit, standard ) 
   ADD_TEST( TestTestUnit, standard2 )
END_DEFINE_TEST( TestTestUnit )



void TestTestUnit::standard()
{
   System::out->println("TestTestUnit::standard()");
}

void TestTestUnit::standard2()
{
   System::out->println("TestTestUnit::standard2()");
}


ACDK_TEST_MAIN
