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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_CmdLineParser_Test.cpp,v 1.3 2005/02/17 22:34:38 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/CmdLineParser.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( CmdLineParser_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( strip )
END_DECLARE_TEST( CmdLineParser_Test  )

BEGIN_DEFINE_TEST( CmdLineParser_Test )
  ADD_TEST( CmdLineParser_Test, standard ) 
  ADD_TEST( CmdLineParser_Test, strip ) 
END_DEFINE_TEST( CmdLineParser_Test )

using namespace acdk::lang;

RStringArray getTestArgs()
{
  RStringArray args = new StringArray(0);
  args->append("<threre is the executable>"); // first will be skipped, because there is normally the executable name
  args->append("-firstopt");
  args->append("/secondopt");
  args->append("-firstval=value1");
  args->append("/secondval");
  args->append("value 2");
  return args;
}

void CmdLineParser_Test::standard()
{
  RStringArray args = getTestArgs();

  RCmdLineParser cmdLineParser = new CmdLineParser();
  cmdLineParser->addOption(SCS("-firstopt"), SCS("FIRST"), false, SCS("With firstopt set FIRST"), true);
  cmdLineParser->addOption(SCS("/secondopt"), SCS("SECOND"), false, SCS("With firstopt set FIRST"), false);
  cmdLineParser->addOption(SCS("-thirdopt"), SCS("THIRD"), false, SCS("With firstopt set THIRD"), false);
  cmdLineParser->addOption(SCS("-firstval"), SCS("VAL1"), true, SCS("set VAL1"), true);
  cmdLineParser->addOption(SCS("/secondval"), SCS("VAL2"), true, SCS("set VAL2"), false);
  cmdLineParser->addOption(SCS("-thirdval"), SCS("VAL4"), true, SCS("set VAL3"), false);
  cmdLineParser->printHelp(System::out);

  ::acdk::util::RProperties props = cmdLineParser->parse(args,  false, false);
  testAssert(props->getProperty("VAL1") != Nil && props->getProperty("VAL1")->equals("value1") == true);
  testAssert(props->getProperty("SECOND") != Nil);
  testAssert(props->getProperty("THIRD") == Nil);
}

void
CmdLineParser_Test::strip()
{
  RStringArray args = getTestArgs();
  args->append("file1");
  args->append("file2");
  RCmdLineParser cmdLineParser = new CmdLineParser();
    cmdLineParser->addOption(SCS("-firstopt"), SCS("FIRST"), false, SCS("With firstopt set FIRST"), true);
  cmdLineParser->addOption(SCS("/secondopt"), SCS("SECOND"), false, SCS("With firstopt set FIRST"), false);
  cmdLineParser->addOption(SCS("-thirdopt"), SCS("THIRD"), false, SCS("With firstopt set THIRD"), false);
  cmdLineParser->addOption(SCS("-firstval"), SCS("VAL1"), true, SCS("set VAL1"), true);
  cmdLineParser->addOption(SCS("/secondval"), SCS("VAL2"), true, SCS("set VAL2"), false);
  cmdLineParser->addOption(SCS("-thirdval"), SCS("VAL4"), true, SCS("set VAL3"), false);
  ::acdk::util::RProperties props = cmdLineParser->parse(args,  false, true);
  testAssert(props->getProperty("VAL2") != Nil && props->getProperty("VAL2")->equals("value 2") == true);
  RString strargs = args->toString();
  testAssert(args->length() == 3);
}


} // namespace lang 
} //namespace acdk 
} //namespace tests




