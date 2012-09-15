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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/cfgscript/acdk_cfgscript_Script2_Test.cpp,v 1.16 2005/04/19 21:27:47 kommer Exp $




#include <acdk/cfgscript/Script.h>
#include <acdk/io/CharArrayReader.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/tools/aunit/DmiTestClass.h>

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/tools/aunit/CfgScriptTestSuite.h>

using namespace acdk::tools::aunit;

/* add all csf test files in the directory */
TestRunnerStaticAdder scriptTests(new CfgScriptTestSuite("$(ACDKHOME)/acdk_core/cfg/csf/tests/acdk/cfgscript", true));

/*
old code no more needed

namespace tests {
namespace acdk {
namespace cfgscript {
  
BEGIN_DECLARE_TEST( Script2_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( Script2_Test  )

BEGIN_DEFINE_TEST( Script2_Test )
  ADD_TEST( Script2_Test, standard ) 
END_DEFINE_TEST( Script2_Test )


using namespace ::acdk::util::logging;
using namespace ::acdk::cfgscript;

  

void 
Script2_Test::standard()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString s = System::getAcdkHome() + "/acdk_core/cfg/csf/tests/acdk/cfgscript";
  ::acdk::io::File dir(s);
  RStringArray files = dir.list(new ::acdk::io::GlobFilenameFilter("*_Test.csf"), ::acdk::io::FileListRecursive | ::acdk::io::FileListFiles);
  bool scriptTestFailed = false;
  for (int i = 0; i < files->length(); ++i)
  {
    RString fname = ::acdk::io::File(&dir, files[i]).getCanonicalPath();
    if (fname->indexOf("NoBatchTest") != -1)
      continue;
    
    try {
      RScript script = new Script(fname);
      RProps props = new Props();
      System::out->println("Eval Script: " + fname);
      int ret = script->readEval(props);
      Script::clearStack();
    } catch (RThrowable ex) {
      System::out->println("TEST FAILED: Script " + fname + " throws Exception: " + ex->getMessage());
      Script::clearStack();
      scriptTestFailed = true;
    }
  }
  testAssertComment(scriptTestFailed == false, "One or more scripts doesn't work");
}

} // namespace cfgscript
} // namespace acdk
} // namespace tests

*/

