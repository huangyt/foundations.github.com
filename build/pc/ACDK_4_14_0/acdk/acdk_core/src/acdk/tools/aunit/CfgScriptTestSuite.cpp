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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/CfgScriptTestSuite.cpp,v 1.3 2005/04/19 16:50:42 kommer Exp $


#include "CfgScriptTestSuite.h"
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>

namespace acdk {
namespace tools {
namespace aunit {

using namespace ::acdk::io;
using namespace ::acdk::lang;

void 
CfgScriptTestCase::runTest()
{
  try {
    RObject props = (RObject)New("acdk/cfgscript/Props");
    RObject script = (RObject)New("acdk/cfgscript/Script", inOf(_scriptFile));
    int ret = script->invoke("readEval", inOf(props));
  } catch (RThrowable ex) {
    System::out->println("TEST FAILED: Script " + _scriptFile + " throws Exception: " + ex->getMessage());
    RString s = (RString)invoke_static("acdk/cfgscript/Script", "getScriptBackTrace");
    StringBuffer sb;
    sb << "Test script failed: " << _scriptFile;
    if (s != Nil)
      sb << " on:\n" + s;
    invoke_static("acdk/cfgscript/Script", "clearStack");
    testAssertComment(false, sb.toString());
  }
}

RTest 
CfgScriptTestSuite::suite()
{
  collectTests();
  return this;
}

void 
CfgScriptTestSuite::collectTests()
{
  RString sydir = System::getProperties()->eval(_directory);
  File dir(sydir);
  if (dir.exists() == false)
    return;
  if (dir.isFile() == true)
  {
    addTest(new CfgScriptTestCase(dir.getName(), dir.getCanonicalPath()));
    return;
  }
  GlobFilenameFilter filter("*_Test.csf");
  RFileArray fa = dir.listFiles(&filter, FileListRecursive | FileListFiles);
  for (int i = 0; i < fa->length(); ++i)
  {
    RFile f = fa[i];
    addTest(new CfgScriptTestCase(f->getName(), f->getCanonicalPath()));
  }
}

}
}
}

