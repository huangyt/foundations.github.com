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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/CfgScriptTestSuite.h,v 1.2 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_CfgScriptTestSuite_h
#define acdk_tools_CfgScriptTestSuite_h

#include "TestUnit.h"

namespace acdk {
namespace tools {
namespace aunit {



ACDK_DECL_CLASS(CfgScriptTestCase);
/**
  execute one CfgScript script as test case.
  This class controls the acdk::cfgscript::Script class
  via reflection/dmi, so static linking of the acdk_cfgscript 
  library is not needed
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC CfgScriptTestCase
: extends TestCase
{
  ACDK_WITH_METAINFO(CfgScriptTestCase)
protected:
  RString _scriptFile;
public:
  CfgScriptTestCase(IN(RString) name, IN(RString) scriptFile)
  : TestCase(name)
  , _scriptFile(scriptFile)
  {
  }
  RString getName() { return name(); }
  virtual void runTest();
};

ACDK_DECL_CLASS(CfgScriptTestSuite);

/**
  execute all CfgScripts in a given directory  
  - including sub directories if recursive is true -
  which ends with _Test.csf
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC CfgScriptTestSuite
: extends TestSuite
{
  ACDK_WITH_METAINFO(CfgScriptTestSuite)
protected:
  RString _directory;
  bool _recursive;
public:
  /** 
    @param dir directory or single CfgScript file
           if dir is a directory will search for "*_Test.csf" files
           dir may also contains $(KEY) acdk::util::Property (from System::getProperties()) expressions
    @param recursive if true search in all sub directories for script files
  */
  CfgScriptTestSuite(IN(RString) dir, bool recursive)
  : TestSuite(dir)
  , _directory(dir)
  {
    //collectTests();
  }
  RString getName() { return "CfgScriptTests"; }
  RTest suite();
protected:
  /**
    internal
  */
  foreign void collectTests();
};


}
}
}


#endif //acdk_tools_CfgScriptTestSuite_h
