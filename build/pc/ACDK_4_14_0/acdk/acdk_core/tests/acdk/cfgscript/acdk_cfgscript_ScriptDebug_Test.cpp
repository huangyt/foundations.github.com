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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/cfgscript/acdk_cfgscript_ScriptDebug_Test.cpp,v 1.8 2005/02/05 10:45:08 kommer Exp $


#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/cfgscript/Script.h>
#include <acdk/io/CharArrayReader.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/tools/aunit/DmiTestClass.h>

namespace tests {
namespace acdk {
namespace cfgscript {

BEGIN_DECLARE_TEST( ScriptDebug_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( ScriptDebug_Test  )

BEGIN_DEFINE_TEST( ScriptDebug_Test )
  ADD_TEST( ScriptDebug_Test, standard )
END_DEFINE_TEST( ScriptDebug_Test )


using namespace ::acdk::util::logging;
using namespace ::acdk::cfgscript;



void
ScriptDebug_Test::standard()
{
  if (ACDK_FQ_SUPER_QUALIFIER(::acdk::tools::aunit::, TestCase)::TestInBatchMode == true)
  {
    System::out->println("Skip interactive Test");
    return;
  }
  System::out->println("Start interactive Test");
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  acdk::cfgscript::Props props;
  RString code =
  // "/*\n Code Sample\n*/\n"
    "x = new acdk.lang.StringBuffer(new acdk.lang.String(\"Hallo\")); // call a constructor\n"
    "acdkstr = \" ACDK\";\n"
    "x.append(acdkstr);\n"
    "erg = x.toString();\n"
  ;
  System::out->println("Evaluating:\n" + code);
  ExecutionStack::setDebugFlags(DbgBreakStatements);
  //script->_debugFlags = 1;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
}

} // namespace cfgscript
} // namespace acdk
} // namespace tests



