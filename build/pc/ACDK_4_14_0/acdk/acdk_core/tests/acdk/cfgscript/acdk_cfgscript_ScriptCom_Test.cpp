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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/cfgscript/acdk_cfgscript_ScriptCom_Test.cpp,v 1.5 2005/02/19 18:46:41 kommer Exp $


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
  
BEGIN_DECLARE_TEST( ScriptCom_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( ScriptCom_Test  )

BEGIN_DEFINE_TEST( ScriptCom_Test )
  ADD_TEST( ScriptCom_Test, standard ) 
END_DEFINE_TEST( ScriptCom_Test )


using namespace ::acdk::util::logging;
using namespace ::acdk::cfgscript;

  

void 
ScriptCom_Test::standard()
{
#if defined(ACDK_OS_WIN32)
  try {
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  acdk::cfgscript::RProps props = new acdk::cfgscript::Props();
  RString code =
  "word = new acdkx.com.ComObject(\"Word.Application\");\n"
  "word.Visible = 1;\n"
  "doc = word.Documents.add();\n"
  "sel = word.ActiveWindow.Selection;\n"
  "sel.TypeText(\"This is \");\n"
  "sel.Font.Bold =  1;\n"
  "sel.TypeText(\"ACDK\");\n"
  "sel.Font.Bold = 0;\n"
  "sel.TypeText(\" instrumenting Word through acdk_cfgscript\");\n"
  "acdk.lang.Thread.sleep(3000);\n"
  "word.Quit(0);\n"
  ;
  System::out->println("Evaluating:\n" + code);
  script->eval(code, props, ::acdk::cfgscript::ScriptReadWriteParent);
  } catch (RThrowable ex) {
    Script::clearStack();
    THROW_INSTANCE(ex);
  }
#endif //defined(ACDK_OS_WIN32)
}

} // namespace cfgscript
} // namespace acdk
} // namespace tests



