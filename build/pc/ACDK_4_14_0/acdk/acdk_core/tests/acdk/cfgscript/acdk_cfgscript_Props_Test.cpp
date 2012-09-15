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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/cfgscript/acdk_cfgscript_Props_Test.cpp,v 1.11 2005/03/26 14:58:37 kommer Exp $




#include <acdk/cfgscript/Script.h>
#include <acdk/io/CharArrayReader.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/tools/aunit/DmiTestClass.h>

#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace cfgscript {
  
BEGIN_DECLARE_TEST( Props_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( propsViaScript )
  DECLARE_TEST( scriptInEval )
END_DECLARE_TEST( Props_Test  )

BEGIN_DEFINE_TEST( Props_Test )
  ADD_TEST( Props_Test, standard ) 
  ADD_TEST( Props_Test, propsViaScript ) 
  ADD_TEST( Props_Test, scriptInEval ) 
  
END_DEFINE_TEST( Props_Test )


using namespace ::acdk::util::logging;
using namespace ::acdk::cfgscript;

void 
Props_Test::standard()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RProps props = new Props();
  props->poke("Val", inOf(42));
  int erg = props->peek("Val");
  testAssert(erg == 42);
}

void 
Props_Test::propsViaScript()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  props->getProps("tp");
  //props.set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "/* Code Sample */\n"
  "tp.value1 = \"val\";\n"
  "erg1 = tp.value1;\n"
  "tp.value2 = 42;\n"
  "tp.dir1 = new acdk.cfgscript.Props();\n"
  "tp.dir1.value1 = 3;\n"
  
  "erg2 = tp.value2;\n"
  "erg3 = tp.dir1.value1;\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getStringVal("erg1")->equals("val") == true);
  testAssert(props->getIntVal("erg2") == 42);
  testAssert(props->getIntVal("erg3") == 3);
}

void 
Props_Test::scriptInEval()
{
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  props->setIntVal("i", 42);
  props->setIntVal("j", 1);
  // ${ } evaluates an expression
  RString s = props->eval("${i + j}");
  testAssert(s->equals("43"));

  s = props->eval("!{ out.print(i + j); }!");
  testAssert(s->equals("43"));
}

} // namespace cfgscript
} // namespace acdk
} // namespace tests



