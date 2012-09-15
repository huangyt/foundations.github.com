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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/cfgscript/acdk_cfgscript_ScriptStatements_Test.cpp,v 1.8 2005/03/23 22:26:38 kommer Exp $




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
  
BEGIN_DECLARE_TEST( ScriptStatements_Test )
  DECLARE_TEST( ifCondition )
  DECLARE_TEST( with )
  DECLARE_TEST( whileStm )
  DECLARE_TEST( doStm )
  DECLARE_TEST( forStm )
  DECLARE_TEST( usingStm )
  DECLARE_TEST( tryCatch )
END_DECLARE_TEST( ScriptStatements_Test  )

BEGIN_DEFINE_TEST( ScriptStatements_Test )
  ADD_TEST( ScriptStatements_Test, ifCondition ) 
  ADD_TEST( ScriptStatements_Test, with ) 
  ADD_TEST( ScriptStatements_Test, whileStm ) 
  ADD_TEST( ScriptStatements_Test, doStm ) 
  ADD_TEST( ScriptStatements_Test, forStm ) 
  ADD_TEST( ScriptStatements_Test, usingStm ) 
  ADD_TEST( ScriptStatements_Test, tryCatch ) 
END_DEFINE_TEST( ScriptStatements_Test )


using namespace ::acdk::util::logging;

void 
ScriptStatements_Test::ifCondition()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
    "/* Code Sample */\n"
    "x = \"A\";\n"
    "erg1 = \"\"; erg2 = \"\";\n"
    "if (x.equals(\"B\")) {\n"
    "  erg1 = \"Is B\";\n"
    "}\n"
    "if (x.equals(\"A\")) {\n"
    "  erg2 = \"Is A\";\n"
    "}\n"
    "erg3 = \"\";\n"
    "if (x.equals(\"B\")) {\n"
    "  erg3 = \"is B\";\n"
    "} else {\n"
    "  erg3 = \"Is Not B\";\n"
    "}\n "
    "erg4 = \"\";\n"
    "if (x.equals(\"B\")) {\n"
    "  erg4 = \"is not A\";\n"
    "} else if (x.equals(\"A\")) {\n"
    "  erg4 = \"Is A\";\n"
    "}\n"
    "if (x.equals(\"B\")) {\n" // test empty blocks
    "} else if (x.equals(\"A\")) {\n"
    "}\n"
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, props, ::acdk::cfgscript::ScriptReadWriteParent);
  RString erg1 = props->getStringVal("erg1");
  testAssert(erg1->equals("") == true);
  RString erg2 = props->getStringVal("erg2");
  testAssert(erg2->equals("Is A") == true);
  RString erg3 = props->getStringVal("erg3");
  testAssert(erg3->equals("Is Not B") == true);
  RString erg4 = props->getStringVal("erg4");
  testAssert(erg4->equals("Is A") == true);

}


void
ScriptStatements_Test::with()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "/* Code Sample */\n"
  "str = \"ACDK\";\n"
  "erg1 = 0; // declarate in global, otherwise erg1 will only declared in with { } block \n"
  "erg2 = 0;\n"
  "with (str.substr(1)) {\n"
  " erg1 = .length();\n"
  " erg2 = .substr(1);\n"
  "}\n"
  ;
 
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  int e1 = props->getIntVal("erg1");
  testAssert(props->getIntVal("erg1") == 3);
  testAssert(props->getStringVal("erg2")->equals("DK") == true);
}

void
ScriptStatements_Test::whileStm()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "i = 3;\n"
  "while (i > 0)\n"
  "  i = i - 1;\n"
  "erg1 = i;\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getIntVal("erg1") == 0);

  code =
  "arr = new acdk.util.ArrayList();\n"
  "arr.add(\"first\");\n"
  "arr.add(\"second\");\n"
  "arr.add(\"third\");\n"
  "arrit = arr.listIterator();\n"
  "erg = \"\";\n"
  "while (arrit.hasNext())\n"
  "{\n"
  " s = arrit.next();\n"
  " erg = erg + \" \" + s;\n"
  "}\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getStringVal("erg")->equals(" first second third") == true);
}

void
ScriptStatements_Test::doStm()
{
  
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "i = 0;\n"
  "do {\n"
  "  i++;\n"
  "} while(i < 3);\n"
  "erg1 = i;\n"
  ;
  //script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  //testAssert(props->getIntVal("erg1") == 3);
  
  code =
  
  "i = 0;\n"
  "do {\n"
  "  if (i < 2) { i = 10; continue; }\n"
  "  if (i > 12) break; \n"
  "  ++i;\n"
  "} while(true);\n"
  "erg1 = i;\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  int erg1 = props->getIntVal("erg1");
  testAssert(erg1 == 13);
  
}


void
ScriptStatements_Test::forStm()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "int sum = 0;\n"
  "for (int i = 0; i < 10; i = i + 1){\n"
  "  sum = sum + i;\n"
  "}\n"
  "System.out.println(\"Sum from 0 - 10: \" + sum);\n"
  "erg1 = sum;\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getIntVal("erg1") == 45);
}

void
ScriptStatements_Test::usingStm()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "using acdk.util.ArrayList;\n"
  "sb = new ArrayList();\n"
  "System.out.println(\"Hallo\");\n"
  "p = new Props();\n"
  
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  //testAssert(props->getIntVal("erg1") == 0);

}


void
ScriptStatements_Test::tryCatch()
{
   RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "#pragma strict\n"
  "try {\n"
  "  int i = \"asdf\";\n" // throws ClassCast
  "} catch (DmiException ex) {\n"
  "  System.out.println(\"catched classcast: \" + ex.getMessage());\n"
  "}\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);

  code =
  "#pragma strict\n"
  "try {\n"
  "  throw new acdk.lang.DmiException(\"Just a Test\");\n" // throws ClassCast
  "} catch (ClassCastException ex) {\n"
  "  System.out.println(\"Should be heere: \" + ex.getMessage());\n"
  "} catch (acdk.lang.DmiException ex) {\n"
  "  System.out.println(\"expected ex: \" + ex.getMessage());\n"  
  "} finally {\n"
  "  System.out.println(\"finally block called\");\n"
  "}\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);

  code =
  "#pragma strict\n"
  "try {\n"
  "  \n"
  "} catch (acdk.lang.DmiException ex) {\n"
  "  System.out.println(\"expected ex: \" + ex.getMessage());\n"  
  "} finally {\n"
  "  System.out.println(\"finally block called\");\n"
  "}\n"
  "System.out.println(\"after finally\");\n"
  ;

  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);

   code =
  "#pragma strict\n"
  "try {\n"
  "  try {\n"
  "    throw new acdk.lang.DmiException(\"test\");\n"
  "  } catch (acdk.io.IOException ) {\n"
  "  } finally {\n"
  "     System.out.println(\"finally 1 block called\");\n"
  "  }\n"
  "  throw new Throwable(\"never reach here\");\n"
  "} catch (acdk.lang.DmiException ex) {\n"
  "  System.out.println(\"expected ex: \" + ex.getMessage());\n" 
  "} finally {\n"
  "  System.out.println(\"finally 2 block called\");\n"
  "}\n"
  "System.out.println(\"after finally\");\n"
  ;

  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);

  // ### TODO test derived exception
}


} // namespace cfgscript
} // namespace acdk
} // namespace tests



