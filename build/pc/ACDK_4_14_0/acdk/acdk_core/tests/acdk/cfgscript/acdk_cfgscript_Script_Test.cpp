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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/cfgscript/acdk_cfgscript_Script_Test.cpp,v 1.25 2005/04/18 14:30:56 kommer Exp $




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
  
BEGIN_DECLARE_TEST( Script_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( basicTypes )
  DECLARE_TEST( functions )
  DECLARE_TEST( stackFunctionCalls )
  DECLARE_TEST( plus )
  DECLARE_TEST( arithmetic )
  DECLARE_TEST( complexPlus )
  DECLARE_TEST( postAndPrefixOps )
  DECLARE_TEST( newExpression )
  DECLARE_TEST( namedArgs )
  DECLARE_TEST( atom )
  DECLARE_TEST( peek_poke )
  DECLARE_TEST( statics )
  DECLARE_TEST( chainedCalls )
  DECLARE_TEST( equalStm )
  DECLARE_TEST( syntaxError )
  DECLARE_TEST( classDecl )
  DECLARE_TEST( methods )
  DECLARE_TEST( derivedClassDecl )
  DECLARE_TEST( strongTypes )
  DECLARE_TEST( arrays )
  DECLARE_TEST( typeDecl )
  DECLARE_TEST( instanceOfExpr )
  DECLARE_TEST( enumerations )
  DECLARE_TEST( scriptDelegate )
  DECLARE_TEST( misc )
  DECLARE_TEST( templateText )
END_DECLARE_TEST( Script_Test  )

BEGIN_DEFINE_TEST( Script_Test )
  ADD_TEST( Script_Test, standard ) 
  ADD_TEST( Script_Test, basicTypes ) 
  ADD_TEST( Script_Test, functions ) 
  ADD_TEST( Script_Test, stackFunctionCalls  ) 
  ADD_TEST( Script_Test, plus ) 
  ADD_TEST( Script_Test, arithmetic ) 
  ADD_TEST( Script_Test, complexPlus ) 
  ADD_TEST( Script_Test, postAndPrefixOps ) 
  
  ADD_TEST( Script_Test, newExpression ) 
  
  ADD_TEST( Script_Test, namedArgs ) 
  ADD_TEST( Script_Test, atom ) 
  ADD_TEST( Script_Test, peek_poke ) 
  ADD_TEST( Script_Test, statics ) 
  ADD_TEST( Script_Test, chainedCalls ) 
  ADD_TEST( Script_Test, equalStm ) 
  ADD_TEST( Script_Test, syntaxError ) 
  ADD_TEST( Script_Test, methods ) 
  ADD_TEST( Script_Test, classDecl ) 
  ADD_TEST( Script_Test, derivedClassDecl ) 
  ADD_TEST( Script_Test, strongTypes ) 
  ADD_TEST( Script_Test, arrays ) 
  ADD_TEST( Script_Test, typeDecl ) 
  ADD_TEST( Script_Test, instanceOfExpr ) 
  ADD_TEST( Script_Test, enumerations ) 
  ADD_TEST( Script_Test, scriptDelegate ) 
  
  ADD_TEST( Script_Test, misc ) 
  ADD_TEST( Script_Test, templateText ) 
  
END_DEFINE_TEST( Script_Test )


using namespace ::acdk::util::logging;

void 
Script_Test::standard()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  //System::in->readLine();
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  props->setObjectVal("sb", new StringBuffer("Hallo"));
  RString code =
    "acdk.lang.System.out.println(\"Hallo\");\n"
    //"x = new acdk.lang.StringBuffer(new acdk.lang.String(\"Hallo\"));\n"
    //"t = \"Hallo\";\n"
    //"t = t.substr(1).substr(0, 1);\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  
  //testAssert(props->getStringVal("acdkstr")->equals(" ACDK") == true);
  //RString erg = props->getStringVal("erg");
  //testAssert(erg->equals("Hallo ACDK") == true);

}

void
Script_Test::basicTypes()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
    "i = 3;\n"
    "erg1 = i + 1;\n"
    "erg2 = i < 5;\n"
    "erg3 = i >= 3;\n"
    "erg4 = i != 3;\n"
    "erg5 = i == 3;\n"
    //"erg3 = ++i;\n"
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getIntVal("erg1") == 4);
  testAssert(props->getBoolVal("erg2") == true);
  testAssert(props->getBoolVal("erg3") == true);
  testAssert(props->getBoolVal("erg4") == false);
  testAssert(props->getBoolVal("erg5") == true);
  
}

void 
Script_Test::stackFunctionCalls()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
    "/*\n Code Sample\n*/\n"
    "x = new acdk.lang.StringBuffer(new acdk.lang.String(\"Hallo\")); // call a constructor\n"
    "acdkstr = \" ACDK\";\n"
    "x.append(acdkstr);\n"
    "erg = x.toString();\n"
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  
  testAssert(props->getStringVal("acdkstr")->equals(" ACDK") == true);
  RString erg = props->getStringVal("erg");
  testAssert(erg->equals("Hallo ACDK") == true);

}




void
Script_Test::functions()
{
  RString code =
    "The Code: "
    "${ (new acdk.lang.StringBuffer(\"Hallo\")).append(\" ACDK\").toString() }"
  ;
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  RString str = props->eval(code);
  testAssert(str->equals("The Code: Hallo ACDK") == true);
}


void
Script_Test::plus()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
  "/* Code Sample */\n"
    "x = \"Hello \";\n"
    "erg1 = \"Hello \" + \"ACDK\";\n"
    "erg2 = x + \"ACDK\";\n"
    "erg3 = \"ACDK \" + x;\n"
    "erg4 = \"ACDK \" + x + \"all\";\n"
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  RString erg1 = props->getStringVal("erg1");
  testAssert(erg1->equals("Hello ACDK") == true);
  RString erg2 = props->getStringVal("erg2");
  testAssert(erg2->equals("Hello ACDK") == true);
  RString erg3 = props->getStringVal("erg3");
  testAssert(erg3->equals("ACDK Hello ") == true);
  RString erg4 = props->getStringVal("erg4");
  testAssert(erg4->equals("ACDK Hello all") == true);
}

void
Script_Test::arithmetic()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
    "erg7 = 4 - 1 * 2 | 1 + 4;\n" // == 7
    
    "erg1 = 2;\n"
    "2 + 3;\n"
    "erg1 = 1 + 2;\n"
    "erg1 = 2 * 3 + 4;\n" // == 10
    "erg2 = 2 + 3 * 4;\n" // == 20
    "erg5 = 2 + (3 * 4);\n" // == 14
    "erg3 = 1.toString();\n"
    "erg3 = \"a\".toString().concat(\"x\");\n"
    "erg3 = \"a\";\n"
    "1;\n"
    "erg3 = \"a\" + 1 + 2;\n"
    "erg4 = 1.toString() + \"a\";\n"
    "erg6 = ((new String(\"x\")).toString()) + \"y\";\n"
    
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  int erg1 = props->getIntVal("erg1");
  int erg2 = props->getIntVal("erg2");
  RString erg3 = props->getStringVal("erg3");
  int erg5 = props->getIntVal("erg5");
  RString erg6 = props->getStringVal("erg6");
  int erg7 = props->getIntVal("erg7");
}

void
Script_Test::complexPlus()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
  "/* Code Sample */\n"
    "x = \"b\";\n"
    "//sb1 = new acdk.lang.StringBuffer(\"a\");\n"
    "//sb2 = new acdk.lang.StringBuffer(\"c\");\n"
    "sb1.append(x + sb2.toString() + \"d\");\n"
    "//sb1.append(x + \"c\" + \"d\");\n"
    "//sb1.append(sb2.toString());\n"
    "erg1 = sb1.toString();\n"
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  props->setStringVal("x", "b");
  props->setObjectVal("sb1", new StringBuffer("a"));
  props->setObjectVal("sb2", new StringBuffer("c"));
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  RString erg1 = props->getStringVal("erg1");
}

void
Script_Test::postAndPrefixOps()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
    "erg1 = ++1;\n"
    "erg2 = erg1++;\n"
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  int erg1 = props->getIntVal("erg1");
  int erg2 = props->getIntVal("erg2");
  testAssert(erg1 == 3);
  testAssert(erg2 == 2);
}

void
Script_Test::newExpression()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
    //"sb1 = new acdk.lang.StringBuffer(\"a\");\n"
    //"erg1 = \"x\".toString();\n"
    //"erg1 = \"1\" + \"232\".substr(0, 2).substr(0, 1);\n"
    "erg1 = acdk.lang.Integer.toString(2);\n"
    //"erg1 = sb1.toString();\n"
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  RString erg1 = props->getStringVal("erg1");
}

void
Script_Test::namedArgs()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
#if !defined(ACDK_STATIC_LIB)
  RString code =
  "/* Code Sample */\n"
  "obj = new acdk.tools.aunit.DmiTestClass();\n"
  "sb = new acdk.lang.StringBuffer(\"sbarg\");\n"
  "erg1 = obj.namedArgsMethod(:sbarg = sb, :iarg = 42, :sarg = \"sarg\");\n"
  
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  bool b = props->getBoolVal("erg1");
#endif //!defined(ACDK_STATIC_LIB)
}

void
Script_Test::atom()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;

  RString code =
  "/* Code Sample */\n"
  "obj = new Atom(5.5);\n"
  "erg = obj.addition(4.5);\n"
  "out << erg << \"\\n\";\n"
  "erg1 = new Atom(erg).getIntVar();\n"
  "tobj = 11;\n"
  ;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  int ival = props->getIntVal("erg1");
  testAssert(ival == 10);
}

void
Script_Test::peek_poke()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  props->setObjectVal("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  props->setObjectVal("obj2", new ::acdk::tools::aunit::DmiTestClass("Script", 4));
  RString code =
  //"/* Code Sample */\n"
  //"obj = new acdk.tools.aunit.DmiTestClass(\"Script\", 3);\n"
  "erg4 = obj.pubInt - obj2.pubInt;\n"
  "erg1 = obj.pubInt;\n"
  "erg2 = obj.pubString;\n"
  "obj.pubInt = 5;\n"
  "erg3 = obj.pubInt;\n"
  
  ;
 
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  
  testAssert(props->getIntVal("erg1") == 3);
  testAssert(props->getStringVal("erg2")->equals("Script") == true);
  testAssert(props->getIntVal("erg3") == 5);
}


void
Script_Test::statics()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "/* Code Sample */\n"
  "sic = acdk.tools.aunit.DmiTestClass.pubStaticInt;\n"
  "acdk.tools.aunit.DmiTestClass.pubStaticInt = 3;\n"
  "erg1 = acdk.tools.aunit.DmiTestClass.pubStaticInt;\n"
  "erg2 = acdk.tools.aunit.DmiTestClass.getPubStaticInt();\n"
  "acdk.tools.aunit.DmiTestClass.pubStaticInt = sic;\n"
  ;
 
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getIntVal("erg1") == 3);
  testAssert(props->getIntVal("erg2") == 3);
  
}

void
Script_Test::chainedCalls()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "/* Code Sample */\n"
  "obj = \"ACDK\";\n"
  "erg = obj.substr(1).substr(2).length();\n"
  "t = (new String(\"asdf\")).length();\n"
  ;
 
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  
  testAssert(props->getIntVal("erg") == 1);
}


void
Script_Test::equalStm()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "num = 3;\n"
  "erg2 = num == 3;\n"
  "erg1 = Nil;\n"
  "t = 4;\n"
  "e = t == 4;\n"
  "if (erg1 == Nil) erg1 = new String(\"sdf\");\n"
  "if (erg1 == Nil) erg1 = new String(\"asd\");\n"
  "num = 3;\n"
  "erg2 = num == 3;\n"
  // ### @todo cannot find DmiObject::operator_eq_eq(object); -> "erg3 = num == \"asdf\";\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getStringVal("erg1")->equals("sdf") == true);
  testAssert(props->getBoolVal("erg2") == true);
  testAssert(props->getBoolVal("erg3") == false);
}

void
Script_Test::syntaxError()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "num = 3;\n"
  "if num = 3 then num = 4;\n" // syntax error
  ;
  try {
    script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
    testAssertComment(false, "expect ex");
  } catch (RDmiException ex) {
    System::out->println("Caught expected exception: " + ex->getMessage());
  }
  code =
  "num = 3;\n"
  "num.foobaz();\n" // semantic error
  ;
  try {
    script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
    testAssertComment(false, "expect ex");
  } catch (RDmiException ex) {
    System::out->println("Caught expected exception: " + ex->getMessage());
  }
  code =
  "num = 3;\n"
  "blubb.foobaz();\n" // semantic error
  ;
  try {
    script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
    testAssertComment(false, "expect ex");
  } catch (RDmiException ex) {
    System::out->println("Caught expected exception: " + ex->getMessage());
  }
}


void
Script_Test::classDecl()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "class AClass extends acdk.lang.Object implements acdk.lang.Comparable {\n"
  "  public int _foo;\n" 
  "  public AClass(int value) { this._foo = value; }\n"
  "  public int foo() { return this._foo; }\n"
  "  public void foo(int v) { this._foo = v; }\n"
  "  public bool compareTo(acdk.lang.Object other) { return this._foo - other._foo; }\n"
  "}\n"
  "a = new AClass(42);\n"
  "b = new AClass(43);\n"
  "c = a.compareTo(b);\n"
  "erg1 = a._foo == 42;\n"
  "erg2 = a.foo() == 42;\n"
  "coll = new acdk.util.TreeSet();\n"
  "coll.add(a);\n"
  "coll.add(b);\n"
  "coll.add(new AClass(41));\n"
  "a.equals(b);\n"
  "it = coll.iterator();\n"
  "while (it.hasNext() == true){\n"
  "  System.out.println(it.next().foo());\n"
  "}\n"

  
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getBoolVal("erg1") == true);
  testAssert(props->getBoolVal("erg2") == true);
}

void
Script_Test::derivedClassDecl()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  "class AClass implements acdk.lang.Comparable {\n"
  "  private int _foo;\n" 
  "  public AClass(int value) { this._foo = value; }\n"
  "  public int foo() { return this._foo; }\n"
  "  public int compareTo(acdk.lang.Object other) { return this._foo - other._foo; }\n"
  "}\n"
  "\n"
  "class BClass extends AClass implements acdk.lang.Comparable, acdk.io.Serializable {\n"
  "  public BClass(int value) { super(value); }\n"
  "  public int compareTo(acdk.lang.Object other) { return super.compareTo(other); }\n"
  "}\n"
  "b = new BClass(42);\n"
  "erg1 = b.compareTo(b);\n"
  "erg2 = b.foo();\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getIntVal("erg1") == 0);
  testAssert(props->getIntVal("erg2") == 42);
  code =
  "class AClass extends acdk.lang.Exception {\n"
  "  private int _foo;\n" 
  "  public AClass(String str, int value) { super(str); this._foo = value; }\n"
  "  public int compareTo(acdk.lang.Object other) { return this._foo - other._foo; }\n"
  "}\n"
  "\n"
 
  "b = new AClass(\"hello\", 42);\n"
  "erg1 = b.getMessage();\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getStringVal("erg1")->equals("hello") == true);
}


void
Script_Test::strongTypes()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  RString code =
  "int i = 42;\n"
  "try {\n"
  " int j = \"asdf\";\n"
  //"} catch(acdk.lang.ClassCastException ex) {\n"  
  "} catch(acdk.lang.DmiTypeConversionException ex) {\n"  
  " acdk.lang.System.out.println(\"Expected exception: \" + ex.getMessage());\n"  
  "}\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
}

void
Script_Test::arrays()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  // basic array
  "iarr = new intArray(2);\n"
  "iarr.set(0, 42);\n"
  "iarr.set(1, 43);\n"
  // multiple basic array
  "iiarr = new intArrayArray(2);\n"
  "iiarr.set(0, new intArray(2));\n"
  "iiarr.set(1, new intArray(2));\n"
  "iiarr.get(0).set(0, 41);\n"
  "iiarr.get(0).set(1, 42);\n"
  "iiarr.get(1).set(0, 43);\n"
  "iiarr.get(1).set(1, 44);\n"
  "System.out.println(iiarr.toString());\n"

  "oarr = new StringArrayArray(2);\n"
  "oarr.set(0, new StringArray(2));\n"
  "oarr.set(1, new StringArray(2));\n"
  "oarr.get(0).set(0, \"Hallo\");\n"
  "oarr.get(0).set(1, \"ACDK\");\n"
  "oarr.get(1).set(0, \"Hallo 2\");\n"
  "oarr.get(1).set(1, \"ACDK 2\");\n"
  "System.out.println(oarr.toString());\n"
  "// not working System.out.println(oarr[0][0]);\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
}

void
Script_Test::typeDecl()
{
   RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();

  //props->set("obj", new ::acdk::tools::aunit::DmiTestClass("Script", 3));
  RString code =
  
  "#pragma strict\n"
  "int i;\n"
  "int j = 42;\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
}

void
Script_Test::instanceOfExpr()
{
    RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  RString code =
  "#pragma strict\n"
  "Object o = new Integer(42);\n"
  "if (o instanceof acdk.lang.String) {\n"
  "  throw new Throwable(\"should never reached\");\n"
  "} else if (o instanceof Integer)\n"
  "  System.out.println(\"instanceof working\");\n"
  "else\n"
  "  throw new acdk.tools.aunit.TestException(\"instanceof does not work\");\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
}

void
Script_Test::methods()
{
  struct X 
  {
    int x;
    X(int i) : x(i) {}
    X operator+(const X& other) { return X(x + other.x); }
    X operator*(const X& other) { return X(x * other.x); }
  };
  X x = X(1) + X(2) * X(3);

   RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  RString code =
  "#pragma strict\n"
  "class AClass"
  "{\n"
  " public static int callAFunction(int count)\n"
  " {\n"
  "    for (int i = 0; i < count; ++i)\n"
  "       acdk.lang.System.out.println(\"count: \" + count + \", i:\" + i);\n"
  "    return 42;\n"
  "  }\n"
  "}\n"
  "int erg = AClass.callAFunction(3);\n"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  testAssert(props->getIntVal("erg") == 42);
}


void
Script_Test::misc()
{
   RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  RString code =
  "attr = new acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiCiWeakBind);"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  
}

void
Script_Test::enumerations()
{
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  RString code =
  "attr = (acdk.lang.dmi.MiCiWeakBind | acdk.lang.dmi.MiNoDmiProxy);"
  ;
  script->eval(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);
  int attr = props->getIntVal("attr");
  testAssert(attr == (::acdk::lang::dmi::MiCiWeakBind | ::acdk::lang::dmi::MiNoDmiProxy));
}

void
Script_Test::scriptDelegate()
{

  RString arg1 = "Hello";
  ::acdk::lang::dmi::RDmiDelegate del;
  del = ::acdk::cfgscript::Script::createScriptDelegate(inOf(arg1), "[text] { System.out.println(text); }");
  //del->call(::acdk::lang::dmi::ScriptVarArray());
  del = ::acdk::cfgscript::Script::createScriptDelegate(inOf(this), "[test] void() { System.out.println(test.getName()); }");
  ::acdk::lang::dmi::ScriptVarArray args;
  del->call(args);

}

void
Script_Test::templateText()
{
  RString code = 
    "This is the header"
    "<@ StringArray messages = [ \"A\", \"B\" ]; @>\n"
    "<@ foreach (String msg in messages) { @>\n"
    " Hello:  <@= msg @>\n"
    "<@ } out.flush(); @>\n";

  ::acdk::cfgscript::RScript script = new ::acdk::cfgscript::Script("<mem>");
  ::acdk::cfgscript::RProps props = new ::acdk::cfgscript::Props();
  script->evalTemplate(code, &props, ::acdk::cfgscript::ScriptReadWriteParent);

}

} // namespace cfgscript
} // namespace acdk
} // namespace tests



