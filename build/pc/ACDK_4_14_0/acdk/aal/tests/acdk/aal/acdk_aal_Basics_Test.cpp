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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_Basics_Test.cpp,v 1.17 2005/02/05 10:44:51 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/aal/AalCompiler.h>
#include <acdk/io/MemReader.h>
#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>

namespace tests {
namespace acdk {
namespace aal {

// Declare test cases
BEGIN_DECLARE_TEST( Basics_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( syntax )
  DECLARE_TEST( expressions )
  DECLARE_TEST( statements )
  DECLARE_TEST( functionDecl )
  DECLARE_TEST( CondAndLoops )
  DECLARE_TEST( objectDecls )
  DECLARE_TEST( objectCalls )
  DECLARE_TEST( objectOperator )
  DECLARE_TEST( experimental )
END_DECLARE_TEST( Basics_Test  )

BEGIN_DEFINE_TEST( Basics_Test )
  ADD_TEST( Basics_Test, standard ) 
  ADD_TEST( Basics_Test, syntax ) 
  ADD_TEST( Basics_Test, expressions ) 
  ADD_TEST( Basics_Test, statements  ) 
  ADD_TEST( Basics_Test, functionDecl ) 
  ADD_TEST( Basics_Test, CondAndLoops ) 
  ADD_TEST( Basics_Test, objectDecls ) 
  ADD_TEST( Basics_Test, objectCalls ) 
  ADD_TEST( Basics_Test, objectOperator ) 
  ADD_TEST( Basics_Test, experimental ) 
  
END_DEFINE_TEST( Basics_Test )


void setupLogger()
{
  /*
  using namespace ::acdk::util::logging;
  static RLogger scannerlog = Nil;
  static RLogger parserlog = Nil;
  if (scannerlog != Nil)
    return;
  scannerlog = LogManager::getLogger("acdk.aci.Scanner");
  scannerlog->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  parserlog = LogManager::getLogger("acdk.aci.Parser");
  parserlog->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::MinLevel = Trace;//Debug; //Trace;
  LogManager::Threshold = Trace; //Trace;
  */
}

using namespace ::acdk::aci;
using namespace ::acdk::aal;

void parseTree(IN(RString) text, IN(RString) initalEl = "CodeText");

void parseLoop()
{
  System::out->println("Input Text:"); System::out->flush();
  StringBuffer sb;
  RString cline;
  do 
  {
    cline = System::in->readLine();
    sb.append(cline);
  } while (cline->length());

  if (sb.length() == 0)
    return;
  System::out->println("Start Rule:"); System::out->flush();
  cline = System::in->readLine();
  if (cline->length() == 0)
    cline = "CodeText";
  parseTree(sb.toString(), cline);
}

void parseTree(IN(RString) text, IN(RString) initalEl)
{
  
  ::acdk::aal::AalCompiler compiler;
  compiler.scanner->setInBuffer(text);
  try 
  {
    ::acdk::aci::RCode codetext =  compiler.parseComplete(initalEl);
    if (codetext == Nil)
      System::out->println("Cannot Parse [" + text + "] begins with [" + initalEl + "]");
    else
    {
      System::out->println("Parsed: [" + text + "]");
      codetext->printCodeTree(::acdk::lang::System::out, "");
      //codetext->postParse(&compiler);
    }
    
  } 
  catch (RException ex)
  {
    System::out->println("Parse failed: " + ex->getMessage());
  }
  //parseLoop();
}

using namespace ::acdk::aal;

RCode parseSyntax(IN(RCompiler) comp, IN(RString) initialtk, IN(RString) text)
{
  comp->scanner->setInBuffer(text);
  RCode erg = comp->parseComplete(initialtk);
  erg->printCodeTree(::acdk::lang::System::out, "");
  //System::in->readLine();
  return erg;
}



void 
Basics_Test::syntax()
{

  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "Literal"));
    RString text = "\"asdf\"";
    //parseSyntax(&compiler, "test", text);
  }
  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "'AKeyWord' 'BKeyWord'"));
    RString text = "AKeyWord BKeyWord";
    parseSyntax(&compiler, "test", text);
  }
 
  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "'AKeyWord' 'BKeyWord' | 'AKeyWord' 'CKeyWord'"));
    RString text = "AKeyWord BKeyWord";
    parseSyntax(&compiler, "test", text);
  }
 
  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "'AKeyWord' 'BKeyWord' | 'AKeyWord' 'CKeyWord'"));
    RString text = "AKeyWord CKeyWord";
    parseSyntax(&compiler, "test", text);
  }
  {
    AalCompiler compiler;
    
    compiler.registerRule(new ParseNode("test", "'AKeyWord' [ 'BKeyWord' ]"));
    RString text = "AKeyWord";
    parseSyntax(&compiler, "test", text);
  }
  
  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "'AKeyWord' [ 'BKeyWord' ]"));
    RString text = "AKeyWord BKeyWord";
    parseSyntax(&compiler, "test", text);
  }
   
  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "'AKeyWord' [ 'BKeyWord' | 'CKeyWord' ]"));
    RString text = "AKeyWord CKeyWord";
    parseSyntax(&compiler, "test", text);
  }
  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "'AKeyWord' ( 'BKeyWord' | 'CKeyWord' ) 'DKeyWord'"));
    compiler.printSyntax(System::out);
    RString text = "AKeyWord CKeyWord DKeyWord";
    parseSyntax(&compiler, "test", text);
  }
 

  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "'param' [ ',' test ]"));
    //compiler.printSyntax(System::out);
    RString text = "param";
    //parseSyntax(&compiler, "test", text);
    text = "param, param";
    parseSyntax(&compiler, "test", text);
    text = "param, param, param";
    parseSyntax(&compiler, "test", text);
  }
   
  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "'A' ( 'B' )*"));
    //compiler.printSyntax(System::out);
    RString text = "A";
    parseSyntax(&compiler, "test", text);
    text = "A B";
    parseSyntax(&compiler, "test", text);
    text = "A B B";
    parseSyntax(&compiler, "test", text);
  }
  {
    AalCompiler compiler;
    compiler.registerRule(new ParseNode("test", "mlt ( '+' mlt )* $"));
    compiler.registerRule(new ParseNode("mlt", "zahl ( '*' zahl )* $"));
    compiler.registerRule(new ParseNode("zahl", "'a' | 'b' | 'c' | 'd' $"));
    //compiler.printSyntax(System::out);
    RString text = "a + b * c + d";
    parseSyntax(&compiler, "test", text);
    
  }
}

void 
Basics_Test::standard()
{
  
}

void 
Basics_Test::expressions()
{
  RString text;
  text = " a + b";
  parseTree(text, "Expression");
  text = "a == b";
  parseTree(text, "Expression");
  text = "a != b";
  parseTree(text, "Expression");
  text = "a == b ? a : b";
  parseTree(text, "Expression");
  text = "a==b?a:b";
  parseTree(text, "Expression");
  text = "acdk.lang.Object";
  parseTree(text, "FqTypeName");
}

void 
Basics_Test::statements()
{
  RString text;
  text = "a;";
  parseTree(text, "Statement");
  text = "b = a;";
  parseTree(text, "Statement");
  text = "b + a;";
  parseTree(text, "Statement");
  text = "j = k + 1;";
  parseTree(text, "Statement");
  text = "int i = 0;";
  //parseTree(text, "CodeText");
  text = "int i = 0; i = i + 1;";
  parseTree(text, "CodeText");
  
}

void 
Basics_Test::functionDecl()
{
  RString text = "void foo(int i, String str);";
  //parseTree(text);
  text = 
"int foo(int i, int j)\n"
"{\n"
"  return i + j;\n"
"}\n"
"void bar() { }\n"
;
  parseTree(text, "CodeText");
}

void 
Basics_Test::CondAndLoops()
{
  RString text;
  
  
  text = "if (true) i = 3;";
  parseTree(text, "Statement");
  text = "if (true) { }";
  parseTree(text, "Statement");
  text = "if (true) { a = i; b = x; }";
  parseTree(text, "Statement");
  text = "if (true) { } else x = 4;";
  parseTree(text, "Statement");
  text = "if (true) { } else if (false) x = 4;";
  parseTree(text, "Statement");
  
  text = "while (true) x = 1;";
  parseTree(text, "Statement");
  text = "while (true) {}";
  parseTree(text, "Statement");
  text = "do x = 1; while (true);";
  parseTree(text, "Statement");
  text = "do { } while (true);";
  parseTree(text, "Statement");
  text = "switch(x) { case a : case b: x = 1; break; default: break; }";
  parseTree(text, "Statement");
  
  //Compiler compiler;
  //compiler.printSyntax(System::out);
}


void 
Basics_Test::objectDecls()
{
  RString text;
  text = "class AClass { }";
  parseTree(text, "CodeText");
  text = "class AClass { int ival; }";
  parseTree(text, "CodeText");
  text = "class AClass { void foo() { } }";
  parseTree(text, "CodeText");
  text = "class AClass { public void foo() { } static int ival; }";
  parseTree(text, "CodeText");
  text = "class AClass { void foo(int i) { } int ival; }";
  parseTree(text, "CodeText");
  text = "class AClass { void foo(int i) { ival = i; } int ival; }";
  parseTree(text, "CodeText");
  text = "class AClass { int ival; void foo(int i) { ival = i; }  }";
  parseTree(text, "CodeText");

  text = "class AClass { AClass(int i) { ival = i; }  }";
  parseTree(text, "CodeText");
  text = "class AClass { AClass(int i) : ival(i) { }  }";
  parseTree(text, "CodeText");
  text = "class AClass extends acdk.lang.Object { }";
  parseTree(text, "CodeText");
}


void 
Basics_Test::objectCalls()
{
  RString text;
  text = "o = new Object();";
  parseTree(text, "CodeText");
  text = "acdk.lang.Object s;";
  parseTree(text, "CodeText");
  text = "acdk.lang.Object s = a;";
  parseTree(text, "CodeText");
  text = "acdk.lang.Object s = new Object();";
  parseTree(text, "CodeText");
  text = "acdk.lang.Object s = new acdk.lang.String(1);";
  parseTree(text, "CodeText");
  text = "o.member = x;";
  parseTree(text, "CodeText");
  text = "o1 = a2.m2;\n"
         "o.foo(x);\n"
         "o.foo(x)[i];\n"
         "o.foo = x;\n"
         "o.foo[i] = x;\n"
         "o[i] = x;\n"
         "o[i].bar = x;\n"
         "o[i].foo() = x;\n"
         ;
  parseTree(text, "CodeText");
  text = 
      "(o).toString();\n"
      "(i + 1).toString();\n"
      ;
  parseTree(text, "CodeText");
  text = 
      "(((foo(i)[j].bar + 1)(v) + 2).bar + 4)[x];\n"
      ;
  parseTree(text, "CodeText");
  text = "s = (new acdk.lang.Object()).toString();";
  parseTree(text, "CodeText");
}

void
Basics_Test::objectOperator()
{
  setupLogger();
  RString text;
  text = "class AClass { String operator+(String s) { return \"AClass\"; } }";
  parseTree(text, "CodeText");
  text = "a + 42;";
  parseTree(text, "CodeText");
}




void
Basics_Test::experimental()
{
  
}

} // namespace aal
} // namespace acdk
} // namespace tests

