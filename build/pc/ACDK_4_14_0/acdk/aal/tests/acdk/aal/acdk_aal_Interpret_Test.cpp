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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_Interpret_Test.cpp,v 1.17 2005/02/05 10:44:51 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/aal/AalCompiler.h>
#include <acdk/io/MemReader.h>
#include <acdk/lang/System.h>
#include <acdk/aci/OpCode.h>

namespace tests {
namespace acdk {
namespace aal {


using namespace ::acdk::aci;
using namespace ::acdk::aal;


// Declare test cases
BEGIN_DECLARE_TEST( Interpret_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( assignment )
  DECLARE_TEST( fqClassInvoke )
  DECLARE_TEST( classOperator )
  DECLARE_TEST( derivedClass )
  DECLARE_TEST( interfaceClass )
END_DECLARE_TEST( Interpret_Test  )

BEGIN_DEFINE_TEST( Interpret_Test )
  ADD_TEST( Interpret_Test, standard ) 
  ADD_TEST( Interpret_Test, assignment ) 
  ADD_TEST( Interpret_Test, fqClassInvoke ) 
  
  ADD_TEST( Interpret_Test, classOperator ) 
  
  ADD_TEST( Interpret_Test, derivedClass ) 
  ADD_TEST( Interpret_Test, interfaceClass ) 
  
  
END_DEFINE_TEST( Interpret_Test )


void parseTreeInterpret(IN(RString) text, IN(RString) initalEl = "CodeText")
{
   ::acdk::aal::AalCompiler compiler;
  compiler.scanner->setInBuffer(text);
  //compiler.printSyntax(System::out);
  RCode codetext =  compiler.parseComplete(initalEl);
  if (codetext == Nil)
    System::out->println("Cannot Parse [" + text + "] begins with [" + initalEl + "]");
  else
  {
    System::out->println("Parsed: [" + text + "] to");
    codetext->printCodeTree(::acdk::lang::System::out, "");
    codetext->postParse(&compiler);
    codetext->printCodeTree(::acdk::lang::System::out, "");
    RExecutableArray oca = new ExecutableArray(0);
    codetext->emitOpCode(&compiler, oca);
    for (int i = 0; i < oca->length(); ++i)
    {
      System::out->print(RString("") + i + ": ");
      oca[i]->printOpCode(System::out);
    }
    compiler.execute(oca);

    //codetext->printOpCode(::acdk::lang::System::out);
    //EvalEnv env(compiler);
    //oca->execute(env);
  }


}

void setupLogger();

void 
Interpret_Test::standard()
{

  //setupLogger();
  AalInterpreter interp;
  RString terg = "";
  interp.setGlobalVar("erg", inOf(terg));
  interp.parseTreeInterpret("erg = new acdk.lang.String(\"asdf\");");
  terg = interp.getGlobalVar("erg").getStringVar();
  testAssert(terg->equals("asdf") == true);

  RString text;
  text = "int i = 0; i = i + 1;"; //  obj.v1 = obj.v1;
  //parseTreeInterpret(text);
  text = "int i = 0; while (i < 3) { i = i + 1; }";
  //parseTreeInterpret(text);
  
  /*
  */
  text = 
    "acdk.lang.StringBuffer sb = new acdk.lang.StringBuffer(\"Hello \");\n"
    "sb.append(\"ACDK\");\n"
    "acdk.lang.System.out.println(sb.toString());\n"
    ;
 
  parseTreeInterpret(text);
  text =
    "class AClass { AClass() { } void bar() {} int foo(int i) { return i; } }\n"
    "AClass cls = new AClass();\n"
    "int j = cls.foo(41);\n"
    ;
  //parseTreeInterpret(text);
  text = 
    "class AClass\n"
    "{ \n"
    "  int _ivar;\n"
    "  static int _iv2;\n"
    "  AClass(int i)\n"
    "  {\n"
    "    this._ivar = i;\n"
    "    AClass._iv2 = i;\n"
    "    this.foo(i);\n"
    "  }\n"
    "  int foo(int i)\n"
    "  {\n"
    "    return i;\n"
    "  }\n "
    "}\n"
    "AClass a = new AClass(42);\n"
    "a.foo(45);\n"
    ;
  //parseTreeInterpret(text);
  text = 
    "class AClass { AClass() {} acdk.lang.String foo(acdk.lang.String s) { return s; } }\n"
    "AClass a = new AClass();\n"
    "a.foo(new acdk.lang.String(\"asdf\"));\n"
  ;
  parseTreeInterpret(text);
  text = 
    "class AClass { int _ivar; AClass(int i) : _ivar(i) {} }\n"
    "AClass a = new AClass(42);"
    ;
  //parseTreeInterpret(text);
}

void
Interpret_Test::assignment()
{
  RString text;
  text = "int i = 0;\nint j = 0; i = j = 42;\n";
  parseTreeInterpret(text);
}

void
Interpret_Test::fqClassInvoke()
{
  RString text;
  text = "acdk.lang.System.out.println(\"hello from Aal\");\n";
  parseTreeInterpret(text);
}



void
Interpret_Test::classOperator()
{
  //setupLogger();
  RString text;
 text = 
   "class AClass { AClass() { } acdk.lang.String operator+(acdk.lang.String s) { return \"AClass String\"; } }\n"
   "AClass a = new AClass();\n"
   "acdk.lang.String s1 = a.operator_pl(\"asdf\");\n"
   "s1 = a + \"asdf\";\n"
   ;
  parseTreeInterpret(text);

}



void
Interpret_Test::derivedClass()
{
  /*
  int dmiflags = ::acdk::lang::dmi::IsConstructor;
  int argcount = 3;
  int namedargs = 10;
  ::acdk::lang::dmi::ScriptVar sv = createInvokeFlagsA(dmiflags, argcount, namedargs);
  int rdmiflags = getDmiFlagsA(sv);
  int sargcount = getArgCountA(sv);
  int snamedcount = getNamedArgCountA(sv);
  */
   RString text;
 text = 
   "class AClass { AClass() { } void foo() { acdk.lang.System.out.println(\"Hello from BaseClass\"); } }\n"
   "class BClass extends AClass { BClass() { } }\n"
   "BClass bcls = new BClass();\n"
   "bcls.foo();\n"
   ;
  //parseTreeInterpret(text);
  text = 
   "class AClass { int _ivar; AClass(int iv) : _ivar(iv) { } }\n"
   "class BClass extends AClass { int _bv; BClass() : AClass(42)/*, _bv(2)*/ { } }\n"
   "BClass bcls = new BClass();\n"
   ;
  //parseTreeInterpret(text);
  text = 
   "class AClass { int _ivar; AClass(int iv) : _ivar(iv) { } void print() { acdk.lang.System.out.println(_ivar); } }\n"
   "AClass acls = new AClass(42);\n"
   "acls.print();\n"
   ;
  //parseTreeInterpret(text);
  text = 
   "class AClass { AClass() { } void foo() { this.print(); print(); } void print() { acdk.lang.System.out.println(\"Call via implicite this\"); } }\n"
   "AClass acls = new AClass();\n"
   "acls.foo();\n"
   ;
  //parseTreeInterpret(text);

  text = // Casting Class
   "class AClass { AClass() { } void print() { acdk.lang.System.out.println(\"Call AClass.print \"); } }\n"
   "class BClass extends AClass { BClass() { } void print() { ((AClass)this).print(); } }\n"
   "BClass bcls = new BClass();\n"
   "bcls.print();\n"
   ;
  //parseTreeInterpret(text);

  text = // Generate default constructor if non
   "class AClass { int i; public void print() { acdk.lang.System.out.println(\"Call AClass.print \"); } }\n"
   "AClass acls = new AClass();\n"
   "acls.print();\n"
   ;
  //parseTreeInterpret(text);

  text = // calling super protected
   "class AClass { protected AClass() { } public void print() { acdk.lang.System.out.println(\"Call AClass.print \"); } }\n"
   "class BClass extends AClass { public BClass() : AClass() { } public void print() { ((AClass)this).print(); } }\n"
   "BClass bcls = new BClass();\n"
   "bcls.print();\n"
   ;
  //parseTreeInterpret(text);

  text = // Implicit Super constructor invokation
   "class AClass { protected AClass() { this.print(); print(); } public void print() { acdk.lang.System.out.println(\"Call AClass.print \"); } }\n"
   "class BClass extends AClass { public BClass() { } }\n"
   "BClass bcls = new BClass();\n"
   ;
  parseTreeInterpret(text);

  text = // calling private protected
   "class AClass { private AClass() { } public void print() { acdk.lang.System.out.println(\"Call AClass.print \"); } }\n"
   "class BClass extends AClass { public BClass() : AClass() { } public void print() { ((AClass)this).print(); } }\n"
   "BClass bcls = new BClass();\n"
   "bcls.print();\n"
   ;
  try {
    //parseTreeInterpret(text);
  } catch (RException ex) {
    System::out->println("Expected Exception: " + ex->getMessage());
  }
  // testing namespace
  text = 
    "namespace test {\n"
    "namespace sub {\n"
    "class AClass { public AClass() {} }\n"
    "}\n"
    "}\n"
    "test.sub.AClass a = new test.sub.AClass();\n"
    ;
  //parseTreeInterpret(text);

  text = 
    "namespace test {\n"
    "namespace sub {\n"
    "class AClass { public AClass() {} }\n"
    "}\n"
    "}\n"
    "using test.sub;\n"
    "AClass a = new AClass();\n"
    "using acdk.lang;\n"
    "StringBuffer sb = new StringBuffer(\"asdf\");\n"
    ;
  //parseTreeInterpret(text);
  
  text = 
    "namespace test {\n"
    "namespace sub {\n"
    "using acdk.lang;\n"
    "StringBuffer sb = new StringBuffer(\"asdf\");\n"
    "}\n"
    "}\n"
    "acdk.lang.StringBuffer sb = new acdk.lang.StringBuffer(\"asdf\"); // multiple var definitions (sb) allowed in different ns\n"
    ;
  //parseTreeInterpret(text);

  text = 
    "namespace test {\n"
    "namespace sub {\n"
    "using acdk.lang;\n"
    "StringBuffer sb = new StringBuffer(\"asdf\");\n"
    "}\n"
    "}\n"
    "StringBuffer sb = new StringBuffer(\"asdf\"); // StringBuffer is unknown type\n"
    ;
   try {
    parseTreeInterpret(text);
  } catch (RException ex) {
    System::out->println("Expected Exception: " + ex->getMessage());
  }


}

void
Interpret_Test::interfaceClass()
{

   RString text;
 text = 
   "interface AInterface { AInterface() {} void foo(); }\n"
   "class AClass implements AInterface { AClass() {} void foo() { acdk.lang.System.out.println(\"Hello from InterfaceImplemenation\"); } }\n"
   "AInterface iface = new AClass();\n"
   "iface->foo();\n"
   ;
  parseTreeInterpret(text);
}

} // namespace aal
} // namespace acdk
} // namespace tests

