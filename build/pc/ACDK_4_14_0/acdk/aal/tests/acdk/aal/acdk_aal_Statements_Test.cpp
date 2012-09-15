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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_Statements_Test.cpp,v 1.8 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( Statements_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( varDecl )
  DECLARE_TEST( block )
  DECLARE_TEST( whileStatement )
  DECLARE_TEST( doStatement )
  DECLARE_TEST( forStatement )
  DECLARE_TEST( gotoStatement )
  DECLARE_TEST( ifStatement )
  DECLARE_TEST( switchStatement )
  DECLARE_TEST( expressionIf )
END_DECLARE_TEST( Statements_Test  )

BEGIN_DEFINE_TEST( Statements_Test )
  ADD_TEST( Statements_Test, standard ) 
  ADD_TEST( Statements_Test, varDecl ) 
  
  ADD_TEST( Statements_Test, block ) 
  ADD_TEST( Statements_Test, whileStatement ) 
  ADD_TEST( Statements_Test, doStatement  ) 
  ADD_TEST( Statements_Test, forStatement ) 
  ADD_TEST( Statements_Test, gotoStatement  ) 
  ADD_TEST( Statements_Test, ifStatement ) 
  ADD_TEST( Statements_Test, switchStatement ) 
  ADD_TEST( Statements_Test, expressionIf ) 
  
END_DEFINE_TEST( Statements_Test )


void setupLogger();
void parseTreeInterpret(IN(RString) text, IN(RString) initalEl = "CodeText");

void 
Statements_Test::standard()
{
  //setupLogger();
  RString text;
  text =
    ""
    ;
  parseTreeInterpret(text);
}

void
Statements_Test::varDecl()
{
   RString text;
  AalInterpreter aint;
  text = 
    "int i;\n"
    "__assert(i == 0, \"varDecl without initalizer don't work\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text = 
    "int i = 2;\n"
    "__assert(i == 2, \"varDecl with initalizer don't work\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
Statements_Test::block()
{
  RString text;
  AalInterpreter aint;
  text = 
    "int i = 2;\n"
    "int j = 10;\n"
    "{\n"
    "  int j = i;\n"
    "  __assert(j == 2, \"Scoped Block local vars\");\n"
    "}\n"
    "i = j;\n"
    "__assert(i == 10, \"Scoped Block local vars\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text = 
    "int i = 2;\n"
    "{\n"
    "  int j = i;\n"
    "  __assert(j == 2, \"Scoped Block local vars\");\n"
    "}\n"
    "int j = 4;\n"
    "__assert(i == 2, \"Scoped Block local vars\");\n"
    "__assert(j == 4, \"Scoped Block local vars\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

}

void
Statements_Test::whileStatement()
{
  RString text;
  AalInterpreter aint;
  text = 
    "int i = 0;\n"
    "while (i < 10)\n"
    "{\n"
    "  i = i + 1;\n"
    "}\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text = 
    "int i = 0;\n"
    "while (true)\n"
    "{\n"
    "  if (i >= 10)\n"
    "    break;\n"
    "  i = i + 1;\n"
    "}\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
Statements_Test::doStatement()
{
   RString text;
   
  AalInterpreter aint;
  text = 
    "int i = 0;\n"
    "do\n"
    "{\n"
    "  i = i + 1;\n"
    "} while  (i < 4);\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text = 
    "int i = 0;\n"
    "do\n"
    "{\n"
    "  i = i + 1;\n"
    "  if (i > 3) break;\n"
    "} while  (i < 10);\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
Statements_Test::forStatement()
{
  RString text;
  //setupLogger();
  AalInterpreter aint;
  
  
  text = 
    "for (int i = 0; i < 1; ++i)\n"
    "{\n"
    "}\n"
    "int i = 0;\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  {
    int i = 0;
    for (; i < 2; ++i)
    {
      if (i == 1)
        continue;
    }
    int j = i;

  }
  text = 
    "int i = 0;\n"
    "for (; i < 1; ++i)\n"
    "{\n"
    "}\n"
    "int j = i;\n"
    "__assert(j == 1, \"for loop local var handling\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text = 
    "int i = 0;\n"
    "for (;;)\n"
    "  if (++i > 2)\n"
    "    break;\n"
    "__assert(i == 3, \"for loop local var handling\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
Statements_Test::gotoStatement()
{
   RString text;
   
  AalInterpreter aint;
  text = 
    "int i = 0;\n"
    "do\n"
    "{\n"
    "  i = i + 1;\n"
    "  if (i > 3) goto ende;\n"
    "} while  (i < 4);\n"
    "__assert(false, \"unreachable. goto doesn't work expected\");\n"
    "ende:\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text = 
    "goto ende;\n"
    "ende: goto realende;\n"
    "__assert(false, \"goto doesn't work\");\n"
    "realende:\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  
  text = 
    "void foo()\n"
    "{\n"
    "  goto endoffoo;\n"
    "  __assert(false, \"goto in methods does not work\");\n"
    "endoffoo: ;\n"
    "}\n"
    "foo();\n"
    ";\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
Statements_Test::ifStatement()
{
  RString text;
  text = 
    "int i = 0;\n"
    "if (i < 10) i = 11; else i = 9;"
    ;
  parseTreeInterpret(text);
  text = 
    "int i = 12;\n"
    "if (i < 10) i = 11; else i = 9;"
    ;
  parseTreeInterpret(text);

  text = 
    "int i = 5;\n"
    "if (i < 10) { if (3 < i) i = 2; else i = 4; }\n"
    
    ;
  parseTreeInterpret(text);
}

void
Statements_Test::switchStatement()
{
  AalInterpreter aint;
  //setupLogger();
  RString text;
  
 text =
    "int i = 2;\n"
    "switch (i) {\n"
    "case 1: i = 3; break;\n"
    "case 2: i = 4; break;\n"
    "}\n"
    "__assert(i == 4, \"switch failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "int i = 2;\n"
    "switch (i) {\n"
    "case 1: i = 3; break;\n"
    "case 4: break;\n"
    "default: i = 10;\n"
    "}\n"
    "__assert(i == 10, \"switch failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "int i = 1;\n"
    "switch (i) {\n"
    "case 1: i = 2; // nobreak\n"
    "case 4: ++i; break;\n"
    "default: i = 10;\n"
    "}\n"
    "__assert(i == 3, \"switch failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "int i = 0;\n"
    "switch (\"B\") {\n"
    "case \"A\": i = 1; break;\n"
    "case \"B\": i = 3; break;\n"
    "default: i = 10;\n"
    "}\n"
    "__assert(i == 3, \"switch failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "int i = 0;\n"
    "switch (\"Z\") {\n"
    "case \"A\": i = 1; break;\n"
    "case \"B\": i = 3; break;\n"
    "default: i = 10;\n"
    "}\n"
    "__assert(i == 10, \"switch failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();


  text =
    "int i = 0;\n"
    "acdk.lang.String s = \"A\";\n"
    "switch (\"AB\") {\n"
    "case s.concat(\"A\"): i = 1; break;\n"
    "case s.concat(\"B\"): i = 3; break;\n"
    "default: i = 10;\n"
    "}\n"
    "__assert(i == 3, \"switch failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
Statements_Test::expressionIf()
{
  AalInterpreter aint;
  //setupLogger();
  RString text;
  
 text =
    "__assert((1 < 2 ? 3 : 4) == 3, \"expressional if failed\");\n"
    "__assert((1 > 2 ? 3 : 4) == 4, \"expressional if failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

} // namespace aal
} // namespace acdk
} // namespace tests

