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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_Compiler_Test.cpp,v 1.10 2005/02/05 10:44:51 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/aal/AalCompiler.h>
#include <acdk/aci/Code.h>
#include <acdk/io/MemReader.h>
#include <acdk/lang/System.h>

namespace tests {
namespace acdk {
namespace aal {

// Declare test cases
BEGIN_DECLARE_TEST( Compiler_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( grammar )
  DECLARE_TEST( errors )
END_DECLARE_TEST( Compiler_Test  )

BEGIN_DEFINE_TEST( Compiler_Test )
  ADD_TEST( Compiler_Test, standard ) 
  ADD_TEST( Compiler_Test, grammar  ) 
  ADD_TEST( Compiler_Test, errors ) 
END_DEFINE_TEST( Compiler_Test )

using namespace ::acdk::aci;
using namespace ::acdk::aal;




void 
Compiler_Test::standard()
{
  
  /*
  {
    Compiler compiler;
    compiler.registerRule("Test",new ParseNode(" 'new' 'class' 'a\\'c' "));
  }
  */
  /*{
    Compiler compiler;
    compiler.registerTerminal(new TerminalParseNode("DIGITS", "\\d+"));
    compiler.registerTerminal(new TerminalParseNode("FLOAT", "(<DIGITS>)\\.(<DIGITS>)"));
    compiler.registerRule(new ParseNode("Zahl","FLOAT"));
    compiler.scanner->setInBuffer("12345");
    RCode code;
    //code = compiler.parseComplete("Zahl");
    //code->printParseTree(::acdk::lang::System::out, "");
    compiler.scanner->setInBuffer("12345.1234");
    code = compiler.parseComplete("Zahl");
    code->printCodeTree(::acdk::lang::System::out, "");

  }*/
  {
    Compiler compiler;
    compiler.registerRule(new ParseNode("ARule", "'A' $"));
    compiler.registerRule(new ParseNode("BRule", "'B' $"));
    compiler.registerRule(new ParseNode("Test", "ARule BRules $"));
    compiler.registerRule(new ParseNode("BRules", "( BRule )* %"));
    

    compiler.scanner->setInBuffer("A");
    RCode code;
    code = compiler.parseComplete("Test");
    code->printCodeTree(::acdk::lang::System::out, "");
  }
}

void 
parseComplete(IN(RCompiler) comp, IN(RString) startrule, IN(RString) text)
{
  comp->scanner->setInBuffer(text);
  RCode code;
  code = comp->parseComplete(startrule);
  if (code == Nil)
    System::out->println("Not parsed !");
  else 
    code->printCodeTree(::acdk::lang::System::out, "");
}

void
Compiler_Test::grammar()
{
  Compiler compiler;
  compiler.registerRule(new ParseNode("ARule", "'A'"));
  compiler.registerRule(new ParseNode("BRule", "'B'"));
  compiler.registerRule(new ParseNode("CRule", "'C'"));
  compiler.registerRule(new ParseNode("Test", "( ARule | BRule ) ( CRule )* $"));

  
  RCode code;
  /*
  compiler.scanner->setInBuffer("A");
  code = compiler.parseComplete("Test");
  if (code == Nil)
    System::out->println("Not parsed");
  else 
    code->printParseTree(::acdk::lang::System::out, "");
  */
  compiler.scanner->setInBuffer("C");
  /*
  code = compiler.parseComplete("Test");
  if (code == Nil)
    System::out->println("Not parsed");
  else 
    code->printParseTree(::acdk::lang::System::out, "");
  */
  compiler.registerRule(new ParseNode("DRule", "'X'& ' ' CRule $"));
  parseComplete(&compiler, "DRule", "X C"); // does not work, because terminals should not continue with characters, 
                                          // it will be search for terminal 'XC'
  
}

void
Compiler_Test::errors()
{
  Compiler compiler;
  compiler.registerRule(new ParseNode("ARule", "'A'"));
  compiler.registerRule(new ParseNode("BRule", "'B'"));
  compiler.registerRule(new ParseNode("CRule", "'C'"));
  compiler.registerRule(new ParseNode("XRule", "'X'"));
  compiler.registerRule(new ParseNode("Test", " ( ARule ! CRule ) | ( BRule ! CRule ) $"));
  //parseComplete(&compiler, "Test", "AC");
  try 
  {
    parseComplete(&compiler, "Test", "AX");
  } 
  catch (RException ex) 
  {
    System::out->println("Expected Ex: " + ex->getMessage());
  }
}

} // namespace aal
} // namespace acdk
} // namespace tests

