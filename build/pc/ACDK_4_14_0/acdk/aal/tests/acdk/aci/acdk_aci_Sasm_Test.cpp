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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aci/acdk_aci_Sasm_Test.cpp,v 1.4 2005/02/05 10:44:52 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/aci/Compiler.h>
#include <acdk/aci/parser/SyntaxParseNode.h>
#include <acdk/aci/sasm/AsmParseNode.h>
#include <acdk/aci/ast/AstNode.h>
#include <acdk/aci/vm/Executable.h>
#include <acdk/aci/ast/Terminal.h>

#include <acdk/io/MemReader.h>
#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>

namespace tests {
namespace acdk {
namespace aci {



// Declare test cases
BEGIN_DECLARE_TEST( Sasm_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( Sasm_Test  )

BEGIN_DEFINE_TEST( Sasm_Test )
  ADD_TEST( Sasm_Test, standard ) 
END_DEFINE_TEST( Sasm_Test )

//using namespace ::acdk::aci;


void setupLogger()
{
  using namespace ::acdk::util::logging;
  static RLogger scannerlog = Nil;
  static RLogger parserlog = Nil;
  if (scannerlog != Nil)
    return;
  scannerlog = LogManager::getCreateLogger("acdk.aci");
  scannerlog->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  //parserlog = LogManager::getLogger("acdk.aci.Parser");
  //parserlog->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::MinLevel = Debug;//Debug; //Trace;
  LogManager::Threshold = Debug; //Trace;
  
}

void renderTree(IN(::acdk::aci::ast::RAstNode) node, StringBuffer& sb, IN(RString) ident)
{
  sb << ident << node->toString() << "\n";
  RString nident = ident + " ";
  for (int i = 0; i < node->getChildCount(); ++i)
    renderTree(node->getChild(i), sb, nident);
}
void printTree(IN(::acdk::aci::ast::RAstNode) node)
{
  StringBuffer sb;
  renderTree(node, sb, "");
  ::acdk::lang::System::out->println(sb.toString());
}

void
Sasm_Test::standard()
{
  setupLogger();
  ::acdk::aci::Compiler compiler;
  ::acdk::aci::sasm::AsmParseNode::createRules(&compiler);
  compiler.registerSyntaxRule(new ::acdk::aci::parser::SyntaxParseNode("AsmPrelude", 
                            "!{ acdk.aci.parser.WhiteSpaceParseNode.registerParseNode(compiler);\n"
                            "   compiler.getParseEnv().addIgnoreToken(compiler.getParseNode(\"WS\").getScannerTokenId());\n"
                            "}!"
                            ));
  compiler.registerSyntaxRule(new ::acdk::aci::parser::SyntaxParseNode("AsmText", "AsmPrelude (AsmParseNode)+"));
  RString text = 
    "nop;\n"
    "push 2;\n" // start with 2
    "redo: push 1;\n"
    "add;\n"
    "dup;\n"
    "push 10;\n" // loop until 10
    "gt;\n"
    "brtrue end;\n"
    "br redo;\n"
    "end: pop;\n"
    //"scipall: nop;\n"
    ;
  compiler.scanner->setInBuffer(text);
  ::acdk::aci::ast::RAstNode codetext =  compiler.parseComplete("AsmText");
  printTree(codetext);
  codetext->genOpCode(&compiler);
  printTree(codetext);
  //::acdk::aci::vm::RExecutableArray oca = new ::acdk::aci::vm::ExecutableArray(0);
  //codetext->emitOpCode(&compiler, oca); 
  ::acdk::aci::vm::ExecutableCollector execol;
  codetext->traverse(&execol, &compiler, ::acdk::aci::ast::PSOpCode);
  printTree(codetext);
  
  compiler.execute(execol.getExecutables());
}


} // namespace aci
} // namespace acdk
} // namespace tests

