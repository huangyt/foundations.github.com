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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aci/acdk_aci_Asd_Test.cpp,v 1.4 2005/02/05 10:44:51 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/aci/Compiler.h>
#include <acdk/aci/parser/SyntaxParseNode.h>
#include <acdk/aci/sasm/AsmParseNode.h>
#include <acdk/aci/ast/AstNode.h>
#include <acdk/aci/ast/Terminal.h>
#include <acdk/aci/vm/Executable.h>

#include <acdk/io/MemReader.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>

#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>

namespace tests {
namespace acdk {
namespace aci {



// Declare test cases
BEGIN_DECLARE_TEST( Asd_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( single )
END_DECLARE_TEST( Asd_Test  )

BEGIN_DEFINE_TEST( Asd_Test )
  ADD_TEST( Asd_Test, standard ) 
  ADD_TEST( Asd_Test, single ) 
  
END_DEFINE_TEST( Asd_Test )

//using namespace ::acdk::aci;

namespace {
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

} // anon namespace

void 
execute(IN(RString) asd, IN(RString) asf)
{
  ::acdk::aci::Compiler compiler;
  ::acdk::aci::parser::SyntaxParseNode::parseSyntaxText(&compiler, asd);
  compiler.scanner->setInBuffer(asf);
  ::acdk::aci::ast::RAstNode codetext =  compiler.parseComplete("CodeText");
  printTree(codetext);
  codetext->buildSemantic(&compiler);
  codetext->genOpCode(&compiler);
  printTree(codetext);
  
    
}

void
Asd_Test::standard()
{
  setupLogger();
  RString p = ::acdk::lang::System::getAcdkHome() + "/aal/cfgs/aci/tests";
  ::acdk::io::File d(p);
  if (d.exists() == false)
  {

    return;
  }
  ::acdk::io::RFileArray files = d.listFiles(new ::acdk::io::GlobFilenameFilter("*.asd"));
  for (int i = 0; i < files->length(); ++i)
  {
    RString syntax = files[i]->getReader()->getCharReader()->readString();
    RString fn = files[i]->getName();
    fn = fn->substr(0, fn->length() - 4);
    ::acdk::io::RFileArray acis =  d.listFiles(new ::acdk::io::GlobFilenameFilter(fn + "*.aci"));
    for (int j = 0; j < acis->length(); ++j)
    {
      RString code = acis[j]->getReader()->getCharReader()->readString();
    
      execute(syntax, code);
    }

  }
}

void
Asd_Test::single()
{
  setupLogger();
  RString s = ::acdk::lang::System::getAcdkHome() + "/aal/cfgs/aci/tests/aal.asd";
  RString p = ::acdk::lang::System::getAcdkHome() + "/aal/cfgs/aci/tests/aal_stm_if.aci";
  RString syntax = ::acdk::io::File(s).getReader()->getCharReader()->readString();
  RString code = ::acdk::io::File(p).getReader()->getCharReader()->readString();
  execute(syntax, code);

}



} // namespace aci
} // namespace acdk
} // namespace tests

