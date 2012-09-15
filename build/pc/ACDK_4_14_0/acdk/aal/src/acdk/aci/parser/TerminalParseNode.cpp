// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 


#include "TerminalParseNode.h"
#include "../Compiler.h"
#include "../ast/Terminal.h"
#include "Scanner.h"


namespace acdk {
namespace aci {
namespace parser {


RAstNode 
TerminalParseNode::createStandardAstNode() 
{ 
  return new acdk::aci::ast::Terminal(this, Nil, getNodeName()); 
}

RAstNode 
TerminalParseNode::parse(IN(RCompiler) compiler)
{
  ScannerTokenStack ss(compiler->scanner);
  RTerminal term = ss.getNext();
  if (term->getScannerTokenId() == getScannerTokenId())
  {
    ss.commit();
    return &term;
  }
  return Nil;
}

} // parser
} // aci
} // acdk



