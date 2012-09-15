// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 


#include "IdentifierParseNode.h"
#include "../ast/Identifier.h"
#include "../Compiler.h"

namespace acdk {
namespace aci {
namespace parser {

IdentifierParseNode::IdentifierParseNode()
: RegScanParseNode("IDENTIFIER", "^[a-zA-Z_][a-zA-Z_0-9]*", 0, "A simple C/Java Identifier")
{}

RAstNode 
IdentifierParseNode::createStandardAstNode() 
{ 
  return new ::acdk::aci::ast::Identifier(this, Nil, Nil); 
}

acdk::aci::ast::RTerminal 
IdentifierParseNode::createTerminal(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cp)
{
  return new ::acdk::aci::ast::Identifier(this, cp, input); 
}

RAstNode 
IdentifierParseNode::parse(IN(RCompiler) comp)
{
  ScannerTokenStack ss(comp->scanner);
  RTerminal pn = ss.getNext();
  if (pn->getScannerTokenId() != getScannerTokenId())
    return Nil;
  ss.commit();
  return &pn;
}
  
//static 
void 
IdentifierParseNode::registerParseNode(IN(RCompiler) comp)
{
  if (comp->getParseNode("IDENTIFIER") == Nil)
    comp->registerTerminal(new IdentifierParseNode());
}

} // parser
} // aci
} // acdk



