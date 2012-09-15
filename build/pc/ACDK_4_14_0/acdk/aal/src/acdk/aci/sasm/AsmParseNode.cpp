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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/sasm/AsmParseNode.cpp,v 1.4 2005/02/05 10:44:51 kommer Exp $

#include "AsmParseNode.h"
#include "../parser/DecimalLiteralParseNode.h"
#include "../ast/Identifier.h"
#include "../ast/Literal.h"
#include "../vm/OpCodeStm.h"

namespace acdk {
namespace aci {
namespace sasm {

AsmParseNode::AsmParseNode(IN(RString) nodeName, IN(RString) syntax) 
: SyntaxParseNode(nodeName, syntax) 
{
}

RAstNode 
AsmParseNode::parse(IN(acdk::aci::RCompiler) compiler)
{
  RAstNode pn = SyntaxParseNode::parse(compiler);
  if (pn == Nil)
    return Nil;
  // create OpCode here
  return pn;
}

RAstNode 
AsmParseNode::createStandardAstNode() 
{
  return new AstStm(this);
}


void 
AsmParseNode::printSyntax(IN(acdk::io::RPrintWriter) out)
{
}

//static 
void
AsmParseNode::createRules(IN(RCompiler) compiler)
{
  compiler->registerSyntaxRule(new AsmParseNode("AsmParseNode", "[ AsmGotoLabel ] ! IDENTIFIER [ AsmLiteral | IDENTIFIER ] ';' $"));
  compiler->registerSyntaxRule(new SyntaxParseNode("AsmIdentifier", "!{ acdk.aci.parser.IdentifierParseNode.registerParseNode(compiler); }!"
                                                                   "IDENTIFIER $" ));
  compiler->registerSyntaxRule(new SyntaxParseNode("AsmGotoLabel", "AsmIdentifier ':' $"));
  compiler->registerSyntaxRule(new SyntaxParseNode("AsmLiteral", "DEC_LITERAL"));
  compiler->registerTerminal(new DecimalLiteralParseNode());
}

void 
AstStm::genOpCode(IN(RCompiler) comp)
{
  acdk::aci::ast::RIdentifier id = (acdk::aci::ast::RIdentifier)queryChild(acdk::aci::ast::Identifier::GetClass());
  if (id == Nil)
    THROW1(Exception, "Cannot find identifier");
  RString identifier = id->getIdentifier();
  RAstNode labelast = queryChild("AsmGotoLabel");
  RString label;
  if (labelast != Nil)
  {
    label = ((acdk::aci::ast::RIdentifier)labelast->queryChild(acdk::aci::ast::Identifier::GetClass(), 0, 3))->getIdentifier();
  }
  RAstNode asmLiteral = queryChild("AsmLiteral");
  acdk::lang::dmi::ScriptVar var;
  if (asmLiteral != Nil)
  {
    var = ((acdk::aci::ast::RLiteral)asmLiteral->queryChild(acdk::aci::ast::Literal::GetClass()))->getSv();
  }
  else
  {
    acdk::aci::ast::RIdentifier target = (acdk::aci::ast::RIdentifier)queryChild(acdk::aci::ast::Identifier::GetClass(), 1);
    if (target != Nil)
      var = inOf(target->getIdentifier());
  }
  addChild(&OpCodeStm::createOpCode(identifier, var, label), true);
}


} // sasm
} // aci
} // acdk



