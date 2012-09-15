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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/sasm/AsmParseNode.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_sasm_AsmParseNode_h
#define acdk_aci_sasm_AsmParseNode_h

#include <acdk.h>
#include "../Config.h"
#include "../parser/SyntaxParseNode.h"
#include "../Compiler.h"

namespace acdk {
namespace aci {
namespace sasm {


ACDK_DECL_CLASS(AsmParseNode);

/**
  parses an asm statement
  AsmParseNode: [ GotoLabel ] SimpleIdentifier AsmArgument
  GotoLabel: SimpleIdentifier ':' $
  AsmArgument: Literal
*/
class ACDK_ACI_PUBLIC AsmParseNode
: extends acdk::aci::parser::SyntaxParseNode
{
public:
  AsmParseNode(IN(RString) nodeName, IN(RString) syntax);
  virtual RAstNode parse(IN(RCompiler) compiler);
  virtual RAstNode createStandardAstNode();
  virtual void printSyntax(IN(acdk::io::RPrintWriter) out);
  static void createRules(IN(RCompiler) compiler);
};

ACDK_DECL_CLASS(AstStm);
class ACDK_ACI_PUBLIC AstStm
: extends acdk::aci::ast::AstNodeWithChilds
{
public:
  AstStm(IN(acdk::aci::parser::RParseNode) pn)
    : AstNodeWithChilds(Nil, pn, "AsmStm")
  {
  }
  virtual void genOpCode(IN(RCompiler) comp);
};

} // sasm
} // aci
} // acdk



#endif //acdk_aci_asm_AsmParseNode_h
