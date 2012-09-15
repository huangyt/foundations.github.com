// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_parser_IdentifierParseNode_h
#define acdk_aci_parser_IdentifierParseNode_h

#include "RegScanParseNode.h"

namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(IdentifierParseNode);

/**
  IdentifierParseNode parses a simple identifier 
  with the syntax "[a-zA-Z_][a-zA-Z_0-9]*"
  By default it creates an acdk::aci::ast::Identifier AstNode

  @seealso acdk::aci::ast::Terminal
*/
class ACDK_ACI_PUBLIC IdentifierParseNode
: extends RegScanParseNode
{
  ACDK_WITH_METAINFO(IdentifierParseNode)
public:
  IdentifierParseNode();

  RAstNode createStandardAstNode();
  RAstNode parse(IN(RCompiler) comp);
  virtual acdk::aci::ast::RTerminal createTerminal(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cp);

  static void registerParseNode(IN(RCompiler) comp);
};

} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_IdentifierParseNode_h

