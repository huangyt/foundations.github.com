// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_parser_RegScanParseNode_h
#define acdk_aci_parser_RegScanParseNode_h

#include <acdk/text/RegExp.h>

#include "TerminalParseNode.h"
//#include "parser.h"


namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(RegScanParseNode);

/**
  RegScanParseNode uses a regular expression scanner
  @seealso acdk::aci::ast::Terminal
*/
class ACDK_ACI_PUBLIC RegScanParseNode
: extends TerminalParseNode
{
  ACDK_WITH_METAINFO(RegScanParseNode)
protected:
  RString _syntax;  
  acdk::text::RRegExp _regExp;
  /**
    bind to sub expression
    by default 1
  */
  int _bindTo;
  
public:
  RegScanParseNode(IN(RString) nodename, IN(RString) regExpSyntax, int bindTo = 1, IN(RString) help = "")
  : TerminalParseNode(nodename, help)
  , _syntax(regExpSyntax)
  , _regExp(new acdk::text::RegExp(regExpSyntax))
  , _bindTo(bindTo)
  {
  }
  
  virtual RTerminal scanNextFromSource(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cl);
  virtual RString getSyntax() { return _syntax; }
  void setScannerSyntax(IN(RString) syntax)
  {
    _syntax = syntax;
    _regExp = new acdk::text::RegExp(syntax);
  }
  //virtual RAstNode parse(IN(RCompiler) compiler);
  
  

};

} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_RegScanParseNode_h

