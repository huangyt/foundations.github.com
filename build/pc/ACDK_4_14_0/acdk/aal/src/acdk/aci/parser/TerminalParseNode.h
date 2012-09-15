// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_parser_TerminalParseNode_h
#define acdk_aci_parser_TerminalParseNode_h

//#include "../aci.h"
#include "ParseNode.h"
#include "../util/CodeLocation.h"

// note: Terminal is only predeclared

namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(TerminalParseNode);

/**
  TerminalParseNode parses end nodes like whitespace, identifier, literals, keywords, operator.
  A TerminalParseNode will also be used by the scanner to read next token.
  the resulting token will be stored in Terminal

  @seealso acdk::aci::ast::Terminal
*/
class ACDK_ACI_PUBLIC TerminalParseNode
: extends ParseNode
{
  ACDK_WITH_METAINFO(TerminalParseNode)
protected:
  /**
    identifies the scanner token
  */
  int _tk;
  /**
    While scanning the scanner try to use all TerminalParse not to read
    a Terminal from the input stream.
    The ScannerPrio is used to sort the TermalParseNodes. The TerminalParseNode with higher
    ScannerPrio will be tried first.
    The common used values are between 0 and 10
  */
  int _scannerPrio;
public:
  TerminalParseNode()
  : ParseNode("Terminal", "")
  , _tk(-1)
  , _scannerPrio(5)
  {
  }
  TerminalParseNode(IN(RString) nodename, IN(RString) help = "")
  : ParseNode(nodename, help)
  , _tk(-1)
  , _scannerPrio(5)
  {}
  /**
    @see _tk
  */
  int getScannerTokenId() { return _tk; }
  /**
    @see _tk
  */
  void setScannerTokenId(int tk) { _tk = tk; }
  /**
    @see _scannerPrio
  */
  int getScannerPrio() { return _scannerPrio; }
  /**
    @see _scannerPrio
  */
  void setScannerPrio(int prio) { _scannerPrio = prio; }

  acdk::aci::ast::RAstNode createStandardAstNode();

  virtual acdk::aci::ast::RTerminal createTerminal(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cp) 
  {
    // ### @todo throw
    return Nil;
  }
  /**
    try to read token from source input
    @return the length of the matching string
            -1 if not matched
  */            
  virtual acdk::aci::ast::RTerminal scanNextFromSource(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cl)
  {
    return Nil;
  }

  virtual acdk::aci::ast::RAstNode parse(IN(RCompiler) compiler);
  virtual void printSyntax(IN(acdk::io::RPrintWriter) out) 
  {
  }

  virtual bool isWhiteSpace() { return false; }
  virtual bool isComment() { return false; }
  
  virtual int compareTo(IN(RTerminalParseNode) other)
  {
    return _scannerPrio - other->getScannerPrio();
  }
  virtual int compareTo(IN(RObject) obj)
  {
    return compareTo(RTerminalParseNode(obj));
  }
  

  //RAstNode parse(IN(RCompiler) comp);
  //void printCodeTree(IN(acdk::io::RPrintWriter) out, IN(RString) indent);
};

} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_TerminalParseNode_h

