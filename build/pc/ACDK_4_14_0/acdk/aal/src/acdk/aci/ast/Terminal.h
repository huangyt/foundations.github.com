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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/ast/Terminal.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_ast_Terminal_h
#define acdk_aci_ast_Terminal_h

#include "ast.h"
#include "AstNode.h"
#include "../parser/TerminalParseNode.h"

namespace acdk {
namespace aci {
namespace ast {

ACDK_DECL_CLASS(Terminal);
/**
  Terminals are end nodes in the syntax grammar
  Terminals was created from TerminalParseNode
*/
class ACDK_ACI_PUBLIC Terminal
: extends AstNodeFromParseNode
{
  ACDK_WITH_METAINFO(Terminal)
protected:
  acdk::aci::parser::RTerminalParseNode _terminalParseNode;
  acdk::aci::util::RCodeLocation _codeLocation;
public:
  Terminal(IN(acdk::aci::parser::RTerminalParseNode) templ, IN(acdk::aci::util::RCodeLocation) cl, IN(RString) nodeName = "Terminal");
  
  virtual acdk::aci::util::RCodeLocation getCodeLocation() { return _codeLocation; }
  virtual int getScannerPrio();
  virtual bool isCachable() { return true; }
  virtual int getScannerTokenId();
  virtual bool isWhiteSpace() { return false; }
  virtual bool isComment() { return false; }
  
  acdk::aci::parser::RTerminalParseNode getTerminalParseNode() { return _terminalParseNode; }

  /**
  virtual void postParse(IN(RCompiler) comp);
  virtual void emitOpCode(IN(RCompiler) comp, IN(acdk::aci::vm::RExecutableArray) oca);
  void printCodeTree(IN(acdk::io::RPrintWriter) out, IN(RString) indent);
  */
};


} // ast
} // aci
} // acdk



#endif //acdk_aci_ast_Terminal_h
