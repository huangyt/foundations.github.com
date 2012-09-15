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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/ParseNode.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_parser_ParseNode_h
#define acdk_aci_parser_ParseNode_h

#include <acdk.h>
#include "../Config.h"
#include "../aci.h"
#include "../ast/ast.h"

namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(Scanner);

using namespace ::acdk::aci;
using namespace ::acdk::aci::ast;


ACDK_DECL_CLASS(ParseNode);


class ACDK_ACI_PUBLIC ParseNode
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(ParseNode)
  
protected:
  RString _nodeName;
  RString _helpText;
public:
  ParseNode(IN(RString) nodeName, IN(RString) helpText = "")
  : _nodeName(nodeName)
  , _helpText(helpText)
  {
  }
  /**
    will be called, when ParseNode is registerd
  */
  virtual void onRegister(IN(RCompiler) comp) {}
  /**
    Parses this node.
    @return the parsed RCode or Nil if input doesn't match
  */
  virtual RAstNode parse(IN(RCompiler) compiler)  = 0;
  virtual RAstNode createStandardAstNode() = 0;
  virtual RString getNodeName() { return _nodeName; }
  virtual RString getSyntax() { return ""; }
  virtual void printSyntax(IN(acdk::io::RPrintWriter) out) = 0;
  virtual RString toString() { return getNodeName(); } //  + ": " + getSyntax(); }
};



} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_ParseNode_h

