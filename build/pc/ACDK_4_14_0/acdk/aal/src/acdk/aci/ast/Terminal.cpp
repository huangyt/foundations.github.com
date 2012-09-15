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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/ast/Terminal.cpp,v 1.4 2005/02/05 10:44:51 kommer Exp $


#include "Terminal.h"
#include "../parser/TerminalParseNode.h"

namespace acdk {
namespace aci {
namespace ast {

Terminal::Terminal(IN(acdk::aci::parser::RTerminalParseNode) templ, IN(acdk::aci::util::RCodeLocation) cl, IN(RString) nodeName)
: AstNodeFromParseNode(Nil, &templ, nodeName)
, _terminalParseNode(templ)
, _codeLocation(cl)
{
}

int 
Terminal::getScannerTokenId() 
{ 
  return _terminalParseNode->getScannerTokenId(); 
}

int 
Terminal::getScannerPrio() 
{ 
  return _terminalParseNode->getScannerPrio(); 
}

/*
void 
Terminal::postParse(IN(RCompiler) comp)
{
  setSemanticElement(&DClazzInfo::getInstance(_sv.getClazzInfo()));
}

void
Terminal::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca) 
{
  oca->append(new OpCodeStm1(OCO_PUSH, this, _sv, "Terminal"));
}

void 
Terminal::printCodeTree(IN(acdk::io::RPrintWriter) out, IN(RString) indent)
{
  out->println(indent + getName() + ": " + _sv.toString());
}
*/

} // ast
} // aci
} // acdk


