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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/ast/Identifier.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_ast_Identifier_h
#define acdk_aci_ast_Identifier_h

#include "Terminal.h"

namespace acdk {
namespace aci {
namespace ast {

ACDK_DECL_CLASS(Identifier);
/**
  Identifiers are end nodes in the syntax grammar
*/
class ACDK_ACI_PUBLIC Identifier
: extends Terminal
{
  ACDK_WITH_METAINFO(Identifier)
protected:
  RString _identifier;
public:
  Identifier(IN(acdk::aci::parser::RTerminalParseNode) templ, IN(acdk::aci::util::RCodeLocation) cl, IN(RString) identifier)
  : Terminal(templ, cl, "Identifier")
  , _identifier(identifier)
  {
    setSaveNodeAfterBuild(true);
  }
  RString toString()
  {
    return "Identifier: " + _identifier;
  }
  RString getIdentifier() { return _identifier; }
};


} // ast
} // aci
} // acdk



#endif //acdk_aci_ast_Identifier_h
