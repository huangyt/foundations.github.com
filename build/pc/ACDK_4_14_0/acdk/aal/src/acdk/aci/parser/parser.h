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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/parser.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_parser_parser_h
#define acdk_aci_parser_parser_h

#include <acdk.h>
#include "../Config.h"

namespace acdk {
namespace aci {

/**
  Parser classes which creates the AstNodes in acdk::aci::ast
*/
namespace parser {


ACDK_DECL_CLASS(ParseEnv);
ACDK_DECL_CLASS(TerminalParseNode);
ACDK_DECL_CLASS(ParseNode);
ACDK_DECL_CLASS(SyntaxParseNode);
ACDK_DECL_INTERFACE(Expression);

} // namespace parser
} // namespace aci
} // namespace acdk

/*
#include "KeywordParseNode.h"
#include "SyntaxParseNode.h"
#include "TerminalParseNode.h"
#include "ParseEnv.h"
#include "Scanner.h"
#include "../Compiler.h"
*/

#endif //acdk_aci_parser_parser_h

