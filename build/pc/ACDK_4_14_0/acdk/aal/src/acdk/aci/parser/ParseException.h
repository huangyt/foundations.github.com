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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/ParseException.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_parser_ParseException_h
#define acdk_aci_parser_ParseException_h

#include "ParseNode.h"
#include "../util/CodeLocation.h"

namespace acdk {
namespace aci {
namespace parser {


ACDK_DECL_THROWABLE(ParseException, Exception);
/** 
  @todo rename to ParseException
*/
class ACDK_ACI_PUBLIC ParseException
: extends acdk::lang::Exception
{
  ACDK_WITH_METAINFO(ParseException)
public:
  RParseNode _pNode;
  //acdk::aci::parser::RScanner _scanner;
  acdk::aci::util::RCodeLocation _codeLocation;

  ParseException(IN(RString) text, IN(RParseNode) pnode, IN(acdk::aci::util::RCodeLocation) cl)
  : Exception(text)
  , _pNode(pnode)
  , _codeLocation(cl)
  {
  }
  virtual RString getMessage();
};

} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_ParseException_h

