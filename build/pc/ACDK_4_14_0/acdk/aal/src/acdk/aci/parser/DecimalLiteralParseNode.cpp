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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/DecimalLiteralParseNode.cpp,v 1.4 2005/02/05 10:44:51 kommer Exp $


#include "DecimalLiteralParseNode.h"
#include "../ast/Literal.h"


namespace acdk {
namespace aci {
namespace parser {


acdk::aci::ast::RTerminal 
DecimalLiteralParseNode::createTerminal(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cp) 
{
  RString s = input;
  RString tstr = s;
  char postfix = 0;
  if (s->endsWith("L") == true || s->endsWith("I") == true || 
      s->endsWith("S") == true || s->endsWith("B") == true)
  {
    char postfix = s->charAt(s->length() - 1);
    s = s->substr(0, s->length() - 1);
  }
  jlong lvar = Long::parseLong(s);
  if (lvar > Integer::MAX_VALUE || lvar < Integer::MIN_VALUE || postfix == 'L')
  {
    if (postfix != 0 && postfix != 'L')
      THROW1(Exception, "Constant to big for declarated size: " + tstr);
    return new Literal(this, cp, inOf(lvar));
  }
  int ivar = (int)lvar;
  if (ivar > Short::MAX_VALUE || ivar < Short::MIN_VALUE || postfix == 'I')
  {
    if (postfix != 0 && postfix != 'I')
      THROW1(Exception, "Constant to big for declarated size: " + tstr);
    return new Literal(this, cp, inOf(ivar));
  }
  short svar = (short)ivar;
  if (svar > Byte::MAX_VALUE || svar < Byte::MIN_VALUE || postfix == 'S')
  {
    if (postfix != 0 && postfix != 'S')
      THROW1(Exception, "Constant to big for declarated size: " + tstr);
    return new Literal(this, cp, inOf(svar));
  }
  return new Literal(this, cp, inOf(byte(svar)));
}


} // parser
} // aci
} // acdk



