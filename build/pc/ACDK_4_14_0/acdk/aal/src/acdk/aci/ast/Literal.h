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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/ast/Literal.h,v 1.5 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_ast_Literal_h
#define acdk_aci_ast_Literal_h

#include <acdk.h>
#include <acdk/lang/dmi/DmiObject.h>
#include "Terminal.h"
#include "Expression.h"
#include "../vm/OpCodeStm.h"

namespace acdk {
namespace aci {
namespace ast {

ACDK_DECL_CLASS(Literal);

/**
  Literal are expression, where value can be computed at compile time,
  like simple literals Strings, ints or component literals like Arrays, 
*/
class ACDK_ACI_PUBLIC Literal
: extends Terminal
, implements Expression
{
  ACDK_WITH_METAINFO(Literal)
protected:
  acdk::lang::dmi::ScriptVar _sv;
public:
  
  Literal(IN(acdk::aci::parser::RTerminalParseNode) templ, IN(acdk::aci::util::RCodeLocation) cl, IN(acdk::lang::dmi::ScriptVar) sv)
  : Terminal(templ, cl, "Literal")
  , _sv(sv)
  {
    setSaveNodeAfterBuild(true);
  }
  Literal(IN(acdk::aci::parser::RTerminalParseNode) templ, IN(acdk::aci::util::RCodeLocation) cl)
  : Terminal(templ, cl, "Literal")
  , _sv()
  {
    setSaveNodeAfterBuild(true);
  }
  Literal(IN(acdk::aci::parser::RTerminalParseNode) templ, IN(acdk::aci::util::RCodeLocation) cl, 
          IN(acdk::lang::dmi::RDmiObject) sv)
  : Terminal(templ, cl, "Literal")
  , _sv(*sv)
  {
    setSaveNodeAfterBuild(true);
  }

  virtual RString toString()
  {
    return "Literal: " + _sv.toCode();
  }
  virtual RSemanticElem getExpressionSem()
  {
    // ### @todo implement me
    return Nil; //new SemanticElement(_sv.getClazzInfo());
  }
  foreign const acdk::lang::dmi::ScriptVar& getSv() { return _sv; }

  acdk::lang::dmi::RDmiObject getValue() { return new acdk::lang::dmi::DmiObject(_sv); }
  /*
  virtual void genOpCode(IN(RCompiler) comp)
  {
    getParent()->addChild(new acdk::aci::vm::OpCodeStm1(this, acdk::aci::vm::OCO_PUSH, _sv), false);
  }
  */
};


} // ast
} // aci
} // acdk



#endif //acdk_aci_ast_Literal_h
