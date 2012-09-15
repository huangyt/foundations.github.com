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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispException.cpp,v 1.6 2005/02/05 10:45:12 kommer Exp $



#include "lisp.h"
#include "LispEnvironment.h"
#include "LispException.h"

#include <acdk/util/ArrayList.h>


namespace acdk {
namespace lisp {

LispException::LispException(IN(RLispEnvironment) env, IN(RString) what) 
: RuntimeException(what)
{ 
  acdk::util::RIterator it = env->_evalStack.iterator();
  StringBuffer sb(1024);
  sb.append("\nEval Stack:\n");
  while (it->hasNext() == true) {
    RLispVar lvar = (RLispVar)it->next();
    sb.append("  ");
    RString tstr = (lvar == Nil ? RString("NIL") : lvar->toCode());
    sb.append(tstr);
    sb.append("\n");
  }
  _where = sb.toString();
  env->setBreak(1);
}

//virtual 
RString 
LispException::getMessage()
{
  return RuntimeException::getMessage() + _where;
}
  


} // namespace lisp
} // namespace acdk


