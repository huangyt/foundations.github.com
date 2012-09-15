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



#include "LispCallBack.h"
#include "LispEnvironment.h"

namespace acdk {
namespace lisp {


LispCallBack::LispCallBack(const char* delc, EvalFunc evalfunc, bool evalargs/* = true*/, bool ownscope) //char* helptext/* = ""*/) 
: Object()
, _evalfunc(evalfunc)
, _evalargs(evalargs)
, _ownScope(ownscope)
//, _helptext(helptext)
{
  _declCode = LispEnvironment::parseToList(delc);
}

//virtual 
RString 
LispCallBack::getHelpText()
{
  return _declCode->toCode();
}

} // namespace lisp
} // namespace acdk



