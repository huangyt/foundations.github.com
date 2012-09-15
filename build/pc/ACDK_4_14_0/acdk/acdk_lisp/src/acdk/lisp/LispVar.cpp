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




#include "LispClass.h"
#include "LispException.h"
#include "LispEnvironment.h"

namespace acdk {
namespace lisp {

RString 
LispVar::getStringToken()
{
  RLispVar self = this;
  if (instanceof(self, LispAtom) == true)
  {
    return toString();
  }
  if (instanceof(self, LispList) == true)
  {
    RLispList ll = RLispList(self);
    if (ll->length() == 2 && ll->car()->toString()->equals("quote") == true)
      return ll->cdr()->car()->toString();
  }
  return Nil;
}

} // namespace lisp
} // namespace acdk



