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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispException.h,v 1.9 2005/02/05 10:45:12 kommer Exp $

#ifndef acdk_lisp_LispException_h
#define acdk_lisp_LispException_h

#include "lisp.h"

#include <acdk/lang/RuntimeException.h>

namespace acdk {
namespace lisp {

ACDK_DECL_CLASS(LispEnvironment);


ACDK_DECL_THROWABLE(LispException, RuntimeException);

class ACDK_ACDK_LISP_PUBLIC LispException
: public acdk::lang::RuntimeException
{
  ACDK_WITH_METAINFO(LispException)
private:
  RString _where;
public:
  LispException(IN(RLispEnvironment) env, IN(RString) msg);
  virtual RString getMessage();

};


} // namespace lisp
} // namespace acdk

#endif //acdk_lisp_LispException_h

