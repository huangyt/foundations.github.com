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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispVar.h,v 1.22 2005/02/05 10:45:12 kommer Exp $

#ifndef acdk_lisp_LispVar_h
#define acdk_lisp_LispVar_h


#include "lisp.h"

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

namespace acdk {
namespace lisp {

using namespace acdk::lang;
using namespace acdk::lang::sys;
using namespace acdk::lang::dmi;
//using namespace acdk::text;

ACDK_DECL_CLASS(LispVar);

class ACDK_ACDK_LISP_PUBLIC LispVar
: public acdk::lang::Object,
  implements acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(LispVar)
public:

  LispVar() : Object(){ }
  /// converts it to string without paranteses or white spaces
  virtual RString toString() = 0; 
  /// converts it to Lisp code
  virtual RString toCode() = 0;
  // just to force implementation
  virtual RObject clone() = 0;
  virtual RObject clone(sys::Allocator* alc) = 0;
  virtual RObject getObject() { return RObject(this); } 
  /**
    "token" and (quote token) returns token
  */
  RString getStringToken();
};



} // namespace lisp
} // namespace acdk

#endif //acdk_lisp_LispVar_h

