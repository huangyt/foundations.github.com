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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/lisp.h,v 1.8 2005/02/05 10:45:12 kommer Exp $

#ifndef acdk_lisp_lisp_h
#define acdk_lisp_lisp_h

// this is temporary necessary, or VC won't compile anything...
#include <acdk.h>

#include "Config.h"
// defines the unit acdk/tools/aunit
ACDK_DECL_UNIT(acdk_java)

namespace acdk {
/**
  Implements the ACDK Lisp interpreter 
*/
namespace lisp {

using namespace acdk::lang;



} // namespace lisp
} // namespace acdk

#endif //acdk_lisp_lisp_h

