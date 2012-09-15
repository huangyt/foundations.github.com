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


#ifndef acdk_lisp_LispBinaryCode_h
#define acdk_lisp_LispBinaryCode_h

#include "LispEnvironment.h"

namespace acdk {
namespace lisp {

ACDK_DECL_CLASS(LispBinaryCode);

class ACDK_ACDK_LISP_PUBLIC LispBinaryCode
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(LispBinaryCode)
public:
    acdk::util::RHashMap globals;
    acdk::util::RHashMap macros;
    acdk::util::RHashMap defuns;
    // needed for serialization
    LispBinaryCode() {}
    
};



} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispBinaryCode_h
