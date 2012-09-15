
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


#ifndef acdk_tools_mc_ThrowableAttribute_h
#define acdk_tools_mc_ThrowableAttribute_h

#include "mc.h"
#include "CodeAttribute.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(ThrowableAttribute);
/**
  This attribute will attached to each Throwable classes.
  Its main purpose is to attach a MetaAttribute to each exception
  class, wich contains a pointer to a method, which throws the
  exception 
*/
class ACDK_TOOLS_MC_PUBLIC ThrowableAttribute
: extends CodeAttribute
{
  ACDK_WITH_METAINFO(ThrowableAttribute)
public:
  ThrowableAttribute() {}
  foreign virtual bool apply(IN(RCodeInfo) cm);
  
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_ThrowableAttribute_h
