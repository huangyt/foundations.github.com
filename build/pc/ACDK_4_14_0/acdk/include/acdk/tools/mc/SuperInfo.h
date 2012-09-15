
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


#ifndef acdk_tools_mc_SuperInfo_h
#define acdk_tools_mc_SuperInfo_h

#include "mc.h"
#include "CodeAttribute.h"
#include "CodeInfo.h"

namespace acdk {
namespace tools {
namespace mc {


ACDK_DECL_CLASS(SuperInfo);

class ACDK_TOOLS_MC_PUBLIC SuperInfo
: public CodeInfo
{
public:
  RClassInfo _classInfo;
  SuperInfo(int flag, IN(RString) n, IN(RClassInfo) classInfo)
  : CodeInfo(flag, n)
  , _classInfo(classInfo)
  {
  }
  virtual RString getMetaInfoCIdentifier();
};



} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_SuperInfo_h
