
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


#ifndef acdk_tools_mc_UnitInfo_h
#define acdk_tools_mc_UnitInfo_h

#include "mc.h"
#include "CodeAttribute.h"
#include "CodeInfo.h"
#include <acdk/io/StreamTokenizer.h>

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(UnitInfo);

class ACDK_TOOLS_MC_PUBLIC UnitInfo
: extends CodeInfo
//, implements acdk::lang::Comparable
{
  DECL_ACDK_DEFAULT_METACLASS(Object)
public:
  UnitInfo(IN(RString) name)
    : CodeInfo(0, name)
  {
  }
  virtual RString getMetaInfoCIdentifier() 
  {
    return name + "_unitInfo";
  }
};


} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_UnitInfo_h
