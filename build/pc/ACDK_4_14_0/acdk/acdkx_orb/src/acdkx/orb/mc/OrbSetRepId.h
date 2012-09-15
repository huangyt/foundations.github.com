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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/mc/OrbSetRepId.h,v 1.6 2005/02/05 10:45:40 kommer Exp $

#ifndef acdkx_orb_mc_OrbSetRepId_h
#define acdkx_orb_mc_OrbSetRepId_h


#include <acdk.h>
#include <acdk/tools/mc/CodeAttribute.h>
#include <acdk/tools/mc/FieldInfo.h>
#include <acdk/tools/mc/MetaCompiler.h>

#include "mc.h"

namespace acdkx {
namespace orb {
namespace mc {

USING_CLASS(::acdk::tools::mc::, MetaCompiler);
USING_CLASS(::acdk::tools::mc::, CodeInfo);
USING_CLASS(::acdk::tools::mc::, ClassInfo);
  
ACDK_DECL_CLASS(OrbSetRepId);

class ACDKX_ORB_MC_PUBLIC OrbSetRepId
: extends acdk::tools::mc::CodeAttribute
{
  ACDK_WITH_METAINFO(OrbSetRepId)
public:
  RString _repId;
  short _major;
  short _minor;
  OrbSetRepId(IN(RString) repid, short major = 1, short minor = 0) 
  : _repId(repid)
  , _major(major)
  , _minor(minor)
  {
  }
  
  virtual bool apply(IN(RCodeInfo) cm);
  
};


} // namespace mc
} // namespace orb
} // namespace acdkx


#endif //acdkx_orb_mc_OrbSetRepId_h
