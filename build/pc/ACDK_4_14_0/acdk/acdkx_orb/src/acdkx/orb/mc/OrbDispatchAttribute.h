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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/mc/OrbDispatchAttribute.h,v 1.11 2005/02/07 17:18:05 kommer Exp $

#ifndef acdkx_orb_mc_OrbDispatchAttribute_h
#define acdkx_orb_mc_OrbDispatchAttribute_h


#include "Config.h"

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
USING_CLASS(::acdk::tools::mc::, MethodInfo);
USING_CLASS(::acdk::tools::mc::, ArgumentInfo);
USING_CLASS(::acdk::tools::mc::, ModuleInfo);
USING_CLASS(::acdk::tools::mc::, FieldInfo);

  
ACDK_DECL_CLASS(OrbDispatchAttribute);

class ACDKX_ORB_MC_PUBLIC OrbDispatchAttribute
: extends acdk::tools::mc::CodeAttribute
{
  ACDK_WITH_METAINFO(OrbDispatchAttribute)
public:
  OrbDispatchAttribute() {}
  static void initAttribute(IN(RMetaCompiler) mc); // will be called via DMI
  virtual bool apply(IN(RCodeInfo) cm);
  foreign void writeMethodProxy(IN(RMethodInfo) mi, StringBuffer& sb);
};


} // namespace mc
} // namespace orb
} // namespace acdkx


#endif //acdkx_orb_mc_OrbDispatchAttribute_h
