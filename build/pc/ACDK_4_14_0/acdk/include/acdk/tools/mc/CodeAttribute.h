
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


#ifndef acdk_tools_mc_CodeAttribute_h
#define acdk_tools_mc_CodeAttribute_h

#include "mc.h"
//#include "MetaCompiler.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(CodeInfo);
ACDK_DECL_CLASS(MetaCompiler);
ACDK_DECL_CLASS(CodeAttribute);
ACDK_DECL_CLASS(ModuleInfo);
ACDK_DECL_CLASS(ClassInfo);
ACDK_DECL_CLASS(MethodInfo);
ACDK_DECL_CLASS(ArgumentInfo);
ACDK_DECL_CLASS(FieldInfo);

class ACDK_TOOLS_MC_PUBLIC CodeAttribute
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(CodeAttribute)
public:
  static void initAttribute(IN(RMetaCompiler) mc); // will be called via DMI

  virtual bool apply(IN(RCodeInfo) cm);
  /*
  foreign virtual bool apply(IN(RModuleInfo) cm) { return false; }
  foreign virtual bool apply(IN(RClassInfo) cm) { return false; }
  foreign virtual bool apply(IN(RMethodInfo) cm) { return false; }
  foreign virtual bool apply(IN(RArgumentInfo) cm) { return false; }
  foreign virtual bool apply(IN(RFieldInfo) cm) { return false; }
  */
  foreign bool attachAttribute(IN(RCodeInfo) ci);
  static int getCounter();
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_CodeAttribute_h
