
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


#ifndef acdk_tools_mc_ArgumentInfo_h
#define acdk_tools_mc_ArgumentInfo_h

#include "mc.h"
#include "CodeAttribute.h"
#include "MethodInfo.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(ArgumentInfo);

class ACDK_TOOLS_MC_PUBLIC ArgumentInfo
: extends CodeInfo
{
public:
  RString type;
  bool hasDefaultInitializer;
  RString defaultInitializer;
  ArgumentInfo(IN(RTypeScope) parent, IN(RString) t, IN(RString) n, int aflags)
  : CodeInfo(aflags, n, parent)
  , type(t),
    hasDefaultInitializer(false)
  {
  }
  /** print to original Code, including IN/OUT/BYVAL values */
  RString toCode();
  RObject clone(sys::Allocator* alc);
  virtual RString getMetaInfoCIdentifier();

  static RString getScriptVarGetter(IN(RString) type);
  static RString castToScriptVar(IN(RString) type);
  
  //RString getOrbOutputMethod();
  //RString getOrbInputMethod();
  //static RString getOrbOutputMethod(IN(RString) tpname, int flags);
  //static RString getOrbInputMethod(IN(RString) tpname, int flags);
  //static RString getOrbInOutputMethod(IN(RString) readwrite, IN(RString) tpname, int flags, bool withArgsPara = false);
  RString getCaster();
  static RString getCaster(IN(RString) tpname);
  bool isEnum() { return hasType(type) == TsEnum; }
  RString getOrgType()
  {
    return type;
  }
  RString getMappedType()
  {
    if (hasType(type) == TsEnum)
      return "int";
    return type;
  }
  void asIDLType(IN(RPrintWriter) out);
  bool invokeCodeAttributes(IN(RModuleInfo) cm, IN(RClassInfo) ci, IN(RMethodInfo) mi);
};



} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_ArgumentInfo_h
