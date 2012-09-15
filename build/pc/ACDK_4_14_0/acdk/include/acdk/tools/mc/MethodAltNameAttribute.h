
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


#ifndef acdk_tools_mc_MethodAltNameAttribute_h
#define acdk_tools_mc_MethodAltNameAttribute_h

#include "mc.h"
#include "CodeAttribute.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(MethodAltNameAttribute);
/**
  Sets the altname field in acdk::lang::dmi::MethodInfo.
*/
class ACDK_TOOLS_MC_PUBLIC MethodAltNameAttribute
: extends CodeAttribute
{
  ACDK_WITH_METAINFO(MethodAltNameAttribute)
protected:
  RString _altName;
  int _paramCount;
public:
  /**
    Constructor used with ACDK_METHODATTRIBUTE.
    @param altname alternative name for the method
    @param paramCount in case the method has default parameter
           paramCount identifies the method number of parameters.
    @Sample
    <code>
    ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("methodWithoutArgs", 0))
    ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("methodWithOneArgs", 1))
    int methodWithDefaultArgs(int i = 3)
    {
      return i;
    }
    </code>
    via DMI the method methodWithDefaultArgs is callable as
    int methodWithoutArgs(); 
    and 
    int methodWithOneArgs(int i);
  */
  MethodAltNameAttribute(IN(RString) altName, int paramCount = -1);
  
  static void initAttribute(IN(RMetaCompiler) mc); // will be called via DMI
  foreign virtual bool apply(IN(RCodeInfo) cm);

  
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_MethodAltNameAttribute_h
