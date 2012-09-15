
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


#ifndef acdk_tools_mc_InvokeForwardAttribute_h
#define acdk_tools_mc_InvokeForwardAttribute_h

#include "mc.h"
#include "CodeAttribute.h"
#include "MethodInfo.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(InvokeForwardAttribute);
/**
  This class define for all defined methods
  of the related class or for related the method 
  an body in the clazzinfo which forwards the call
  to the underlying DMI invoke().
  This only works for non-static methods.
  With the optional argument generate an single
  method also can be excluded for generating 
  an default forwarding to invoke
*/
class ACDK_TOOLS_MC_PUBLIC InvokeForwardAttribute
: extends CodeAttribute
{
  ACDK_WITH_METAINFO(InvokeForwardAttribute)
public:
  bool _generate;
  InvokeForwardAttribute(bool generate = true) 
  : _generate(generate)
  {
  }
  foreign virtual bool apply(IN(RCodeInfo) codeinfo);
  bool applyToMethod(IN(RMethodInfo) methodinfo);
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_InvokeForwardAttribute_h
