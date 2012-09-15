
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


#ifndef acdk_tools_mc_DmiProxyAttribute_h
#define acdk_tools_mc_DmiProxyAttribute_h

#include "mc.h"
#include "CodeAttribute.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(DmiProxyAttribute);
/**
  This class add DmiProxy interface to an
  ACDK interface. A DMIProxy enables an
  Script language to implement an ACDK interface.
*/
class ACDK_TOOLS_MC_PUBLIC DmiProxyAttribute
: extends CodeAttribute
{
  ACDK_WITH_METAINFO(DmiProxyAttribute)
  bool _genProxy;
public:
  DmiProxyAttribute(bool generateProxy = true) : _genProxy(generateProxy) {}
  static void initAttribute(IN(RMetaCompiler) mc); // will be called via DMI
  foreign virtual bool apply(IN(RCodeInfo) codeinfo);
  foreign void writeMethodProxy(IN(RMethodInfo) mi, StringBuffer& sb);
  bool generateProxy() { return _genProxy; }
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_DmiProxyAttribute_h
