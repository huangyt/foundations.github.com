
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


#ifndef acdk_tools_mc_DmiProxyGeneratorExt_h
#define acdk_tools_mc_DmiProxyGeneratorExt_h

#include "mc.h"
#include "CodeAttribute.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(DmiProxyGeneratorExt);
/**
  This class add DmiProxy interface to an
  ACDK interface. A DMIProxy enables an
  Script language to implement an ACDK interface.
*/
class ACDK_TOOLS_MC_PUBLIC DmiProxyGeneratorExt
: extends acdk::lang::Object
{
  //ACDK_WITH_METAINFO(DmiProxyGeneratorExt)
private:
  bool _generateProxy;
  bool _parentOnly;
public:
  typedef acdk::lang::sys::core_vector<const acdk::lang::dmi::ClazzMethodInfo*> ClazzMethodInfoArray;
  void generateProxyMethodArgDecl(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb);
  void generateProxyMethodArgCall(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb);
  void generateProxyMethodThrowDecl(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb);
  void generateProxyClassDeclDefConstructor(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb);
  void generateProxyClassMethod(const acdk::lang::dmi::ClazzInfo* super, const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb);
  void generateDispatchMethod(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb);
  void generateProxyClassDecl(const acdk::lang::dmi::ClazzInfo* ci, StringBuffer& sb, ClazzMethodInfoArray& constr, ClazzMethodInfoArray& methods);
  void generateProxyCiMethod(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb);
  void generateProxyCiMethods(const acdk::lang::dmi::ClazzInfo* ci, ClazzMethodInfoArray& methods, StringBuffer& sb);
  void generateProxyCiInterfaces(const acdk::lang::dmi::ClazzInfo* ci, StringBuffer& sb);
  void generateProxy(const acdk::lang::dmi::ClazzInfo* ci, StringBuffer& sb);
  void generateProxy(IN(RString) classname, IN(::acdk::io::RPrintWriter) out);
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_DmiProxyGeneratorExt_h
