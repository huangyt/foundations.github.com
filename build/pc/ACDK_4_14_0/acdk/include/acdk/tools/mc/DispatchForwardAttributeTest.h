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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/mc/DispatchForwardAttributeTest.h,v 1.7 2005/04/13 12:59:59 kommer Exp $

#ifndef acdk_tools_mc_DispatchForwardAttributeTest_h
#define acdk_tools_mc_DispatchForwardAttributeTest_h

#include "Config.h"
#include <acdk.h>

namespace acdk {
  namespace tools {
  namespace mc {

ACDK_DECL_CLASS(DispatchForwardAttributeTest);
/**
  this class is for internal testing only.
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.SetDispatchAttribute("DispatchForwardAttributeTest::stddispatch", false))
class ACDK_TOOLS_MC_PUBLIC DispatchForwardAttributeTest
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(DispatchForwardAttributeTest)
public:
  DispatchForwardAttributeTest(IN(RObject) obj) : dummyvar(obj) {}
  static const ::acdk::lang::dmi::ClazzMethodInfo* stddispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    return _invoke_dynamic(This, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  }

  static const ::acdk::lang::dmi::ClazzMethodInfo* fooDispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = inOf(RString(args[1].getStringVar() + " intercepted"));
    return (const ::acdk::lang::dmi::ClazzMethodInfo*)1;
  }

  ACDK_METHODATTRIBUTE(acdk.tools.mc.SetDispatchAttribute("DispatchForwardAttributeTest::fooDispatch", false))
  RString foo(IN(RString) str) { return str; }

  ACDK_FIELDATTRIBUTE(acdk.tools.mc.SetDispatchAttribute("DispatchForwardAttributeTest::dummyacc", false))
  RObject dummyvar;

  static const ::acdk::lang::dmi::ClazzFieldInfo* dummyacc(
                                    ::acdk::lang::Object* This, 
                                      IN(acdk::lang::RString) fname, 
                                      ::acdk::lang::dmi::ScriptVar& var, 
                                      ::acdk::lang::dmi::DmiClient& dc,
                                      int flags,
                                      const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                      const ::acdk::lang::dmi::ClazzFieldInfo* fieldinf)
  {
    var = new Integer(42);
    return (const ::acdk::lang::dmi::ClazzFieldInfo*)1;
  }
};

} // mc
} // tools
} // acdk

#endif //acdk_tools_mc_DispatchForwardAttributeTest_h

