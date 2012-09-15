
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


#ifndef acdk_tools_mc_SetDispatchAttribute_h
#define acdk_tools_mc_SetDispatchAttribute_h

#include "mc.h"
#include "CodeAttribute.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(SetDispatchAttribute);
/**
  With this attribute the dmi dispatch method for a class (static or dynamic methods), 
  a method or a field can be set.

  @code
// sample
// by default all non-static dmi calls are passed to the DispatchTest::stddispatch
// method
ACDK_CLASSATTRIBUTE(acdk.tools.mc.SetDispatchAttribute("DispatchTest::stddispatch", false))
class DispatchTest
: public acdk::lang::Object
{
  ACDK_WITH_METAINFO(DispatchTest)
public:
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
   }
   // set dmi method for foo to fooDispatch
  ACDK_METHODATTRIBUTE(acdk.tools.mc.SetDispatchAttribute("DispatchTest::fooDispatch", false))
  void foo(IN(RString) str) {}

  ACDK_FIELDATTRIBUTE(acdk.tools.mc.SetDispatchAttribute("DispatchTest::dummyacc", false))
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
  }
};
@endcode
*/
class ACDK_TOOLS_MC_PUBLIC SetDispatchAttribute
: extends CodeAttribute
{
  ACDK_WITH_METAINFO(SetDispatchAttribute)
public:
  RString _functionSignature;
  bool _staticCall;
  SetDispatchAttribute(IN(RString) signature = "", bool staticCall = false);
  static void initAttribute(IN(RMetaCompiler) mc); // will be called via DMI
  foreign virtual bool apply(IN(RCodeInfo) cm);
  foreign virtual bool apply(IN(RModuleInfo) cm);
  foreign virtual bool apply(IN(RClassInfo) cm);
  foreign virtual bool apply(IN(RMethodInfo) cm);
  foreign virtual bool apply(IN(RArgumentInfo) cm);
  foreign virtual bool apply(IN(RFieldInfo) cm);
  foreign bool attachAttribute(IN(RCodeInfo) ci);
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_SetDispatchAttribute_h
