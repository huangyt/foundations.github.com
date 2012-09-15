
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


#ifndef acdk_tools_mc_ClassInitAttribute_h
#define acdk_tools_mc_ClassInitAttribute_h

#include "mc.h"
#include "CodeAttribute.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(ClassInitAttribute);

/**
  If a ClazzInfo will be registered
  (typically when shared library with ACDK objects will be loaded)
  the given initialization method will be called for static class initialization
  If the ClazzInfo will be derigistered (shared library will be 
  unloaded) the given deinitialization method will be called.
  The initialization and deinitialization methods must be exported DMI-Methods
  [private/protected/public] static void methodname();
  Only one initialization and deinitialization method can be declared per class.
  The name of the method is either a fully qualified method name (namespace.class.method)
  or the name of the method only if the method is defined in the attributed class.
  Sample:
  @code
    namespace my {
    namespace package {
    
    ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClassInitAttribute(: initFunction = "my.package.MyClass.classInit"))
    class MY_PACKAGE_PUBLIC MyClass
    : extends acdk::lang::Object
    {
    private:
      static void classInit() 
      {
        // make class wide initialization here  
      }
    };
  @endcode
*/
class ACDK_TOOLS_MC_PUBLIC ClassInitAttribute
: extends CodeAttribute
{
  ACDK_WITH_METAINFO(ClassInitAttribute)
public:
  RString _initFunction;
  RString _deinitFunction;
  ClassInitAttribute(IN(RString) initFunction = Nil, IN(RString) deinitFunction = Nil);
  static void initAttribute(IN(RMetaCompiler) mc); // will be called via DMI
  foreign virtual bool apply(IN(RCodeInfo) cm);
  
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_ClassInitAttribute_h
