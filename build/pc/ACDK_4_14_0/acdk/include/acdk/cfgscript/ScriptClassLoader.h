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

#ifndef acdk_cfgscript_ScriptClassLoader_h
#define acdk_cfgscript_ScriptClassLoader_h

#include <acdk.h>
#include "Script.h"
#include <acdk/lang/ClassLoader.h>

namespace acdk {
namespace cfgscript {

ACDK_DECL_CLASS(ScriptClassLoader);

/**
  implements a ClassLoader for CfgScript.
  $ACDKHOME/cfg/csf/lib/namespace/namespace/Class.csf
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC ScriptClassLoader
: extends ::acdk::lang::ClassLoader
{
  ACDK_WITH_METAINFO(ScriptClassLoader)
public:
  ScriptClassLoader() {}
  virtual RClass findClass(IN(RString) name, bool nothrow = false) THROWS1(RClassNotFoundException);
  virtual bool loadClassLibrary(IN(RString) classname);
  virtual RClass findLoadedClass(IN(RString) name, bool nothrow = true) THROWS1(RClassNotFoundException);
  virtual acdk::util::RIterator findResources(IN(RString) name);
  virtual bool loadMetaInfoLibrary(IN(RString) classname) { return true; }
  virtual bool loadDmiProxyLibrary(IN(RString) classname) { return true; }
  virtual void resolveClass(IN(RClass) c) { }

  bool loadClassLibrary(IN(RString) path, IN(RString) classname);
  
};

} // namespace cfgscript
} // namespace acdk 
  
#endif //acdk_cfgscript_ScriptClassLoader_h
