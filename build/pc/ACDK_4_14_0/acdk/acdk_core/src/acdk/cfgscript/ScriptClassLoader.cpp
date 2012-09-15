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


#include "ScriptClassLoader.h"
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/util/StringTokenizer.h>

namespace acdk {
namespace cfgscript {

bool 
ScriptClassLoader::loadClassLibrary(IN(RString) ipath, IN(RString) classname)
{
  RString path = ipath;
  RString normalizedClass = classname->replace("::", "/")->replace(".", "/");
  //path = path + normalizedClass + ".csf";
  acdk::io::File file(path, normalizedClass + ".csf");
  if (file.exists() == false)
    return false;
  try 
  {
    ACDK_NLOG("acdk.cfgscript", Trace, "Load CfgScript class: " + file.getCanonicalPath());
    RProps props = new Props();
    RScript script = new Script(file.getCanonicalPath());
    script->readEval(&props) ;
    ACDK_NLOG("acdk.cfgscript", Trace, "Loaded CfgScript class: " + file.getCanonicalPath());
  }
  catch (acdk::io::RThrowable ex)
  {
    ACDK_NLOGP("acdk.system", Error, "ScriptClassLoader, Error loading CfgScriptFile", LOG_NPV(ScriptFile, path) << LOG_NPV(Error, ex->getMessage()));
    return false;
  }
  RClass cls = ClassLoader::findLoadedClass(classname);
  return cls != Nil;
}

bool 
ScriptClassLoader::loadClassLibrary(IN(RString) classname)
{
  // TODO this will be called too much, improve performance in CfgScript
  RString pathList = System::getProperty("CSFPATH");
  if (pathList != Nil)
  {

    acdk::util::StringTokenizer stk(pathList, acdk::io::File::pathSeparator());
    RStringArray sa = stk.allToken();
    for (int i = 0; i < sa->length(); ++i)
    {
      if (loadClassLibrary(sa[i], classname) == true)
        return true;
    }
  }
  /*
    acdk/cfg/csf/lib/myns/MyClass  
  */
  RString path = System::getAcdkHome() + "/cfg/csf/lib/";
  return loadClassLibrary(path, classname);
  /*
  RString normalizedClass = classname->replace("::", "/")->replace(".", "/");
  path = path + normalizedClass + ".csf";
  acdk::io::File file(path);
  if (file.exists() == false)
    return false;
  try 
  {
    RProps props = new Props();
    RScript script = new Script(path);
    script->readEval(&props) ;
  }
  catch (acdk::io::RThrowable ex)
  {
    ACDK_NLOGP("acdk.system", Error, "ScriptClassLoader, Error loading CfgScriptFile", LOG_NPV(ScriptFile, path) << LOG_NPV(Error, ex->getMessage()));
    return false;
  }
  RClass cls = ClassLoader::findLoadedClass(classname);
  return cls != Nil;
  */
}

RClass 
ScriptClassLoader::findClass(IN(RString) name, bool nothrow) THROWS1(RClassNotFoundException)
{
  if (loadClassLibrary(name) == false)
  {
    if (nothrow == true)
      return Nil;
    THROW1(ClassNotFoundException, "Class cannot be found: " + name);
    return Nil;
  }
  return Class::forName(name);
}


RClass 
ScriptClassLoader::findLoadedClass(IN(RString) name, bool nothrow) THROWS1(RClassNotFoundException)
{
  if (nothrow == true)
    return Nil;
  THROW1(ClassNotFoundException, "Class cannot be found: " + name);
  return Nil;
}

acdk::util::RIterator 
ScriptClassLoader::findResources(IN(RString) name)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return Nil;
}
  

struct RegisterClassLoader
{
  RegisterClassLoader()
  {
    ClassLoader::getSystemClassLoader()->registerClassLoader(new ScriptClassLoader());
  }
};
RegisterClassLoader _registerClassLoader;

} // namespace cfgscript
} // namespace acdk 
  
