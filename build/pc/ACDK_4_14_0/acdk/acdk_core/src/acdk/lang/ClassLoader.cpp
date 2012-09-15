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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ClassLoader.cpp,v 1.43 2005/04/19 21:28:00 kommer Exp $


#include <acdk.h>
#include "System.h"
#include "ClassLoader.h"
#include "SharedLibrary.h"
#include "UnsupportedOperationException.h"
#include "ClassNotFoundException.h"
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace lang {




//static 
  OUT(RClassLoader) 
ClassLoader::_getSystemClassLoader()
{
  static RClassLoader __systemClassLoader = new ClassLoader();
  return __systemClassLoader;
}

ClassLoader::ClassLoader()
: Object()
, _registeredClassLoader(new ClassLoaderArray(0))
{

}

ClassLoader::ClassLoader(IN(RClassLoader) parent)
: Object()
, _parent(parent)
, _registeredClassLoader(new ClassLoaderArray(0))
{
}

//virtual 
RClass 
ClassLoader::defineClass(IN(RString) name, IN(RbyteArray) b, int off, int len)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return Nil;
}

bool 
ClassLoader::loadClassLibrary(IN(RString) classname)
{
  if (classname->endsWith("_DmiProxy") == true)
    return loadDmiProxyLibrary(classname);
  RString fqname = classname->replace(".", "_")->replace("/", "_")->replace("::", "_");
  RString libname = findLibrary(fqname);
  while (fqname != Nil && libname == Nil) 
  {
    int idx;
    if ((idx = fqname->lastIndexOf('_')) == -1) 
      goto notfound;
    fqname = fqname->substr(0, idx);
    libname = findLibrary(fqname);
  }
notfound:
  if (libname == Nil)
  {
    for (int i = 0; i < _registeredClassLoader->length(); ++i)
      if (_registeredClassLoader[i]->loadClassLibrary(classname) == true)
        return true;
  }
  return libname != Nil;
}

//virtual 
RClass 
ClassLoader::findClass(IN(RString) name, bool nothrow) THROWS1(RClassNotFoundException)
{
  RClass cls = findLoadedClass(name);
  if (cls)
    return cls;
  if (loadClassLibrary(name) == true)
  {
    cls = findLoadedClass(name);
    if (cls != Nil)
      return cls;
  }

  for (int i = 0; i < _registeredClassLoader->length(); ++i)
  {
    RClass cls = _registeredClassLoader[i]->findClass(name, true);
    if (cls != Nil)
      return cls;
  }
  if (nothrow == false)
    THROW1(ClassNotFoundException, "Class cannot be found: " + name);
  return Nil;
}


RString 
ClassLoader::findLibrary(IN(RString) libname)
{
  acdk::util::RMap aliase = System::getProperties()->getMapProperty("acdk.lang.ClassLoader.libalias");
  if (aliase != Nil)
  {
    acdk::util::RIterator it = aliase->keySet()->iterator();
    while (it->hasNext() == true)
    {
      RString k = (RString)it->next();
      if (libname->startsWith(k) == true)
      {
        RString repl = (RString)aliase->get((RObject)k);
        RString l = repl + libname->substr(k->length());
        RString s = findLibrary2(l);
        if (s != Nil)
          return s;
      }
    }
  }
  return findLibrary2(libname);
}


//virtual 
RString 
ClassLoader::findLibrary2(IN(RString) libname)
{
#if defined(ACDK_OS_WIN32)
# if defined(_MSC_VER)
#   if defined(ACDK_DEBUG)
  RString fqlibname = libname + "_d.dll";
#   else
  RString fqlibname = libname + "_r.dll";
#   endif
# else //defined(_MSC_VER)
  RString fqlibname = libname + ".dll";
# endif //defined(_MSC_VER)

#else // unix
#if defined(ACDK_OS_DARWIN)
 RString fqlibname = "lib" + libname + ".dylib";
#else
  RString fqlibname = "lib" + libname + ".so";
#endif
#endif

  
  RStringArray pathlist = System::getAcdkPath();
  for (int i = 0; i < pathlist->length(); ++i)
  {
    RString fql = pathlist[i] + acdk::io::File::separator() + "bin" + acdk::io::File::separator() + fqlibname;
    if (acdk::io::File(fql).exists() == true)
    {
      fqlibname = fql;
      break;
    }
  }
  /*
  RString mh = System::getAcdkToolsHome();
  if (mh != Nil) 
  {
    RString fql = mh + acdk::io::File::separator() + "bin" + acdk::io::File::separator() + fqlibname;
    if (acdk::io::File(fql).exists() == true)
      fqlibname = fql;
  }*/
  /*
#if defined(ACDK_OS_WIN32)
  RString msg = fqlibname->convert(CCUcs2);
  MessageBox(NULL, msg->uc2c_str(), _T("try ClassLoader::findLibrary"), MB_OK);
#endif
  */
  
  SharedLibrary slib(fqlibname); 
  slib.loadLibary(); //### Unload it at exit
  if (slib.loaded() == true)
  {
    ACDK_NLOGP("acdk.lang.classloader", SysDebug, "Library loaded", LOG_NPV(Library, fqlibname));
      //System::out->println("Load Library: " + fqlibname);
    return fqlibname;
  }
  //System::out->println("Load Library Failed: " + fqlibname);
  return Nil;
}


bool 
ClassLoader::loadExtInfoLibrary(IN(RString) classname, IN(RString) sufix)
{
  RString fqname = classname->replace(".", "_")->replace("/", "_")->replace("::", "_");
  if (fqname->startsWith("_") == true)
    fqname = fqname->substr(1);
  RString libname = findLibrary(fqname + sufix);
  while (fqname != Nil && libname == Nil) 
  {
    int idx;
    if ((idx = fqname->lastIndexOf('_')) == -1) 
      goto notfound;
    fqname = fqname->substr(0, idx);
    libname = findLibrary(fqname + sufix);
  }
notfound:
  /*
  if (libname != Nil)
      System::out->println("ClassLoader loaded metainfolib: " + libname);
  else
      System::out->println("ClassLoader cannot load metainfolib for class: " + classname);
  */
  return libname != Nil;
}

bool 
ClassLoader::loadMetaInfoLibrary(IN(RString) classname) 
{
  return loadExtInfoLibrary(classname,  "_metainf");
}


bool 
ClassLoader::loadDmiProxyLibrary(IN(RString) classname)
{
  static bool _loaded = false;
  if (_loaded == false)
  {
     if (loadExtInfoLibrary("acdk_core", "_dmiproxy") == false)
       return false;
    RString clsname = classname;
    if (clsname->endsWith("_DmiProxy") == false)
      clsname = clsname + "_DmiProxy";
    RClass cls = findLoadedClass(clsname);
    if (cls != Nil)
      return true;
    _loaded = true;
  }
  RString clsname = classname;
  
  if (clsname->endsWith("_DmiProxy") == false)
    clsname = clsname + "_DmiProxy";
  RClass cls = findLoadedClass(clsname);
  if (cls != Nil)
    return true;
  
  clsname = classname;
  if (clsname->endsWith("_DmiProxy") == true)
    clsname = clsname->substr(0, clsname->length() - strlen("_DmiProxy"));
  return loadExtInfoLibrary(clsname,  "_dmiproxy");
}

//virtual 
RClass 
ClassLoader::findLoadedClass(IN(RString) name, bool nothrow) THROWS1(RClassNotFoundException)
{
  RClass cls =  Class::_forName(name);
  if (nothrow == false && cls == Nil)
    THROW1(ClassNotFoundException, "Class cannot be found: " + name);
  return cls;
}


//virtual 
RClass 
ClassLoader::findSystemClass(IN(RString) name) THROWS1(RClassNotFoundException)
{
  return findClass(name);
}

//virtual 
RPackage 
ClassLoader::getPackage(IN(RString) name)
{
  //???
  return Nil;
}

//virtual 
RPackageArray
ClassLoader::getPackages()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return Nil;
}


  
//static 
RClassLoader 
ClassLoader::getSystemClassLoader()
{
  return _getSystemClassLoader();
}


//virtual 
RClass 
ClassLoader::loadClass(IN(RString) name) THROWS1(RClassNotFoundException)
{
  return loadClass(name, true);
}

//virtual 
RClass 
ClassLoader::loadClass(IN(RString) name, bool resolve) THROWS1(RClassNotFoundException)
{
  return loadClass(name);
}


//virtual 
acdk::lang::reflect::RUnit 
ClassLoader::loadUnit(IN(RString) name) THROWS1(RClassNotFoundException)
{
  acdk::lang::reflect::RUnit unit = acdk::lang::reflect::Unit::getUnit(name);
  if (unit != Nil)
    return unit;
  RString fqname = name->replace(".", "_")->replace("/", "_")->replace("::", "_");
  RString libname = findLibrary(fqname);
  if (libname == Nil) 
  {
    int idx;
    if ((idx = fqname->lastIndexOf('_')) == -1) 
      goto notfound;
    fqname = fqname->substr(0, idx);
    libname = findLibrary(fqname);
    if (libname == Nil)
      return Nil;
  }
  unit = acdk::lang::reflect::Unit::getUnit(name);
  if (unit != Nil)
    return unit;
notfound:
  THROW1(ClassNotFoundException, "Unit cannot be found: " + name);
  return Nil;
}

//virtual 
void 
ClassLoader::resolveClass(IN(RClass) c)
{
  // nothing
}

//virtual 
void 
ClassLoader::setSigners(IN(RClass) c, IN(RObjectArrayImpl<RObject>) signers)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

acdk::io::RFile 
ClassLoader::findResource(IN(RString) name)
{
  RString acdk_home = System::getAcdkHome();
  RString fname = name;//must not because of '.filext': name->replace('.', acdk::io::File::separatorChar());
  fname = fname->replace('/', acdk::io::File::separatorChar());

  RString fqname = acdk_home + acdk::io::File::separator() + "cfg" + 
      acdk::io::File::separator() + fname;
  acdk::io::RFile f = new acdk::io::File(fqname);
  if (f->exists() == false)
    return Nil;
  return f;
}

acdk::util::RIterator 
ClassLoader::findResources(IN(RString) name)
{
  acdk::io::RFileArray fa = new acdk::io::FileArray(0);
  acdk::io::RFile f = findResource(name);
  if (f != Nil)
    fa->append(f);
  return fa->iterator();
}



acdk::io::RFile 
ClassLoader::getResource(IN(RString) name)
{
  if (_parent != Nil)
  {
    acdk::io::RFile f = _parent->getResource(name);
    if (f != Nil)
      return f;
  }
  return findResource(name);
}

acdk::util::RIterator 
ClassLoader::getResources(IN(RString) name)
{
  return findResources(name);
}

acdk::io::RReader 
ClassLoader::getResourceAsStream(IN(RString) name)
{
  acdk::io::RFile f = getResource(name);
  if (f != Nil)
    return f->getReader();
  return Nil;
}

acdk::io::RFile 
ClassLoader::getSystemResource(IN(RString) name)
{
  return getSystemClassLoader()->getResource(name);
}
  
acdk::io::RReader 
ClassLoader::getSystemResourceAsStream(IN(RString) name)
{
  return getSystemClassLoader()->getResourceAsStream(name);
}

acdk::util::RIterator 
ClassLoader::getSystemResources(IN(RString) name)
{
  return getSystemClassLoader()->getResources(name);
}

} // namespace lang 
} // namespace acdk 


