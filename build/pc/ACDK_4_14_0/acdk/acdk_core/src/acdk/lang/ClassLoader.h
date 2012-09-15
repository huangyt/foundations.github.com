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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ClassLoader.h,v 1.25 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_ClassLoader_h
#define acdk_lang_ClassLoader_h

#include "Package.h"

#include <acdk/io/Reader.h>
#include <acdk/io/File.h>
#include <acdk/util/Iterator.h>
#include <acdk/lang/ClassNotFoundException.h>
#include <acdk/lang/reflect/Unit.h>

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(ClassLoader);

/** 
  ClassLoader find and loads new classes.
  API: Java Modified
  Note Will be used with SharedLibrary and/or the common type library.
   
  @author Roger Rene Kommer
  @version $Revision: 1.25 $
  @date $Date: 2005/04/09 19:26:48 $
  @see gw_ref[acdk_hb_mi_nameservice]
  @see ResourceBundle
  @see Package
  @see Class
  @bug some function, which enables loaded Java-Classes from remote are not implemented

*/
class ACDK_CORE_PUBLIC ClassLoader 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(ClassLoader)
protected:
  RClassLoader _parent;
  foreign static OUT(RClassLoader) _getSystemClassLoader();
  RClassLoaderArray _registeredClassLoader;
public:
  ClassLoader();
  ClassLoader(IN(RClassLoader) parent);
  /**
   This method is not implemented
   */
  virtual RClass defineClass(IN(RString) name, IN(RbyteArray) b, int off, int len);

  /**
    Try to find a class by name.
    If the class currently is not loaded it uses the acdk::lang::SharedLibrary to
    find a shared library which mapps the full class name or parts of the namespace.
    It uses following pattern:
    findClass("acdk::security::GeneralSecurityException")
    try to load following librares:
    Unix:
      acdk_security_GeneralSecurityException.so
      acdk_security.so
      acdk.so
    Windows (Debug-Release):
      acdk_security_GeneralSecurityException_d.dll
      acdk_security_d.dll
      acdk_d.dll
    @see also ClassLoader::findLibrary
    @param name of the class in one of following forms:
            "acdk::security::GeneralSecurityException"
            "acdk.security.GeneralSecurityException"
            "acdk/security/GeneralSecurityException"
    @param nothrow, if true method does't throw ex
           if not found
    @return the loaded class if found otherwise Nil
  */
  virtual RClass findClass(IN(RString) name, bool nothrow = false) THROWS1(RClassNotFoundException);
  virtual bool loadClassLibrary(IN(RString) classname);
  /**
    Try to find and load a library corresponding the package name.
    this method consul the property "acdk.lang.ClassLoaderAliase" if there are any
    aliase. It checks param libname starts with a key of an alias. if yes it replaces
    the beginning of libname with the value defined in the property.

    @param libname name of the library without extends
           For example "acdk_security" try to load acdk_security_d.dll (Windows Debug)
           or acdk_security.so (Unix)
    @return the name of the loaded library. Otherwise Nil.
  */
  virtual RString findLibrary(IN(RString) libname);
  /**
     @param libname name of the library without extends
           For example "acdk_security" try to load acdk_security_d.dll (Windows Debug)
           or acdk_security.so (Unix)
    @return the name of the loaded library. Otherwise Nil.
  */
  virtual RString findLibrary2(IN(RString) libname);
  
  /**
    find a class, which is already loaded
    @param name of the class
    @return the Class corresponds to the name. Nil if the class is not loaded
  */
  virtual RClass findLoadedClass(IN(RString) name, bool nothrow = true) THROWS1(RClassNotFoundException);

  

  /**
    just return findClass(name)
  */
  virtual RClass findSystemClass(IN(RString) name) THROWS1(RClassNotFoundException);

  /**
    Method is not implemented
  */
  virtual RPackage getPackage(IN(RString) name);

  /**
    Method is not implemented
  */
  virtual RPackageArray getPackages();
  
  /**
    @return the parent Classloader if any otherwise Nil
  */
  virtual RClassLoader getParent() { return _parent; }
  
  /**
    return a ressource located by name otherwise Nil
    @param name identifies a file resource like acdk/locale/LocaleInfo.properties
  */
  virtual acdk::io::RFile findResource(IN(RString) name);
  /**
    return iterator to acdk::io::RFile
  */
  virtual acdk::util::RIterator findResources(IN(RString) name);
  /**
    Different to Java this method returns a File (which may also contain URL)
  */
  virtual acdk::io::RFile getResource(IN(RString) name);
  /**
    is equaly to getResource(name)->getReader()
  */
  virtual acdk::io::RReader getResourceAsStream(IN(RString) name);

  static acdk::io::RFile getSystemResource(IN(RString) name);
  /**
    returns   getSystemClassLoader()->getResourceAsStream()
  */
  static acdk::io::RReader getSystemResourceAsStream(IN(RString) name);
  /**
    return getSystemClassLoader()->getSystemResources(name) 
  */
  static acdk::util::RIterator getSystemResources(IN(RString) name);
  

  /**
    return iterator to acdk::io::RFile
  */
  virtual acdk::util::RIterator getResources(IN(RString) name);
  
  /**
    The normal ClassLoader which loads native ACDK classes and libraries
    @return the SystemClassLoader
  */
  static RClassLoader getSystemClassLoader();
  
  
  
  
  virtual RClass loadClass(IN(RString) name) THROWS1(RClassNotFoundException);
  /**
    @param resolve has no effect
  */
  virtual RClass loadClass(IN(RString) name, bool resolve) THROWS1(RClassNotFoundException);
  /**
    try to load Unit. 
    Tries to load also shared library 
    
  */
  virtual acdk::lang::reflect::RUnit loadUnit(IN(RString) name) THROWS1(RClassNotFoundException);
  /**
    try to load metainfo library of given class
    @return false if no meta info library can be found
  */
  virtual bool loadMetaInfoLibrary(IN(RString) classname);
  /**
    try to load dmi proxy library of given class
    @return false if no meta info library can be found
  */
  virtual bool loadDmiProxyLibrary(IN(RString) classname);
  /**
    Method has no functionality
  */
  virtual void resolveClass(IN(RClass) c);
  /**
    Method is not implemented
  */
  virtual void setSigners(IN(RClass) c, IN(RObjectArray) signers);
  void registerClassLoader(IN(RClassLoader) classLoader)
  {
    _registeredClassLoader->append(classLoader);
  }
protected:
  bool loadExtInfoLibrary(IN(RString) classname, IN(RString) sufix);

};


} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_ClassLoader_h

