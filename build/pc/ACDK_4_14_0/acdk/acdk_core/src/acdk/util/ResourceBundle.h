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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/ResourceBundle.h,v 1.19 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_ResourceBundle_h
#define acdk_util_ResourceBundle_h

#include <acdk.h>
#include <acdk/lang/ClassLoader.h>

#include "Map.h"

#include "Locale.h"
#include "MissingResourceException.h"

namespace acdk {
namespace util {


ACDK_DECL_CLASS(PropertyResourceBundle);

ACDK_DECL_CLASS(ResourceBundle);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.19 $
  @date $Date: 2005/04/09 19:26:57 $
  The ResourceBundle tries first to load a class on given name.
  If no class was not found it try to load a property file.
  The BundleName com.artefaktur.MyStrings will try to load
  a property file $ACDKHOME/cfg/com/artefaktur/MyString[locale].properties.
  The resolution of [locale] are resolved equal to Java spec 
  
*/
class ACDK_CORE_PUBLIC  ResourceBundle 
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(ResourceBundle)  
private:
  static OUT(RMap) _resourceBundleCache();

protected:
  RResourceBundle _parent;
  RLocale _locale;
public:
  ResourceBundle()
  : Object(),
    _parent(Nil),
    _locale(acdk::util::Locale::getDefault())
  {
  }
  virtual bool hasValue(IN(RString) key);
  virtual RString getString(IN(RString) key) THROWS1(RMissingResourceException)
  {
    return (RString)getObject(key);
  }
  
  virtual RStringArray getStringArray(IN(RString) key) THROWS1(RMissingResourceException);
  /**
    ACDK extension
    return a String to String map of values
  */
  virtual RMap getMap(IN(RString) key) THROWS1(RMissingResourceException);

  virtual RObject getObject(IN(RString) key) THROWS1(RMissingResourceException);
  
  /**
    load a resourcebundle
    @param baseName name without locale
    @param locale the locale to use. if this parameter is Nil Locale::getDefault() will be used
    @param classLoader the classLoader to use loading Class or Resource. If this is Nil use ClassLoader::getSystemClassLoader()
  */
  static RResourceBundle getBundle(IN(RString) baseName, IN(RLocale) locale = Nil, 
                                   IN(RClassLoader) classLoader = Nil)  THROWS1(RMissingResourceException);
  
 
  RLocale getLocale() { return _locale; }
  virtual RIterator getKeys() = 0;
  /**
    resets the cache of loaded ResourceBundles
  */
  static void flushResourceBundleCache();
protected:
  /**
    Nil values are by default not a valid ressource bundle value
    Different to the Java spec this method should not throw an exception
    but simply return Nil;
  */
  virtual RObject handleGetObject(IN(RString) key) = 0;
  void setParent(IN(RResourceBundle) parent)
  {
    _parent = parent;
  }
private:
  foreign static RResourceBundle _lockupPropertiesRessource(IN(RString) baseName,  IN(RLocale) locale, IN(RClassLoader) classLoader);
  foreign static RPropertyResourceBundle  _tryLoadPropertiesRessource(IN(RString) componentname, IN(RClassLoader) classLoader, OUT(bool) cached);
  foreign static RPropertyResourceBundle  _lookupPropResBundle(IN(RString) baseName, IN(RClassLoader) classLoader, INOUT(RString) language, INOUT(RString) country, 
                            INOUT(RString) variant, OUT(bool) cached);
  foreign static RClass _lookupRessourceClass(IN(RString) componentname, IN(RClassLoader) classLoader);
  foreign static RClass _lockupClassRessource(IN(RString) baseName,  IN(RLocale) locale, IN(RClassLoader) classLoader);

};
  



} // namespace util 
} //namespace acdk 



#endif //acdk_util_ResourceBundle_h

