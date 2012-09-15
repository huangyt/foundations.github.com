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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/ResourceBundle.cpp,v 1.23 2005/04/19 10:28:42 kommer Exp $


#include <acdk.h>
#include <acdk/lang/ClassNotFoundException.h>
#include "ResourceBundle.h"
#include "HashMap.h"

#include <acdk/lang/System.h>
#include <acdk/lang/ClassLoader.h>
#include <acdk/io/File.h>

#include "MissingResourceException.h"
#include "Locale.h"
#include "PropertyResourceBundle.h"

namespace acdk {
namespace util {

using namespace ::acdk::lang;
//using namespace ::acdk::locale;


RStringArray 
ResourceBundle::getStringArray(IN(RString) key) THROWS1(RMissingResourceException) 
{
  return RStringArray(getObject(key));
}

RMap 
ResourceBundle::getMap(IN(RString) key) THROWS1(RMissingResourceException)
{
  return (RMap)getObject(key);
}

//static 
OUT(RMap)
ResourceBundle::_resourceBundleCache()
{
  static RMap __resourceBundleCache;
  if (__resourceBundleCache != Nil)
    return __resourceBundleCache;
  __resourceBundleCache = new HashMap;
  System::registerStaticReference(__resourceBundleCache);
  return __resourceBundleCache;
}

//static 
void 
ResourceBundle::flushResourceBundleCache()
{
  _resourceBundleCache() = Nil;
}

bool 
ResourceBundle::hasValue(IN(RString) key)
{
  for (RResourceBundle bundle = this;  bundle != Nil; bundle = bundle->_parent) 
  {
    if (bundle->hasValue(key) == true)
      return true;
  }
  return false;
}

RObject 
ResourceBundle::getObject(IN(RString) key) THROWS1(RMissingResourceException)
{
  for (RResourceBundle bundle = this;  bundle != Nil; bundle = bundle->_parent) 
  {
    RObject o = bundle->handleGetObject(key);
    if (o != Nil)
      return o;
  }
  THROW1(MissingResourceException, "Key not found: " +  getClass()->getName() + key);
  return Nil;
}
  


RClass 
ResourceBundle::_lookupRessourceClass(IN(RString) componentname, IN(RClassLoader) classLoader)
{
  return classLoader->findClass(componentname, true);
}

RPropertyResourceBundle 
ResourceBundle::_tryLoadPropertiesRessource(IN(RString) componentname, IN(RClassLoader) classLoader, OUT(bool) cached)
{
  RMap m = ResourceBundle::_resourceBundleCache();
  RObject o = m->get(&componentname);
  if (o != Nil)
  {
    cached = true;
    return (RPropertyResourceBundle)o;
  }
  RString fname = componentname->replace('.', '/') + ".properties";
  acdk::io::RFile f = classLoader->getResource(fname);
  
  if (f == Nil)
    return Nil;
  RPropertyResourceBundle resb = new PropertyResourceBundle(f->getReader());
  m->put(&componentname, &resb);
  return resb;
}

RClass 
ResourceBundle::_lockupClassRessource(IN(RString) baseName,  IN(RLocale) locale, IN(RClassLoader) classLoader)
{
  if (locale == Nil) 
    return _lookupRessourceClass(baseName, classLoader);
  
  RString l = locale->getLanguage();
  RString c = locale->getCountry();
  RString v = locale->getVariant();

  RClass cls = _lookupRessourceClass(baseName 
                + (l->length() > 0 ? "_" : "") + l 
                + (c->length() > 0 ?  "_"  : "") + c 
                + (v->length() > 0 ? "_"  : "") + v, classLoader);
  RObject ocls = &cls;
  RObject rocs = &cls;
  if (v->length() > 0) 
  {
    cls = _lookupRessourceClass(baseName + "_" + l + "_" + c, classLoader);
    if (cls != Nil)
      return cls;
  }
  if (c->length() > 0) {
    cls = _lookupRessourceClass(baseName + "_" + l, classLoader);
    if (cls != Nil)
      return cls;
  }
  
  cls = _lookupRessourceClass(baseName + "_en_US", classLoader);
  if (cls != Nil)
    return cls;
  cls = _lookupRessourceClass(baseName + "_en", classLoader);
  if (cls != Nil)
    return cls;
  return _lookupRessourceClass(baseName, classLoader);
}

RPropertyResourceBundle 
ResourceBundle::_lookupPropResBundle(IN(RString) baseName, IN(RClassLoader) classLoader, INOUT(RString) language, INOUT(RString) country, 
                            INOUT(RString) variant, OUT(bool) cached)
{
  StringBuffer sb(baseName);
  bool wasSet = false;
  if (language != Nil)
  {
    sb << "_" << language;
    if (country == Nil)
      language = Nil;
    wasSet = true;
  }
  if (country != Nil)
  {
    sb << "_" << country;
    if (variant == Nil)
      country = Nil;
    wasSet = true;
  }
  if (variant != Nil)
  {
    sb << "_" << variant;
    variant = Nil;
    wasSet = true;
  }
  RPropertyResourceBundle resprops = _tryLoadPropertiesRessource(sb.toString(), classLoader, cached);
  if (resprops != Nil)
    return resprops;
  
  if (wasSet == true)
  {
    return _lookupPropResBundle(baseName, classLoader, language, country, variant, cached);
  }
  return Nil;
}



RResourceBundle
ResourceBundle::_lockupPropertiesRessource(IN(RString) baseName,  IN(RLocale) locale, IN(RClassLoader) classLoader)
{
  RString lang = locale->getLanguage();
  RString country = locale->getCountry();
  RString variant = locale->getVariant();
  if (variant != Nil && variant->length() == 0)
    variant = Nil;
  if (country != Nil && country->length() == 0)
    country = Nil;
  if (lang != Nil && lang->length() == 0)
    lang = Nil;
  RPropertyResourceBundle rootBundle = Nil;
  RPropertyResourceBundle lastBundle = Nil;
  bool langWasSet = true;
  do {
    bool cached = false;
    if (lang != Nil)
      langWasSet = true;
    else
      langWasSet = false;

    RPropertyResourceBundle props = _lookupPropResBundle(baseName, classLoader, lang, country, variant, cached);
    if (props == Nil)
      break;
    if (rootBundle == Nil)
      rootBundle = props;
    else
    {
      lastBundle->setParent(&props);
    }
    if (cached == true)
      break;
    if (langWasSet == false)
      break;
    lastBundle = props;
  } while (true);

  if (rootBundle != Nil)
    return &rootBundle;

  if (locale->equals(Locale::getUS()) == false)
    return _lockupPropertiesRessource(baseName, Locale::getUS(), classLoader);
  return Nil;
}

//static 
RResourceBundle 
ResourceBundle::getBundle(IN(RString) baseName,  IN(RLocale) lc, IN(RClassLoader) cl)  THROWS1(RMissingResourceException)
{
  RLocale locale = lc;
  RClassLoader classLoader = cl;
  if (locale == Nil)
    locale = Locale::getDefault();
  if (classLoader == Nil)
    classLoader = ClassLoader::getSystemClassLoader();

  RClass cls = _lockupClassRessource(baseName, locale, classLoader);
  if (cls != Nil)
  {
    RClass resbudleclass = ResourceBundle::GetClass();
    if (cls != Nil) {
      if (resbudleclass->isAssignableFrom(cls)== false) 
        THROW1(MissingResourceException, "Class  [" + cls->toString() + "] is not compatible to ResourceBundle");
      return (RResourceBundle)cls->newInstance();
    }
  }
  
  RResourceBundle resb = _lockupPropertiesRessource(baseName, locale, classLoader);
  if (resb != Nil)
    return resb;
  THROW1(MissingResourceException, "Class [" + baseName + "] not found");
  return Nil;
}



} // namespace util 
} //namespace acdk 




