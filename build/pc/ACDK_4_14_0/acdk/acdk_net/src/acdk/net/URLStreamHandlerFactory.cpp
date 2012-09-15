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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLStreamHandlerFactory.cpp,v 1.6 2005/04/08 10:53:20 kommer Exp $

#include "URLStreamHandlerFactory.h"
#include "MalformedURLException.h"

#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace net {

;
/** @internal */
class URLFactoryInitializer 
: extends ::acdk::lang::Object
, implements Initializer 
{
  void beforeMain() 
  {
    
  }
  void afterMain() 
  {
    URLStreamHandlerFactory::getFactories() = Nil;
  }
};

/** @internal */
SystemInitializer<URLFactoryInitializer> __u;

  //static
OUT(RHashMap)
URLStreamHandlerFactory::getFactories()
{
  static RHashMap _urlFactories = new HashMap();
  return _urlFactories;
}

//static 
RURLStreamHandlerFactory 
URLStreamHandlerFactory::_getProtocolFromProperties(IN(RString) protocol)
{
  static bool initialized = false;
  if (System::configurationLoaded() == false)
    return Nil;
  static acdk::util::RMap urlMap;
  if (initialized == false)
  {
    initialized = true;
    acdk::util::RProperties props = System::getProperties();
    urlMap = props->getMapProperty("acdk.net.urlfactories");
  }
  RString cls = (RString)urlMap->get(&protocol);
  if (cls == Nil)
    return Nil;
  try {
    RClass factoryClass = Class::forName(cls);
    RURLStreamHandlerFactory factory = (RURLStreamHandlerFactory)factoryClass->newInstance();
    return factory;
  } catch (RThrowable ex) {
    ACDK_NLOG("acdk.net.URLStreamHandlerFactory", Error, "cannot not load URLStreamHandlerFactory class: " + cls);
    urlMap->put(&protocol, Nil);
  }
  return Nil;
}

//static 
RURLStreamHandlerFactory 
URLStreamHandlerFactory::getFactory(IN(RString) protocol)
{
  RURLStreamHandlerFactory ret = (RURLStreamHandlerFactory) getFactories()->get(&protocol);
  if (ret != Nil)
    return ret;
  ret = _getProtocolFromProperties(protocol);
  if (ret != Nil)
    return ret;
  THROW1(MalformedURLException, "Unknown protocol: " + protocol);
  return Nil;
}

//static 
RURLStreamHandler 
URLStreamHandlerFactory::getProtocolHandler(IN(RString) protocol)
{
  return getFactory(protocol)->createURLStreamHandler(protocol);
}

// static
void 
URLStreamHandlerFactory::registerFactory(IN(RString) protocol, IN(RURLStreamHandlerFactory) factory)
{
  getFactories()->put(&protocol, (RObject)factory);
}

} // namespace acdk
} // namespace net



