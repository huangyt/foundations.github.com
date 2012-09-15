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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLStreamHandlerFactory.h,v 1.11 2005/03/30 17:30:15 kommer Exp $

#ifndef acdk_net_URLStreamHandlerFactory_h
#define acdk_net_URLStreamHandlerFactory_h


#include "URLStreamHandler.h"
#include <acdk/util/HashMap.h>

namespace acdk {
namespace net {



ACDK_DECL_INTERFACE(URLStreamHandlerFactory);

/**
  registry for  URLStreamHandler
*/
class ACDK_NET_PUBLIC URLStreamHandlerFactory
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(URLStreamHandlerFactory)
public:
/**
  * This method maps the protocol portion of a URL to a URLStreamHandler
  * object.
  *
  * @param protocol The protocol name to map (", etc).
  *
  * @return The URLStreamHandler for the specified protocol
  */

  virtual RURLStreamHandler createURLStreamHandler(IN(RString) protocol) = 0;
  /**
    register specific factory for a protocol
  */
  static void registerFactory(IN(RString) protocol, IN(RURLStreamHandlerFactory) factory);
  /**
    @return factory for a given protocol
    @throw MalformedURLException if no factory exists for this protocol
  */
  static RURLStreamHandlerFactory getFactory(IN(RString) protocol);
  static RURLStreamHandler getProtocolHandler(IN(RString) protocol);
protected:

private:
  friend class URLFactoryInitializer;
  foreign static OUT(acdk::util::RHashMap) getFactories();
  foreign static RURLStreamHandlerFactory _getProtocolFromProperties(IN(RString) protocol);
};

} // namespace acdk
} // namespace net

#endif //acdk_net_URLStreamHandlerFactory_h

