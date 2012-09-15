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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdkx/net/ssl/HTTPSUrlStreamHandler.h,v 1.1 2005/03/30 17:37:27 kommer Exp $

#ifndef acdkx_net_ssl_HTTPSUrlStreamHandler_h
#define acdkx_net_ssl_HTTPSUrlStreamHandler_h

#include "Config.h"
#include <acdk/io/FileSystem.h>
#include <acdk/net/URL.h>
#include <acdk/net/URLStreamHandler.h>
#include <acdk/net/URLStreamHandlerFactory.h>
#include <acdk/net/HttpURLConnectionImpl.h>
#include "SSLSocket.h"

namespace acdkx {
namespace net {
namespace ssl {


ACDK_DECL_CLASS(HTTPSUrlConnection);

/**
  Implements a URLConnection for HTTPS

*/
class ACDKX_NET_SSL_PUBLIC HTTPSUrlConnection
: extends acdk::net::HttpURLConnectionImpl
{
  ACDK_WITH_METAINFO(HTTPSUrlConnection)
public:
  HTTPSUrlConnection(IN(acdk::net::RURL) url)
    : HttpURLConnectionImpl(url)
  {
  }
  virtual int getStandardPort() { return 443; }
  virtual acdk::net::RSocket createSocket(IN(acdk::lang::RString) host, int port)
  {
    return new SSLSocket(host, port);
  }
};


ACDK_DECL_CLASS(HTTPSUrlStreamHandler);

/**
  internal implementation to handle HTTP Streams
*/
class ACDKX_NET_SSL_PUBLIC HTTPSUrlStreamHandler
: extends acdk::net::URLStreamHandler
{
  ACDK_WITH_METAINFO(HTTPSUrlStreamHandler)
public:
  HTTPSUrlStreamHandler() {}

  virtual acdk::net::RURLConnection openConnection(IN(acdk::net::RURL) url)
  {
    return new HTTPSUrlConnection(url);
  }
  virtual acdk::net::RURLStreamHandler createInstance() { return new HTTPSUrlStreamHandler(); }
};


ACDK_DECL_CLASS(HTTPSUrlStreamHandlerFactory);

/**
  Factory class for creating HTTPSUrlStreamHandler for "https" protocol
*/
class ACDKX_NET_SSL_PUBLIC HTTPSUrlStreamHandlerFactory
: extends acdk::lang::Object
, implements acdk::net::URLStreamHandlerFactory
{
  ACDK_WITH_METAINFO(HTTPSUrlStreamHandlerFactory)
public:
  HTTPSUrlStreamHandlerFactory() {}
  static RObject create_instance() { return new HTTPSUrlStreamHandlerFactory(); }
  virtual acdk::net::RURLStreamHandler createURLStreamHandler(IN(RString) protocol) 
  {
    if (protocol->compareTo("https") == 0)
      return new HTTPSUrlStreamHandler();
    return Nil;
  }
};

} // namespace ssl
} // namespace acdkx
} // namespace net

#endif //acdkx_net_ssl_HTTPSUrlStreamHandler_h

