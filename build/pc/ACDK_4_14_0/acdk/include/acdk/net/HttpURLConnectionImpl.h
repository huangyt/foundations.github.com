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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/HttpURLConnectionImpl.h,v 1.9 2005/03/30 17:30:15 kommer Exp $
#ifndef acdk_net_HttpURLConnectionImpl_h
#define acdk_net_HttpURLConnectionImpl_h

#include "HttpURLConnection.h"
#include "HeaderFieldHelper.h"
#include "Socket.h"
#include "ProtocolException.h"
#include <acdk/io/LineNumberCharReader.h>

namespace acdk {
namespace net {

ACDK_DECL_CLASS(HttpURLConnectionImpl);
/**
  Implementation of HttpURLConnection
  Currently only HTTP 1.0 is supported
  It doesn't evaluate the HTTP repsonse header and does no 
  decoding of content.
*/
class ACDK_NET_PUBLIC HttpURLConnectionImpl
: extends HttpURLConnection
{
  ACDK_WITH_METAINFO(HttpURLConnectionImpl)
private:
  RString proxy_name;
  int proxy_port;
  bool with_proxy;
  RSocket socket;
  //acdk::io::RLineNumberCharReader in_stream;
  acdk::io::RReader in_stream;
  acdk::io::RWriter out_stream;
  // not needed acdk::io::RPrintWriter out_writer;
  RHeaderFieldHelper headers;
public:

  HttpURLConnectionImpl(IN(RURL) url);

  void connect();
  
  RReader getInputStream()
  {
    if (!connected)
      connect();
    return in_stream;
  }

  void disconnect() 
  {
    socket->close();
  }

  void setRequestMethod(IN(RString) method);

  bool usingProxy()
  {
    return(with_proxy);
  }
  int getHeaderFieldCount() { return headers->getNumberOfEntries(); }
  RString getHeaderFieldKey(int n)
  {
    return headers->getHeaderFieldKeyByIndex(n);
  }

  RString getHeaderField(int n)
  {
    return headers->getHeaderFieldValueByIndex(n);
  }
  virtual int getStandardPort() { return 80; }
  virtual RSocket createSocket(IN(RString) host, int port);
};

} // namespace acdk
} // namespace net

#endif //acdk_net_HttpURLConnectionImpl_h


