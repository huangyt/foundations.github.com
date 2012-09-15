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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLStreamHandler.h,v 1.9 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_URLStreamHandler_h
#define acdk_net_URLStreamHandler_h

#include "Config.h"
#include <acdk.h>

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::util;

ACDK_DECL_CLASS(URLStreamHandler);

ACDK_DECL_CLASS(URLConnection);
ACDK_DECL_CLASS(URL);

class ACDK_NET_PUBLIC URLStreamHandler
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(URLStreamHandler)
public:
  URLStreamHandler() 
    : Object()
  {
  }
protected:
  
  virtual RURLConnection openConnection(IN(RURL) url) = 0;
  
  /**
    Parses url_string and set values into url
  */
  virtual void parseURL(IN(RURL) url, IN(RString) url_string);

  virtual RString toExternalForm(IN(RURL) url);
  
  virtual RURLStreamHandler createInstance() = 0;
  
  virtual void setURL(IN(RURL) url, IN(RString) protocol, IN(RString) host, int port, IN(RString) user, 
                      IN(RString) password, IN(RString) file, IN(RString) anchor);

  friend class URL;
};

} // namespace acdk
} // namespace net

#endif //acdk_net_URLStreamHandler_h


