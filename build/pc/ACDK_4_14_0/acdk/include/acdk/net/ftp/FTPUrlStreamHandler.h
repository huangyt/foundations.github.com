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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPUrlStreamHandler.h,v 1.5 2005/03/30 17:30:16 kommer Exp $

#ifndef acdk_net_ftp_FTPUrlStreamHandler_h
#define acdk_net_ftp_FTPUrlStreamHandler_h

#include "Config.h"
#include "FTPClient.h"
#include <acdk/io/FileSystem.h>
#include <acdk/net/URL.h>
#include <acdk/net/URLStreamHandler.h>
#include <acdk/net/URLStreamHandlerFactory.h>

namespace acdk {
namespace net {
namespace ftp {




ACDK_DECL_CLASS(FTPURLStreamHandler);




class ACDK_NET_FTP_PUBLIC FTPURLStreamHandler
: extends acdk::net::URLStreamHandler
{
  ACDK_WITH_METAINFO(FTPURLStreamHandler)
public:
  FTPURLStreamHandler() {}

  void parseURL(IN(RURL) url, IN(RString) url_string);
  virtual acdk::net::RURLConnection openConnection(IN(acdk::net::RURL) url);
  virtual acdk::net::RURLStreamHandler createInstance() { return new FTPURLStreamHandler(); }
};


ACDK_DECL_CLASS(FTPUrlStreamHandlerFactory);

class ACDK_NET_FTP_PUBLIC FTPUrlStreamHandlerFactory
: extends acdk::lang::Object
, implements acdk::net::URLStreamHandlerFactory
{
public:
  virtual RURLStreamHandler createURLStreamHandler(IN(RString) protocol) 
  {
    if (protocol->compareTo("ftp") == 0)
      return new FTPURLStreamHandler();
    return Nil;
  }
};

} // namespace ftp
} // namespace acdk
} // namespace net

#endif //acdk_net_ftp_FTPUrlStreamHandler_h

