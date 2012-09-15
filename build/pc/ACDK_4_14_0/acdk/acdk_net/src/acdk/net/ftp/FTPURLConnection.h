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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPURLConnection.h,v 1.4 2005/02/20 13:55:41 kommer Exp $

#ifndef acdk_net_ftp_FTPURLConnection_h
#define acdk_net_ftp_FTPURLConnection_h

#include "Config.h"
#include "FTPClient.h"
#include <acdk/io/FileSystem.h>
#include <acdk/net/URL.h>
#include <acdk/net/URLConnection.h>

namespace acdk {
namespace net {
namespace ftp {

ACDK_DECL_CLASS(FTPURLConnection);


class ACDK_NET_FTP_PUBLIC FTPURLConnection
: extends acdk::net::URLConnection
{
  DECL_ACDK_DEFAULT_METACLASS(URLConnection)
public:
  FTPURLConnection(IN(RURL) url) 
  : URLConnection(url) 
  {
  }
  virtual void connect();

  virtual acdk::io::RReader getInputStream();

  virtual acdk::io::RWriter getOutputStream();
};

} // namespace ftp
} // namespace acdk
} // namespace net

#endif //acdk_net_ftp_FTPURLConnection_h

