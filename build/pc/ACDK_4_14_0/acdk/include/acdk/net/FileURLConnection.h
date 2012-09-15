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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/FileURLConnection.h,v 1.6 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_FileURLConnection_h
#define acdk_net_FileURLConnection_h

#include "URLConnection.h"
#include <acdk/io/File.h>

namespace acdk {
namespace net {


ACDK_DECL_CLASS(FileURLConnection);

class ACDK_NET_PUBLIC FileURLConnection
  : extends URLConnection
{
  ACDK_WITH_METAINFO(FileURLConnection)
private:
  ::acdk::io::RFile file;
public:
  FileURLConnection(IN(RURL) url)
  : URLConnection(url)
  {
    doOutput = false;
  }

  void connect();
  virtual int getContentLength()
  {
    return file->length();
  }

  RReader getInputStream()
  {
    if (!connected)
      connect();
    return file->getReader();
  }

  RWriter getOutputStream()
  {
    if (!connected)
      connect();
    return file->getWriter();
  }

};



} // namespace acdk
} // namespace net

#endif //acdk_net_FileURLConnection_h


