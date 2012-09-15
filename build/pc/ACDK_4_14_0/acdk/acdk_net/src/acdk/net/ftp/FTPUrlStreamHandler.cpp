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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPUrlStreamHandler.cpp,v 1.4 2005/03/30 17:30:16 kommer Exp $


#include "FTPUrlStreamHandler.h"
#include "FTPURLConnection.h"
#include <acdk/net/URLStreamHandlerFactory.h>

namespace acdk {
namespace net {
namespace ftp {

namespace {

struct RegisterStreamHandlerFactory
{
  RegisterStreamHandlerFactory(IN(RString) name, IN(RURLStreamHandlerFactory) factory)
  {
    URLStreamHandlerFactory::registerFactory(name, factory);
  }
};

RegisterStreamHandlerFactory _ftpRegisterFactory("ftp", new FTPUrlStreamHandlerFactory());

} // anon namespace


RURLConnection 
FTPURLStreamHandler::openConnection(IN(RURL) url)
{
  return new FTPURLConnection(url);
}

void 
FTPURLStreamHandler::parseURL(IN(RURL) url, IN(RString) url_string)
{
  URLStreamHandler::parseURL(url, url_string);
  if (url->getPort() == -1)
    url->setPort(21);
}

} // namespace ftp
} // namespace acdk
} // namespace net



