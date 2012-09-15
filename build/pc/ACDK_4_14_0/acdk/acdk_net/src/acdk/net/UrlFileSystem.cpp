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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/UrlFileSystem.cpp,v 1.2 2005/03/17 13:11:40 kommer Exp $

#include "UrlFileSystem.h"


namespace acdk {
namespace net {

using namespace acdk::io;

RString 
UrlFileImpl::getName()
{
  int idx = _url->getFile()->lastIndexOf("/");
  if (idx == -1)
    return _url->getFile();
  return _url->getFile()->substr(idx + 1);
}

RString 
UrlFileImpl::getPath()
{
  return _url->toString();
}

acdk::io::RFile 
UrlFileImpl::getParentFile()
{
  RString urlstr = _url->toString();

  int idx = urlstr->lastIndexOf("/");
  if (idx == -1)
    return Nil;
  RString np = urlstr->substr(0, idx);
  return new File(new UrlFileImpl(new URL(np)));
}

acdk::io::RFile 
UrlFileImpl::makeChild(IN(RString) subfile)
{
  RString p = getPath();
  if (p->endsWith("/") == false)
    p = p + "/";
  return new File(new UrlFileImpl(new URL(p)));
}

acdk::io::RReader 
UrlFileImpl::getReader()
{
  return connect()->getInputStream();
}

acdk::io::RWriter 
UrlFileImpl::getWriter()
{
  return connect()->getOutputStream();
}
  
acdk::io::RFileSystem 
UrlFileImpl::getFileSystem()
{
  return new UrlFileSystem(new URL(_url->getProtocol(), _url->getHost(), _url->getPort(), ""));
}


RFile 
UrlFileSystem::file(IN(RString) path)
{
  return new File(getFileImpl(_rootUrl->toString() + "/" + path));
}

RFileImpl 
UrlFileSystem::getFileImpl(IN(RString) fqpath)
{
  return new UrlFileImpl(new URL(fqpath));
}

RFileSystem 
UrlFileSystemFactory::create(IN(RString) file)
{
  if (file->startsWith("ftp://") == true)
  {
    RClass cls = Class::findClass("acdk/net/ftp/FTPFileSystemFactory");
    if (cls != Nil)
    {
      RFileSystemFactory ftpFactory = (RFileSystemFactory)cls->newInstance();
      return ftpFactory->create(file);
    }
  }
  return new UrlFileSystem(file);
}

ACDK_REGISTER_FILESYSTEM(UrlFileSystemFactory);

} // namespace acdk
} // namespace net




