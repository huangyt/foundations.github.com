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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPFileSystem.cpp,v 1.7 2005/03/17 12:36:06 kommer Exp $


#include "FTPFileSystem.h"
#include "FTPFileImpl.h"


namespace acdk {
namespace net {
namespace ftp {

using namespace acdk::net;

FTPFileSystem::FTPFileSystem(IN(RString) url)
: _cache(new acdk::io::FileInfoArray(0))
{
  ACDK_SAFE_CONSTRUCTOR();
  _rootUrl = new URL(url);
  _rootUrl->setFile("");
  _connect();
  FileSystem::registerFileSystem(this);
}

FTPFileSystem::FTPFileSystem(IN(RURL) url)
: _cache(new acdk::io::FileInfoArray(0))
{
  ACDK_SAFE_CONSTRUCTOR();
  _rootUrl = url;
  _connect();
  FileSystem::registerFileSystem(this);
}

FTPFileSystem::~FTPFileSystem()
{
}

RFTPClient 
FTPFileSystem::getClient() 
{ 
  if (_client == Nil)
    _connect();
  return _client; 
}



bool 
FTPFileSystem::ownsFile(IN(RString) fname)
{
  if (fname->startsWith("ftp://") == false)
    return false;
  // convert to url to retrive the default port
  URL url(fname);
  RString ustr = url.toString();
  RString str = toString();
  
  if (ustr->startsWith(str) == true)
    return true;
  return false;
}

RString 
FTPFileSystem::getRootName()
{
  return _rootUrl->toExternalForm();
}

RFileArray 
FTPFileSystem::listFiles(IN(RString) directory, int listflags)
{
  RString ndir = directory;
  if (ndir->startsWith("/") == false)
    ndir = "/" + ndir;
  if (_client->setCwd(ndir) == false)
    return new FileArray(0);
  //### @todo support listflags
  RFileInfoArray arr = _client->listFileInfos();
  RFileArray fa = new FileArray(arr->length());
  for (int i = 0; i < arr->length(); ++i)
  {
    fa[i] = new File(new FTPFileImpl(this, arr[i]));
  }
  return fa;
}

RFile 
FTPFileSystem::file(IN(RString) path)
{
  return Nil;
}
  
RFileImpl 
FTPFileSystem::getFileImpl(IN(RString) fqpath)
{
  RString relname = fqpath;
  if (fqpath->startsWith("ftp://") == true)
    relname = fqpath->substr(6);
  int idx = relname->indexOf('/');
  if (idx == -1) // is root
  {
    return new FTPFileImpl(this, new FileInfo(FileInfoExists | FileInfoIsDir | FileInfoCanRead | FileInfoCanWrite, "", ""));
  }
  relname = relname->substr(idx + 1);
  return new FTPFileImpl(this, _getFileInfo(relname));
}

void
FTPFileSystem::_connect()
{
  _client = new FTPClient();
  int port = _rootUrl->getPort();
  if (port == -1)
    port = 21;
  _client->connect(_rootUrl->getHost(), port);
  RString user = _rootUrl->getUser();
  RString pass = _rootUrl->getPassword();
  if (user == Nil)
  {
    user = "ftp";
    pass = "anon@anon";
  }
  _client->login(user, pass);
  /*
  RString f = _rootUrl->getFile();
  if (f != Nil && f->length() > 0)
    _client->setCwd(f);
  */
}

acdk::io::RFileInfo 
FTPFileSystem::_findFileInfoFromCache(IN(RString) file)
{
  for (int i = 0; i < _cache->length(); ++i)
    if (_cache[i]->getPath()->equals(file) == true)
      return _cache[i];
  return Nil;
}

void 
FTPFileSystem::removeFromCache(IN(RFileInfo) fi)
{
  for (int i = 0; i < _cache->length(); ++i)
  {
    if (_cache[i]->equals(fi) == true)
    {
      _cache->remove(i);
      return;
    }
  }
}

acdk::io::RFileInfo 
FTPFileSystem::_getFileInfo(IN(RString) file)
{
  RFileInfo fi = _findFileInfoFromCache(file);
  if (fi != Nil)
    return fi;
  int idx = file->lastIndexOf('/');
  RString ncwd = "/";
  if (idx != -1)
    ncwd = file->substr(0, idx);
  if (_client->setCwd(ncwd) == false)
    return Nil;
  RFileInfoArray arr = _client->listFileInfos();
  RString nfile = file;
  if (file->startsWith("/") == false)
    nfile = "/" + file;
  // ### todo add to cache
  for (int i = 0; i < arr->length(); ++i)
  {
    RFileInfo fi = arr[i];
    RString fipath = fi->getPath();
    if (File::separatorChar() != '/') 
      fipath = fipath->replace(File::separatorChar(), '/');
    if (fipath->startsWith("/") == false)
      fipath = "/" + fipath;

    if (fipath->equals(nfile) == true)
      return arr[i];
  }
  fi = new FileInfo();
  if (idx != -1)
  {
    fi->name = file->substr(idx + 1);
    fi->dir = ncwd;
  }
  else
    fi->name = file;
  return fi;
}


RFileSystem 
FTPFileSystemFactory::create(IN(RString) file)
{
  return new FTPFileSystem(file);
}

ACDK_REGISTER_FILESYSTEM(FTPFileSystemFactory);


} // namespace ftp
} // namespace acdk
} // namespace net



