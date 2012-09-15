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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/UrlFileSystem.h,v 1.2 2005/04/30 14:06:55 kommer Exp $
#ifndef acdk_net_UrlFileSystem_h
#define acdk_net_UrlFileSystem_h

#include "HttpURLConnection.h"
#include "HeaderFieldHelper.h"
#include "Socket.h"
#include "ProtocolException.h"
#include <acdk/io/LineNumberCharReader.h>

namespace acdk {
namespace net {


ACDK_DECL_CLASS(UrlFileImpl);

/**
  Adapter for the FileImpl interface
*/
final
class ACDK_NET_PUBLIC UrlFileImpl
: extends ::acdk::lang::Object
, implements ::acdk::io::FileImpl
{
  ACDK_WITH_METAINFO(UrlFileImpl)
private:
  RURL _url;
  RURLConnection _connection;
public:  
  UrlFileImpl(IN(RURL) url, IN(RURLConnection) connection = Nil)
  : _url(url)
  , _connection(connection)
  {
  }
  foreign RString getCanonicalPath()
  {
    return _url->toString();
  }
  foreign RString getAbsolutePath()
  {
    return _url->toString();
  }
  foreign RString getName();
  foreign RString getPath();
  foreign acdk::io::RFile getParentFile();
  foreign acdk::io::RFile makeChild(IN(RString) subfile); 
  foreign bool isAbsolute() { return true; }
  foreign bool exists() { return true; }
  foreign bool canRead() { return true; }
  foreign bool canWrite() { return true; }
  foreign bool isDirectory() { return false; }
  foreign bool isFile() { return true; }
  foreign bool isHidden() { return false; }
  foreign jlong length() { return -1; }
  
  foreign bool createNewFile() { return false; }
  foreign bool deleteFile() { return false; }
  foreign jlong lastModified() { return -1; }
  foreign jlong fileCreated() { return -1; }
  foreign bool mkdir(int mode = 0777) { return false; }
  foreign bool renameTo(IN(acdk::io::RFile) dest) { return false; }
  foreign bool setLastModified(jlong time) { return false; }

  foreign acdk::io::RReader getReader();
  foreign acdk::io::RWriter getWriter();
  foreign acdk::io::RFileSystem getFileSystem();

  RURLConnection connect()
  {
    if (_connection == Nil)
      _connection = _url->openConnection();
    return _connection;
  }
};

ACDK_DECL_CLASS(UrlFileSystem);
/**
  
*/
class ACDK_NET_PUBLIC UrlFileSystem
: extends acdk::lang::Object
, implements acdk::io::FileSystem
{
  ACDK_WITH_METAINFO(UrlFileSystem)
  RURL _rootUrl;
public:
  UrlFileSystem(IN(RString) url)
    : _rootUrl(new URL(url))
  {
  }
  UrlFileSystem(IN(RURL) url)
    : _rootUrl(url)
  {
  }
  
  virtual bool ownsFile(IN(RString) fname)
  {
    return fname->startsWith(_rootUrl->toString());
  }

  virtual RString getRootName()
  {
    return _rootUrl->toString();
  }
  /**
    @param directory the absolute name with out the FSname
    @param listflags a combination of ListFlags
    @return list of files
  */
  virtual acdk::io::RFileArray listFiles(IN(RString) directory, int listflags)
  {
    return new acdk::io::FileArray(0);
  }
  /**
    creates an instance of given file. 
    @param path is without the FS name
    @return returns a new File.
  */
  virtual RFile file(IN(RString) path);
  
  /**
    returns a file implementation for this
    full qualified file implementation
  */
  virtual RFileImpl getFileImpl(IN(RString) fqpath);
};


ACDK_DECL_CLASS(UrlFileSystemFactory);

class ACDK_NET_PUBLIC UrlFileSystemFactory
: extends ::acdk::lang::Object
, implements acdk::io::FileSystemFactory
{
  ACDK_WITH_METAINFO(UrlFileSystemFactory)

public:
  UrlFileSystemFactory() {}
  virtual bool isRootFileSystem() { return true; }
  virtual int handleFile(IN(RString) file)
  {
    if (file->startsWith("http://") == true)
      return 2; // low number, specialized Url handler (like FTP) should return higher numbers
    return -1;
  }
  virtual RFileSystem create(IN(RString) file);
};



} // namespace acdk
} // namespace net

#endif //acdk_net_UrlFileSystem_h


