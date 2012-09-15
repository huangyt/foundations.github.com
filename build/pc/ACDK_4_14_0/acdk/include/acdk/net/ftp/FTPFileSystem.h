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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPFileSystem.h,v 1.5 2005/03/17 12:36:06 kommer Exp $

#ifndef acdk_net_ftp_FTPFileSystem_h
#define acdk_net_ftp_FTPFileSystem_h

#include "Config.h"
#include "FTPClient.h"
#include <acdk/io/FileSystem.h>
#include <acdk/net/URL.h>
#include <acdk/util/HashMap.h>

namespace acdk {
namespace net {
namespace ftp {

ACDK_DECL_CLASS(FTPFileSystem);

/**
  Represends a File system mounted via class FTPClient

*/
class ACDK_NET_FTP_PUBLIC FTPFileSystem
: extends acdk::lang::Object
, implements acdk::io::FileSystem
{
  DECL_ACDK_DEFAULT_METACLASS(FileSystem)
private:
  acdk::net::RURL _rootUrl;
  RFTPClient _client;
  acdk::io::RFileInfoArray _cache;
  
public:
  FTPFileSystem(IN(RString) url);
  FTPFileSystem(IN(acdk::net::RURL) url);
  ~FTPFileSystem();
  bool equals(IN(RObject) other)
  {
    if (instanceof(other, FTPFileSystem) == false)
      return false;
    return equals(RFTPFileSystem(other));
  }
  bool equals(IN(RFTPFileSystem) other)
  {
    return toString()->equals(other->toString()) == true;
  }

  RString toString() 
  { 
    if (_rootUrl == Nil)
      return "<unconnected ftp filesystem>";
    return _rootUrl->toString();
  }
  /**
    @return true, if given (absolut) file name
            is part of this file system.
            Example:
            /dir/subdir/file.dat -> owns StandardFileSystem
            /dir/file.zip@/subdir/file.data -> owns ZipFileSystem("/dir/file.zip")
  */
  virtual bool ownsFile(IN(RString) fname);

  /**
    returns the root name of the this file system
    Examples: 
    ""
    "/home/roger/myfile.zip"
    "ftp://ftp.artefaktur.com"
  */
  virtual RString getRootName();
  /**
    @param directory the absolute name with out the FSname
    @param listflags a combination of ListFlags
    @return list of files
  */
  virtual RFileArray listFiles(IN(RString) directory, int listflags);
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

  RFTPClient getClient();
  void removeFromCache(IN(RFileInfo) fi);
  acdk::io::RFileInfo _getFileInfo(IN(RString) file);
protected:
  /**
    get file info from cache or read it from ftp server
  */
  
  acdk::io::RFileInfo _findFileInfoFromCache(IN(RString) file);
private:
  void _connect();
};


ACDK_DECL_CLASS(FTPFileSystemFactory);

class ACDK_NET_FTP_PUBLIC FTPFileSystemFactory
: extends ::acdk::lang::Object
, implements acdk::io::FileSystemFactory
{
  ACDK_WITH_METAINFO(FTPFileSystemFactory)
public:
  FTPFileSystemFactory() {}

  virtual bool isRootFileSystem() { return true; }
  virtual int handleFile(IN(RString) file)
  {
    if (file->startsWith("ftp://") == false)
      return -1;
    return 7;
  }
  virtual RFileSystem create(IN(RString) file);
};


} // namespace ftp
} // namespace acdk
} // namespace net

#endif //acdk_net_ftp_FTPFileSystem_h

