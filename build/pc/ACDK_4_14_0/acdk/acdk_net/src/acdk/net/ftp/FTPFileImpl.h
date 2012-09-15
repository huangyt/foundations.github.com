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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPFileImpl.h,v 1.3 2005/02/05 10:45:30 kommer Exp $

#ifndef acdk_net_ftp_FTPFileImpl_h
#define acdk_net_ftp_FTPFileImpl_h

#include "Config.h"
#include "FTPClient.h"
#include <acdk/io/FileImpl.h>
#include <acdk/io/File.h>
#include "FTPFileSystem.h"

namespace acdk {
namespace net {
namespace ftp {

ACDK_DECL_CLASS(FTPFileImpl);

class ACDK_NET_FTP_PUBLIC FTPFileImpl
: extends acdk::lang::Object
, implements acdk::io::FileImpl
{
  DECL_ACDK_DEFAULT_METACLASS(FileImpl)
private:
  RFTPFileSystem _fileSystem;
  acdk::io::RFileInfo _fileInfo;
public:
  FTPFileImpl(IN(RFTPFileSystem) fsys, IN(acdk::io::RFileInfo) fi)
  : _fileSystem(fsys)
  , _fileInfo(fi)
  {
  }
  virtual RString getCanonicalPath();
  virtual RString getAbsolutePath();
  virtual RString getName() { return _fileInfo->name; }
  virtual RString getPath() { return _fileInfo->getPath(); }
  virtual acdk::io::RFile getParentFile();
  virtual acdk::io::RFile makeChild(IN(RString) subfile); 
  virtual bool isAbsolute() { return true; }
  virtual bool exists()  { return _fileInfo->exists(); }
  virtual bool canRead() { return _fileInfo->canRead(); }
  virtual bool canWrite() { return _fileInfo->canWrite(); }
  virtual bool isDirectory() { return _fileInfo->isDirectory(); }
  virtual bool isFile() { return _fileInfo->isFile(); }
  virtual bool isHidden() { return _fileInfo->isHidden(); }
  virtual jlong length() { return _fileInfo->size; }
  
  virtual bool createNewFile();
  virtual bool deleteFile();
  virtual jlong lastModified() { return _fileInfo->modified; }
  virtual jlong fileCreated() { return _fileInfo->created; }

  virtual bool mkdir(int mode = 0777);
  virtual bool renameTo(IN(acdk::io::RFile) dest);
  virtual bool setLastModified(jlong time);
  virtual bool setFileCreated(jlong time);
  virtual acdk::io::RReader getReader();
  virtual acdk::io::RWriter getWriter();
  virtual acdk::io::RFileSystem getFileSystem();
  acdk::io::RFileInfo getFileInfo() { return _fileInfo; }
  RFTPClient getClient() { return _fileSystem->getClient(); }
  /**
    return absolut path inside the ftp server
    /dir/sub/file.dat
  */
  RString _getFtpFileName();
};

} // namespace ftp
} // namespace acdk
} // namespace net

#endif //acdk_net_ftp_FTPFileImpl_h

