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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/SRFileImpl.h,v 1.6 2005/02/05 10:45:30 kommer Exp $


#ifndef acdk_net_srsync_SRFileImpl_h
#define acdk_net_srsync_SRFileImpl_h

#include <acdk.h>
#include "srfsys.h"
#include "SRFileImpl.h"
#include "SRFileSystemClient.h"
//#include "SRFileSystem.h"

namespace acdk {
namespace net {
namespace srfsys {

USING_CLASS(::acdk::lang::, String);
USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::io::, FileImpl);
USING_CLASS(::acdk::io::, FileSystem);
USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Writer);

ACDK_DECL_CLASS(SRFileImpl);

class ACDK_NET_SRFSYS_PUBLIC SRFileImpl
: extends ::acdk::lang::Object
, implements ::acdk::io::FileImpl
{
public:
  RSRFileSystemClient _fsys; 
  RFileInfo _fileInfo;
  SRFileImpl(IN(RSRFileSystemClient) fsys, IN(RFileInfo) fileInfo)
  : Object()
  , _fsys(fsys)
  , _fileInfo(fileInfo)
  {
  }
  
  virtual RString getCanonicalPath() 
  {
    return _fsys->getRootName() + _fileInfo->getPath();
  }
  virtual RString getAbsolutePath()
  {
    return getCanonicalPath();
  }
  virtual RString getName()
  {
    return _fileInfo->name;
  }
  virtual RString getPath()
  {
    return _fileInfo->getPath();
  }

  virtual RFile getParentFile()
  {
    return _fsys->file(_fileInfo->getPath());
  }

  virtual RFile makeChild(IN(RString) subfile)
  {
    return _fsys->file(_fileInfo->getPath() + "/" + subfile);
  }
  virtual bool isAbsolute() { return true; }
  virtual bool exists() { return _fileInfo->exists(); }
  virtual bool canRead() { return _fileInfo->canRead(); }
  virtual bool canWrite() { return _fileInfo->canWrite(); }
  virtual bool isDirectory() { return _fileInfo->isDirectory(); }
  virtual bool isFile() { return _fileInfo->isFile(); }
  virtual bool isHidden() { return _fileInfo->isHidden(); }
  virtual jlong length() { return _fileInfo->size; }
  
  virtual bool createNewFile() { return false; } 
  virtual bool deleteFile() { return false; }
  virtual jlong lastModified() { return 0; }
  virtual jlong fileCreated() { return 0; }
  virtual bool mkdir(int mode = 0777) { return false; }
  virtual bool renameTo(IN(RFile) dest) { return false; }
  virtual bool setLastModified(jlong time) { return false; }
  virtual RReader getReader();
  virtual RWriter getWriter();
  virtual RFileSystem getFileSystem() { return &_fsys; }
  /**
    write the file back to server
  */
  void writeBackToServer(IN(RbyteArray) data) { _fsys->sendFile(_fileInfo, data); }
};

ACDK_DECL_CLASS(SRSFileWriter);

class ACDK_NET_SRFSYS_PUBLIC SRSFileWriter
: extends ::acdk::io::MemWriter
{
  RSRFileImpl _fileImpl;
  bool _closed;
public:
  SRSFileWriter(RSRFileImpl fileimpl)
  : _fileImpl(fileimpl)
  , _closed(false)
  {

  }
  foreign virtual void close();
};


} // namespace srfsys
} // namespace net
} // namespace acdk 


#endif //acdk_net_srsync_SRFileImpl_h
