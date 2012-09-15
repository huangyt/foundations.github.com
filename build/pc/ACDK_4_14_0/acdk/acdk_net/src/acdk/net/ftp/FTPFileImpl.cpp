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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPFileImpl.cpp,v 1.6 2005/03/19 21:30:32 kommer Exp $


#include "FTPFileImpl.h"

namespace acdk {
namespace net {
namespace ftp {

using namespace acdk::io;

RString 
FTPFileImpl::getCanonicalPath()
{
  return _fileSystem->getRootName() + _fileInfo->getPath();
}

RString 
FTPFileImpl::getAbsolutePath()
{
  return getCanonicalPath();
}

RFile 
FTPFileImpl::getParentFile()
{
  if (_fileInfo->name->length() == 0)
    return Nil;

  RString p = _fileInfo->dir;
  RString fname = p;
  /*
  int idx = p->indexOf(File::separatorChar());
  if (idx != -1)
    p = p->substr(0, idx - 1);
  else
    p = "";
    */
  RFileInfo fi = _fileSystem->_getFileInfo(fname);
  //RFileInfo fi = new FileInfo(FileInfoIsDir, p, fname);
  return new File(new FTPFileImpl(_fileSystem, fi));
}

RFile 
FTPFileImpl::makeChild(IN(RString) subfile)
{
  RString np = "";
  if (_fileInfo->getPath()->length() > 0)
    np = _fileInfo->getPath() + File::separator() + subfile;
  else
    np = subfile;
  RFileInfo fi = _fileSystem->_getFileInfo(np);
  return new File(new FTPFileImpl(_fileSystem, fi));
}

  
bool 
FTPFileImpl::createNewFile()
{
  return true;
}

bool 
FTPFileImpl::deleteFile()
{
  // ### remove file from cache
  // ### check if dir
  try {
    _fileSystem->getClient()->deleteFile(_getFtpFileName());
    _fileSystem->removeFromCache(_fileInfo);
    return true;
  } catch (RIOException ex) {
    return false;
  }
  return false;
}


bool 
FTPFileImpl::mkdir(int mode)
{
  _fileSystem->getClient()->mkdir(_getFtpFileName());
  return false;
}

bool 
FTPFileImpl::renameTo(IN(RFile) dest)
{
  RFileImpl fi = dest->getFileImpl();
  if (instanceof(fi, FTPFileImpl) == false)
    THROW1(IOException, "cannot rename files on different file systems");
  RFTPFileImpl ftpfi = (RFTPFileImpl)fi;
  if (ftpfi->getClient() != getClient())
    THROW1(IOException, "cannot rename files on different ftp file systems");
  getClient()->rename(_getFtpFileName(), ftpfi->_getFtpFileName());
  return true;
}

bool 
FTPFileImpl::setLastModified(jlong time)
{
  return false;
}

bool 
FTPFileImpl::setFileCreated(jlong time)
{
  return false;
}


class FTPFileWriter
: extends acdk::io::AbstractFilterWriter
{
  RFTPFileImpl _file;
  bool _closed;
public:
  FTPFileWriter(IN(RFTPFileImpl) fileimpl) 
  : AbstractFilterWriter(fileimpl->getClient()->getRemoteFileWriter(fileimpl->_getFtpFileName()))
  , _file(fileimpl) 
  , _closed(false)
  {
  }
  ~FTPFileWriter()
  {
    close();
  }
  virtual void close()
  {
    if (_closed == false)
    {
      try {
        _file->getClient()->closeRemoteFileWriter();
        _closed = true;
      } catch (RThrowable ex) {
      
      }
    }
  }
};

class FTPFileReader
: extends acdk::io::AbstractFilterReader
{
  RFTPFileImpl _file;
  bool _closed;
public:
  FTPFileReader(IN(RFTPFileImpl) fileimpl) 
  : AbstractFilterReader(fileimpl->getClient()->getRemoteFileReader(fileimpl->_getFtpFileName()))
  , _file(fileimpl) 
  , _closed(false)
  {
  }
  ~FTPFileReader()
  {
    close();
  }
  virtual void close()
  {
    if (_closed == false)
    {
      try {
        _file->getClient()->closeRemoteFileReader();
        _closed = true;
      } catch (RThrowable ex) {
      }
    }
  }
};

RReader 
FTPFileImpl::getReader()
{
  return new FTPFileReader(this);
}


RWriter 
FTPFileImpl::getWriter()
{
  return new FTPFileWriter(this);
}

RFileSystem 
FTPFileImpl::getFileSystem()
{
  return &_fileSystem;
}

RString 
FTPFileImpl::_getFtpFileName()
{
  RString ret = getFileInfo()->getPath()->replace('\\', '/');
  if (ret->startsWith("/") == false)
    return "/" + ret;
  return ret;
}


} // namespace ftp
} // namespace acdk
} // namespace net



