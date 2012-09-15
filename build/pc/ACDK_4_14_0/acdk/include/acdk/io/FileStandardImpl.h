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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileStandardImpl.h,v 1.14 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_FileStandardImpl_h
#define acdk_io_FileStandardImpl_h

#include <acdk.h>
#include "File.h"
#include "FileAbstractImpl.h"



namespace acdk {
namespace io {

ACDK_DECL_INTERFACE(FileStandardImpl);

/**
  Implementation of the FileImpl interface
  for standard operation system files.
*/
class ACDK_CORE_PUBLIC FileStandardImpl
: extends ::acdk::io::FileAbstractImpl
{
public:
  RFileImpl create(IN(RString) fname);
  FileStandardImpl(IN(RString) fname)
  : FileAbstractImpl(fname)
  {
  }
  virtual bool exists();
  virtual bool canRead();
  virtual bool canWrite();
  virtual bool isDirectory();
  virtual bool isFile();
  virtual bool isHidden();
  virtual jlong length();
  
  virtual bool createNewFile();
  virtual bool deleteFile();
  virtual RStringArray list(IN(RFilenameFilter) filter = Nil, int listFlags = FileListBoth);
  virtual RFileArray listFiles(IN(RFileFilter) filter = Nil, int listFlags = FileListBoth);
  virtual jlong lastModified();
  virtual jlong fileCreated();
  virtual bool mkdir(int mode = 0777);
  virtual bool renameTo(IN(RFile) dest);
  virtual bool setLastModified(jlong time);
  virtual bool setFileCreated(jlong time);
  virtual RFileInfo getFileInfo();
  virtual bool setFileAttributes(int mask, int flags);
  virtual RReader getReader();
  virtual RWriter getWriter();
  virtual RFileSystem getFileSystem();

  static RString fileUrlToFileName(IN(RString) furl);
protected:
  RStringArray _listFiles(IN(RString) dir);
};

} // io
} // acdk


#endif //acdk_io_FileStandardImpl_h

