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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/zip/ZipFileImpl.h,v 1.8 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_vfile_zip_ZipFileImpl_h
#define acdk_vfile_zip_ZipFileImpl_h

#include <acdk.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/io/FileImpl.h>
#include <acdk/io/FileAbstractImpl.h>

#include "../Config.h"
#include "LocalFileHeader.h"
#include "ZipFileSystem.h"

namespace acdk {
namespace vfile {
/**
   Virtual file system implemenation for zip files
 */
namespace zip {




ACDK_DECL_CLASS(ZipFileImpl);

/**
  Represents a file in a zip.
*/

class ACDK_VFILE_PUBLIC ZipFileImpl
: extends ::acdk::lang::Object
, implements ::acdk::io::FileImpl
{
private:
  friend class ZipFileReader;
  /**
    The underlying files sytem.
    Is Nil if no such zip in StandardFileSystem
  */
  RZipFileSystem _fsys;  
  RLocalFileHeader _lfh;
public:  
  ZipFileImpl(IN(RZipFileSystem) fsys, IN(RLocalFileHeader) lfh)
  : _fsys(fsys)
  , _lfh(lfh)
  {
  }
  foreign RString getCanonicalPath();
  foreign RString getAbsolutePath();
  foreign RString getName();
  foreign RString getPath();
  foreign acdk::io::RFile getParentFile();
  foreign acdk::io::RFile makeChild(IN(RString) subfile); 
  foreign bool isAbsolute();
  foreign bool exists();
  foreign bool canRead();
  foreign bool canWrite();
  foreign bool isDirectory();
  foreign bool isFile();
  foreign bool isHidden();
  foreign jlong length();
  
  foreign bool createNewFile();
  foreign bool deleteFile();
  foreign jlong lastModified();
  foreign jlong fileCreated();
  foreign bool mkdir(int mode = 0777);
  foreign bool renameTo(IN(acdk::io::RFile) dest);
  foreign bool setLastModified(jlong time);

  foreign acdk::io::RReader getReader();
  foreign acdk::io::RWriter getWriter();
  foreign acdk::io::RFileSystem getFileSystem();
};




} // zip
} // vfile
} // acdk



#endif //acdk_vfile_zip_ZipFileImpl_h

