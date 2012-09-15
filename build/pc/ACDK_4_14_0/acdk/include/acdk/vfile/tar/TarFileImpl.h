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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/tar/TarFileImpl.h,v 1.11 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_vfile_tar_TarFileImpl_h
#define acdk_vfile_tar_TarFileImpl_h

#include <acdk.h>
#include "../Config.h"

#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/io/File.h>
#include <acdk/util/Map.h> ///## remove this, should not be needed
#include <acdk/util/SortedMap.h>

#include <acdk/io/FileAbstractImpl.h>
#include <acdk/io/FileStatus.h>

#include "TarFileHeader.h"
#include "TarFileSystem.h"

namespace acdk {
namespace vfile {
namespace tar {

USING_CLASS(::acdk::io::, FileImpl);
USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Writer);
USING_CLASS(::acdk::io::, FilenameFilter);
USING_CLASS(::acdk::io::, FileFilter);
USING_CLASS(::acdk::io::, FileSystem);
USING_CLASS(::acdk::io::, IOException);


ACDK_DECL_CLASS(TarFileSystem);
ACDK_DECL_CLASS(TarFileImpl);


/**
  Represents a plugin for Tarfiles
  The string representation for tar files are:
  /normal/file/system/path/normal_file.tar@/path/in/archive/file.dat
*/

class ACDK_VFILE_PUBLIC TarFileImpl
: extends ::acdk::lang::Object
, implements ::acdk::io::FileImpl
{
  RTarFileSystem _fs;
  RTarFileHeader _fh;
public:

  TarFileImpl(RTarFileSystem fs, RTarFileHeader fh)
  : _fs(fs)
  , _fh(fh)
  {
  }
  foreign RString getCanonicalPath();
  foreign RString getAbsolutePath();
  foreign RString getName();
  foreign RString getPath();
  foreign RFile getParentFile();
  
  foreign RString getParent();
  foreign RFile makeChild(IN(RString) subfile); 
  foreign bool isAbsolute();
  foreign bool exists();
  foreign bool canRead();
  foreign bool canWrite() { return false; }
  foreign bool isDirectory();
  foreign bool isFile();
  foreign bool isHidden();
  foreign jlong length();
  
  foreign bool createNewFile();
  foreign bool deleteFile();
  foreign jlong lastModified();
  foreign jlong fileCreated();
  foreign bool mkdir(int mode = 0777);
  foreign bool renameTo(IN(RFile) dest);
  foreign bool setLastModified(jlong time);

  foreign RReader getReader();
  foreign RWriter getWriter();  
  foreign RFileSystem getFileSystem();
};



} // tar
} // vfile
} // acdk



#endif //acdk_vfile_tar_TarFileImpl_h

