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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/tar/TarFileSystem.h,v 1.13 2005/03/17 12:18:39 kommer Exp $

#ifndef acdk_vfile_tar_TarFileSystem_h
#define acdk_vfile_tar_TarFileSystem_h

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

namespace acdk {
namespace vfile {
namespace tar {

USING_CLASS(::acdk::io::, FileImpl);
USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Writer);
USING_CLASS(::acdk::io::, FilenameFilter);
USING_CLASS(::acdk::io::, FileFilter);




ACDK_DECL_CLASS(TarFileSystem);

class ACDK_VFILE_PUBLIC TarFileSystem
: extends acdk::lang::Object
, implements acdk::io::FileSystem
{
private:
  friend class TarFileImpl;
  RString _fileName;
  TarFileHeaderArray _fileHeaders;
public:
  TarFileSystem(IN(RString) zipfile);
  
  foreign bool ownsFile(IN(RString) fname);
  foreign RString getRootName();
  foreign acdk::io::RFileArray listFiles(IN(RString) directory, int listflags);
  foreign acdk::io::RFile file(IN(RString) path);
  foreign acdk::io::RFileImpl getFileImpl(IN(RString) fqpath);
  int fileCount() { return _fileHeaders.length(); }
    /** 
    this is needed to be sure, that this .dll/.so will be 
    loaded into memory
  */
  static void loadFileSystem();
protected:
  /** path is part of file in tar */
  RTarFileHeader _find(IN(RString) path);
  void _openTarFile();
  void _readEntries();
  void _addDerived();
  void _addDirLfh(IN(RString) dirname);
};

ACDK_DECL_CLASS(TarFileSystemFactory);


class ACDK_VFILE_PUBLIC TarFileSystemFactory
: extends ::acdk::lang::Object
, implements ::acdk::io::FileSystemFactory
{
  ACDK_WITH_METAINFO(TarFileSystemFactory)
public:
  /**
    returns true, if it handle the given file
  */
  foreign int handleFile(IN(RString) file)
  {
    int idx1 = file->lastIndexOf(".tar@");
    int idx2 = file->lastIndexOf(".TAR@");
    if (idx1 == -1 && idx2 == -1)
      return -1;
    if (idx1 == -1)
      return idx2;
    return idx1;
    // acdk::io::FileStatus(file).isFile() == true;
  }
  /**
    create a new FileImplementation of the 
    given file name
  */
  foreign acdk::io::RFileSystem create(IN(RString) file)
  {
    return new TarFileSystem(file);
  }

};

} // tar
} // vfile
} // acdk



#endif //acdk_vfile_tar_TarFileSystem_h

