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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/zip/ZipFileSystem.h,v 1.12 2005/03/17 12:21:22 kommer Exp $

#ifndef acdk_vfile_zip_ZipFileSystem_h
#define acdk_vfile_zip_ZipFileSystem_h

#include <acdk.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/io/File.h>
#include <acdk/io/FileAbstractImpl.h>
#include <acdk/io/FileStatus.h>

#include <acdk/util/Map.h> ///## remove this, should not be needed
#include <acdk/util/SortedMap.h>
#include <acdk/util/Iterator.h>

#include "../Config.h"
#include "LocalFileHeader.h"

namespace acdk {
namespace vfile {
namespace zip {



ACDK_DECL_CLASS(ZipFileSystem);

class ACDK_VFILE_PUBLIC ZipFileSystem
: extends acdk::lang::Object
, implements acdk::io::FileSystem
{
private:
  RString _fileName;
  bool _loaded;
  RLocalFileHeaderArray _lfheaders;
  friend class ZipFileImpl;
public:
  ZipFileSystem(IN(RString) zipfile);
  ~ZipFileSystem();
  foreign bool ownsFile(IN(RString) fname);
  foreign RString getRootName();
  foreign acdk::io::RFileArray listFiles(IN(RString) directory, int listflags);
  foreign acdk::io::RFile file(IN(RString) path);
  foreign acdk::io::RFileImpl getFileImpl(IN(RString) fqpath);
  int fileCount() { return _lfheaders->length(); }
  void dump(IN(acdk::io::RPrintWriter) out);
  /** 
    this is needed to be sure, that this .dll/.so will be 
    loaded into memory
  */
  static void loadFileSystem();
protected:
  foreign void _load();
  foreign void _addDerived();
  foreign void _addDirLfh(IN(RString) dirname);
  RLocalFileHeader _find(IN(RString) fname);
  foreign void _readCentralDirectory(acdk::io::Reader& in);
  foreign void _findCentralDirectory(acdk::io::Reader& in);
  foreign void _scanLocalHeader(acdk::io::Reader& in);
};

ACDK_DECL_CLASS(ZipFileSystemFactory);

class ACDK_VFILE_PUBLIC ZipFileSystemFactory
: extends acdk::lang::Object
, implements acdk::io::FileSystemFactory
{
  ACDK_WITH_METAINFO(ZipFileSystemFactory)
public:
  ZipFileSystemFactory() {}
  virtual int handleFile(IN(RString) file)
  {
    int idx1 = file->lastIndexOf(".zip@");
    int idx2 = file->lastIndexOf(".ZIP@");
    if (idx1 == -1 && idx2 == -1)
      return -1;
    if (idx1 == -1)
      return idx2;
    return idx1;
  }
  virtual acdk::io::RFileSystem create(IN(RString) file)
  {
    return new ZipFileSystem(file);
  }
};

} // zip
} // vfile
} // acdk



#endif //acdk_vfile_zip_ZipFileSystem_h

