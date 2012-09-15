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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileImpl.h,v 1.21 2005/03/19 21:48:18 kommer Exp $

#ifndef acdk_io_FileImpl_h
#define acdk_io_FileImpl_h

#include <acdk.h>

#include "FileSystem.h"
#include "FilenameFilter.h"
#include "FileFilter.h"
#include "FileInfo.h"
#include "IOException.h"

namespace acdk {
namespace io {

ACDK_DECL_INTERFACE(FileImpl);


/**
  This Interface should implement the File operations.
  The class FileStandardImpl implements the file operations
  on a normal file system.
  
  Please refer also to gw_ref[acdk_io_FileSystem].

  @author Roger Rene Kommer
  @see FileStandardImpl 
  @see File
  @see FileSystem
*/
class ACDK_CORE_PUBLIC FileImpl
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(FileImpl)
public:
  /**
    corresponding to File
  */
  virtual RString getCanonicalPath() = 0;
  virtual RString getAbsolutePath() = 0;
  virtual RString getName() = 0;
  virtual RString getPath() = 0;
  virtual RFile getParentFile() = 0;
  virtual RFile makeChild(IN(RString) subfile) = 0; 
  virtual bool isAbsolute() = 0;
  virtual bool exists() = 0;
  virtual bool canRead() = 0;
  virtual bool canWrite() = 0;
  virtual bool isDirectory() = 0;
  virtual bool isFile() = 0;
  virtual bool isHidden() = 0;
  virtual jlong length() = 0;
  
  virtual bool createNewFile() = 0;
  virtual bool deleteFile() = 0;
  virtual RStringArray list(IN(RFilenameFilter) filter = Nil, int listFlags = FileListBoth);
  virtual RFileArray listFiles(IN(RFileFilter) filter = Nil, int listFlags = FileListBoth);
  virtual jlong lastModified() = 0;
  virtual jlong fileCreated() = 0;
  virtual bool mkdir(int mode = 0777) = 0;
  virtual bool renameTo(IN(RFile) dest) = 0;
  virtual bool setLastModified(jlong time) = 0;
  virtual bool setFileCreated(jlong time) { return false; }
  /**
    return File Info using other methods provided by FileImpl
    for performance reason this method should be implemented
  */
  virtual RFileInfo getFileInfo();
  /**
    by default does nothing
    @see acdk::io::File::setFileAttributes
  */
  virtual bool setFileAttributes(int mask, int flags) { return false; }
  virtual bool setFileFlags(int flags) { return false; }
  virtual RReader getReader() = 0;
  virtual RWriter getWriter() = 0;
  virtual RFileSystem getFileSystem() = 0;
};

ACDK_DECL_CLASS(AbstractFileImpl);
/**
  implements a abstract FileImpl
  If not derived from other class this implementation
  stands for a file, which does not exists
*/
class ACDK_CORE_PUBLIC AbstractFileImpl
: extends acdk::lang::Object
, implements FileImpl
{
  ACDK_WITH_METAINFO(AbstractFileImpl)
protected:
  RFileSystem _fsys;
  RString _filePath;
  RString _dir;
  RString _name;
public:
  AbstractFileImpl(IN(RFileSystem) fsys, IN(RString) fpath);
  AbstractFileImpl(IN(RFileSystem) fsys, IN(RString) path, IN(RString) dir, IN(RString) name)
    : _fsys(fsys)
    , _filePath(path)
    , _dir(dir)
    , _name(name)
  {
  }
  virtual RString getCanonicalPath() { return _filePath; }
  virtual RString getAbsolutePath() { return _filePath; }
  virtual RString getName() { return _name; }
  virtual RString getPath() { return _dir; }
  virtual RFile getParentFile() { _throwFileNotExits("getParentFile");  return Nil; }
  virtual RFile makeChild(IN(RString) subfile) { _throwFileNotExits("makeChild");  return Nil; }
  virtual bool isAbsolute()  { return true; }
  virtual bool exists() { return false; }
  virtual bool canRead() { return false; }
  virtual bool canWrite() { return false; }
  virtual bool isDirectory() { return false; }
  virtual bool isFile() { return false; }
  virtual bool isHidden()  { return false; }
  virtual jlong length() { return -1; }
  
  virtual bool createNewFile() { return false; }
  virtual bool deleteFile() { return false; }
  virtual RStringArray list(IN(RFilenameFilter) filter = Nil, int listFlags = FileListBoth)
  {
    _throwFileNotExits("list");
    return Nil; 
  }
  virtual RFileArray listFiles(IN(RFileFilter) filter = Nil, int listFlags = FileListBoth)
  {
    _throwFileNotExits("listFiles");
    return Nil;
  }
  virtual jlong lastModified() { return -1; }
  virtual jlong fileCreated() { return -1; }
  virtual bool mkdir(int mode = 0777) { return false; }
  virtual bool renameTo(IN(RFile) dest) { return false; }
  virtual bool setLastModified(jlong time) { return false; }
  virtual bool setFileCreated(jlong time) { return false; }
  
  virtual RFileInfo getFileInfo()
  {
    return new FileInfo(0, _dir, _name, -1, -1, -1);
  }
  /**
    by default does nothing
    @see acdk::io::File::setFileAttributes
  */
  virtual bool setFileAttributes(int mask, int flags) { return false; }
  virtual bool setFileFlags(int flags) { return false; }
  virtual RReader getReader()
  {
    _throwFileNotExits("getReader");
    return Nil;
  }
  virtual RWriter getWriter() 
  {
    _throwFileNotExits("getWriter");
    return Nil;
  }
  virtual RFileSystem getFileSystem()
  {
    return _fsys;
  }
protected:
  void _throwFileNotExits(IN(RString) method)
  {
    THROW1(IOException, "File " + _filePath + " is not a valid file in access " + method); 
  }
};


} // io
} // acdk


#endif //acdk_io_FileImpl_h

