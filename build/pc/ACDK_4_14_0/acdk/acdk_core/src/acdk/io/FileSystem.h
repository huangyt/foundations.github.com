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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileSystem.h,v 1.21 2005/04/28 14:58:14 kommer Exp $

#ifndef acdk_io_FileSystem_h
#define acdk_io_FileSystem_h

#include <acdk.h>


namespace acdk {
namespace io {

ACDK_DECL_INTERFACE(FileSystem);
ACDK_DECL_INTERFACE(FileImpl);
ACDK_DECL_INTERFACE(FileSystemFactory);
ACDK_DECL_CLASS(File);

/**
  used in acdk::io::FileSystem 
*/
enum FileListFlags
{
  /// list files
  FileListFiles        = 0x0001,
  // list directories
  FileListDirectories  = 0x0002,
  /// list files and directories
  FileListBoth         = 0x0003,
  /// list files and/or directories recursivelly in sub directories.
  FileListRecursive    = 0x0004,
    /**
      In case on error in list, resume with
      next directory. Otherwise listing file
      will be aborted.
    */
  FileListAllReadable     = 0x0010
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, FileListFlags);

/**
  This Interface should implement the basic Filesystem operations.
  It cooperates with FileImpl. 
  An important reason to introduce this indirection in the File
  interface is to be able to support archive files (TAR, ZIP)
  remote file systems (FTP) and other virtual file systems like
  DCOM documents or shell directories.
  
  Please refer also to gw_ref[acdk_io_FileSystem].

  @author Roger Rene Kommer
  @see  FileImpl
*/
class ACDK_CORE_PUBLIC FileSystem
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(FileSystem)
public:
  
  FileSystem();
  /**
    @return true, if given (absolut) file name
            is part of this file system.
            Example:
            /dir/subdir/file.dat -> owns StandardFileSystem
            /dir/file.zip@/subdir/file.data -> owns ZipFileSystem("/dir/file.zip")
  */
  virtual bool ownsFile(IN(RString) fname) = 0;

  /**
    returns the root name of the this file system
    Examples: 
    ""
    "/home/roger/myfile.zip"
    "ftp://ftp.artefaktur.com"
  */
  virtual RString getRootName() = 0;
  /**
    @param directory the absolute name with out the FSname
    @param listflags a combination of ListFlags
    @return list of files
  */
  virtual RFileArray listFiles(IN(RString) directory, int listflags) = 0;
  /**
    creates an instance of given file. 
    @param path is without the FS name
    @return returns a new File.
  */
  virtual RFile file(IN(RString) path) = 0;
  
  /**
    returns a file implementation for this
    full qualified file implementation
  */
  virtual RFileImpl getFileImpl(IN(RString) fqpath) = 0;
  /**
    To improve performance of File system File system will be cached
    in a weak referenced hash map. This means, the FileSystem
    will only hold, if there is an File instance in this FileSystem.
  */
  static void registerFileSystem(IN(RFileSystem) fs);
  static void registerFileSystemFactory(IN(RFileSystemFactory) fsf);
  static void unRegisterFileSystemFactory(IN(RFileSystemFactory) fsf);
  static RFileSystem findFileSystem(IN(RString) file);
protected:
  /**
    look if in the Systems properterty a Filesystem is registered
    for this filename
  */
  foreign static void loadPropertyFileSystemHandler();
};




} // io
} // acdk

#include "FileSystemFactory.h"
#include "StandardFileSystem.h"

#endif //acdk_io_FileSystem_h

