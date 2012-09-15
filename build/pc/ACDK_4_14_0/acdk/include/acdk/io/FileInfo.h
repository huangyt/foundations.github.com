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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileInfo.h,v 1.7 2005/02/05 10:44:54 kommer Exp $


#ifndef acdk_io_FileInfo_h
#define acdk_io_FileInfo_h

#include <acdk.h>

#include "Serializable.h"
#include "PrintWriter.h"




namespace acdk {
namespace io {

  
ACDK_DECL_CLASS(FileInfo);


/**
  Information about file
*/
enum FileInfoFlags
{
  /**
    file exists or not
    This attribute is read only
  */
  FileInfoExists =    0x0001,
  /**
    file is a file
    This attribute is read only
  */
  FileInfoIsFile =    0x0002,
  /**
    file is a directory
    This attribute is read only
  */
  FileInfoIsDir =     0x0004,
  /**
    file can be read
    This attribute is read/write
  */
  FileInfoCanRead   = 0x0010,
  /**
    file can be written
    This attribute is read/write
  */
  FileInfoCanWrite  = 0x0020,
  /**
    file can be executed
    This attribute is read only on Windows
  */
  FileInfoCanExec   = 0x0040,
  /**
    File is hidden, either through hidden bit (win32)
    or a leading dot in file name (unix)
    on Unix read only
  */
  FileInfoIsHidden  = 0x0100,
  /**
    DOS specific archive bit
    doesn't have any usage on non-win32 platforms
  */
  FileInfoHasABit   = 0x0200,
  /**
    not used
  */
  FileInfoChecked   = 0x1000
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, FileInfoFlags);

/**
  Hold logical information of a file
  Important: The information about the file is cached
  and may get out of sync with the read file information
*/
class ACDK_CORE_PUBLIC FileInfo
: extends ::acdk::lang::Object
, implements Serializable
, implements ::acdk::lang::Comparable
{
  ACDK_WITH_METAINFO(FileInfo)
public:
  int flags;
  RString dir;
  RString name;
  jlong size;
  jlong created;
  jlong modified;
  
  /// hash value of content of file?
  jlong digest;
  FileInfo()
  : flags(0)
  , size(0)
  , created(0)
  , modified(0)
  , digest(0)
  {
  }
  FileInfo(IN(RFileInfo) other)
  : flags(other->flags)
  , dir(other->dir)
  , name(other->name)
  , size(other->size)
  , created(other->created)
  , modified(other->modified)
  , digest(other->digest)
  {
  }
  FileInfo(IN(RString) path);
  FileInfo(int fiflags, IN(RString) parent, IN(RString) filename, jlong filesize = 0, jlong creat = 0, jlong modif = 0)
  : Object()
  , flags(fiflags)
  , dir(parent)
  , name(filename)
  , size(filesize)
  , created(creat)
  , modified(modif)
  , digest(0)
  {
  }
  RString getPath();

  
  inline bool exists() { return flags & FileInfoExists; }
  /// does only modify information, not underlying file
  inline void exists(bool b) { if (b) flags |= FileInfoExists; else flags &= ~FileInfoExists; }
  inline bool canRead() { return flags & FileInfoCanRead; }
  /// does only modify information, not underlying file
  inline void canRead(bool b) { if (b) flags |= FileInfoCanRead; else flags &= ~FileInfoCanRead; }
  inline bool canWrite() { return flags & FileInfoCanWrite; }
  /// does only modify information, not underlying file
  inline void canWrite(bool b) { if (b) flags |= FileInfoCanWrite; else flags &= ~FileInfoCanWrite; }
  inline bool isDirectory() { return flags & FileInfoIsDir; }
  /// does only modify information, not underlying file
  inline void isDirectory(bool b) { if (b) flags |= FileInfoIsDir; else flags &= ~FileInfoIsDir; }
  inline bool isFile() { return flags & FileInfoIsFile; }
  /// does only modify information, not underlying file
  inline void isFile(bool b) {  if (b) flags |= FileInfoIsFile; else flags &= ~FileInfoIsFile; }
  inline bool isHidden() { return flags & FileInfoIsHidden; }
  /// does only modify information, not underlying file
  inline void isHidden(bool b) { if (b) flags |= FileInfoIsHidden; else flags &= ~FileInfoIsHidden; }
  inline bool isChecked() { return flags & FileInfoChecked; }
  /// does only modify information, not underlying file
  inline void isChecked(bool b) { if (b) flags |= FileInfoChecked; else flags &= ~FileInfoChecked; }
  /**
    render the flags to a string, where each file flags is represented by a character
    <pre>
    E Exists
    N Does not exists
    F File
    D Directory
    R CanRead
    W CanWrite
    X CanExecute
    H Hidden
    A Archiv Bit
    </pre>
    @param flags combination of FileInfoFlags
  */
  static RString flagString(int flags);
  /**
    uses acdk::util::SysDate to render Time string
  */
  static RString timeToString(jlong time);

  RString toString();
  
  void dump(IN(::acdk::io::RPrintWriter) out);
  
  bool equals(IN(RFileInfo) other)
  {
    return dir->equals(other->dir) == true &&
           name->equals(other->name) == true;
  }
 
  bool equals(IN(RObject) other)
  {
    if (instanceof(other, FileInfo) == false)
      return false;
    return equals(RFileInfo(other));
  }
  int compareTo(IN(RFileInfo) other)
  {
    int t = dir->compareTo(other->dir);
    if (t != 0) return t;
    return name->compareTo(other->name);
  }
  int compareTo(IN(RObject) other)
  {
    return compareTo(RFileInfo(other));
  }
};

} // namespace io
} // namespace acdk 


#endif //acdk_io_FileInfo_h
