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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/tar/TarFileHeader.h,v 1.10 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_vfile_tar_TarFile_h
#define acdk_vfile_tar_TarFile_h

#include <acdk.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/io/File.h>
#include <acdk/io/FileAbstractImpl.h>

#include <acdk/util/Map.h> ///## remove this, should not be needed
#include <acdk/util/SortedMap.h>
#include <acdk/util/Iterator.h>
#include "../Config.h"


namespace acdk {
namespace vfile {
  /**
     Virtual file system implemenation for tar files
  */
namespace tar {


enum LinkFlag
{
  /** Normal disk file, Unix compatible */
  OldNormal = '\0',    
  /** Normal disk file */
  Normal = '0',        
  /** Link to previously dumped file */
  Link = '1',
  /* Symbolic link */
  SymbolLink = '2',
  CharacterSpecialFile = '3',
  BlockSpecialFile = '4',
  Directory = '5',
  Fifo = '6',
  Reserved = '7'

};

ACDK_DECL_CLASS(TarFileHeader);

class ACDK_VFILE_PUBLIC TarFileHeader
: extends ::acdk::lang::Object
{
public:
  enum {
    TarFileHeaderSize = 512
  };
  TarFileHeader()
  : offset(0)
  , derived(false)
  , filemode(0)
  , uid(0)
  , guid(0)
  , size(0)
  , mtime(0)
  , chksum(0)
  , linkflag(0)
  {
  }
  jlong offset;
  bool derived;
  /** name of file [100] */
  RString filename;
  /**  file mode octal ascii [8] */
  int filemode;
  /**   uid  owner user ID  octal ascii [8]*/
  int uid;
  /**   gid  owner group ID  octal ascii [8] */
  int guid;
  /** 12  size  length of file in bytes  octal ascii [12] */
  jlong size;
  /** modify time of file  octal ascii [12]*/
  jlong mtime; 
  /** checksum for header  octal ascii [8] */
  int chksum; 
  /** link  indicator for links  */
  char linkflag; 
  /** linkname  name of linked file  [100]*/
  RString linkname; 
  /** "ustar  \0" [6]*/
  RString magic;
  /** USTAR version  [2]*/
  RbyteArray version;
  /** [32] */
  RString uname;
  /** [32] */
  RString gname;
  //char devmajor[8];
  //char devminor[8];
  //char padding[167];

  bool isDirectory() { return linkflag == Directory; }
  bool isFile() { return linkflag == OldNormal || linkflag == Normal; }
  bool canRead() { return true; } //## may to read filemode
  jlong length() { return size; }
  RbyteArray getContent(::acdk::io::Reader& in);
  int getEndOffset() { return offset + TarFileHeaderSize + size + (size % 512); }
  bool read(jlong start, ::acdk::io::Reader& in);
private:
  
  void write(::acdk::io::Writer& out);
  static RString readString(::acdk::io::Reader& in, int len);
  static void writeString(::acdk::io::Writer& out, IN(RString) str, int len);
  static jlong readLong(::acdk::io::Reader& in, int len);
  static void writeLong(::acdk::io::Writer& out, jlong val, int len);
  
};

#if 0

ACDK_DECL_CLASS(TarFile);

class ACDK_VFILE_PUBLIC TarFile
: extends ::acdk::lang::Object
{
private:
  friend class TarFileSystem;
  
public:  
  /**
    @param fname the raw archive name without internal names
  */
  TarFile(IN(RString) fname);
  
  RStringArray list(IN(::acdk::io::RFilenameFilter) filter = Nil);
  
  /**
    @param path complete internal path to file
           /path/in/archive/file_in_archiv.txt
  */
  RTarFileHeader getFile(IN(RString) path);
  /**
    returns Iterator over strings which contains the name of the entries.
  */
  ::acdk::util::RIterator fileNames() 
  { 
    _openTarFile();
    return  _fileHeaders->keySet()->iterator(); 
  }
  /**
    returns Iterator over TarFileHeader
      */
  ::acdk::util::RIterator fileEntries() 
  { 
    _openTarFile();
    return  _fileHeaders->entrySet()->iterator(); 
  }
  int elementCount() { return  _fileHeaders->size(); }
  RString fileName() { return _fname; }
protected:
  void _openTarFile();
  void _readEntries();
};


#endif //0

} // tar
} // vfile
} // acdk



#endif //acdk_vfile_tar_TarFile_h

