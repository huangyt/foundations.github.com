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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/zip/LocalFileHeader.h,v 1.6 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_vfile_zip_LocalFileHeader_h
#define acdk_vfile_zip_LocalFileHeader_h

#include <acdk.h>
#include <acdk/io/BinaryDataReader.h>
#include "../Config.h"


namespace acdk {
namespace vfile {
namespace zip {

ACDK_DECL_CLASS(CentralDirectoryEntry);
/** not used */
class ACDK_VFILE_PUBLIC CentralDirectoryEntry
: extends ::acdk::lang::Object
{
public:
  
  int signature;
  short prodversion;
  short extrversion;
  short gpflag;
  short method;
  short lmtime;
  short lmdate;
  int crc;
  int comprsize;
  int size;
  short fnamelength;
  short extrafieldlength;
  short commentlength;
  short disknumberstart;
  short internlfileattr;
  int fileattr;
  int lheaderoffset;
  RString filename;
  RbyteArray extrafield;
  RString comment;
  void read(acdk::io::BinaryDataReader& in);
};

/** not used */
class ACDK_VFILE_PUBLIC CentralDirectory
: extends ::acdk::lang::Object
{
public:
  enum 
  {
    CDStart = 0x02014b50,
    CDEnd = 0x06054b50
  };
  RCentralDirectoryEntryArray _entries;
  int endofcdir; //0x06054b50
  short disksnum;
  short ctdisknum;
  short numentries;
  short numentriesglobal;
  int cdsize;
  int cdoffset;
	short zfcommentlength;
  RString zfcomment;
  void read(acdk::io::BinaryDataReader& in);
};



ACDK_DECL_CLASS(LocalFileHeader);

class ACDK_VFILE_PUBLIC LocalFileHeader
: extends ::acdk::lang::Object
{
public: 
  enum 
  {
    Magic = 0x04034b50,
    DerivedDir = 0x04034b51
  };
  LocalFileHeader() 
  : valid(false)
  , signature(0)
  , version(0)
  , gpflag(0)
  , method(0)
  , lmodtime(0)
  , lmoddate(0)
  , crc(0)
  , cmprsize(0)
  , size(0)
  , fnameLength(0)
  , extraFieldLength(0)
  , dataOffset(0)
  {
  }
  bool valid;
  int signature;
	short version;
	short gpflag;
	short method;
	short lmodtime;
	short lmoddate;
	int crc;
	int cmprsize;
	int size;
	short fnameLength;
	short extraFieldLength;
  RString filename;
  RbyteArray extraField;
  /** where the data begins */
  jlong dataOffset;
  void read(acdk::io::BinaryDataReader& in);
  bool isDerivedDir() { return signature == DerivedDir; }
  bool isDirectory() { return isDerivedDir(); }
  bool isFile() { return isDirectory() == false; }
};




} // zip
} // vfile
} // acdk



#endif //acdk_vfile_zip_ZipFile_h

