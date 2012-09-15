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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/zip/LocalFileHeader.cpp,v 1.9 2005/02/05 10:45:33 kommer Exp $


#include <acdk.h>
#include "../Config.h"
#include "LocalFileHeader.h"

namespace acdk {
namespace vfile {
namespace zip {

namespace {

RString readString(acdk::io::BinaryDataReader& in, int len)
{
  byteArray ba(len);
  if (in.read(&ba) != len)
    ; //### problem
  return new String((const char*)ba.data(), len, NormalSST | CCAscii);
}

RbyteArray readBa(acdk::io::BinaryDataReader& in, int len)
{
  RbyteArray ba = new byteArray(len);
  if (in.read(&ba) != len)
    ; //### problem
  return ba;
}

} // anon namespace

void 
CentralDirectoryEntry::read(acdk::io::BinaryDataReader& in)
{
  
  prodversion = in.readShort();
  extrversion = in.readShort();
  gpflag = in.readShort();
  method = in.readShort();
  lmtime = in.readShort();
  lmdate = in.readShort();
  crc = in.readInt();
  comprsize = in.readInt();
  size = in.readInt();
  fnamelength = in.readShort();
  extrafieldlength = in.readShort();
  commentlength = in.readShort();
  disknumberstart = in.readShort();
  internlfileattr = in.readShort();
  fileattr = in.readInt();
  lheaderoffset = in.readInt();
  filename = readString(in, fnamelength);
  extrafield = readBa(in, extrafieldlength);
  comment = readString(in, commentlength);
  
}

void 
CentralDirectory::read(acdk::io::BinaryDataReader& in)
{
  bool endFound = false;
  _entries = new CentralDirectoryEntryArray(0);
  do {
    int signature = in.readInt();
    if (signature == CDStart) {
      RCentralDirectoryEntry cde = new CentralDirectoryEntry();
      cde->read(in);
      _entries->append(cde);
    } else if (signature == CDEnd) {
      endFound = true;
      break;
    }
  } while(true);
}
  
/*
  endofcdir = in.readInt(); 
  if (endofcdir != CDEnd)
    ; ///####
  disksnum = in.readShort();
  ctdisknum = in.readShort();
  numentries = in.readShort();
  numentriesglobal = in.readShort();
  cdsize = in.readInt();
  cdoffset = in.readInt();
	zfcommentlength = in.readShort();
  zfcomment = readString(in, zfcommentlength);
}
*/



void 
LocalFileHeader::read(acdk::io::BinaryDataReader& in)
{
  valid = false;
  signature = in.readInt();
  if (signature != Magic)
    return;
	version = in.readShort();
	gpflag = in.readShort();
	method = in.readShort();
	lmodtime = in.readShort();
	lmoddate = in.readShort();
	crc = in.readInt();
	cmprsize = in.readInt();
	size = in.readInt();
	fnameLength = in.readShort();
	extraFieldLength = in.readShort();
  filename = readString(in, fnameLength);
  if (filename->endsWith("/") == true)
    filename = filename->substr(0, filename->length() - 1);
  extraField = readBa(in, extraFieldLength);
  dataOffset = in.seek(acdk::io::SeekCur, 0);
  valid = true;
}



} // zip
} // vfile
} // acdk




