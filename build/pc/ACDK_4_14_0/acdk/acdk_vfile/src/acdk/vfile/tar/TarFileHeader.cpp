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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/tar/TarFileHeader.cpp,v 1.12 2005/03/17 12:18:39 kommer Exp $


#include <acdk.h>
#include "TarFileHeader.h"
#include <acdk/lang/Long.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/EOFException.h>

namespace acdk {
namespace vfile {
namespace tar {

USING_CLASS(::acdk::lang::, String);
USING_CLASS(::acdk::lang::, Long);


bool
TarFileHeader::read(jlong off, ::acdk::io::Reader& in)
{
  offset = off;
  filename = readString(in, 100);
  if (filename->length() == 0) {
    byte tbuf[412];
    int readed = in.read(tbuf, 0, 412);
    if (readed < 412)
      return false;
    return true;
  }
  if (filename->endsWith("/") == true)
    filename = filename->substr(0, filename->length() - 1);
  filemode = readLong(in, 8);
  uid = readLong(in, 8);
  guid = readLong(in, 8);
  size = readLong(in, 12);
  mtime = readLong(in, 12);
  chksum = readLong(in, 8);
  linkflag = in.read();
  linkname = readString(in, 100);

  magic = readString(in, 6);
  if (magic->length() >= 5 &&
      magic->charAt(0) == 'u' 
      && magic->charAt(1) == 's' 
      && magic->charAt(2) == 't'
      && magic->charAt(3) == 'a' 
      && magic->charAt(4) == 'r')
  {
    version = new byteArray(2);
    in.read(version, 0, 2);
    uname = readString(in, 32);
    gname = readString(in, 32);
    in.skip(183); 
  } else {
    in.skip(249);
  }
  int newsize = size == 0 ? 0 : size + (512 - (size % 512));
  in.skip(newsize);
  return true;
}

void
TarFileHeader::write(::acdk::io::Writer& out)
{
}


RString 
TarFileHeader::readString(::acdk::io::Reader& in, int len)
{
  char maxbuf[256];
  int ret = in.read((byte*)maxbuf, 0, len);
  if (ret < len)
    THROW0_FQ(::acdk::io::, EOFException);
  len = strlen(maxbuf);
  return new String((const char*)maxbuf, len, NormalSST | CCAscii);
}

void
TarFileHeader::writeString(::acdk::io::Writer& out, IN(RString) str, int len)
{
  
  char maxbuf[256];
  out.getCharWriter()->writeString(str);
  int rest = len - str->length();
  while (rest-- < 0)
    out.write(0, 0, 1);
}

inline
bool isoktalDigit(char ch)
{
  return ch >= '0' && ch < '8';
}

RString stripLong(IN(RString) str)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  int i;
  for (i = 0; it < end; ++it, ++i) 
  {
    if (isoktalDigit(*it) == false)
      break;
  }
  return str->substr(0, i);
}

//static 
jlong 
TarFileHeader::readLong(::acdk::io::Reader& in, int len)
{
  char maxbuf[32];
  int ret = in.read((byte*)maxbuf, 0, len);
  if (ret < len)
    THROW0_FQ(::acdk::io::, EOFException);
  RString readed = SCS(maxbuf);
  readed = stripLong(readed->trim());
  if (readed->length() == 0)
    return 0;
  return Long::parseLong(readed, 8);
}

//static 
void 
TarFileHeader::writeLong(::acdk::io::Writer& out, jlong val, int len)
{
  char maxbuf[32];
  RString erg = Long::toString(val, 8);
  StringBuffer sb(len);
  int rest = len - erg->length();
  while (rest-- > 0) 
    out.write('0');
  out.getCharWriter()->writeString(erg);
}


RbyteArray 
TarFileHeader::getContent(::acdk::io::Reader& in)
{
  in.seek(::acdk::io::SeekSet, offset + TarFileHeaderSize);
  RbyteArray ba = new byteArray(size);
  in.read(ba);
  return ba;
}



} // tar
} // vfile
} // acdk


