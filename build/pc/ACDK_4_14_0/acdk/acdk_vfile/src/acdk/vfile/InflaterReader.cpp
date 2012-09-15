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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/InflaterReader.cpp,v 1.9 2005/02/05 10:45:33 kommer Exp $

#include "InflaterReader.h"
#include <acdk/lang/Math.h>
#include <acdk/io/IOException.h>

namespace acdk {
namespace vfile {

const int BufferSize = 1024;

InflaterReader::InflaterReader(IN(::acdk::io::RReader) in)
: acdk::io::AbstractFilterReader(in)
, _initialized(false)
, _inbuf(BufferSize)
, _outbuf(BufferSize)
, _pos(BufferSize)
, _available(0)
{
  memset(&_zstream, 0, sizeof(_zstream));
 
}

InflaterReader::~InflaterReader()
{
  int erg = inflateEnd(&_zstream);
}

int 
InflaterReader::available()
{
  return _available - _pos; 
}

void 
InflaterReader::close()
{
  _in->close();
}

jlong 
InflaterReader::seek(acdk::io::SeekPos seekrel, jlong seekpos)
{
  if (seekrel == acdk::io::SeekCur && seekpos > 0) {
    jlong curseekpos = _in->seek(acdk::io::SeekCur, 0) - available();
    jlong ns = skip(seekpos);
    return curseekpos + ns;
  } else
    THROW1_FQ(acdk::io::, IOException, "InflaterReader doesn't support seeking mode");
  return 0;
}


jlong 
InflaterReader::skip(jlong n)
{
  byte buffer[1024];
  jlong seeked = 0;
  while (n > 0) {
    if (n > 1024) {
      n -= 1024;
      int erg = read(buffer, 0, 1024);
      seeked += erg;
      if (erg < 1024) {
        return seeked;
      }
    } else {
      read(buffer, 0, n);
      seeked += n;
      n = 0;
    }
  }
  return seeked;
}

int 
InflaterReader::read()
{
  byte d[1];
  int i = read(d, 0, 1);
  if (i <= 0)
    return -1;
  return d[0];
}

int 
InflaterReader::read(byte* buffer, int offset, int len)
{
  buffer += offset;
  if (_pos >= _available)
      fillBuffer();
  if (_pos == _available)
    return 0;
  int tlen = len;
  while (tlen > 0) {
    int sizetocopy = ::acdk::lang::Math::min(tlen, BufferSize - _pos);
    memcpy(buffer, &_outbuf[_pos], sizetocopy);
    _pos += sizetocopy;
    tlen -= sizetocopy;
    buffer += sizetocopy;
    if (_pos >= _available)
      fillBuffer();
  }
  return len - tlen;
}

void 
InflaterReader::init()
{
  _zstream.next_in  = _inbuf.data();
  _zstream.avail_in = 0 ;
  
 

  int erg = inflateInit2(&_zstream, -MAX_WBITS);
  if (Z_OK != erg) 
    THROW1_FQ(acdk::io::, IOException, RCS("Initializing zlib failed: ") + zError(erg));

  _initialized = true;
}




void
InflaterReader::fillBuffer()
{
  if (_initialized == false)
    init();

  _zstream.avail_out = BufferSize;
  _zstream.next_out = _outbuf.data();
  int zerg = Z_OK ;
  while (_zstream.avail_out > 0 && zerg == Z_OK) 
  {
    if (_zstream.avail_in == 0) 
    { 
      int readed = _in->read(_inbuf.data(), 0, BufferSize);
      if (readed <= 0) {
        //###???
        return;
      }
      _zstream.avail_in = readed;
      _zstream.next_in = _inbuf.data();
    }
    zerg = inflate(&_zstream, Z_NO_FLUSH);
  }
  if (zerg != Z_OK && zerg != Z_STREAM_END) 
    THROW1_FQ(acdk::io::, IOException, RString("inflate failed: ") + zError(zerg) + "; " + _zstream.msg);
  
  _available = BufferSize - _zstream.avail_out;
  if (_available > 0)
    _pos = 0;
  else
    _pos = BufferSize;
}

} // vfile
} // acdk




