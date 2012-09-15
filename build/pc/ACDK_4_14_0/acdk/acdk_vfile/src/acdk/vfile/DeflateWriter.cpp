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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/DeflateWriter.cpp,v 1.9 2005/02/05 10:45:33 kommer Exp $


#include <acdk.h>
#include "Config.h"
#include "DeflateWriter.h"
#include "zlib/zlib.h"
#include <acdk/lang/Math.h>
#include <acdk/io/IOException.h>

namespace acdk {
namespace vfile {

namespace {
const int BufferSize = 1024;
}

using acdk::io::AbstractFilterWriter;

DeflateWriter::DeflateWriter(IN(::acdk::io::RWriter) out)
: AbstractFilterWriter(out)
, _initialized(false)
, _inbuf(BufferSize)
, _outbuf(BufferSize)
, _inpos(0)
{
  memset(&_zstream, 0, sizeof(_zstream));
}

DeflateWriter::~DeflateWriter()
{
  if (_initialized)
    deflateEnd(&_zstream);
}

void 
DeflateWriter::write(byte c)
{
  byte b[1];
  b[0] = c;
  write(b, 0, 1);
}


void 
DeflateWriter::write(const byte* cstr, int offset, int len)
{
  cstr += offset;
  while (len > 0) 
  {
    if (_inpos == BufferSize)
      flushBuffer(Z_NO_FLUSH);
    int towrite = Math::min(len, BufferSize - _inpos);
    memcpy(_inbuf.data() + _inpos, cstr, towrite);
    len -= towrite;
    _inpos += towrite;
    
  }
}

void 
DeflateWriter::write(IN(RbyteArray) ch, int offset/* = 0*/, int len/* = -1*/)
{
  if (len == -1)
    len = ch->length() - offset;
  write(ch->data(), offset, len);
}


void 
DeflateWriter::flush()
{
  flushBuffer(Z_SYNC_FLUSH);
  _out->flush();
}




void 
DeflateWriter::close()
{
  flushBuffer(Z_FINISH);
  _out->flush();
  _out->close();
}


void 
DeflateWriter::init()
{
  int erg = deflateInit(&_zstream, Z_DEFAULT_COMPRESSION);
  
  if (erg != Z_OK) 
    THROW1_FQ(acdk::io::, IOException, RCS("Initializing zlib failed: ") + zError(erg));
  _zstream.next_in = _inbuf.data();
  _zstream.avail_in = _inpos;
  
  _initialized = true;
}

void 
DeflateWriter::flushBuffer(int flushtype)
{
  if (_initialized == false)
    init();
  
  _zstream.next_in = _inbuf.data();
  _zstream.avail_in = _inpos;
  int erg = Z_OK;
  while ((_zstream.avail_in > 0 && Z_FINISH != flushtype) || 
        (Z_FINISH == flushtype && erg == Z_OK))
  {
    _zstream.next_out = _outbuf.data();
    _zstream.avail_out = BufferSize;
    erg = deflate( &_zstream, flushtype );
    if (erg != Z_OK && erg != Z_STREAM_END) {
      THROW1_FQ(acdk::io::, IOException, RCS("error in deflating zlib: ") + zError(erg));
    }
    if (BufferSize - _zstream.avail_out > 0)
      _out->write(_outbuf.data(), 0, BufferSize - _zstream.avail_out);
  }
  _inpos = 0;
}

} // vfile
} // acdk



