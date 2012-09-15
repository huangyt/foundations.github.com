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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/InflaterReader.h,v 1.8 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_vfile_InflaterReader_h
#define acdk_vfile_InflaterReader_h

#include <acdk.h>
#include "Config.h"
#include <acdk/io/AbstractFilterReader.h>

#include "zlib/zlib.h"

namespace acdk {
namespace vfile {


ACDK_DECL_CLASS(InflaterReader);

class ACDK_VFILE_PUBLIC InflaterReader
: extends acdk::io::AbstractFilterReader
{
private:
  z_stream _zstream;
  bool _initialized;
  ::acdk::lang::sys::core_vector<byte> _inbuf;
  ::acdk::lang::sys::core_vector<byte> _outbuf;
  int _pos;
  /** how many are availabe in _outbuf */
  int _available;
public:
  InflaterReader(IN(::acdk::io::RReader) in);
  ~InflaterReader();
  foreign virtual int available();
  foreign virtual void close();
  foreign virtual jlong seek(acdk::io::SeekPos seekrel, jlong seekpos);
  foreign virtual jlong skip(jlong n);
  foreign virtual int read();
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1) 
  {
    return read(buffer->data(), offset, len);
  }
  foreign virtual int read(byte* buffer, int offset, int len);
  foreign virtual void reset()
  {
    _pos = -1;
    _in->reset();
  }
  foreign virtual bool ready()
  {
    return _in->ready();
  }
  foreign virtual void mark(int readAheadLimit)
  {
    THROW0(UnsupportedOperationException);
  }
  foreign virtual bool markSupported()
  { 
    return false;
  }
protected:
  void init();
  void fillBuffer();
};

} // vfile
} // acdk



#endif //acdk_vfile_InflaterReader_h

