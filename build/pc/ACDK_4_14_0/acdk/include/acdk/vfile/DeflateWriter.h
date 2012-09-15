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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/DeflateWriter.h,v 1.8 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_vfile_DeflateWriter_h
#define acdk_vfile_DeflateWriter_h

#include <acdk.h>
#include "Config.h"
#include <acdk/io/AbstractFilterWriter.h>

#include "zlib/zlib.h"

namespace acdk {
namespace vfile {


ACDK_DECL_CLASS(DeflateWriter);

class ACDK_VFILE_PUBLIC DeflateWriter
: extends acdk::io::AbstractFilterWriter
{
private:
  z_stream _zstream;
  bool _initialized;
  ::acdk::lang::sys::core_vector<byte> _inbuf;
  ::acdk::lang::sys::core_vector<byte> _outbuf;
  /** position of inbuf */
  int _inpos;
  
public:
  DeflateWriter(IN(::acdk::io::RWriter) in);
  ~DeflateWriter();
  foreign virtual void write(byte c);
  foreign virtual void write(const byte* cstr, int offset, int len);
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
  foreign virtual void flush();
  foreign virtual void close();
protected:
  void init();
  void flushBuffer(int flushflag);
};

} // vfile
} // acdk



#endif //acdk_vfile_DeflateWriter_h

