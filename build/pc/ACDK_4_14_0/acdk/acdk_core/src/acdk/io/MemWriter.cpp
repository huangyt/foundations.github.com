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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/MemWriter.cpp,v 1.15 2005/03/18 20:22:09 kommer Exp $





#include <acdk.h>
#include "MemWriter.h"

namespace acdk {
namespace io {
using namespace acdk::lang;

int MemWriter::DefaultBufferSize = 256;
int MemWriter::IncrementBufferSize = 256;



//virtual 
void 
MemWriter::write(const byte* cstr, int offset, int len)
{
  if (cstr == 0)
    return;
  int writepos = _buffer->length();
  ensureCapacity(writepos + len  + 1);
  _buffer->resize(writepos + len);
  cstr += offset;
  while (len--) 
    _buffer[writepos++] = *cstr++;
}
  
RcharArray 
MemWriter::getBufferAsChars()
{
  RcharArray ca = new charArray(_buffer->length());

  byteArray::iterator it = _buffer->begin();
  byteArray::iterator end = _buffer->end();
  charArray::iterator cit = ca->begin();
  charArray::iterator cend = ca->end();
  
  for (int i = 0; it != end; ++it, ++cit)
  {
    *cit = (char)*it;
  }
  return ca;
}

} // io
} // acdk
