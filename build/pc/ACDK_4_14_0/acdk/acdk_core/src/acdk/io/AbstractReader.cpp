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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractReader.cpp,v 1.14 2005/03/08 12:45:35 kommer Exp $




#include <acdk.h>
#include "AbstractReader.h"
#include "AbstractWriter.h"

#include <acdk/lang/ArrayIndexOutOfBoundsException.h>

namespace acdk {
namespace io {

int
AbstractReader::read(IN(RbyteArray) buffer, int offset, int len)
{
  if (len == -1)
    len = buffer->length() - offset;
  if (offset < 0 || len < 0 || offset > buffer->length() - 1 ||
      len + offset > buffer->length()/* - 1*/)
    THROW0(ArrayIndexOutOfBoundsException);
  return read(buffer->data(), offset, len);
}

int
AbstractReader::read(byte* buffer, int offset, int len)
{
  int i;
  int c;
  for (i = offset; i < len; i++) {
    c = read();
    if (c == -1)
      break;
    buffer[i] = (byte)c;
  }
  return i - offset;
}

//virtual 
void
AbstractWriter::write(const byte* cstr, int offset, int len)
{
  if (len == -1)
    len = strlen((const char*)cstr) - offset;
  const byte* ptr = cstr + offset;
  for (; len > 0; --len, ++ptr)
    write(*ptr);
}

} // io
} // acdk
