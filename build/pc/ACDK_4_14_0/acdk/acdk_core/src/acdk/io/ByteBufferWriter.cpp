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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ByteBufferWriter.cpp,v 1.1 2005/04/05 21:34:40 kommer Exp $


#include <acdk.h>
#include "ByteBufferWriter.h"
#include "AbstractWriter.h"
#include "ByteBufferReader.h"


namespace acdk {
namespace io {

void 
BytePtrWriter::write(const byte* cstr, int offset, int len)
{
  cstr += offset;
  if (len > _end - _it)
    THROW0(EOFException);
  memcpy(_it, cstr, len);
  _it += len;
}

void 
BytePtrWriter::write(byte c)
{
  if (_it == _end)
    THROW0(EOFException);
  *_it = c;
  ++_it;
}

void 
BytePtrWriter::write(IN(RbyteArray) ch, int offset, int len)
{
  byte* d = ch->data() + offset;
  if (len == -1)
    len = ch->length() - offset;
  write(d, 0, len);
}

void 
ByteBufferWriter::write(const byte* cstr, int offset, int len)
{
  if (_curPos + len > _buffer->length())
    THROW0(EOFException);
  const byte* d = cstr + offset;
  for (int i = 0; i < len; ++i)
    _buffer->set(_curPos++, *d++);
}

void 
ByteBufferWriter::write(byte c)
{
  if (_curPos <= _buffer->length())
    THROW0(EOFException);
  _buffer->set(_curPos++, c);
}

void 
ByteBufferWriter::write(IN(RbyteArray) ch, int offset, int len)
{
  byte* d = ch->data() + offset;
  if (len == -1)
    len = ch->length() - offset;
  write(d, 0, len);
}



void 
ByteBufferAppendWriter::write(const byte* cstr, int offset, int len)
{
  const byte* d = cstr + offset;
  for (int i = 0; i < len; ++i)
    _buffer->append(*d++);
}

void 
ByteBufferAppendWriter::write(byte c)
{
  _buffer->append(c);
}

void 
ByteBufferAppendWriter::write(IN(RbyteArray) ch, int offset, int len)
{
  byte* d = ch->data() + offset;
  if (len == -1)
    len = ch->length() - offset;
  write(d, 0, len);
}


} // io
} // acdk


