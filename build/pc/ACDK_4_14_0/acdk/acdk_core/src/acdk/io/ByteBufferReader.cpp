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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ByteBufferReader.cpp,v 1.2 2005/04/05 21:44:14 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Math.h>
#include "ByteBufferReader.h"


namespace acdk {
namespace io {

jlong 
BytePtrReader::seek(SeekPos seekrel, jlong seekpos)
{
  if (seekrel == SeekCur)
  {
    if (_ptr + seekpos >= _end)
      _ptr = _end;
    else
      _ptr += seekpos;
  }
  else if (seekrel == SeekEnd)
  {

    if (seekpos > (_end - _begin))
      _ptr = _begin;
    else
      _ptr = _end - seekpos;
  }
  else // SeekBegin 
  {
    if (_begin + seekpos > _end)
      _ptr = _end;
    else if (seekpos < 0)
      _ptr = _begin;
    else
      _ptr = _begin + seekpos;

  }
  return _ptr - _begin;
}

jlong 
BytePtrReader::skip(jlong n)
{
  const byte* oit = _ptr;
  seek(SeekCur, n);
  return _ptr - oit;
}

int 
BytePtrReader::read(IN(RbyteArray) buffer, int offset, int len) 
{
  byte* data = buffer->data() + offset;
  if (len == -1)
    len = buffer->length() - offset;
  return read(data, 0, len);
}

int 
BytePtrReader::read(byte* buffer, int offset, int len)
{
  byte* d = buffer + offset;
  int size = Math::min(len, available());
  memcpy(d, _ptr, size);
  _ptr += size;
  return size;
}

jlong 
ByteBufferReader::seek(SeekPos seekrel, jlong seekpos)
{
  if (seekrel == SeekCur)
  {
    if (_curPos + seekpos < _buffer->length())
      _curPos = _buffer->length();
    else
      _curPos += seekpos;
  }
  else if (seekrel == SeekEnd)
  {
    if (seekpos > _buffer->length())
      _curPos = 0;
    else
      _curPos = _buffer->length() - seekpos;
  }
  else 
  {
    if (seekpos > _buffer->length())
      _curPos = _buffer->length();
    else if (seekpos < 0)
      _curPos = 0;
    else
      _curPos = seekpos;
  }
  return _curPos;
}

jlong 
ByteBufferReader::skip(jlong n)
{
  int sicpos = _curPos;
  seek(SeekCur, n);
  return _curPos - sicpos;
}
 
int 
ByteBufferReader::read()
{
  if (_curPos > _buffer->length())
    THROW0(EOFException);
  if (_curPos == _buffer->length())
    return -1;
  byte b = _buffer->get(_curPos);
  
  ++_curPos;
  return b;
}

void 
ByteBufferReader::reset() 
{ 
  if (_mark == -1)
    THROW1(IOException, "ByteBufferReader: mark was not set");
  _curPos = _mark; 
}

int 
ByteBufferReader::read(IN(RbyteArray) buffer, int offset, int len) 
{ 
  byte* data = buffer->data() + offset;
  if (len == -1)
    len = buffer->length() - offset;
  return read(data, 0, len);
}

int 
ByteBufferReader::read(byte* buffer, int offset, int len) 
{
  byte* d = buffer + offset;
  int size = Math::min(len, available());
  for (int i = 0; i < size; ++i)
    *d++ = _buffer->get(_curPos++);
  return size;
}


} // io
} // acdk


