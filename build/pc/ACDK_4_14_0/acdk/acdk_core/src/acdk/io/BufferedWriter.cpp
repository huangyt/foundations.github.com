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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BufferedWriter.cpp,v 1.13 2005/02/05 10:44:53 kommer Exp $




#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/IllegalArgumentException.h>

#include "BufferedWriter.h"


namespace acdk {
namespace io {

//static 
int BufferedWriter::DEFAULT_BUFFER_SIZE = 512;

BufferedWriter::BufferedWriter(IN(RWriter) writer, int buffsize/* = BufferedWriter::DEFAULT_BUFFER_SIZE*/)
: AbstractFilterWriter(writer)
, _buffer(new (allocator()) byteArray(buffsize))
, _bufferLength(0)
, _overflowed(false)
{
  if (buffsize <= 0)
    THROW1(IllegalArgumentException, "buffzize must be creater than 0");
}

BufferedWriter::~BufferedWriter()
{
  overflow();
}

//virtual 
void
BufferedWriter::flush() 
{
  if (_bufferLength == 0)
    return;
  SYNCTHIS();
  overflow();
  _out->flush();
}

//virtual 
void 
BufferedWriter::close()
{
  SYNCTHIS();
  overflow();
  if (_out != Nil)
  {
    _out->close();
  }
}

void 
BufferedWriter::overflow()
{
  if (_bufferLength == 0)
    return;
  _overflowed = true;
  _out->write(_buffer, 0, _bufferLength);
  _bufferLength = 0;
}

//virtual 
void 
BufferedWriter::write(byte b)
{
  SYNCTHIS();
  _buffer[_bufferLength] = b;
  ++_bufferLength;
  if (_bufferLength == _buffer->length())
    overflow();
}

//virtual 
void
BufferedWriter::write(const byte* buf, int offset, int len)
{
  if (len == 0)
    return;
  SYNCTHIS();
  if (len == -1)
    len = strlen((const char*)buf) - offset;
  //buf += offset;

  if (len < (_buffer->length() - _bufferLength)) {
    System::arraycopy(buf, offset, _buffer, _bufferLength, len);
    _bufferLength += len;
    if (_bufferLength == _buffer->length())
      overflow();
  } else {
    flush();
    _out->write(buf, offset, len);
    
    /*
      maybe write bufsize chunks first, and rest (modulo into buffer)
    */
    /*
    int i = 0;
    // does not work ### should be fixed later
    if ((len / _buffer->length()) != 0) {
      for (i = 0; i < (len / _buffer->length()); i++) {
        _out->write(buf, offset + (i * _buffer->length()), _buffer->length());
        len -= _buffer->length();
      }
    }
    if ((len % _buffer->length()) != 0) { // has rest
      System::arraycopy(buf, offset + (i * _buffer->length()), _buffer, _bufferLength,  len - (i * _buffer->length()));
      _bufferLength += (len - (i * _buffer->length()));
    }*/
  }
}

void 
BufferedWriter::setBufferSize(int newSize)
{
  if (newSize < _buffer->length())
    overflow();
  _buffer->resize(newSize);
}

} // io
} // acdk

