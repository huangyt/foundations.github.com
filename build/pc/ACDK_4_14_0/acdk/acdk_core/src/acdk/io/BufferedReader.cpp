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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BufferedReader.cpp,v 1.13 2005/03/08 12:45:35 kommer Exp $



#include <acdk.h>
#include <acdk/lang/System.h>
#include "BufferedReader.h"
#include "Storage.h"
#include "EOFException.h"

namespace acdk {
namespace io {
    
using namespace acdk::lang;
    
    //static 
int BufferedReader::DEFAULT_BUFFER_SIZE = 512;
    
BufferedReader::BufferedReader(IN(RReader) in, int buffsize) 
: AbstractFilterReader(in),
  _buffer(new (allocator()) byteArray(buffsize)),
  _count(0),
  _pos(0),
  _markpos(-1),
  _marklimit(0),
  _markbuf(),
  _markbufpos(0),
  _markbufcount(0),
  _doing_reset(false),
  _primed(false),
  _eof(false)
{ 
}

//virtual 
int 
BufferedReader::available() 
{ 
  return (_count - _pos) + _in->available();
}

//virtual 
void 
BufferedReader::mark(int readlimit)
{
  SYNCTHIS();
  if (_doing_reset && (_markbuf != Nil)) {
    RbyteArray tmpbuf(new (allocator()) byteArray(readlimit + _buffer->length()));
    if (_pos != _count)
      System::arraycopy(_buffer, _pos, tmpbuf, 0, _count - _pos);
    int copy_bytes = readlimit;
    if ((_markbufcount - _markbufpos) <= readlimit)
      copy_bytes = _markbufcount - _markbufpos;
    System::arraycopy(_markbuf, _markbufpos, tmpbuf, _count - _pos, copy_bytes);
    _primed = false;
    _markbuf = tmpbuf;
    _markbufpos = 0;
    _markbufcount = copy_bytes + (_count - _pos);
    _pos = 0;
    _count = 0;
  }
  if ((readlimit <= _buffer->length()) && (readlimit > (_count - _pos)) && (_pos != _count) && !_doing_reset) {
    RbyteArray tmpbuf(new (allocator()) byteArray(_buffer->length()));
    System::arraycopy(_buffer, _pos, tmpbuf, 0, _count - _pos);
    _buffer = tmpbuf;
    _count = _count - _pos;
    _pos = 0;
  }
  _markpos = _pos;
  _marklimit = readlimit;
  if (_in->markSupported())
    _in->mark(readlimit);
  
}

//virtual
void
BufferedReader::reset() 
{
  if (_markpos == -1)
    throw new IOException("Stream not marked");
  
  SYNCTHIS();
  _doing_reset = false;
  
  if (_markbuf == Nil) {
    _pos = _markpos;  
    _markpos = -1;
  } else {
    _markpos = -1;
    if (_in->markSupported()) {
      _in->reset();
      if (_markbuf != Nil)
      {
        System::arraycopy(_markbuf, 0, _buffer, 0, _markbuf->length());
        _pos = 0;
        _count = _markbuf->length();
        _markbuf = Nil;
      }
    } else {
      _pos = 0;
      _count = 0;
      _markbufpos = 0;
      _primed = false;
      _doing_reset = true;
    }
  }
}

//virtual 
bool  
BufferedReader::ready()
{
  if (((_count - _pos) > 0) || _in->ready())
    return true;
  return false;
}

jlong 
BufferedReader::skip(jlong num_bytes) 
{
  if (num_bytes <= 0)
    return 0;
  SYNCTHIS();

  if (jlong(_count - _pos) >= num_bytes) {
    _pos += num_bytes;
    return num_bytes;
  }

  jlong bytes_discarded = _count - _pos;
  _pos = 0;
  _count = 0;

  jlong bytes_skipped;

  jlong dif = num_bytes - bytes_discarded;
#ifdef __BORLANDC__ // BC6 BUG
  Reader& r = *_in;
  bytes_skipped = r.skip(dif);
#else
  bytes_skipped = _in->skip(dif);
#endif
  return bytes_discarded + bytes_skipped;
}


//virtual
int
BufferedReader::read()
{
  SYNCTHIS();
  if ((_pos == _count) || _primed == false) 
  {
    if (_eof == true)
     THROW1(EOFException, getStorage()->getDeviceName());
    _fillBuffer();
    if (_pos == _count) {
      _eof = true;
      return -1;
    }
  }
  ++_pos;
  return (unsigned char)_buffer[_pos - 1];
}

//virtual 
int 
BufferedReader::read(IN(RbyteArray) buffer, int offset, int len) 
{
  if (len == -1)
     len = buffer->length() - offset;
  if (len == 0)
    return 0;
  
  SYNCTHIS();
  int readed = read();
  if (readed == -1) {
    _eof = true;
    return -1;
  }
  buffer[offset] = (byte)readed;
  int total = 1;
  if (len == total)
    return total;
  
  // Read the rest of the bytes
  try {
    while (total != len) {
      if (_pos == _count)
        _fillBuffer();
      if (_pos == _count)
        if (total == 0) {
          _eof = true;
          return -1;
        } else
          return total;
        
      if ((len - total) <= (_count - _pos)) {
        System::arraycopy(_buffer, _pos, buffer, offset + total, len - total);

        _pos += (len - total);
        total += (len - total);
      } else {
        System::arraycopy(_buffer, _pos, buffer, offset + total, _count - _pos);
          
        total += (_count - _pos);
        _pos += (_count - _pos);
      }
    }
  } catch (RIOException) {
    return total;
  }
  return total ;
} 


void
BufferedReader::_fillBuffer() 
{
  _primed = true;
  if (_doing_reset) {
    if ((_markbufcount - _markbufpos) <= _buffer->length()) {
      System::arraycopy(_markbuf, _markbufpos, _buffer, 0, _markbufcount - _markbufpos);
      _pos = 0;
      _count = _markbufcount - _markbufpos;
      _markbuf = Nil;
      _doing_reset = false;
      return;
    } 
    System::arraycopy(_markbuf, _markbufpos, _buffer, 0, _buffer->length());
    _pos = 0;
    _count = _buffer->length();
    _markbufpos += _buffer->length();
    return;
  }
  
  
  if ((_markpos != -1) && (_markbuf == Nil)) {
    if (_in->markSupported())
      _markbuf = new (allocator()) byteArray(_count - _markpos);
    else
      _markbuf = new (allocator()) byteArray(_marklimit + _buffer->length());
    System::arraycopy(_buffer, _markpos, _markbuf, 0, _count - _markpos);
    _markbufpos = 0;
    _markbufcount = _count - _markpos;
  }
  int readed;
  readed = _in->read(_buffer, 0, _buffer->length() /* - 1*/);
  
  if (readed == -1) {
    _pos = 0;
    _count = 0;
    _eof = true;
    return;
  }
  
  _pos = 0;
  _count = readed;
  if ((_markbuf != Nil) && (_markbufcount >= (_markbuf->length() - _buffer->length())) &&
    !_in->markSupported()) {
    _markbuf = Nil;
    _markpos = -1;
  }
  if ((_markbuf != Nil) && !_in->markSupported()) {
    int len;
    if (readed > (_markbuf->length() - _markbufcount))
      len = _markbuf->length() - _markbufcount;
    else
      len = readed;
    
    System::arraycopy(_buffer, 0, _markbuf, _markbufcount, len);
    _markbufcount += len;
  }
}
  
      
} // io
} // acdk
