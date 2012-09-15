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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PushbackReader.cpp,v 1.12 2005/03/08 12:45:36 kommer Exp $




#include <acdk.h>

#include "PushbackReader.h"
#include <acdk/lang/System.h>

#include "IOException.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

PushbackReader::PushbackReader(IN(RReader) in, int buffsize)
: AbstractFilterReader(in),
  _buffer(new (allocator()) byteArray(buffsize)),
  _pos(buffsize)
{
}

//virtual 
bool  
PushbackReader::ready()
{
  if (((_buffer->length() - _pos) > 0) || _in->ready())
    return true;
  return false;
}

//virtual 
jlong 
PushbackReader::skip(jlong num_bytes)
{
  if (num_bytes <= 0)
    return 0;
  SYNCTHIS();
  if ((_buffer->length() - _pos) >= (int)num_bytes) {
    _pos += num_bytes;
    return num_bytes;
  }

  jlong bytes_discarded = _buffer->length() - _pos;
  _pos = _buffer->length();
#ifdef __BORLANDC__ // BC6 compiler error!
  Reader& r = *_in;
  jlong bytes_skipped = r.skip(num_bytes - bytes_discarded);
#else
  jlong bytes_skipped = _in->skip(num_bytes - bytes_discarded);
#endif
  return bytes_discarded + bytes_skipped;
}

//virtual 
int 
PushbackReader::read() 
{
  if (_pos == _buffer->length())
    return _in->read();
 
  SYNCTHIS();
  ++_pos;
  int ret = _buffer[_pos  - 1];
  return ret;
}

//virtual
int
PushbackReader::read(IN(RbyteArray) buf, int offset, int len)
{
  if (len == -1)
    len = buf->length() - offset;
  if (len == 0)
    return 0;

  SYNCTHIS();
 
  int byte_read = read();
  if (byte_read == -1)
    return -1;
  buf[offset] = (byte)byte_read;
  if (len == 1)
    return 1;
  
  int total_read = 1;
  
  // Grab bytes from pushback buffer if available
  if (_pos != _buffer->length()) {
    int desired_bytes = 0;
    if ((_buffer->length() - _pos) >= (len - total_read))
      desired_bytes = len - total_read;
    else
      desired_bytes = _buffer->length() - _pos;
    
    System::arraycopy(_buffer, _pos, buf, offset + total_read, desired_bytes);
    
    total_read += desired_bytes;
    _pos += desired_bytes;
  }
  
  // Read from underlying stream if we still need bytes
  if (total_read != len) {
    int bytes_read = 0;
    try {
      bytes_read = _in->read(buf, offset + total_read, len - total_read);
    } catch(RIOException ex) {
      return total_read;
    }
    
    if (bytes_read == -1)
      return total_read;
    
    total_read += bytes_read;
  }
  
  return total_read;
}

//virtual 
void 
PushbackReader::unread(int b) 
{
  if (_pos == 0)
    THROW1(IOException, "Pushback buffer is full");
  SYNCTHIS();
  --_pos;
  _buffer[_pos] = (byte)b;
}

//virtual 
void 
PushbackReader::unread(IN(RbyteArray) ch)
{
  
  if (_pos < ch->length())
    THROW1(IOException, "Pushback buffer is too small");
  
  for (int i = ch->length() - 1; i >= 0; i--, _pos--) 
  {
    _buffer[_pos - 1] = ch[i];
  }
  
}


} // io
} // acdk




