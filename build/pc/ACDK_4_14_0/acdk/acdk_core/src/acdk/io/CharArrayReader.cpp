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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharArrayReader.cpp,v 1.11 2005/02/05 10:44:53 kommer Exp $




#include <acdk.h>
#include "CharArrayReader.h"
#include "CharArrayWriter.h"

#include <acdk/lang/System.h>

namespace acdk {
namespace io {


int 
CharArrayReader::readChar()
{
  if (_pos >= _count)
    return -1;
  SYNCTHIS();
  ++_pos;
  return (unsigned int)_buffer[_pos - 1];
}

RString 
CharArrayReader::readString() 
{
  StringBuffer sb;
  int c;
  while ((c = readChar()) != -1)
    sb.append((ucchar)c);
  return sb.toString();
}

  /*

jlong 
CharArrayReader::seek(SeekPos seekrel, jlong seekpos)
{
  int newpos = 0;
  if (seekrel == SeekCur)
    newpos = _pos + seekpos;
  else if (seekrel == SeekEnd)
    newpos = _count - seekpos;
  else 
    newpos = seekpos;
  if (newpos < 0 && newpos >= _count)
    THROW1(IOException, "CharArrayReader::seek() out of stream");
  return _pos = newpos;
}
  
//virtual 
jlong 
CharArrayReader::skip(jlong num_bytes)
{
  if (num_bytes <= 0)
    return 0;
  SYNCTHIS();
  if (num_bytes > jlong(_count - _pos)) {
    int retval = _count - _pos;
    _pos = _count;
    return retval;
  }
  _pos += num_bytes;
  return num_bytes; 
}
  
//virtual 
int 
CharArrayReader::read()
{
  if (_pos >= _count)
    return -1;
  SYNCTHIS();
  ++_pos;
  return (unsigned int)(byte)_buffer[_pos - 1];
}  

//virtual 
int 
CharArrayReader::read(IN(RbyteArray) buf, int offset, int len)
{
  if (len == -1)
    len = buf->length() - offset;
  if (len == 0)
    return 0;
  if (_pos == _count)
    return -1;
  SYNCTHIS();
  if (len < (_count - _pos)) {
    System::arraycopy(_buffer, _pos, buf, offset, len);
    _pos += len;
    return len;
  }
  System::arraycopy(_buffer, _pos, buf, offset, _count - _pos);
  int retval = _count - _pos;
  _pos = _count;
  return retval;
}

//virtual 
int 
CharArrayReader::read(byte* buffer, int offset, int len)
{
  return AbstractReader::read(buffer, offset, len);
}
*/
} // namespace io 
} // namespace acdk 
