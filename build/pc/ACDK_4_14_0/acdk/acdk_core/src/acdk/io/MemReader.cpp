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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/MemReader.cpp,v 1.19 2005/03/08 12:45:36 kommer Exp $





#include <acdk.h>
#include <acdk/lang/IndexOutOfBoundsException.h>
#include "MemReader.h"
#include "MemWriter.h"
#include "EOFException.h"


namespace acdk {
namespace io {

using namespace acdk::lang;

MemReader::MemReader(IN(RMemWriter) memwriter, int offset/* = 0*/, int endpos/* = -1*/)
: AbstractStorageReader()
, _buffer(memwriter->getBuffer())
, _seekPos(offset)
, _endPos(endpos)
{
 
}

//virtual 
jlong 
MemReader::seek(acdk::io::SeekPos seekrel, jlong seekpos)
{
  jlong newSeekPos = _seekPos;
  jlong oldSeekPos = _seekPos;
  if (seekrel == SeekCur)
    newSeekPos += seekpos;
  else if (seekrel == SeekEnd)
    newSeekPos = endPos() - seekpos;
  else 
    newSeekPos = seekpos;
  if (newSeekPos < 0 || newSeekPos >= (jlong)endPos())
    THROW1(IOException, "MemReader: seek out of index");
  _seekPos = newSeekPos;
  return _seekPos;
}

jlong 
MemReader::skip(jlong n) 
{ 
  jlong scp = _seekPos;
  return seek(acdk::io::SeekCur, n) - scp; 
}
//virtual 
int 
MemReader::read()
{
  if (_seekPos == endPos()) {
    ++_seekPos;
    return -1;
  }
  if (_seekPos > endPos())
    THROW0(EOFException);

  return int(_buffer[_seekPos++]);
}

//virtual 
int 
MemReader::read(IN(RbyteArray) cb, int offset, int len)
{
  if (len == -1)
    len = cb->length() - offset;
  return read(cb->data(), offset, len);
}

//virtual 
int 
MemReader::read(byte* cb, int offset, int len)
{
  if (_seekPos == endPos()) 
  {
    ++_seekPos;
    return 0;
  }
  if (_seekPos > endPos())
    THROW0(EOFException);

  if (endPos() - _seekPos < len) {
    len = endPos() - _seekPos;
  } 

  for (int i = offset; i < len; i++) 
    cb[i] = _buffer[_seekPos++];
  
  return len;
}

//virtual 
void 
MemReader::reset()
{
  
}

//virtual 
bool 
MemReader::ready()
{
  return available() > 0;
}

} // io
} // acdk
