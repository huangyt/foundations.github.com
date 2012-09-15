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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/LineNumberReader.cpp,v 1.15 2005/03/08 12:45:36 kommer Exp $




#include <acdk.h>
#include "LineNumberReader.h"
#include "PushbackReader.h"
#include "BufferedReader.h"
#include "EOFException.h"

namespace acdk {
namespace io {

using namespace acdk::lang;


LineNumberReader::LineNumberReader(IN(RReader) in, IN(RObject) lock)
: AbstractFilterReader(in, lock)
, _charPos(0)
, _markedCharPos(0)
, _lineNumber(0)
, _markedLineNumber(0)
, _xPos(0)
, _markedXPos(0)
, _eof(0)
{
}

//virtual 
void 
LineNumberReader::mark(int readlimit) 
{
  SYNCTHIS();
  _in->mark(readlimit);
  _markedLineNumber = _lineNumber;
  _markedCharPos = _charPos;
  _markedXPos = _xPos;
}

//virtual 
void 
LineNumberReader::reset()
{
  SYNCTHIS();
  _in->reset();
  _lineNumber = _markedLineNumber;
  _charPos = _markedCharPos;
  _xPos = _markedXPos;
}

int 
LineNumberReader::_read()
{
  ++_xPos;
  ++_charPos;
  if (_eof == 2)
    THROW0(EOFException);
  int c = _in->read();
  if (c == -1)
    _eof = 2;
  return c;
}



//virtual 
int 
LineNumberReader::read() 
{
  SYNCTHIS();
  if (_eof == 2)
    THROW0(EOFException);
  if (_eof == 1)
  {
    _eof = 2;
    return -1;
  }
  int byte_read = _read();
  if (byte_read == -1)
     return byte_read;
  if (byte_read == '\n') { 
    ++_lineNumber;
    _xPos = 0;
  } else if (byte_read == '\r') {
    int extra_byte_read = _read();
    if ((extra_byte_read != '\n') && (extra_byte_read != -1)) {
      if (instanceof(_in, PushbackReader)) {
        ((RPushbackReader)_in)->unread(extra_byte_read);
        --_xPos;
      } else {
        return extra_byte_read;
      }
    }
    byte_read = '\n';
    if (extra_byte_read  == -1)
      return extra_byte_read;
    ++_lineNumber;
  }
  return byte_read;
}


//virtual 
int 
LineNumberReader::read(IN(RbyteArray) buffer, int offset, int len)
{
  if (len == -1)
    len = buffer->length() - offset;
  if (len == 0)
    return 0;
  
  SYNCTHIS();
  if (_eof == 2)
    THROW0(EOFException);
  int byte_read = _read();
  if (byte_read == -1)
  {
    _eof = 2;
    return -1;
  }
  //buffer[offset] = (byte)byte_read;
  buffer->set(offset, (byte)byte_read);
  int total_read = 1;
  
  try {
    for (int i = 1; i < len; i++) {
      byte_read = _read();
      if (byte_read == -1)
      {
        _eof = 1;
        return total_read;
      }
      buffer[offset + i] = (byte)byte_read;
      ++total_read;  
    }
  } catch (RIOException ex) {

    return total_read;
  }
  return total_read;
}
/*
//virtual 
RString 
LineNumberReader::readLine()
{
  SYNCTHIS();
   if (_eof == 2)
    THROW0(EOFException);
   if (_eof == 1)
   {
     _eof = 2;
     return Nil;
   }
  int c;
  StringBuffer sb;
  _xPos  = 0;
  while ((c = readChar()) != -1)
  {
    ++_charPos;
    if (c == '\n')
    {
      ++_lineNumber;
      return sb.toString();
    }
    else
    {
      sb.append((ucchar)c);
    }
  }
  if (c == -1)
    _eof = 1;
  if (sb.length() == 0)
    return Nil;
  return sb.toString();
}
*/

} // io
} // acdk
