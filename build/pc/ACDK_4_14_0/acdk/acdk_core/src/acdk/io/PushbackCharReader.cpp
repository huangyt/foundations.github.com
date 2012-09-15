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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PushbackCharReader.cpp,v 1.8 2005/02/05 10:44:54 kommer Exp $
#include "PushbackCharReader.h"


namespace acdk {
namespace io {

void 
PushbackCharReader::unread(IN(RString) str)
{
  for (int i = str->length() - 1; i >= 0; --i)
    unread(str->charAt(i));
}

PushbackCharReaderImpl::PushbackCharReaderImpl(IN(RCharReader) in, int buffsize, IN(RObject) iolock)
: AbstractCharFilterReader(in, iolock)
, _buffer(new (allocator()) uccharArray(buffsize))
, _pos(buffsize)
{
}

int 
PushbackCharReaderImpl::readChar()
{
  if (_pos == _buffer->length())
    return _in->readChar();
 //SYNCTHIS();
  ++_pos;
  int ret = _buffer[_pos  - 1];
  return ret;
}

RString 
PushbackCharReaderImpl::readString()
{
  StringBuffer sb;
  int c;
  while ((c  = readChar()) != -1)
    sb.append((ucchar)c);
  return sb.toString();
}

void 
PushbackCharReaderImpl::unread(ucchar ch)
{
  if (_pos == 0)
    THROW1(IOException, "Pushback buffer is full");
  //SYNCTHIS();
  --_pos;
  _buffer[_pos] = ch;
}

void 
PushbackCharReaderImpl::unread(IN(RString) str)
{
  if (_pos < str->length())
    THROW1(IOException, "Pushback buffer is too small");
  for (int i = str->length() - 1; i >= 0; i--, _pos--) 
  {
    _buffer[_pos - 1] = str->charAt(i);
  }
}


} // io
} // acdk



