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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/LineNumberCharReader.cpp,v 1.8 2005/02/05 10:44:54 kommer Exp $


#include "LineNumberCharReader.h"


namespace acdk {
namespace io {

LineNumberCharReaderImpl::LineNumberCharReaderImpl(IN(RCharReader) in, IN(RObject) iolock)
: AbstractCharFilterReader(in, iolock)
, _charPos(0)
, _markedCharPos(0)
, _lineNumber(0)
, _markedLineNumber(0)
, _xPos(0)
, _markedXPos(0)
, _extra_char(-1)
{
  //_in = new PushbackReader(new BufferedReader(in, size));
  _in = in;
}

int 
LineNumberCharReaderImpl::readChar()
{
  if (_extra_char != -1)
  {
    ++_charPos;
    ++_xPos;
    int ret = _extra_char;
    _extra_char = -1;
    return ret;
  }
  int c = _in->readChar();
  if (c == -1)
    return -1;
  ++_charPos;
  if (c == '\n') 
  { 
    ++_lineNumber;
    _xPos = 0;
    return c;
  }
  else if (c == '\r') 
  {
    int _extra_char = _in->readChar();
    if (_extra_char == -1)
      return -1;
    if (_extra_char == '\n')
    {
      ++_lineNumber;
      _xPos = 0;
      _extra_char = -1;
      return '\n';
    }
    return c;
  }
  else
  {
    ++_xPos;
    return c;
  }
}
RString 
LineNumberCharReaderImpl::readString()
{
  StringBuffer sb;
  int c;
  while ((c = readChar()) != -1)
    sb.append((ucchar)c);
  return sb.toString();
}
RString 
LineNumberCharReaderImpl::readLine()
{
  StringBuffer sb;
  int c;
  int sicline = _lineNumber;
  while ((c = readChar()) != -1)
  {
    sb.append((ucchar)c);
    if (_lineNumber > sicline)
      return sb.toString();
  }
  return sb.toString();
}


} // io
} // acdk



