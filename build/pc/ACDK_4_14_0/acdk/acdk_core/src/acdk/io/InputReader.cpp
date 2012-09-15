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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/InputReader.cpp,v 1.18 2005/03/14 12:20:44 kommer Exp $





#include <acdk.h>
#include "InputReader.h"
#include "File.h"
#include "EOFException.h"
#include "ByteToCharReader.h"
#include <acdk/lang/StringBuffer.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/System.h>
#include <acdk/lang/UnsupportedOperationException.h>


namespace acdk {
namespace io {



InputReader::InputReader(IN(RReader) in, IN(acdk::locale::RDecoder) decoder, IN(RObject) lock) 
: AbstractCharFilterReader(new ByteToCharReader(in, decoder, lock))
, _isEof(false)
, _eofReturned(false)
, _skipNextNl(false)

{ 
}

InputReader::InputReader(IN(RCharReader) in)
: AbstractCharFilterReader(in)
, _isEof(false)
, _eofReturned(false)
, _skipNextNl(false)
{

}

/*
//foreign virtual 
int InputReader::read() 
{ 
  if (_eofReturned == true)
    THROW0(EOFException);
  int i = _in->read(); 
  if (i == -1)
  {
    _isEof = _eofReturned = true;
  }
  return i;
}

//foreign virtual 
int 
InputReader::read(IN(RbyteArray) buffer, int offset, int len) 
{ 
  if (_eofReturned == true)
    THROW0(EOFException);
  return AbstractFilterReader::read(buffer, offset, len); 
}

//foreign virtual 
int 
InputReader::read(byte* buffer, int offset, int len) 
{ 
  if (_eofReturned == true)
    THROW0(EOFException);
  return AbstractFilterReader::read(buffer, offset, len); 
}
*/
/*
RString 
InputReader::readString() 
{
  if (_eofReturned == true)
    THROW0(EOFException);
  StringBuffer sb(1024);
  int c;
  while ((c = _in->read()) != -1)
  {
    sb.append((ucchar)c);
  }
  if (c == -1)
  {
    _isEof = true;
    if (sb.length() == 0) 
    {
      _eofReturned = true;
      return Nil;
    }
  }
  return sb.toString();
}
*/

RString 
InputReader::readAString()
{
  if (_eofReturned == true)
    THROW0(EOFException);
  StringBuffer sb(1024);
  int c;
  bool nonspacereaded = false;
  while ((c = _in->readChar()) != -1) {
    if (Character::isSpace((ucchar)c) == true)
    {
      if (nonspacereaded == true)
        break;
    } else
      nonspacereaded = true;
    sb.append((ucchar)c);
  }
  if (c == -1)
  {
    _isEof = true;
    if (sb.length() == 0) 
    {
      _eofReturned = true;
      return Nil;
    }
  }
  return sb.toString();
}


RString
InputReader::readLine()
{
  // ### SYNCTHIS();
  if (_eofReturned == true)
    THROW0(EOFException);
  if (_isEof == true)
  {
    _eofReturned = true;
    return Nil;
  }
  StringBuffer sb;
  
  RString eol = File::endOfLine();
  int linefeed = '\n';
  int caragereturn = '\r';
  int ch = -1;
  do {
    ch = _in->readChar();
    //acdk::lang::System::out->println(SBSTR("<< " << (int)ch << "," << char(ch)));
    if (ch == -1) 
    {
      _isEof = true;
      if (sb.length() == 0)
      {
        _eofReturned = true;
        return Nil;
      }
      return sb.toString();
    }
    if (ch == '\n') 
    {
      if (_skipNextNl == true)
      {
        _skipNextNl = false;
        continue;
      }
      return sb.toString();
    }
    _skipNextNl = false;
    if (ch == '\r') 
    {
      _skipNextNl = true;
      return sb.toString();
    }
    sb.append((ucchar)ch);
  } while (true);
  return sb.toString();
}


int
InputReader::readInt()
{
  if (_eofReturned == true)
    THROW0(EOFException);
  StringBuffer sb(100);
  int c;
  while ((c = _in->readChar()) != -1) 
  {
    if (Character::isSpace((ucchar)c) == true)
      break;
    sb.append((ucchar)c);
  }
  if (c == -1)
  {
    _isEof = true;
    if (sb.length() == 0) 
    {
      THROW0(EOFException);
    }
  }
  return Integer::parseInt(sb.toString());
}



bool
InputReader::readBoolean()
{
  if (_eofReturned == true)
    THROW0(EOFException);
  StringBuffer sb(100);
  int c;
  while ((c = _in->readChar()) != -1) 
  {
    if (Character::isSpace((ucchar)c) == true)
      break;
    sb.append((ucchar)c);
  }
  if (c == -1)
  {
    _isEof = true;
    if (sb.length() == 0) 
    {
      THROW0(EOFException);
    }
  }
  return Boolean::getBoolean(sb.toString());

}



double
InputReader::readDouble()
{
  if (_eofReturned == true)
    THROW0(EOFException);
  StringBuffer sb(100);
  int c;
  while ((c = _in->readChar()) != -1) {
    if (Character::isSpace((ucchar)c) == true)
      break;
    sb.append((ucchar)c);
  }
  if (c == -1)
  {
    _isEof = true;
    if (sb.length() == 0) 
    {
      THROW0(EOFException);
    }
  }
  return Double::parseDouble(sb.toString());
}



jlong
InputReader::readLong()
{
  if (_eofReturned == true)
    THROW0(EOFException);
  StringBuffer sb(100);
  int c;
  while ((c = _in->readChar()) != -1) 
  {
    if (Character::isSpace((ucchar)c) == true)
      break;
    sb.append((ucchar)c);
  }
  if (c == -1)
  {
    _isEof = true;
    if (sb.length() == 0) 
    {
      THROW0(EOFException);
    }
  }
  return Long::parseLong(sb.toString());
}




int
InputReader::skipBytes(int len)
{
  if (_eofReturned == true)
    THROW0(EOFException);
  int i = 0;
  int c;
  while (i < len && (c = _in->readChar()) != -1) 
  {
    i++;
  }
  if (c == -1)
  {
    _isEof = true;
  }
  return i;
}


RString 
InputReader::readQuoted()
{
  if (_eofReturned == true)
    THROW0(EOFException);
  int c;
  while ((c = _in->readChar()) != -1) 
    if (Character::isSpace((ucchar)c) == false)
      break;
  if (c == -1)
    _isEof = true;
  
  StringBuffer sb;
  if (c != '\"')
  {
    sb.append((ucchar)c);
    return sb.toString();
  }
  while ((c = _in->readChar()) != -1) 
  {
    if (c == '\"')
      return sb.toString();
    if (c == '\\')
    {
      c = _in->readChar();
      if (c == 'n')
        sb.append("\n");
      else if (c == 't')
        sb.append("\t");
      else if (c == '"')
        sb.append("\"");
      else if (c == '\\')
        sb.append("\\");
      else
        sb.append(ucchar(c));
    } else
      sb.append(ucchar(c));
  }
  if (c == -1)
    _isEof = true;
  return sb.toString();
}
    
} // io
} // acdk
