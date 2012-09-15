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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/JoinedReader.cpp,v 1.9 2005/03/08 12:45:36 kommer Exp $


#include <acdk.h>
#include "JoinedReader.h"
#include <acdk/io/EOFException.h>

namespace acdk {
namespace io {

using namespace acdk::lang;

class JoinedReader;
ACDK_DECL_CLASS(JoinedReader);

JoinedReader::JoinedReader(IN(RReader) firstIn, IN(RObject) lock/* = Nil*/)
: AbstractReader(lock),
_ins(new ReaderArray(firstIn == Nil ? 0 : 1)),
_curPos(0)
{
  if (firstIn != Nil)
    _ins[0] = firstIn;
}

//virtual 
JoinedReader::~JoinedReader()
{
  close();
}

//virtual 
int 
JoinedReader::read()
{
  if (_curPos == -1)
    return -1;
  int ret = _ins[_curPos]->read();
  if (ret != -1)
    return ret;
  if (_curPos == _ins->length()) {
    _curPos = -1;
    return -1;
  }
  if (_curPos + 1 >= _ins->length())
    _curPos = -1;
  else
    _curPos++;

  return read();
}

//virtual 
int 
JoinedReader::read(IN(RbyteArray) buffer, int offset/* = 0*/, int len/* = -11*/)
{
  if (len == -1)
     len = buffer->length() - offset;
  if (_curPos == -1)
    THROW1(EOFException, "JoinedReader");
  int ret = _ins[_curPos]->read(buffer, offset, len);
  if (ret == -1) {
    _curPos = -1;
    return -1;
  }
  if (ret == len) 
    return ret;
  if (_curPos == _ins->length()) {
    _curPos = -1;
    return ret;
  }
  if (_curPos + 1 >= _ins->length())
    _curPos = -1;
  else
    _curPos++;
  return read(buffer, offset + ret, len - ret);
}

//virtual 
int 
JoinedReader::read(byte* buffer, int offset, int len)
{
  if (_curPos == -1)
    THROW1(EOFException, "JoinedReader");

  int ret = _ins[_curPos]->read(buffer, offset, len);
  if (ret == -1) {
    _curPos = -1;
    return -1;
  }
  int reallen = len;
  if (ret == reallen) 
    return ret;
  if (_curPos == _ins->length()) {
    _curPos = -1;
    return ret;
  }
  if (_curPos + 1 >= _ins->length())
    _curPos = -1;
  else
    _curPos++;
  return read(buffer, offset + ret, reallen - ret);
}


//virtual 
void 
JoinedReader::close() 
{ 
  for (int i = 0; i < _ins->length(); i++) 
    _ins[i]->close();
  _curPos = -1;
}

//virtual 
void 
JoinedReader::reset()
{
  THROW1(IOException, "mark not supported");  
}
  
//virtual 
bool 
JoinedReader::ready()
{
  if (_curPos == -1)
    return false;
  return _ins[_curPos]->ready();
}

//virtual 
void 
JoinedReader::mark(int readAheadLimit)
{
  THROW1(IOException, "mark not supported");  
}

//virtual 
bool 
JoinedReader::markSupported()
{ 
  return  false;
}
  
//virtual 
RReader 
JoinedReader::getStorageReader()
{
  if (_curPos == -1)
    return Nil;
  
  if (instanceof(_ins[_curPos], FilterReader) == true)
    return RFilterReader(_ins[_curPos])->getStorageReader();
  return _ins[_curPos];
}


} // io
} // acdk



