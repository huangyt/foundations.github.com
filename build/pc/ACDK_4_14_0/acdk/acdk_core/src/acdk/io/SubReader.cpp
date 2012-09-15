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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/SubReader.cpp,v 1.6 2005/02/05 10:44:54 kommer Exp $


#include "SubReader.h"
#include "EOFException.h"

namespace acdk {
namespace io {

SubReader::SubReader(IN(RReader) in, int startoffset, int len)
: AbstractFilterReader(in)
, _start(startoffset)
, _length(len)
, _curpos(0)
{
  if (_in->seek(SeekSet, _start) != _start)
  {
    THROW1(IOException, RCS("Cannot seek to starting position: ") + _start);
  }
}

jlong 
SubReader::seek(::acdk::io::SeekPos seekrel, jlong seekpos)
{
  if (seekrel == ::acdk::io::SeekCur) {
    if (seekpos < 0) {
      if (_curpos + seekpos < 0) 
        seekpos = -_curpos;
      int ret = _in->seek(seekrel, -_curpos);
      _curpos += ret;
      return ret;
    } else if (seekpos > 0) {
      if (_curpos + seekpos > _length) 
        seekpos = _length - _curpos;
      int ret = _in->seek(seekrel, -_curpos);
      _curpos += ret;
      return ret;
    } else {
      return 0;
    }
  } else if (seekrel == ::acdk::io::SeekEnd) {
    int rseek = _length - seekpos;
    if (rseek < 0)
      rseek = 0;
    int ret = _in->seek(::acdk::io::SeekSet, _start + rseek);
    _curpos = ret - _start;
    return ret - _start - _length;
  } else if (seekrel == ::acdk::io::SeekSet) {
    if (seekpos > _length)
      seekpos = _length;
    int ret = _in->seek(::acdk::io::SeekSet, _start + seekpos);
    _curpos = ret - _start;
    return ret - _start;
  }
  // never reach here
  return 0;
} 

int 
SubReader::available()
{
  if (_curpos >= _length)
    return 0;
  int erg = _in->available();
  if (erg > _length - _curpos)
    erg = _length - _curpos;
  return erg;
}

jlong 
SubReader::skip(jlong n)
{
  if (_curpos + n > _length)
    n = _length - _curpos;
  int ret = _in->skip(n);
  _curpos += ret;
  return ret;
} 

int 
SubReader::read(byte* buffer, int offset, int len)
{
  if (_curpos > _length)
    THROW0(EOFException);
  if (_curpos + len > _length)
    len = _length - _curpos;
  int ret = _in->read(buffer, offset, len);
  if (ret == 0)
    _curpos = _length + 1;
  return ret;
}


} // io
} // acdk


