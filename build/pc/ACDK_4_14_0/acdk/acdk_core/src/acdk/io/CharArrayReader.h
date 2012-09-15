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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharArrayReader.h,v 1.13 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_CharArrayReader_h
#define acdk_io_CharArrayReader_h

#include "AbstractStorageReader.h"
#include "CharReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(CharArrayReader);

/** 
  Reads from an uccharArray
  API: Java
  
  @author Roger Rene Kommer
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:44 $
  @see InterfaceXXX
  @bug Incomplete, Untested, etc.

*/
class ACDK_CORE_PUBLIC CharArrayReader 
: extends acdk::lang::Object
, implements CharReader
{
  ACDK_WITH_METAINFO(CharArrayReader)
public:
  acdk::locale::RDecoder _decoder;
  RuccharArray _buffer;
  int _pos;
  int _count;
  int _markedPos;
  CharArrayReader(IN(RuccharArray) buf, int offset = 0 , int length = -1)
  : _buffer(buf)
  , _pos(offset)
  , _count(length)
  , _markedPos(offset)
  {
    if (_count == -1)
      _count = _buffer->length();
  }
  virtual acdk::locale::RDecoder getDecoder()  { return _decoder; }
  virtual void setDecoder(IN(acdk::locale::RDecoder) decoder) { _decoder = decoder; }
  virtual int readChar();
  virtual RString readString();
  
  virtual void close() {}

};


} // namespace io 
} // namespace acdk 

#endif //acdk_io_CharArrayReader_h

