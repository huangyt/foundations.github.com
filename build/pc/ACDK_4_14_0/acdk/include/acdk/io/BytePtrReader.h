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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BytePtrReader.h,v 1.11 2005/04/13 12:59:50 kommer Exp $

#ifndef acdk_io_BytePtrReader_h
#define acdk_io_BytePtrReader_h


#include "EOFException.h"
#include "AbstractReader.h"

namespace acdk {
namespace io {

ACDK_DECL_CLASS(BytePtrReader);
/**
  internal reader class.
  ptr must be owned by other class
*/
final
class ACDK_CORE_PUBLIC BytePtrReader
: extends acdk::io::AbstractReader
{
  ACDK_WITH_METAINFO(BytePtrReader)
protected:
  foreign const byte* _begin;
  foreign const byte* _end;
  foreign const byte* _ptr;
  foreign const byte* _mark;
public:
  foreign BytePtrReader(const byte* begin, int len)
  : _begin(begin)
  , _end(begin + len)
  , _ptr(begin)
  , _mark(0)
  {
  }
  foreign BytePtrReader(const byte* begin, const byte* end)
  : _begin(begin)
  , _end(end)
  , _ptr(begin)
  , _mark(0)
  {
  }
  virtual bool markSupported() { return true; }
  /** note: readAheadLimit will be ignored */
  virtual void mark(int readAheadLimit) { _mark = _ptr; }
  virtual void reset()
  {
    if (_mark == 0)
      THROW1(IOException, "BytePtrReader::reset():  mark was not set");
    _ptr = _mark;
  }

  virtual int available() { return _end - _ptr; }
  virtual jlong seek(SeekPos seekrel, jlong seekpos);
  virtual jlong skip(jlong n);
  inline int read()
  {
    if (_ptr == _end)
    {
      ++_ptr;
      return -1;
    }
    if (_ptr > _end)
      THROW0(EOFException);
    return *_ptr++;
  }
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1) ;
  foreign virtual int read(byte* buffer, int offset, int len);

  void close() {}
};

} // io
} // acdk

#endif //acdk_io_AbstractReader_h

