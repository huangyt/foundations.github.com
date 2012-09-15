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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/StringReader.h,v 1.11 2005/02/05 10:44:54 kommer Exp $

#ifndef acdk_io_StringReader_h
#define acdk_io_StringReader_h

#include "AbstractCharReader.h"
#include "EOFException.h"
#include "../locale/Decoder.h"

namespace acdk {
namespace io {


ACDK_DECL_INTERFACE(StringReader);
// Borland C++Builder defines _eof to eof
#if defined(_eof)
# undef _eof
#endif

/**
  Reads character from a given String
*/
class ACDK_CORE_PUBLIC StringReader
: extends AbstractCharReader
{
  ACDK_WITH_METAINFO(StringReader)
protected:
  RString _string;
  int _readPos;
  bool _eof;
public:
  StringReader(IN(RString) str, int readpos = 0, IN(RObject) iolock = Nil)
  : AbstractCharReader(iolock)
  , _string(str)
  , _readPos(readpos)
  , _eof(false)
  {
  }
  foreign virtual int readChar()
  {
    if (_readPos == _string->length())
    {
      ++_readPos;
       if (_eof == false)
      {
        _eof = true;
        return -1;
      }
    }
    if (_readPos > _string->length())
      THROW0(EOFException);
    return _string->charAt(_readPos++);
  }
  /**
    read until EOF and return as string
  */
  foreign virtual RString readString()
  {
    if (_readPos > _string->length())
      THROW0(EOFException);
    return _string->substr(_readPos);
  }
  foreign virtual void close() {}
  int getReadPos() { return _readPos; }
};



} // namespace io
} // namespace acdk

#endif //acdk_io_CharArrayWriter_h


