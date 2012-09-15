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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharWriter.h,v 1.10 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_CharWriter_h
#define acdk_io_CharWriter_h

#include "Writer.h"
#include "../locale/Encoder.h"

namespace acdk {
namespace io {


ACDK_DECL_INTERFACE(CharWriter);

/** 
  Reads character, not bytes
  Similar to Javas InputStreamReader
*/
class ACDK_CORE_PUBLIC CharWriter 
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(CharWriter)
public:
  /**
    write a single ASCII 7 bit character
  */
  virtual void writeChar(char c)  = 0;
  /**
    write a single unicode character
  */
  virtual void writeChar(ucchar c)  = 0;
  /**
    write ASCII 7 bit characters
  */
  foreign virtual void writeString(const char* cstr) 
  {
    for (; *cstr != 0; ++cstr) writeChar(*cstr);
  }
  /**
    write unicode characters
  */
  foreign virtual void writeString(const ucchar* cstr)
  {
    for (; *cstr != 0; ++cstr) writeChar(*cstr);
  }
  /**
    write a string
  */
  virtual void writeString(IN(RString) str) = 0;
  /**
    flush the writer
  */
  virtual void flush() = 0;
  /**
    close the writer
  */
  virtual void close() = 0;
  /**
    returns the underlying byte reader
    @param decoder If Nil uses the system encoding
  */
  virtual RWriter getWriter(IN(acdk::locale::RDecoder) decoder = Nil);
};



} // namespace io 
} // namespace acdk 

#endif //acdk_io_CharArrayWriter_h


