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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharReader.h,v 1.8 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_CharReader_h
#define acdk_io_CharReader_h

#include "AbstractFilterReader.h"
#include "../locale/Decoder.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(CharReader);

/** 
  Reads character, not bytes
  Similar to Javas InputStreamReader
*/
class ACDK_CORE_PUBLIC CharReader 
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(CharReader)
  
public:
  /**
    read one char.
    @return -1 if end of file
  */
  virtual int readChar() = 0;
  /**
    read until EOF and return as string
  */
  virtual RString readString() = 0;
  /**
    closes the reader
  */
  virtual void close() = 0;
  /**
    returns the underlying reader
    @param encoder uses to write characters. If Nil uses the system encoding
  */
  virtual RReader getReader(IN(acdk::locale::REncoder) encoder = Nil);
};


} // namespace io 
} // namespace acdk 

#endif //acdk_io_CharArrayWriter_h


