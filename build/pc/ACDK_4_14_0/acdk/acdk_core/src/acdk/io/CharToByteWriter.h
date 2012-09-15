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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharToByteWriter.h,v 1.9 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_CharToByteWriter_h
#define acdk_io_CharToByteWriter_h
#include <acdk.h>
#include "Writer.h"
#include "AbstractCharWriter.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(CharToByteWriter);

/** 
  CharWriter interface which writes the encoded bytes in a given Writer.

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:44 $
*/
class ACDK_CORE_PUBLIC CharToByteWriter
: extends AbstractCharWriter
{
  ACDK_WITH_METAINFO(CharToByteWriter)
protected:
  acdk::locale::REncoder _encoder;
  RWriter _out;
public :
  CharToByteWriter(IN(RWriter) out, IN(acdk::locale::REncoder) encoder = Nil, IN(RObject) iolock = Nil);
  acdk::locale::REncoder getEncoder() { return _encoder; }
  void setEncoder(IN(acdk::locale::REncoder) encoder) { _encoder = encoder; }
  virtual void writeChar(char c);
  virtual void writeChar(ucchar c);
  foreign virtual void writeString(const char* cstr);
  foreign virtual void writeString(const ucchar* cstr);
  virtual void writeString(IN(RString) str);
  virtual void flush() { _out->flush(); }
  virtual void close() { _out->close(); }
  /**
    Get writer from this CharWriter
    @param decoder will be ignored in this case
  */
  virtual RWriter getWriter(IN(acdk::locale::RDecoder) decoder = Nil) { return _out; }
};

} // io
} // acdk

#endif //acdk_io_CharToByteWriter_h

