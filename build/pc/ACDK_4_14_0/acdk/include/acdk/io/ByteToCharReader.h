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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ByteToCharReader.h,v 1.9 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_ByteToCharReader_h
#define acdk_io_ByteToCharReader_h

#include "AbstractCharReader.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(ByteToCharReader);

/** 
  Filter translates from a Reader interface to a CharReader interface with the given decoder.
    
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:44 $
*/
class ACDK_CORE_PUBLIC ByteToCharReader 
: extends AbstractCharReader
{
  ACDK_WITH_METAINFO(ByteToCharReader)
protected:
  acdk::locale::RDecoder _decoder;
  RReader _in;
public :
  ByteToCharReader(IN(RReader) in, IN(acdk::locale::RDecoder) decoder = Nil, IN(RObject) iolock = Nil);

  foreign virtual acdk::locale::RDecoder getDecoder() { return _decoder; }
  foreign virtual void setDecoder(IN(acdk::locale::RDecoder) decoder) { _decoder = decoder; }

  virtual int readChar();
  virtual RString readString();
  virtual void close() { _in->close(); }
  /**
    @param encoder will be ignore, instead uses own internal
  */
  virtual RReader getReader(IN(acdk::locale::REncoder) encoder = Nil) { return _in; }

};

} // io
} // acdk

#endif //acdk_io_ByteToCharReader_h

