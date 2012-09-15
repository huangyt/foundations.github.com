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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PushbackCharReader.h,v 1.10 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_PushbackCharReader_h
#define acdk_io_PushbackCharReader_h

#include "AbstractCharFilterReader.h"


namespace acdk {
namespace io {

ACDK_DECL_INTERFACE(PushbackCharReader);

/**
  CharReader where character can be pushed back into the stream.
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:45 $
*/
class ACDK_CORE_PUBLIC PushbackCharReader
: implements CharReader
{
  ACDK_WITH_METAINFO(PushbackCharReader)
public:
   // CharReader
  overwrite int readChar() = 0;
  overwrite RString readString() = 0;
  overwrite void close() = 0;

  // PushbackCharReader
  virtual void unread(ucchar ch) = 0;
  virtual void unread(IN(RString) str);
  virtual void resetPushbackBuffer() = 0;
};

ACDK_DECL_CLASS(PushbackCharReaderImpl);

/**
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC PushbackCharReaderImpl
: extends AbstractCharFilterReader
, implements PushbackCharReader
{
  ACDK_WITH_METAINFO(PushbackCharReaderImpl)
protected:
  RuccharArray _buffer;
  int _pos;
public:
  PushbackCharReaderImpl(IN(RCharReader) in, int buffsize = 1, IN(RObject) iolock = Nil);
  foreign virtual int readChar();
  foreign virtual RString readString();

  foreign virtual void close() { AbstractCharFilterReader::close(); }
  
  virtual void unread(ucchar ch);
  virtual void unread(IN(RString) str);
  virtual void resetPushbackBuffer() { _pos = _buffer->length(); }
};


} // io
} // acdk

#endif //acdk_io_PushbackCharReader_h

