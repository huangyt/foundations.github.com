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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PushbackReader.h,v 1.13 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_PushbackReader_h
#define acdk_io_PushbackReader_h

#include "AbstractFilterReader.h"


namespace acdk {
namespace io {

using namespace acdk::lang;

class PushbackReader;
ACDK_DECL_CLASS(PushbackReader);

/**
  CharReader where bytes can be pushed back into the stream.
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC PushbackReader
: extends AbstractFilterReader
{
  ACDK_WITH_METAINFO(PushbackReader)
protected:
  RbyteArray _buffer;
  int _pos;
public:
  PushbackReader(IN(RReader) in, int buffsize = 1);
  
  /// implement from FilterReader
  foreign virtual bool markSupported() { return false; }
  
  /// implement from Reader
  foreign virtual bool  ready();
  /// implement from Reader
  foreign virtual jlong skip(jlong num_bytes);
  /// implement from Reader
  foreign virtual int read();
  /// implement from Reader
  foreign virtual int read(IN(RbyteArray) buf, int offset = 0, int len = -1);
  /// implement from Reader
  foreign virtual int read(byte* buffer, int offset, int len) { return AbstractFilterReader::read(buffer, offset, len); }
  
  /**
    Pushback 1 byte into the stream
  */
  virtual void unread(int b);
  /** API: Extended. Pushes the complete back into stream */
  virtual void unread(IN(RbyteArray) ch);
};


} // io
} // acdk

#endif //acdk_io_PushbackReader_h

