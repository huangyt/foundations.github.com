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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/MemReader.h,v 1.15 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_MemReader_h
#define acdk_io_MemReader_h

#include "AbstractStorageReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

ACDK_DECL_CLASS(MemWriter);
ACDK_DECL_CLASS(MemReader);

/**
  Provide a Reader interface to a byteArray
  API: ACDK<br/>
  @deprecated use ByteBufferReader
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC MemReader
: extends AbstractStorageReader
{
  ACDK_WITH_METAINFO(MemReader)
protected:
  RbyteArray _buffer;
  /// current position
  int _seekPos;
  /// end of file, if -1 always read end of buffer
  int _endPos;
public :
  MemReader(IN(RMemWriter) memwriter, int offset = 0, int endpos = -1);
  MemReader(IN(RbyteArray) bytearray, int offset = 0, int endpos = -1)
  : AbstractStorageReader()
  , _buffer(bytearray)
  , _seekPos(offset)
  , _endPos(endpos)
  {
  }
  foreign virtual int available() { return endPos() - _seekPos; }
  foreign virtual void close() 
  { 
    _buffer = Nil;
    _seekPos = 0;
    _endPos = 0;
  }
  foreign virtual jlong seek(SeekPos seekrel, jlong seekpos);
  
  foreign virtual jlong skip(jlong n);
  foreign virtual int read();
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len);
// Storage
  foreign virtual RString getDeviceName()
  {
    return "[MemReader]";
  }
  foreign virtual bool isWriteable() { return false; }
  foreign virtual bool isReadable() { return true; }
  foreign virtual void reset();
  foreign virtual bool ready();
  RbyteArray getBuffer() { return _buffer; }
  foreign byte* begin() { return _buffer->data(); }
  foreign byte* end() { return _buffer->data() + endPos(); }
  foreign byte* cit() { return _buffer->data() + _seekPos; }
  inline int endPos() { return _endPos == -1 ? _buffer->length() : _endPos; }
};


} // io
} // acdk

#endif //acdk_io_MemReader_h

