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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/MemWriter.h,v 1.20 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_MemWriter_h
#define acdk_io_MemWriter_h

#include "AbstractStorageWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(MemWriter);
ACDK_DECL_CLASS(MemReader);


/**
  Provides a Writer interface to byteArray.

  API: ACDK<br/>
  @deprecated use ByteBufferWriter
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC MemWriter
: extends AbstractStorageWriter
{
  ACDK_WITH_METAINFO(MemWriter)
public:
  // Default buffer size at initialization
  static int DefaultBufferSize;
  // default for buffer size incremention
  static int IncrementBufferSize;
protected:
  RbyteArray _buffer;
public :
  MemWriter(int initsize = MemWriter::DefaultBufferSize)
  : AbstractStorageWriter()
  , _buffer(new (allocator()) byteArray(0))
  {
    _buffer->ensureCapacity(initsize);
  }
  MemWriter(IN(RbyteArray) buffer)
  : AbstractStorageWriter()
  , _buffer(buffer)
  {
  }
  ~MemWriter()
  {
    close();
  }
  foreign virtual void write(byte c)
  {
    _buffer->append(c);
  }
  foreign virtual void write(const byte* cstr, int offset, int len);
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1)
  {
    write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
  }
  
  foreign virtual void flush()
  {
  }
  foreign virtual void close()
  {
  }
  
  virtual RbyteArray getBuffer() 
  { 
    return _buffer; 
  }
  RcharArray getBufferAsChars();
  void ensureCapacity(int size)
  {
    _buffer->ensureCapacity(size);
  }

  foreign virtual RString getDeviceName()
  {
    return "[MemWriter]";
  }
  foreign virtual bool isWriteable() { return true; }
  foreign virtual bool isReadable() { return false; }
};

} // io
} // acdk


#endif //acdk_io_MemWriter_h

