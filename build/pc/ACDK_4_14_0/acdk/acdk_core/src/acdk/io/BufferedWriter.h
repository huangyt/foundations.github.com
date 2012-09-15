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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BufferedWriter.h,v 1.18 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_BufferedWriter_h
#define acdk_io_BufferedWriter_h

#include "AbstractFilterWriter.h"


namespace acdk {
namespace io {

using namespace acdk::lang;

class BufferedWriter;
ACDK_DECL_CLASS(BufferedWriter);

/**
  Buffers input from a given Writer
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.18 $
  @date $Date: 2005/04/09 19:26:44 $

*/
class ACDK_CORE_PUBLIC BufferedWriter
: extends AbstractFilterWriter
{
  ACDK_WITH_METAINFO(BufferedWriter)
protected:
  static int DEFAULT_BUFFER_SIZE;
protected:
  RbyteArray _buffer;
  int _bufferLength;
  bool _overflowed;
public:
  BufferedWriter(IN(RWriter) writer, int buffsize = BufferedWriter::DEFAULT_BUFFER_SIZE);
  ~BufferedWriter();
  /**
    writes buffer to underlying writer and flush the underlying writer
    @see overflow
  */
  virtual void flush();
  virtual void close();
  virtual void write(byte b);
  foreign virtual void write(const byte* buf, int offset, int len);
  virtual void write(IN(RbyteArray) ch, int offset, int len) { AbstractFilterWriter::write(ch, offset, len); }
  /**
    writes the buffer to underlying writer, but doesn't flush it
  */
  void overflow();
  int getBufferSize() { return _bufferLength; }
  void setBufferSize(int newSize);
  bool isOverflowed() { return _overflowed; }

};

} // io
} // acdk

#endif //acdk_io_BufferedWriter_h

