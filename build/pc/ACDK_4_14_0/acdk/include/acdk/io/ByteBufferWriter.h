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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ByteBufferWriter.h,v 1.1 2005/04/05 21:34:40 kommer Exp $

#ifndef acdk_io_ByteBufferWriter_h
#define acdk_io_ByteBufferWriter_h


#include "EOFException.h"
#include "AbstractWriter.h"
#include "ByteBufferReader.h"


namespace acdk {
namespace io {

ACDK_DECL_CLASS(BytePtrWriter);
/**
  internal class for a writer into a fix byte buffer
*/
class ACDK_CORE_PUBLIC BytePtrWriter
: extends acdk::io::AbstractWriter
{
  ACDK_WITH_METAINFO(BytePtrWriter)
protected:
  RObject _obj;
  foreign byte* _it;
  foreign byte* _end;
public:
  foreign BytePtrWriter(byte* it, byte* end, IN(RObject) obj = Nil)
    : _obj(obj)
    , _it(it)
    , _end(end)
  {
  }
  virtual void close() {}
  virtual void flush() {}
  virtual void write(const byte* cstr, int offset, int len);
  virtual void write(byte c);
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
};


ACDK_DECL_CLASS(ByteBufferWriter);
/**
  Reader interface for a ReadByteBuffer
*/

class ACDK_CORE_PUBLIC ByteBufferWriter
: extends acdk::io::AbstractWriter
{
  ACDK_WITH_METAINFO(ByteBufferWriter)
protected:
  RWriteByteBuffer _buffer;
  int _curPos;
public:
  ByteBufferWriter(IN(RWriteByteBuffer) buffer)
  : _buffer(buffer)
  , _curPos(0)
  {
  }
  virtual void close() {}
  virtual void flush() {}
  virtual void write(const byte* cstr, int offset, int len);
  virtual void write(byte c);
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
  virtual RStorage getWriterStorage() { return new ByteBufferStorage(false, true); }
  RWriteByteBuffer getBuffer() { return _buffer; }
};

ACDK_DECL_CLASS(ByteBufferAppendWriter);
/**
  Reader interface for a ReadByteBuffer
*/

class ACDK_CORE_PUBLIC ByteBufferAppendWriter
: extends acdk::io::AbstractWriter
{
  ACDK_WITH_METAINFO(ByteBufferAppendWriter)
protected:
  RFlexByteBuffer _buffer;
public:
  ByteBufferAppendWriter(IN(RFlexByteBuffer) buffer)
  : _buffer(buffer)
  {
  }
  virtual void close() {}
  virtual void flush() {}
  virtual void write(const byte* cstr, int offset, int len);
  virtual void write(byte c);
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
  virtual RStorage getWriterStorage() { return new ByteBufferStorage(false, true); }
  RFlexByteBuffer getBuffer() { return _buffer; }
};

} // io
} // acdk

#endif //acdk_io_AbstractReader_h

