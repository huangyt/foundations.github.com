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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ByteBufferReader.h,v 1.4 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_ByteBufferReader_h
#define acdk_io_ByteBufferReader_h


#include "EOFException.h"
#include "AbstractReader.h"
#include "../lang/ByteBuffer.h"
#include "BytePtrReader.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(ByteBufferStorage);
/**
  helper class 
*/
final
class ACDK_CORE_PUBLIC ByteBufferStorage
: extends acdk::lang::Object
, implements Storage
{
  ACDK_WITH_METAINFO(ByteBufferStorage)
private:
  bool _readable;
  bool _writeable;
public :
  ByteBufferStorage(bool readable, bool writeable)
    : _readable(readable)
    , _writeable(writeable)
  {
  }
  /** 
    @return the name of the device. Normally the filename 
  */
  virtual RString getDeviceName() { return "[ByteBuffer]"; }
  /** 
    @return true if Storage is writable 
  */
  virtual bool isWriteable() { return _readable; }
  /** 
    @return true if Storage is readable
  */
  virtual bool isReadable() { return _writeable; }
};

ACDK_DECL_CLASS(ByteBufferReader);

/**
  Reader interface for a ReadByteBuffer
*/
class ACDK_CORE_PUBLIC ByteBufferReader
: extends acdk::io::AbstractReader
{
  ACDK_WITH_METAINFO(ByteBufferReader)
protected:
  RReadByteBuffer _buffer;
  int _curPos;
  int _mark;
public:
  ByteBufferReader(IN(RReadByteBuffer) buffer)
  : _buffer(buffer)
  , _curPos(0)
  , _mark(-1)
  {
  }
  virtual int available()
  {
    return _buffer->length() - _curPos;
  }
  virtual jlong seek(SeekPos seekrel, jlong seekpos);
  virtual jlong skip(jlong n);
  virtual int read();
  virtual void mark(int readAheadLimit) { _mark = _curPos; }
  virtual bool markSupported() { return true; }
  virtual void reset();
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len);
  RStorage getReaderStorage() { return new ByteBufferStorage(true, false); }
  
  void close() {  }
};


ACDK_DECL_CLASS(ByteBufferPtrReader);

/**
  same as ByteBufferReader but ReadByteBuffer supports native iterator
*/
class ACDK_CORE_PUBLIC ByteBufferPtrReader
: extends acdk::io::BytePtrReader
{
  ACDK_WITH_METAINFO(ByteBufferPtrReader)
protected:
  acdk::lang::RReadByteBuffer _buffer;
public:
  ByteBufferPtrReader(IN(RReadByteBuffer) buffer)
  : BytePtrReader(buffer->begin(), buffer->end())
  , _buffer(buffer)
  {
  }
};

} // io
} // acdk

#endif //acdk_io_AbstractReader_h

