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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharToByteReader.h,v 1.7 2005/03/01 10:12:47 kommer Exp $

#ifndef acdk_io_CharToByteReader_h
#define acdk_io_CharToByteReader_h

#include "CharReader.h"
#include "AbstractReader.h"

namespace acdk {
namespace io {

/**
  helper for CharToByteReader.
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/03/01 10:12:47 $
*/
foreign
class ACDK_CORE_PUBLIC LittleBytesWriter
: extends acdk::lang::Object
, implements Writer
{
  byte _buffer[32];
  int _writePos;
  int _readPos;
public:
  LittleBytesWriter()
  : _writePos(0)
  , _readPos(0)
  {
  }
  virtual void flush() {}
  virtual void close() {}
  virtual void write(byte c)
  {
    _buffer[_writePos++] = c;
  }
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1) { Writer::write(ch, offset, len); }
  foreign virtual void write(const byte* cstr, int offset, int len) { Writer::write(cstr, offset, len); }
  int _getAvailable() { return _writePos - _readPos; }
  int getByte()
  {
    if (_getAvailable() == 0)
      return -1;
    return _buffer[_readPos++];
  }
  void _reset() { _writePos = _readPos = 0; }
};

ACDK_DECL_CLASS(CharToByteReader);

/**
  Reads from a CharReader and encodes it to a Reader
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/03/01 10:12:47 $
*/
class ACDK_CORE_PUBLIC CharToByteReader
: extends AbstractReader
{
  ACDK_WITH_METAINFO(CharToByteReader)
public:
  RCharReader _cin;
  acdk::locale::REncoder _encoder;
  foreign LittleBytesWriter _buf;
  int _maxChars;
  CharToByteReader(IN(RCharReader) cin, IN(acdk::locale::REncoder) encoder = Nil, IN(RObject) iolock = Nil)
  : AbstractReader(iolock)
  , _cin(cin)
  , _encoder(encoder)
  , _maxChars(0)
  {
    if (_encoder == Nil)
      _encoder = acdk::locale::Encoding::getEncoding()->getEncoder();
    _maxChars = (int)_encoder->getEncoding()->maxBytesPerChar();
  }
  virtual int available() { return 0; }
  virtual void reset() {}

  virtual jlong seek(SeekPos seekrel, jlong seekpos) { return -1; }
  virtual jlong skip(jlong n) { return -1; }
  virtual int read()
  {
    if (_buf._getAvailable() > 0)
      return _buf.getByte();
    _buf._reset();
    int nc = _cin->readChar();
    if (nc == -1)
      return -1;
    _encoder->encode(&_buf, nc);
    return _buf.getByte();
  }
  int read(IN(RbyteArray) buffer, int offset = 0, int len = -1) { return AbstractReader::read(buffer, offset, len); }
  foreign int read(byte* buffer, int offset, int len) { return AbstractReader::read(buffer, offset, len); }
};

} // namespace io
} // namespace acdk

#endif //acdk_io_CharToByteReader_h


