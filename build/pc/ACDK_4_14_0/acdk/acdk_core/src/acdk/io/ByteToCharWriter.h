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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ByteToCharWriter.h,v 1.6 2005/02/05 10:44:53 kommer Exp $

#ifndef acdk_io_ByteToCharWriter_h
#define acdk_io_ByteToCharWriter_h

#include "CharWriter.h"
#include "AbstractWriter.h"
#include "../locale/Encoding.h"

namespace acdk {
namespace io {

/**
  helper for ByteToCharWriter.
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:44:53 $
*/
foreign
class ACDK_CORE_PUBLIC LittleBytesReader
: extends acdk::lang::Object
, implements Reader
{
public:
  byte _buffer[32];
  int _index;
  int _endBuffer;
  LittleBytesReader()
    : _index(0)
    , _endBuffer(0)
  {
  }
  virtual jlong seek(SeekPos seekrel, jlong seekpos) { return -1; }
  virtual jlong skip(jlong n) { return -1; }
  virtual void reset() { }

  virtual int read()
  {
    if (_getAvailable() < 1)
      return -1;
    return _buffer[_index++];
  }
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1) { return Reader::read(buffer, offset, len); }
  foreign virtual int read(byte* buffer, int offset, int len) { return Reader::read(buffer, offset, len); }
  void addByte(byte b)
  {
    if (_index != 0)
    {
      int avil = _getAvailable();
      memmove(_buffer, _buffer + _index, avil);
      _index = 0;
      _endBuffer = avil;

    }
    _buffer[_endBuffer++] = b;
  }
  virtual int available() { return _endBuffer - _index; }
  int _getAvailable() { return _endBuffer - _index; }
};



ACDK_DECL_CLASS(ByteToCharWriter);

/**
  Reads from a CharReader and encodes it to a Reader
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:44:53 $
*/

class ACDK_CORE_PUBLIC ByteToCharWriter
: extends AbstractWriter
{
  ACDK_WITH_METAINFO(ByteToCharWriter)
private:
  RCharWriter _cout;
  acdk::locale::RDecoder _decoder;
  LittleBytesReader _buf;
  int _maxBytesPerChar;
public:
  ByteToCharWriter(IN(RCharWriter) charWriter, IN(acdk::locale::RDecoder) dec = Nil, IN(RObject) iolock = Nil)
  : AbstractWriter(iolock)
  , _cout(charWriter)
  , _decoder(dec)
  , _maxBytesPerChar(0)
  {
    if (_decoder == Nil)
      _decoder = acdk::locale::Encoding::getEncoding()->getDecoder();
    _maxBytesPerChar = (int)_decoder->getEncoding()->maxBytesPerChar();
  }
  virtual void flush()
  {
    _overFlow();
    _cout->flush();
  }
  virtual void close()
  {
    _overFlow();
    _cout->close();
  }

  virtual void write(byte c)
  {
    SYNCTHIS();
    _buf.addByte(c);
    if (_buf._getAvailable() >= _maxBytesPerChar)
    {
      _overFlow();
    }
  }
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1) { AbstractWriter::write(ch, offset, len); }
  foreign virtual void write(const byte* cstr, int offset, int len) { AbstractWriter::write(cstr, offset, len); }
  void _overFlow()
  {
    SYNCTHIS();
    if (_buf._getAvailable() == 0)
      return;
    int ch = _decoder->decodeToChar(&_buf);
    _cout->writeChar((ucchar)ch);
  }
};

} // namespace io
} // namespace acdk

#endif //acdk_io_ByteToCharWriter_h


