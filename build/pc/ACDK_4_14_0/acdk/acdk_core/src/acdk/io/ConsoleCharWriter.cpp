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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ConsoleCharWriter.cpp,v 1.15 2005/03/27 12:03:40 kommer Exp $

#include <acdk.h>
#include "ConsoleCharWriter.h"
#include "ConsoleWriter.h"
#include "CharToByteWriter.h"
#include <acdk/locale/UCS2Encoding.h>
#include <acdk/locale/AsciiEncoding.h>

namespace acdk {
namespace io {


ConsoleCharWriter::ConsoleCharWriter(ConsoleOutChannel fd, IN(RObject) iolock, IN(acdk::locale::REncoder) encoder)
: AbstractCharWriter(iolock)
, _encoder(Nil)
#if !defined(ACDK_OS_WIN32)
, _out(new (allocator()) CharToByteWriter(new (allocator()) ConsoleWriter(fd), _encoder))
#else
, _handle(NULL)
#endif
, _channel(fd)
{
	_encoder = (encoder != Nil ? encoder : acdk::locale::Encoding::getEncoding()->getEncoder());

#if defined(ACDK_OS_WIN32)
  _handle = GetStdHandle(fd == CoutOutChannel ? STD_OUTPUT_HANDLE : STD_ERROR_HANDLE);
  DWORD mode = 0;
  // test if there is really a console
  if (GetConsoleMode(_handle, &mode) == FALSE)
  {
    _handle = NULL;
    _out = new (allocator()) CharToByteWriter(new ConsoleWriter(fd), _encoder);
  }
#endif
}

ConsoleCharWriter::~ConsoleCharWriter()
{
}

void 
ConsoleCharWriter::writeChar(char c)
{
  writeChar((ucchar)c);
}

void 
ConsoleCharWriter::writeChar(uc2char c)
{
  SYNCTHIS();
#if defined(ACDK_OS_WIN32)
  if (_out == Nil)
  {
    DWORD char_written;
    BOOL berg = WriteConsole(_handle, &c, 1, &char_written, NULL);
    return;
  }
#endif
  _out->writeChar(c);
}

void 
ConsoleCharWriter::writeString(IN(RString) str)
{
  SYNCTHIS();
#if defined(ACDK_OS_WIN32)
  if (_out == Nil)
  {
    DWORD char_written;
    RString ts = str->convertToNative();
    BOOL berg = WriteConsole(_handle, ts->native_c_str(), ts->length(), &char_written, NULL);
    return;
  }
#endif
  _out->writeString(str);
}

void 
ConsoleCharWriter::flush()
{
  SYNCTHIS();
#if defined(ACDK_OS_WIN32)
  if (_out == Nil)
    return;
#endif
  _out->flush();

}

void 
ConsoleCharWriter::close()
{
#if defined(ACDK_OS_WIN32)
  if (_out == Nil)
    return;
#endif
  _out->close();
}

RWriter 
ConsoleCharWriter::getWriter(IN(acdk::locale::RDecoder) decoder)
{
  acdk::locale::RDecoder dec = decoder;
  if (dec == Nil && _encoder != Nil)
    dec = _encoder->getEncoding()->getDecoder();
#if defined(ACDK_OS_WIN32)
  if (_out == Nil)
    return new ConsoleWriter(_channel);
#endif
  return _out->getWriter(dec);
}

} // io
} // acdk

