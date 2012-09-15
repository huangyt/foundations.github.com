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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ConsoleCharReader.cpp,v 1.12 2005/03/18 20:29:01 kommer Exp $



#include "ConsoleCharReader.h"
#include "ConsoleReader.h"
#include "ByteToCharReader.h"

#include <acdk/locale/UCS2Encoding.h>
#include <acdk/locale/AsciiEncoding.h>
#include <acdk/lang/System.h>

namespace acdk {
namespace io {

ConsoleCharReader::ConsoleCharReader(IN(RObject) iolock, IN(acdk::locale::RDecoder) decoder)
: AbstractCharReader(iolock)
, _decoder(decoder)
{
  if (_decoder == Nil)
    _decoder = acdk::locale::Encoding::getEncoding()->getDecoder();
#if defined(ACDK_OS_WIN32)
  _handle = GetStdHandle(STD_INPUT_HANDLE);
  DWORD mode = 0;
  // test if there is really a console
  if (GetConsoleMode(_handle, &mode) == FALSE)
  {
    _handle = NULL;
    _in = new (allocator()) ByteToCharReader(new (allocator()) ConsoleReader(CinInChannel), _decoder);
  }
#else
   _in = new (allocator()) ByteToCharReader(new (allocator()) ConsoleReader(CinInChannel), _decoder);
#endif
}

int 
ConsoleCharReader::readChar()
{
#if defined(ACDK_OS_WIN32)
  if (_handle != NULL)
  {
    uc2char ch;
    DWORD char_readed;
    BOOL berg = ReadConsole(_handle, &ch, 1, &char_readed, NULL);
    //if (berg == FALSE)
    //  acdk::lang::System::out->println("ReadConsole FAILED");
    return ch;
  }
#endif
  return _in->readChar();
}
  
void 
ConsoleCharReader::close()
{
  if (_in != Nil)
    _in->close();
}

RReader 
ConsoleCharReader::getReader(IN(acdk::locale::REncoder) encoder)
{
  acdk::locale::REncoder enc = encoder;
  if (enc == Nil && _decoder != Nil)
    enc = _decoder->getEncoding()->getEncoder();
#if defined(ACDK_OS_WIN32)
  if (_handle != NULL)
    return new ConsoleReader(CinInChannel);
#endif
  return _in->getReader(enc);
}

} // io
} // acdk



