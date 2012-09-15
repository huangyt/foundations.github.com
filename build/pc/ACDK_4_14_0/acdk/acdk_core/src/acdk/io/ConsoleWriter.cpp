// -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*-
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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ConsoleWriter.cpp,v 1.16 2005/03/08 12:45:35 kommer Exp $





#include <acdk.h>

#include "ConsoleWriter.h"

#include "IOException.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

static FILE* _coutPtr = 0;
static FILE* _cerrPtr = 0;

ConsoleWriter::ConsoleWriter(ConsoleOutChannel fd, IN(RObject) iolock) 
: AbstractStorageWriter(iolock)
, _fptr(0) 
{ 
  if (fd == CoutOutChannel)
  {
    if (_coutPtr == 0)
      _coutPtr = ::fdopen(fd, "w");
    _fptr = _coutPtr;
  }
  else if (fd == CerrOutChannel)
  {
    if (_cerrPtr == 0)
      _cerrPtr = ::fdopen(fd, "w");
    _fptr = _cerrPtr;
  }
  else if (fd != -1)
    _fptr = ::fdopen(fd, "w");
}

//virtual 
ConsoleWriter::~ConsoleWriter()
{
  if (_fptr != 0 && _fptr != _coutPtr && _fptr != _cerrPtr) 
  {
    fclose(_fptr);
    _fptr = 0;
  }
}

//virtual 
void 
ConsoleWriter::flush()
{
  ::fflush(_fptr);
  //_commit(_fd);
}

//virtual 
void 
ConsoleWriter::close()
{
  //### soll wirklich geclosed weren???
  //close(_fd);
}

//virtual 
void 
ConsoleWriter::write(const byte* cstr, int offset, int len) 
{
  SYNCHRONIZETHIS();

  if (_fptr == 0 || ferror(_fptr) != 0) {
    THROW1(IOException, "File is not valid");
  }
  if (cstr == 0 && *(cstr + offset) == 0)
    return;
  if (int(len) == -1) 
    len = strlen((const char*)cstr);
  ::fwrite(cstr + offset, 1, len, _fptr);

}


//virtual 
void 
ConsoleWriter::write(byte c)
{
  byte cbuf[2]; cbuf[1] = 0; cbuf[0] = c;
  write(cbuf, 0, 1);
}

//virtual 
void 
ConsoleWriter::write(IN(RbyteArray) ch, int offset, int len)
{
  write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
}

//virtual 
RString 
ConsoleWriter::getDeviceName()
{
  return "[ConsoleWriter:" + String::valueOf(fileno(_fptr)) + "]";
}

//virtual 
bool 
ConsoleWriter::isWriteable()
{
  return true;
}

//virtual 
bool 
ConsoleWriter::isReadable()
{
  return false;
}

} // io
} // acdk
