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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ConsoleReader.cpp,v 1.16 2005/03/08 12:45:35 kommer Exp $




#include <acdk.h>

#include "ConsoleReader.h"
#include "EOFException.h"

#include "File.h"
#if defined(ACDK_OS_UNIX)
#include <unistd.h>
#endif


#if defined(_eof)
# undef _eof
#endif

namespace acdk {
namespace io {

using namespace acdk::lang;
using namespace std;

ConsoleReader::ConsoleReader(InChannel fd)
: AbstractStorageReader()
, _last(-1)
, _fd(Nil)
, _eof(false)
{
  if (fd == InvalidInChannel)
    return;
  _fd = new (allocator()) FileDescriptor(fd, O_RDONLY, false);	// never duplicate this descriptor here!
}

//virtual
jlong
ConsoleReader::seek(acdk::io::SeekPos seekrel, jlong seekpos)
{
  THROW1(IOException, getName() + ": seek is not supported ");
  return 0;
}


//virtual
int
ConsoleReader::read()
{
  char buf[1];
  int res;
  if (_last >= 0) {
    buf[0] = _last;
    _last = -1;
    return buf[0];
  }
  if ((res = ::read(_fd->c_fd(), buf, 1)) < 0)
  {
    if (_eof == true)
      THROW0(EOFException);
    _eof = true;
    return -1;
  }
  return 0xff & (int)(buf[0]);
}

//virtual
int
ConsoleReader::unread(byte ch)
{
  if (_last >= 0)
    return -1;
  _last = 0xff & (int)ch;
  return 0;
}

//virtual
void
ConsoleReader::reset()
{
  //### may clrscr or similar
}

//virtual
bool
ConsoleReader::ready()
{
  //### ????
  return true;
}

//virtual
RString
ConsoleReader::getDeviceName()
{
  return RString("[ConsoleReader:]");
}


} // io
} // acdk
