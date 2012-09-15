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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileDescriptor.cpp,v 1.11 2005/02/05 10:44:54 kommer Exp $




#include <acdk.h>
#include "FileDescriptor.h"


#ifdef ACDK_OS_WIN32
#  include <io.h>
#endif
#if defined(HAS_UNISTD_H) || defined(ACDK_OS_UNIX)
#  include <unistd.h>
#endif

#include <fcntl.h>


namespace acdk {
namespace io {

RFileDescriptor FileDescriptor::in;
RFileDescriptor FileDescriptor::out;
RFileDescriptor FileDescriptor::err;

FileDescriptor::FileDescriptor(IN(RFileDescriptor) f, bool dupl)
{
  if (dupl) {
    if (!(f->valid()) || ((_fd = ::dup(f->_fd)) < 0))
      THROW1(IOException, "FileDescriptor");
  } else {
    _fd = f->_fd;
  }
  _omode = f->_omode;
}
  
FileDescriptor::FileDescriptor(int fd, int omode, bool dupl)
{
  if (dupl) {
    if ((fd == -1) || ((_fd = ::dup(fd)) < 0))
      THROW1(IOException, "FileDescriptor");
  } else {
    _fd = fd;
  }
  _omode = omode;
}
  
//virtual 
void 
FileDescriptor::sync()
{
  if (_fd == -1)
    return;
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32) && !defined(ACDK_MINGW) && !defined(__MWERKS__)
  if (::_commit(_fd) == -1)
    THROW1(IOException, "FileDescriptor::sync()");
#endif
#if defined(ACDK_OS_UNIX) 
  if (::fsync(_fd) == -1)
    THROW1(IOException, "FileDescriptor::sync()");
#endif
#if defined(ACDK_MINGW)
  // BOOL FlushFileBuffers(
//#### ACDK_MINGW and __MWERKS__ has not fsync

#endif
}

bool
FileDescriptor::getCloseOnExec()
{
  int val;
  
  if (_fd == -1)
    return false;
  return false; // currently unimplemented, maybe later via GetHandleFlags()
#if defined(ACDK_OS_UNIX) 
  if ((val = ::fcntl(_fd, F_GETFD)) < 0)
    THROW1(IOException, "FileDescriptor::getCloseOnExec()");
  return (val & FD_CLOEXEC)? true:false;
#endif // ACDK_OS_UNIX
}

// note: closeonexec is a security feature. if it fails, it should be noted with an exception.

void
FileDescriptor::setCloseOnExec(bool val)
{
  if (_fd == -1)
    return;
#ifdef ACDK_OS_UNIX
  if (((::fcntl(_fd, F_SETFD, val? (FD_CLOEXEC) : 0)) < 0) || (getCloseOnExec() != val))
#endif
    THROW1(IOException, "FileDescriptor::setCloseOnExec()");
}

} // io
} // acdk
