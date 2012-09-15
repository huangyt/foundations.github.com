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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileReaderWriterImpl.cpp,v 1.19 2005/03/08 12:45:36 kommer Exp $




#include <acdk.h>
#include "FileReaderWriterImpl.h"
#include "File.h"
#include "FileDescriptor.h"

#include <acdk/lang/ArrayIndexOutOfBoundsException.h>
#include "FileNotFoundException.h"
#include "IOException.h"
#include "EOFException.h"
#include <acdk/lang/IndexOutOfBoundsException.h>

#include <fcntl.h>
#ifdef ACDK_OS_LINUX
#include <unistd.h>
#endif
#include <sys/types.h> 
#include <sys/stat.h>


#ifdef _eof
# undef _eof
#endif


#if defined(__MWERKS__) && !defined(_O_BINARY)
# define _O_BINARY O_BINARY 
#endif

namespace acdk {
namespace io {

using namespace acdk::lang;


FileReaderWriterImpl::FileReaderWriterImpl(IN(RFile) file, int omode, int pmode)
: _ownFileDescriptor(true)
, _eof(false)
, _eofReturned(false)
{
  _fd = new  FileDescriptor();
  open(file, omode, pmode);
}

FileReaderWriterImpl::FileReaderWriterImpl(IN(RString) fileName, int omode, int pmode)
: _ownFileDescriptor(true)
, _eof(false)
, _eofReturned(false)
{
  _fd = new  FileDescriptor();
  open(new File(fileName), omode, pmode);
}


FileReaderWriterImpl::FileReaderWriterImpl(IN(RFile) file, IN(RString) mode)
: _ownFileDescriptor(true)
, _eof(false)
, _eofReturned(false)
{
  int omode;
  _fd = new FileDescriptor();
    omode = O_RDWR;
  if (mode->equals("r")) 
    omode = O_RDONLY;
  else if (mode->equals("w")) 
    omode = O_WRONLY;
  else if (mode->equals("rw") || mode->equals("wr")) 
    omode = O_RDWR;
  omode |= O_CREAT;
#ifdef ACDK_OS_WIN32
  omode |= _O_BINARY;
#endif
  open(file, omode);
}

FileReaderWriterImpl::FileReaderWriterImpl(IN(RFileDescriptor) fd, bool dupl)
: _ownFileDescriptor(dupl)
, _eof(false)
{
  _fd = new FileDescriptor(fd, dupl);
}

//virtual 
FileReaderWriterImpl::~FileReaderWriterImpl()
{
  if (_ownFileDescriptor == true) {
    _ownFileDescriptor = false;
    close();
  }
}

void
FileReaderWriterImpl::_setFD(int fd, int omode)
{
  _fd->_fd = fd;
  _fd->_omode = omode;
}

jlong 
FileReaderWriterImpl::fileLength()
{
  if (!(_fd->valid())) 
    THROW0(IOException);
  jlong cur = lseek(_fd->c_fd(), 0, SEEK_CUR);
  jlong length = lseek(_fd->c_fd(), 0, SEEK_END);
  lseek(_fd->c_fd(), cur, SEEK_SET);
  return length;
}

jlong 
FileReaderWriterImpl::curSeekPos()
{
  if (!(_fd->valid())) 
    THROW0(IOException);
  return lseek(_fd->c_fd(), 0, SEEK_CUR);
}



int 
FileReaderWriterImpl::available() 
{ 
  return   fileLength() - curSeekPos();
}

void 
FileReaderWriterImpl::close() 
{ 
  if (!(_fd->valid()))
    return;
  if (_ownFileDescriptor == true) {
    ::close(_fd->c_fd());  
    _setFD(-1, 0);
  }
}


jlong 
FileReaderWriterImpl::seek(acdk::io::SeekPos seekrel, jlong seekpos)
{
  if (!(_fd->valid())) 
    THROW0(IOException);
  int rel = SEEK_CUR;
  if (seekrel == SeekCur)
    rel = SEEK_CUR;
  else if (seekrel == SeekSet)
    rel = SEEK_SET;
  else if (seekrel == SeekEnd)
    rel = SEEK_END;
  jlong erg;
#if defined(_MSC_VER)
  if ((erg = _lseeki64(_fd->c_fd(), seekpos, rel)) == -1) 
    THROW0(IOException);
#else
  if ((erg = lseek(_fd->c_fd(), seekpos, rel)) == -1) 
    THROW0(IOException);
#endif
  return erg;
}
  

int 
FileReaderWriterImpl::read()
{
  
  if (_eofReturned == true)
    THROW0(EOFException);
  if (_eof == true)
  {
    _eofReturned = true;
    return -1;
  }
  if (!(_fd->valid())) 
    THROW0(IOException);
  byte buf[1];
  int ret = ::read(_fd->c_fd(), buf, 1);
  if (ret == -1)
    THROW0(IOException);
  if (ret != 1) {
    _eof = true;
    _eofReturned = true;
    return -1;
  }
  return buf[0];
  //return ret == 1 ? (0xff & (int)(buf[0])) : -1;
}

RString
FileReaderWriterImpl::readLine()
{
  if (_eofReturned == true)
    THROW0(EOFException);
  if (_eof == true)
  {
    _eofReturned = true;
    return Nil;
  }
  StringBuffer sb;
  RString eol = File::endOfLine();
  int ch, eolch = eol->charAt(0);
  while (((ch = read()) != eolch) && (ch >= 0))
    sb.append((char)ch);
  if (ch == -1)
  {
    _eof = true;
    if (sb.length() == 0)
    {
      _eofReturned = true;
      return Nil;
    }
  } else {
    if ((ch == eolch) && (eol->length() > 1))
      read();
  }
  if ((ch < 0) && (sb.length() == 0))
    return Nil;
  return sb.toString();
}

int 
FileReaderWriterImpl::read(IN(RbyteArray) buffer, int offset, int len)
{
  if (_eofReturned == true)
    THROW0(EOFException);
  if (_eof == true)
  {
    _eofReturned = true;
    return -1;
  }
  if (len == -1)
    len = buffer->length() - offset;
  if (buffer->length() < offset + len)
    THROW0(IndexOutOfBoundsException);
  return read(buffer->data(), offset, len);
}

int 
FileReaderWriterImpl::read(byte* buffer, int offset, int len)
{
  if (_eofReturned == true)
    THROW0(EOFException);
  if (_eof == true)
  {
    _eofReturned = true;
    return -1;
  }
  if (!(_fd->valid())) 
    THROW0(IOException);
  if (_eof == true)
    THROW0(EOFException);
  
  byte* ptr = buffer + offset;
  int ret = ::read(_fd->c_fd(), ptr, len);
  if (ret == -1)
    THROW0(IOException);
  if (ret < len)
    _eof = true;
  return ret;
}


void 
FileReaderWriterImpl::open(IN(RFile) fname, int omode, int pmode)
{
  RString canonpath = fname->getCanonicalPath();
  RString anscanonpath = canonpath->convert(CCAscii);
  _setFD(::open(anscanonpath->c_str(), omode, pmode), omode);
  if (!(_fd->valid()))
    THROW1(FileNotFoundException, RString("FileReaderWriterImpl::open(): ") + anscanonpath);
}

RFileDescriptor 
FileReaderWriterImpl::getFD()
{
  return new FileDescriptor(_fd);
}

void 
FileReaderWriterImpl::write(byte c)
{
  if (::write(_fd->c_fd(), &c, 1) == -1)
    THROW1(IOException, "FileReaderWriterImpl::write()");
}

void 
FileReaderWriterImpl::write(IN(RbyteArray) buffer, int offset, int len)
{
  if (buffer->length() < offset + len)
    THROW0(ArrayIndexOutOfBoundsException);
  write(buffer->data(), offset, len);
}

void 
FileReaderWriterImpl::write(const byte* cstr, int offset, int len)
{
  if (len == -1)
    len = strlen((const char*)cstr + offset);
  if (::write(_fd->c_fd(), cstr + offset, len) == -1)
    THROW1(IOException, "FileReaderWriterImpl::write()");
  
}

void 
FileReaderWriterImpl::flush()
{
  _fd->sync();
}

bool 
FileReaderWriterImpl::isReadable()
{
  int mode = _fd->omode();
  return (mode & O_RDWR) == O_RDWR || (mode & O_RDONLY) == O_RDONLY;
}

bool 
FileReaderWriterImpl::isWriteable()
{
  int mode = _fd->omode();
  return (mode & O_RDWR) == O_RDWR || (mode & O_WRONLY) == O_WRONLY;
}


#ifndef HAVE_UNISTD_H
int ftruncate(int fd, int newlength)
{
  lseek(fd, newlength, 0);
  write(fd, 0, 0);
  return 0;
}
#endif //HAVE_FTRUNCATE

void 
FileReaderWriterImpl::setLength(jlong newLength)
{
  jlong curlen = fileLength();
  if (curlen == -1)
    return;
  int rc = 0;
  if (curlen > newLength)
    rc = ftruncate(_fd->c_fd(), newLength);
  else if (curlen < newLength)
    rc = lseek(_fd->c_fd(), newLength - curlen, SEEK_CUR);
  else
    return;
  if (rc == -1)
    THROW0(IOException);
}

} // io
} // acdk
