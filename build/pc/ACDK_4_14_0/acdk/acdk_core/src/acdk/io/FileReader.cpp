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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileReader.cpp,v 1.17 2005/03/08 12:45:36 kommer Exp $




#include <acdk.h>
#include "FileReader.h"
#include "File.h"
#include "FileDescriptor.h"
#include "EOFException.h"

#include <stdio.h>
#include <errno.h>

#ifdef HAS_UNISTD_H
#  include <unistd.h>
#endif

#include <fcntl.h>
#include <sys/types.h> 
#include <sys/stat.h>


#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#  include <direct.h>
#  include <io.h>
#  include <windows.h>
#endif
#ifdef ACDK_OS_UNIX
#  include <dirent.h>
#endif

#include <acdk/lang/UnsupportedOperationException.h>

#ifdef _eof
# undef _eof
#endif

#if defined(__MWERKS__) && !defined(_O_BINARY)
# define _O_BINARY O_BINARY 
#endif

namespace acdk {
namespace io {

using namespace acdk::lang;



FileReader::FileReader(IN(RFile) file, bool binary)
: AbstractStorageReader(),
  _fname(file->getCanonicalPath()),
  _fileImpl(file, O_RDONLY 
#if defined(ACDK_OS_WIN32)
            | (binary == true ? _O_BINARY : 0)
#endif // ACDK_OS_WIN32
            ),
_eof(false)
{
}

FileReader::FileReader(IN(RString) fileName, bool binary)
: AbstractStorageReader(),
  _fname(fileName),
  _fileImpl(fileName, O_RDONLY
#if defined(ACDK_OS_WIN32)
            | (binary == true ? _O_BINARY : 0)
#endif // ACDK_OS_WIN32
            ),
  _eof(false)
{
}

FileReader::FileReader(IN(RFileDescriptor) fd, bool dupl)
: AbstractStorageReader(),
  _fileImpl(fd, dupl),
  _eof(false)
{
}

//virtual 
FileReader::~FileReader()
{
  close();
}


//
jlong 
FileReader::fileLength()
{
  return _fileImpl.fileLength();
}


//virtual 
int 
FileReader::available()  
{
  return _fileImpl.available();
}

//virtual 
void 
FileReader::close() 
{
  _fileImpl.close();
}

//virtual 
jlong 
FileReader::seek(acdk::io::SeekPos seekrel, jlong seekpos)
{
  return _fileImpl.seek(seekrel, seekpos);
}

//virtual 
void 
FileReader::reset()
{
  THROW0(UnsupportedOperationException);
}

//virtual 
bool 
FileReader::ready()
{
  return _fileImpl.available() > 0;
}

//virtual 
int 
FileReader::read()
{
  if (_eof == true)
    THROW1(EOFException, _fname);
  int ret = _fileImpl.read();
  if (ret == -1)
    _eof = true;
  return ret;
}

//virtual
RString
FileReader::readLine()
{
  if (_eof == true)
    THROW1(EOFException, _fname);
  RString res = _fileImpl.readLine();
  if (res == Nil)
    _eof = true;
  return res;
}

//virtual 
int 
FileReader::read(IN(RbyteArray) buffer, int offset, int len)
{
  if (len == -1)
    len = buffer->length() - offset;
  return read(buffer->data(), offset, len);
}

//virtual 
int 
FileReader::read(byte* buffer, int offset, int len)
{
  if (_eof == true)
    THROW1(EOFException, _fname);
  int ret = _fileImpl.read(buffer, offset, len);
  if (ret == 0 && len != 0) {
    _eof = true;
    return -1;
  }
  return ret;
}


//virtual 
void 
FileReader::open(IN(RFile) fname)
{
  _fileImpl.open(fname, O_RDWR);
}

RFileDescriptor 
FileReader::getFD()
{
  return _fileImpl.getFD();
}


//virtual 
RString 
FileReader::getDeviceName()
{
  if (_fname != Nil)
    return _fname;
  return "[FD:" + String::valueOf(_fileImpl.getFD()->c_fd()) + "]";
}

//virtual 
bool 
FileReader::isWriteable()  
{ 
  return _fileImpl.isWriteable(); 
}

//virtual 
bool 
FileReader::isReadable()  
{ 
  return _fileImpl.isReadable(); 
}

jlong 
FileReader::tell()
{
  return _fileImpl.curSeekPos();
}


static 
void __dummyInstance()
{
  RFileReader br = new FileReader(RString("/temp/bla"));
}

} // io
} // acdk
