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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileWriter.cpp,v 1.13 2005/03/08 12:45:36 kommer Exp $






#include <acdk.h>
#include "FileWriter.h"
#include "File.h"
#include "FileDescriptor.h"

#include <fcntl.h>

#if defined(__MWERKS__) && !defined(_O_BINARY)
# define _O_BINARY O_BINARY 
#endif

namespace acdk {
namespace io {

using namespace acdk::lang;

FileWriter::FileWriter(IN(RFile) file, bool append, bool binary) 
: AbstractStorageWriter(),
  _fname(file->getCanonicalPath()),
  _fileImpl(file, (append == true ? O_RDWR | O_APPEND | O_CREAT : O_RDWR | O_CREAT | O_TRUNC)
#ifdef ACDK_OS_WIN32
            | (binary == true ? _O_BINARY : 0)
#endif // ACDK_OS_WIN32
            )
{
}

FileWriter::FileWriter(IN(RFileDescriptor) fd, bool dupl) 
: AbstractStorageWriter(),
  _fileImpl(fd, dupl)
{
}

FileWriter::FileWriter(IN(RString) fileName, bool append, bool binary)  
: AbstractStorageWriter(),
  _fname(fileName),
  _fileImpl(fileName, 
            (append == true ? O_RDWR | O_APPEND | O_CREAT : O_RDWR | O_CREAT | O_TRUNC)
#ifdef ACDK_OS_WIN32
            | (binary == true ? _O_BINARY : 0)
#endif // ACDK_OS_WIN32
            )
{

}

//virtual 
FileWriter::~FileWriter()
{
  close();
}

//virtual 
void 
FileWriter::write(byte c)
{
  _fileImpl.write(c);
}

//virtual 
void 
FileWriter::write(const byte* cstr, int offset, int len)
{
  _fileImpl.write(cstr, offset, len);
}

//virtual 
void 
FileWriter::flush()
{
  _fileImpl.flush();
}

//virtual 
void 
FileWriter::close()
{
  _fileImpl.close();
}

//virtual 
RString 
FileWriter::getDeviceName()
{
  if (_fname != Nil)
    return _fname;
  return "[FD:" + String::valueOf(_fileImpl.getFD()->c_fd()) + "]";
}

//virtual 
bool 
FileWriter::isWriteable()  
{ 
  return _fileImpl.isWriteable(); 
}

//virtual 
bool 
FileWriter::isReadable()  
{ 
  return _fileImpl.isReadable(); 
}

jlong 
FileWriter::tell()
{
  return _fileImpl.curSeekPos();
}



} // io
} // acdk
