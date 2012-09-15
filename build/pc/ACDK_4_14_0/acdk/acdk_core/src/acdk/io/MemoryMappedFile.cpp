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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/MemoryMappedFile.cpp,v 1.8 2005/03/11 14:25:43 kommer Exp $


#include <acdk.h>
#include "MemoryMappedFile.h"

namespace acdk {
namespace io {


MemoryMappedFile::MemoryMappedFile(IN(::acdk::lang::RString) fname, IN(::acdk::lang::RString) mode, 
                                   jlong offset, int len)
: File(fname, mode)
, _mappedAdress(0)
#ifdef ACDK_OS_WIN32
, _mappedFileHandle(NULL)
#endif
{
  _mapFile(offset, len);
}

MemoryMappedFile::MemoryMappedFile(IN(::acdk::io::File) file,
                                    jlong offset, int len)
: File(file)
, _mappedAdress(0)
#ifdef ACDK_OS_WIN32
, _mappedFileHandle(NULL)
#endif
{
  _mapFile(offset, len);
}


MemoryMappedFile::~MemoryMappedFile()
{
#ifdef ACDK_OS_WIN32
  CloseHandle(_mappedFileHandle);
#endif
}



void 
MemoryMappedFile::_mapFile(jlong offset, int len)
{
#ifdef ACDK_OS_WIN32
  OFSTRUCT ofstruct;
  memset(&ofstruct, 0, sizeof(ofstruct));
  RString nstr = getName()->convertToNative();
  HANDLE fh = CreateFile(ACDK_API_CONSTCHARPTR(nstr->native_c_str()), GENERIC_READ | GENERIC_WRITE, 
                          0, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  _mappedFileHandle = CreateFileMapping(fh, NULL, PAGE_READONLY, 0, 0, NULL);
  _mappedAdress = (int)MapViewOfFile(_mappedFileHandle, FILE_MAP_READ, offset >> 32, offset, len);
  CloseHandle(fh);
#endif

  
}

void 
MemoryMappedFile::_unmapFile()
{
#ifdef ACDK_OS_WIN32
  UnmapViewOfFile((void*)_mappedAdress);
  _mappedAdress = 0;
#endif

}

} // io
} // acdk


