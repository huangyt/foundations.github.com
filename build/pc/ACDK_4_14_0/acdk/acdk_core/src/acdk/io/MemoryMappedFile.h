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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/MemoryMappedFile.h,v 1.7 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_MemoryMappedFile_h
#define acdk_io_MemoryMappedFile_h

#include <acdk.h>
#include "File.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(MemoryMappedFile);



/** 
  Non function stub.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/04/09 19:26:45 $
  @todo in which context this class should be:
        Reader/Writer? File?
*/
class ACDK_CORE_PUBLIC MemoryMappedFile
: implements ::acdk::io::File
{
private:
  int _mappedAdress;
#ifdef ACDK_OS_WIN32
  HANDLE _mappedFileHandle;
#endif
public :
  MemoryMappedFile(IN(::acdk::lang::RString) fname, 
                  IN(::acdk::lang::RString) mode,
                  jlong offset, int len);
  MemoryMappedFile(IN(::acdk::io::File) file, jlong offset, int len);
  ~MemoryMappedFile();
  /**
    Mapps the file at file offset with len into memory
    @return The adress will be returned as int
  */
  int adress();
  void flush();
private:
  void _mapFile(jlong offset, int len);
  void _unmapFile();
};

} // io
} // acdk

#endif //acdk_io_MemoryMappedFile_h

