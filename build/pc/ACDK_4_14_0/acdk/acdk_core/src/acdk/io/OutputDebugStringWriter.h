// -*- c++ -*-
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/OutputDebugStringWriter.h,v 1.4 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_OutputDebugStringWriter_h
#define acdk_io_OutputDebugStringWriter_h

#include <acdk.h>
#include "AbstractStorageWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;


ACDK_DECL_CLASS(OutputDebugStringWriter);

/** 
  On MS Windows application this Writer 
  uses OutputDebugStringA() to write to debugger
  console.
  On other platform this just uses printf().

  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC OutputDebugStringWriter
: extends AbstractStorageWriter
{
  //ACDK_WITH_METAINFO(OutputDebugStringWriter)
public :
  OutputDebugStringWriter()  
  : AbstractStorageWriter()
  {
  }
  virtual void write(const byte* cstr, int offset, int len)
  {
    cstr += offset;
#if defined(ACDK_OS_WIN32)
    OutputDebugStringA((const char*)cstr);
#else
    printf((const char*)cstr);
#endif
  }
  void write(byte c)
  {
    char buf[2]; buf[0] = c; buf[1] = 0;
#if defined(ACDK_OS_WIN32)
    OutputDebugStringA(buf);
#else
    printf(buf);
#endif
  }
  virtual void flush() {}
  foreign virtual void close() {}
// storage
  foreign virtual acdk::lang::RString getDeviceName() { return constr("Win32OutputDebugString"); }
  foreign virtual bool isWriteable() { return true; }
  foreign virtual bool isReadable() { return false; }
};


} // io
} // acdk

#endif //acdk_io_OutputDebugStringWriter_h

