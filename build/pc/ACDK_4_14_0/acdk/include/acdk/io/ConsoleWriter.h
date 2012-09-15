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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ConsoleWriter.h,v 1.15 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_ConsoleWriter_h
#define acdk_io_ConsoleWriter_h


#include <stdio.h>

#include "AbstractStorageWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

/**
  Channel of a console
*/
enum ConsoleOutChannel 
{
  /// chanel is invalid/not set
  InvalidConsoleOutChannel = -1,
  // standard output 
  CoutOutChannel = 1,
  // error output
  CerrOutChannel = 2
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, ConsoleOutChannel);

ACDK_DECL_CLASS(ConsoleWriter);

/** 
  Writes bytes to a console.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:44 $
  
*/
class ACDK_CORE_PUBLIC ConsoleWriter
: extends AbstractStorageWriter
{
  ACDK_WITH_METAINFO(ConsoleWriter)
public :
  FILE* _fptr;
  ConsoleWriter(ConsoleOutChannel fd = InvalidConsoleOutChannel, IN(RObject) iolock = Nil);
  virtual ~ConsoleWriter();
  
  foreign virtual void flush();
  foreign virtual void close();
  foreign virtual void write(const byte* cstr, int offset, int len);
  foreign virtual void write(byte c);
  foreign virtual void write(IN(RbyteArray) ch, int offset, int len);
    
// Storage
  foreign virtual RString getDeviceName();
  foreign virtual bool isWriteable();
  foreign virtual bool isReadable();
  
protected:
  
};

} // io
} // acdk

#endif //acdk_io_ConsoleWriter_h

