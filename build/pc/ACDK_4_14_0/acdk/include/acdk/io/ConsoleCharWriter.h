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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ConsoleCharWriter.h,v 1.11 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_ConsoleCharWriter_h
#define acdk_io_ConsoleCharWriter_h

#include "AbstractCharWriter.h"
#include "ConsoleWriter.h"

#include <stdio.h>

namespace acdk {
namespace io {


enum ConsoleOutChannel;

ACDK_DECL_CLASS(ConsoleCharWriter);

/** 
  Write character to a Console (standard output or error output)
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:44 $
  
*/
class ACDK_CORE_PUBLIC ConsoleCharWriter
: extends AbstractCharWriter
{
  ACDK_WITH_METAINFO(ConsoleCharWriter)
public :
 
  
#if defined(ACDK_OS_WIN32)
  HANDLE _handle;
#endif
protected:
  
  acdk::locale::REncoder _encoder;
  /// may be Nil on Win32
  RCharWriter _out;
  ConsoleOutChannel _channel;
  
public:
  ConsoleCharWriter(ConsoleOutChannel fd, IN(RObject) iolock = Nil, IN(acdk::locale::REncoder) encoder = Nil);
  ~ConsoleCharWriter();
  foreign void writeChar(char c);
  foreign void writeChar(uc2char c);
  foreign void writeString(IN(RString) str);
  foreign virtual void flush();
  foreign virtual void close();
  foreign virtual RWriter getWriter(IN(acdk::locale::RDecoder) decoder = Nil);
  
};

} // io
} // acdk

#endif //acdk_io_ConsoleCharWriter_h

