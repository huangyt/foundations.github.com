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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ConsoleCharReader.h,v 1.9 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_ConsoleCharReader_h
#define acdk_io_ConsoleCharReader_h

#include "ConsoleReader.h"
#include <stdio.h>

#include "AbstractCharReader.h"


namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

ACDK_DECL_CLASS(ConsoleCharReader);

/** 
  Reads characters from a console (standard input from the process).

  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:44 $
  
*/
class ACDK_CORE_PUBLIC ConsoleCharReader
: extends AbstractCharReader
{
  ACDK_WITH_METAINFO(ConsoleCharReader)

private:
#ifdef ACDK_OS_WIN32
  foreign HANDLE _handle;
#endif
  // may be Nil in ACDK_OS_WIN32
  acdk::locale::RDecoder _decoder;
  RCharReader _in;
  
public:
  ConsoleCharReader(IN(RObject) iolock = Nil, IN(acdk::locale::RDecoder) decoder = Nil);

  foreign virtual int readChar();
  foreign virtual void close();
  foreign virtual RReader getReader(IN(acdk::locale::REncoder) encoder = Nil);
};

} // io
} // acdk

#endif //acdk_io_ConsoleCharReader_h

