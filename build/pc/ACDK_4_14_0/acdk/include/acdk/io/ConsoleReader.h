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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ConsoleReader.h,v 1.17 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_ConsoleReader_h
#define acdk_io_ConsoleReader_h

#include "AbstractStorageReader.h"
#include "FileDescriptor.h"
#include <stdio.h>

namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

ACDK_DECL_CLASS(ConsoleReader);

/**
  helper enum for ConsoleReader
*/
enum InChannel
{
  InvalidInChannel = -1,
  CinInChannel = 0
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, InChannel);

/** 
  Read bytes from a console (standard input)
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.17 $
  @date $Date: 2005/04/09 19:26:44 $
  
*/
class ACDK_CORE_PUBLIC ConsoleReader
: extends AbstractStorageReader
{
  ACDK_WITH_METAINFO(ConsoleReader)
private:
  int _last;
  RFileDescriptor _fd;
  bool _eof;
public:
 
  ConsoleReader(InChannel fd = InvalidInChannel);
  //virtual int available() { return 0; }
  //virtual void close() { }
  foreign virtual jlong seek(SeekPos seekrel, jlong seekpos);
  foreign virtual int read();
  
  //virtual RString readLine();
  virtual int unread(byte ch);
  
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1) { return AbstractStorageReader::read(buffer, offset, len); }
  foreign virtual int read(byte* buffer, int offset, int len) { return AbstractStorageReader::read(buffer, offset, len); }
  foreign virtual void reset();
  foreign virtual bool ready();
// Storage
  foreign virtual RString getDeviceName();
  foreign virtual bool isReadable() { return true; }
  foreign virtual bool isWriteable() { return false; }
};

} // io
} // acdk

#endif //acdk_io_ConsoleReader_h

