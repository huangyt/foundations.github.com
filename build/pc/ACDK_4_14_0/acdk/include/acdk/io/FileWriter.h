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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileWriter.h,v 1.11 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_FileWriter_h
#define acdk_io_FileWriter_h


#include "FileReaderWriterImpl.h"
#include "AbstractStorageWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;


ACDK_DECL_CLASS(FileWriter);

/** 
  byte Writer into a file
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC FileWriter
: extends AbstractStorageWriter
{
  ACDK_WITH_METAINFO(FileWriter)
private:
  RString _fname;
  FileReaderWriterImpl _fileImpl;
public :
  FileWriter(IN(RFile) file, bool append = false, bool binary = true); 
  FileWriter(IN(RFileDescriptor) fd, bool dupl = false);
  FileWriter(IN(RString) fileName, bool append = false, bool binary = true);
  virtual ~FileWriter();

// Writer
  /// implements form Writer
  foreign virtual void write(byte c);
  /// implements form Writer
  foreign virtual void write(const byte* cstr, int offset, int len);
  /// implements form Writer
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1)
  {
    write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
  }
  /// implements form Writer
  foreign virtual void flush();
  /// implements form Writer
  foreign virtual void close();

// Storage
  /// implements form Storage
  foreign virtual RString getDeviceName();
  /// implements form Storage
  foreign virtual bool isWriteable();
  /// implements form Storage
  foreign virtual bool isReadable();
// won
  jlong tell();
};


} // io
} // acdk


#endif //acdk_io_FileWriter_h

