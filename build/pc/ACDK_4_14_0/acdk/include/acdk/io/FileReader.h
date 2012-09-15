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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileReader.h,v 1.12 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_FileReader_h
#define acdk_io_FileReader_h

#include "AbstractStorageReader.h"
#include "FileReaderWriterImpl.h"

namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

class FileReader;
ACDK_DECL_CLASS(FileReader);

/** 
  Read bytes from a File.

  API: Java<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/

class ACDK_CORE_PUBLIC FileReader
: extends AbstractStorageReader
{
  ACDK_WITH_METAINFO(FileReader)
private:
  RString _fname;
  foreign FileReaderWriterImpl _fileImpl;
  /** will set true if if EndOfFile will be reached */
  bool _eof;
public :
  FileReader(IN(RFile) file, bool binary = true);
  FileReader(IN(RString) fileName, bool binary = true);
  FileReader(IN(RFileDescriptor) fd, bool dupl = false);
  virtual ~FileReader();
// Reader
  /// implements from Reader
  foreign virtual int available();
  /// implements from Reader
  foreign virtual void close();
  /// implements from Reader
  foreign virtual jlong seek(SeekPos seekrel, jlong seekpos);
  /// implements from Reader
  foreign virtual jlong skip(jlong n) { return seek(acdk::io::SeekCur, n); }
  /// implements from Reader
  foreign virtual int read();
  /// implements from Reader
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  /// implements from Reader
  foreign virtual int read(byte* buffer, int offset, int len);
  
  /// implements from Reader
  foreign virtual void reset();

  /// implements from Reader
  foreign virtual bool ready();
// Storage
  /// implements from Storage
  foreign virtual RString getDeviceName();
  /// implements from Storage
  foreign virtual bool isWriteable();
  /// implements from Storage
  foreign virtual bool isReadable();

//class defined
  virtual RString readLine();
  virtual void open(IN(RFile) fname);
  RFileDescriptor getFD();
  jlong fileLength();
  jlong tell();
  
};


} // io
} // acdk

#endif //acdk_io_FileReader_h

