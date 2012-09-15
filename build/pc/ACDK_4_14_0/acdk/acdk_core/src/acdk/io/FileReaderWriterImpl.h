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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileReaderWriterImpl.h,v 1.17 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_FileReaderWriterImpl_h
#define acdk_io_FileReaderWriterImpl_h

//#include "File.h"
#include "Reader.h"
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAS_UNISTD_H
#  include <unistd.h>
#endif
#ifdef ACDK_OS_WIN32
#  include <io.h>
#endif


namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

ACDK_DECL_CLASS(File);

/** 
  Internal Implementation base of FileReader and FileWriter.
  API: Java<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.17 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC FileReaderWriterImpl
{
protected:
  RFileDescriptor _fd;
  bool _ownFileDescriptor;
  bool _eof;
  bool _eofReturned;
public :
  /**
    omode and pmode see system call open(const char *filename, int oflag, int pmode)
  */
  FileReaderWriterImpl(IN(RFile) file, int omode, int pmode = S_IREAD | S_IWRITE);
  /**
    omode and pmode see system call open(const char *filename, int oflag, int pmode)
  */
  FileReaderWriterImpl(IN(RString) fileName, int omode, int pmode = S_IREAD | S_IWRITE);
  /**
    same as above, but with "r", "rw"-syntax
    @param file the file to open
    @param mode "r" or "rw" for read/write access
  */
  FileReaderWriterImpl(IN(RFile) file, IN(RString) mode);
  /**
    @param fd the native file descriptor
  */
  FileReaderWriterImpl(IN(RFileDescriptor) fd, bool dupl = false);
  virtual ~FileReaderWriterImpl();

  int available();
  void close();
  jlong seek(SeekPos seekrel, jlong seekpos);
  jlong skip(jlong n) { return seek(acdk::io::SeekCur, n); }
  int read();
  int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  int read(byte* buffer, int offset, int len);
  RString readLine();

  void write(byte c);
  void write(const byte* cstr, int offset, int len);
  void write(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  void flush();
  
  /**
    omode and pmode see system call open(const byte *filename, int oflag, int pmode)
  */
  void open(IN(RFile) fname, int omode, int pmode = S_IREAD | S_IWRITE);
  RFileDescriptor getFD();
  jlong fileLength();
  jlong curSeekPos();
  bool isReadable();
  bool isWriteable();
  void setLength(jlong newLength);
  bool eof() { return _eof; }
private:
  void _setFD(int fd, int omode);  // friend of FileDescriptor, needed for open()
  friend class FileDescriptor;
};


} // io
} // acdk

// VC6 needs this for FRWI, but FD includes us ...

#ifndef acdk_io_FileDescriptor_h
#include "FileDescriptor.h"
#endif

#endif //acdk_io_FileReader_h

