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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileDescriptor.h,v 1.14 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_FileDescriptor_h
#define acdk_io_FileDescriptor_h


#include "FileReaderWriterImpl.h"
#include "IOException.h"
#ifdef ACDK_OS_WIN32
#  include <io.h>
#endif

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(FileDescriptor);

/**
  Represents a operation system file descriptor.

  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.14 $
  @date $Date: 2005/04/09 19:26:44 $

*/
class ACDK_CORE_PUBLIC FileDescriptor
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(FileDescriptor)
protected:
  foreign int _fd;
  foreign int _omode;

public:
  FileDescriptor(int fd, int omode, bool dupl = false);
  FileDescriptor(IN(RFileDescriptor) f, bool dupl = false);

  static RFileDescriptor in;
  static RFileDescriptor out;
  static RFileDescriptor err;
  FileDescriptor() : _fd(-1), _omode(0) { }

  virtual bool valid() {  return _fd != -1;  }
  virtual void sync();
  int c_fd() { return _fd; }
  void setCloseOnExec(bool val);
  bool getCloseOnExec();
  int omode() { return _omode; }

private:
  friend class FileReaderWriterImpl;
};

inline
bool operator==(IN(RFileDescriptor) f, IN(RFileDescriptor) s)
{
  return f.impl() == s.impl() ||
         (f.impl() != 0 && s.impl() != 0 && f->c_fd() == s->c_fd());
}

inline
bool operator!=(IN(RFileDescriptor) f, IN(RFileDescriptor) s)
{
  return operator==(f, s) == false;
}

} // io
} // acdk

#endif //acdk_io_FileDescriptor_h


