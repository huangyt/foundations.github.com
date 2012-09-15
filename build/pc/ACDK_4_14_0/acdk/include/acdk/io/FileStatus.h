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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileStatus.h,v 1.18 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_FileStatus_h
#define acdk_io_FileStatus_h

#include <acdk.h>
#include "File.h"
#include "FileAbstractImpl.h"
#if defined(ACDK_OS_WIN32) && !defined(__BORLANDC__)
# include <sys/utime.h>
#else
# include <utime.h>
#endif
#include <acdk/util/SysDate.h>

#ifdef ACDK_OS_LINUX
#include <unistd.h>
#endif

namespace acdk {
namespace io {

#ifndef __S_IFDIR
#  define __S_IFDIR _S_IFDIR
#endif
#ifndef __S_IFREG
#  define __S_IFREG _S_IFREG
#endif
#ifndef __S_IREAD
#  define __S_IREAD _S_IREAD
#endif
#ifndef __S_IWRITE
#  define __S_IWRITE _S_IWRITE
#endif
#ifndef S_IFDIR
#  define S_IFDIR __S_IFDIR
#endif
#ifndef S_IREAD
#  define S_IREAD __S_IREAD
#endif
#ifndef S_IWRITE
#  define S_IWRITE __S_IWRITE
#endif
#ifndef S_IFREG
# define S_IWRITE __S_IFREG
#endif

#ifndef R_OK
#  define R_OK    4               /* Test for read permission.  */
#  define W_OK    2               /* Test for write permission.  */
#  define X_OK    1               /* Test for execute permission.  */
#  define F_OK    0               /* Test for existence.  */  
#endif

ACDK_DECL_CLASS(FileStatus);

#if defined(UNICODE) && defined(ACDK_OS_WIN32)
#define taccess _waccess
#define tstat _wstat
#define stat _stat
#else
#define taccess access
#define tstat stat

#endif

/**
  FileStatus operates on normal files
  Normally doesn't used directly
  @see FileInfo
*/
class ACDK_CORE_PUBLIC FileStatus
: extends acdk::lang::Object
{
  RString _path;
public:
  FileStatus(IN(RString) fname) : _path(fname->convertToNative()) { }
  bool exists() { return taccess(ACDK_API_CONSTCHARPTR(_path->native_c_str()), F_OK) == 0; }
  bool canRead() { return taccess(ACDK_API_CONSTCHARPTR(_path->native_c_str()), R_OK) == 0; }
  bool canWrite() { return taccess(ACDK_API_CONSTCHARPTR(_path->native_c_str()), W_OK) == 0; }
  bool isDirectory() 
  { 
    struct stat sbuf;
    if (tstat(ACDK_API_CONSTCHARPTR(_path->native_c_str()), &sbuf) != 0)
      return false;
    return (sbuf.st_mode & S_IFDIR) == S_IFDIR;
  }
  bool isFile()
  {
    struct stat sbuf;
    if (tstat(ACDK_API_CONSTCHARPTR(_path->native_c_str()), &sbuf) != 0)
      return false;
    return (sbuf.st_mode & S_IFREG) == S_IFREG;
  }
  jlong length()
  {
    struct stat sbuf;
    if (tstat(ACDK_API_CONSTCHARPTR(_path->native_c_str()), &sbuf) != 0)
      return 0;
    if (tstat(ACDK_API_CONSTCHARPTR(_path->native_c_str()), &sbuf) != 0)
      return 0;
    return sbuf.st_size;
  }
  jlong lastAccessed();
  bool lastAccessed(long accesstime);
  /**
    milliseconds since 1970-01-01T00:00:00:000
  */
  jlong lastModified();
  /**
    milliseconds since 1970-01-01T00:00:00:000
  */
  bool lastModified(jlong modtime);
  /**
    milliseconds since 1970-01-01T00:00:00:000
  */
  jlong created();
  /** 
    set new Created time. 
    Only works on WIN32
  */
  bool created(jlong createtime);
  /**
    return the file info for this file
  */
  RFileInfo getFileInfo();
  /**
    @see File::setFileAttributes()
  */
  bool setFileAttributes(int mask, int flags);
};

} // namespace io
} // namespace acdk


#endif //acdk_io_FileStatus_h

