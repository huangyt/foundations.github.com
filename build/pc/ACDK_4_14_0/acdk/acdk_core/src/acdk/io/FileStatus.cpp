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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileStatus.cpp,v 1.10 2005/03/11 14:25:43 kommer Exp $


#include <acdk.h>
#include "FileStatus.h"

namespace acdk {
namespace io {

jlong 
FileStatus::lastAccessed()
{
#if defined(ACDK_OS_WIN32)
  FILETIME creationTime;
  FILETIME lastAccessTime;
  FILETIME lastWriteTime;
  
  HANDLE hf = CreateFile(ACDK_API_CONSTCHARPTR(_path->native_c_str()), GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (hf == INVALID_HANDLE_VALUE)
    return 0;
  GetFileTime(hf, &creationTime, &lastAccessTime, &lastWriteTime);
  CloseHandle(hf);
  return acdk::util::SysDate::fileTimeToTime(lastWriteTime);
#else 
  struct stat sbuf;
  if (stat(_path->native_c_str(), &sbuf) != 0)
    return 0;
  return jlong(sbuf.st_atime) * 1000;
#endif
}

bool 
FileStatus::lastAccessed(long accesstime)
{
#if defined(ACDK_OS_WIN32)
  HANDLE hf = CreateFile(ACDK_API_CONSTCHARPTR(_path->native_c_str()), GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (hf == INVALID_HANDLE_VALUE)
    return false;
  FILETIME cft;
  acdk::util::SysDate::timeToFileTime(accesstime, cft);
  BOOL bret = SetFileTime(hf, NULL, &cft, NULL);
  CloseHandle(hf);
  return bret == TRUE;    
#else
  utimbuf buf;
  buf.actime = (long)(accesstime / 1000);
  buf.modtime = (long)lastModified() / 1000;
  return utime(_path->c_str(), &buf) == 0;
#endif
}

jlong 
FileStatus::lastModified()
{
#if defined(ACDK_OS_WIN32)
  FILETIME creationTime;
  FILETIME lastAccessTime;
  FILETIME lastWriteTime;
  HANDLE hf = CreateFile(ACDK_API_CONSTCHARPTR(_path->native_c_str()), GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (hf == INVALID_HANDLE_VALUE)
    return 0;
  GetFileTime(hf, &creationTime, &lastAccessTime, &lastWriteTime);
  CloseHandle(hf);
  return acdk::util::SysDate::fileTimeToTime(lastWriteTime);
#else 
  
  struct stat sbuf;
  if (tstat(_path->native_c_str(), &sbuf) != 0)
    return 0;
  return jlong(sbuf.st_mtime) * 1000;
#endif
}

bool 
FileStatus::lastModified(jlong modtime)
{
#if defined(ACDK_OS_WIN32)
  HANDLE hf = CreateFile(ACDK_API_CONSTCHARPTR(_path->native_c_str()), GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (hf == INVALID_HANDLE_VALUE)
    return false;
  FILETIME cft;
  acdk::util::SysDate::timeToFileTime(modtime, cft);
  BOOL bret = SetFileTime(hf, NULL, NULL, &cft);
  CloseHandle(hf);
  return bret == TRUE;    
#else
  utimbuf buf;
  buf.actime = (long)(lastAccessed() / 1000);
  buf.modtime = (long)(modtime / 1000);
  return utime(_path->c_str(), &buf) == 0;
#endif
}

jlong 
FileStatus::created()
{
#if defined(ACDK_OS_WIN32)
  FILETIME creationTime;
  FILETIME lastAccessTime;
  FILETIME lastWriteTime;
  HANDLE hf = CreateFile(ACDK_API_CONSTCHARPTR(_path->native_c_str()), GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (hf == INVALID_HANDLE_VALUE)
    return 0;
  GetFileTime(hf, &creationTime, &lastAccessTime, &lastWriteTime);
  CloseHandle(hf);
  return acdk::util::SysDate::fileTimeToTime(creationTime);
  
#else 
  struct stat sbuf;
  if (stat(_path->c_str(), &sbuf) != 0)
    return 0;
  return jlong(sbuf.st_ctime) * 1000;
#endif
}

bool 
FileStatus::created(jlong createtime)
{
#if defined(ACDK_OS_WIN32)
  HANDLE hf = CreateFile(ACDK_API_CONSTCHARPTR(_path->native_c_str()), GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (hf == INVALID_HANDLE_VALUE)
    return false;
  FILETIME cft;
  acdk::util::SysDate::timeToFileTime(createtime, cft);
  BOOL bret = SetFileTime(hf, &cft, NULL, NULL);
  CloseHandle(hf);
  return bret == TRUE;    
#else
  return false;
#endif
}

#if defined(ACDK_OS_WIN32)
int win32FileAttrToFileInfoAttr(int attr)
{
  int ret = 0;
  if (attr & FILE_ATTRIBUTE_DIRECTORY)
    ret |= FileInfoIsDir;
  else
    ret |= FileInfoIsFile;

  if (attr & FILE_ATTRIBUTE_HIDDEN)
    ret |= FileInfoIsHidden;
  if (attr & FILE_ATTRIBUTE_READONLY)
    ret |= FileInfoCanRead;
  else
    ret |= FileInfoCanRead | FileInfoCanWrite;
  if (attr & FILE_ATTRIBUTE_ARCHIVE)
    ret |= FileInfoHasABit;
  return ret;
}
#endif

RFileInfo 
FileStatus::getFileInfo()
{
  File file(_path);
  RString parent = file.getParent();
  RString name = file.getName();

#if defined(ACDK_OS_WIN32)
  WIN32_FILE_ATTRIBUTE_DATA fattr;
  if (GetFileAttributesEx(ACDK_API_CONSTCHARPTR(_path->native_c_str()), GetFileExInfoStandard, &fattr) == FALSE)
    return new FileInfo(0, parent, name);
  int flags = win32FileAttrToFileInfoAttr(fattr.dwFileAttributes);
  jlong createTime = acdk::util::SysDate::fileTimeToTime(fattr.ftCreationTime);
  jlong accTime = acdk::util::SysDate::fileTimeToTime(fattr.ftLastAccessTime);
  jlong modTime = acdk::util::SysDate::fileTimeToTime(fattr.ftLastWriteTime);
  jlong filesize = ((jlong)fattr.nFileSizeHigh) << 32 + jlong(fattr.nFileSizeLow);
  return new FileInfo(flags, parent, name, filesize, createTime, modTime);
#else
  return file.getFileImpl()->FileImpl::getFileInfo();
#endif
}


bool 
FileStatus::setFileAttributes(int mask, int flags)
{
#if defined(ACDK_OS_WIN32)
  DWORD oldFlags = GetFileAttributes(ACDK_API_CONSTCHARPTR(_path->native_c_str()));
  DWORD newFlags = oldFlags;
  if (mask & FileInfoIsHidden)
  {
    if (flags & FileInfoIsHidden)
      newFlags |= FILE_ATTRIBUTE_HIDDEN;
    else
      newFlags &= ~FILE_ATTRIBUTE_HIDDEN;
  }
  if (mask & FileInfoCanWrite)
  {
    if (flags & FileInfoCanWrite)
      newFlags &= ~FILE_ATTRIBUTE_READONLY;
    else
      newFlags |= FILE_ATTRIBUTE_READONLY;
  }
  if (mask & FileInfoHasABit)
  {
    if (flags & FileInfoHasABit)
      newFlags |= FILE_ATTRIBUTE_ARCHIVE;
    else
      newFlags &= ~FILE_ATTRIBUTE_ARCHIVE;
  }

  if (oldFlags != newFlags)
  {
    if (SetFileAttributes(ACDK_API_CONSTCHARPTR(_path->native_c_str()), newFlags) == FALSE)
      return false;
  }
  // ### todo handle read permission via _chmod
  return true;
#else
  struct stat sbuf;
  if (stat(ACDK_API_CONSTCHARPTR(_path->native_c_str()), &sbuf) != 0)
    return false;
  int oldFlags = sbuf.st_mode;
  int newFlags = oldFlags;
  if (mask & FileInfoCanWrite)
  {
    if (flags & FileInfoCanWrite)
      newFlags |= S_IWRITE;
    else
      newFlags &= ~S_IWRITE;
  }
  if (mask & FileInfoCanRead)
  {
    if (flags & FileInfoCanRead)
      newFlags |= S_IREAD;
    else
      newFlags &= ~S_IREAD;
  }
  if (mask & FileInfoCanExec)
  {
    if (flags & FileInfoCanExec)
      newFlags |= S_IEXEC;
    else
      newFlags &= ~S_IEXEC;
  }
  if (newFlags == oldFlags)
    return true;
  if (chmod(ACDK_API_CONSTCHARPTR(_path->native_c_str()), newFlags) != 0)
    return false;
  return true;
#endif

}

} // namespace io
} // namespace acdk




