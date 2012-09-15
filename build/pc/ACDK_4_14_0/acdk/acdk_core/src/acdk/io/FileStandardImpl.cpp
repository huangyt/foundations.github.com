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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileStandardImpl.cpp,v 1.22 2005/05/02 23:07:41 kommer Exp $


#include <acdk.h>
#include <acdk/io/File.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/FileReader.h>
#include "../Config.h"
#include "FileStandardImpl.h"
#include "FileStatus.h"
#include <acdk/lang/ObjectArrayImpl.h>
#include <acdk/lang/Math.h>
#include <acdk/util/StringTokenizer.h>

#include <stdio.h>
#include <errno.h>

#ifdef HAS_UNISTD_H
#  include <unistd.h>
#endif

#include <fcntl.h>
#include <sys/types.h> 
#include <sys/stat.h>


#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#  include <direct.h>
#  include <io.h>
#  include <wchar.h>
#  include <windows.h>
#endif
#if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
#  include <dirent.h>
#endif

#include <acdk/lang/UnsupportedOperationException.h>

#if defined(ACDK_OS_WIN32)
#define trmdir _wrmdir
#define tunlink _wunlink
#define tmkdir _wmkdir
#define trename _wrename
#define topen _wopen
#else
#define trmdir rmdir
#define tunlink unlink
#define tmkdir mkdir
#define trename rename
#define topen open
#endif


namespace acdk {
namespace io {




RFileImpl 
FileStandardImpl::create(IN(RString) fname)
{
  return new FileStandardImpl(fname);
}



//virtual 
bool 
FileStandardImpl::exists()
{
   return FileStatus(getCanonicalPath()).exists();
}

//virtual 
bool 
FileStandardImpl::canRead()
{
  return FileStatus(getCanonicalPath()).canRead();
}

//virtual 
bool 
FileStandardImpl::canWrite()
{
  return FileStatus(getCanonicalPath()).canWrite();
}

//virtual 
bool 
FileStandardImpl::isDirectory()
{
  return FileStatus(getCanonicalPath()).isDirectory();
}

//virtual 
bool 
FileStandardImpl::isFile()
{
  return FileStatus(getCanonicalPath()).isFile();
}

//virtual 
bool 
FileStandardImpl::isHidden()
{
  return false;
}

//virtual 
jlong 
FileStandardImpl::length()
{
  return FileStatus(getCanonicalPath()).length();
}
  
//virtual 
bool 
FileStandardImpl::createNewFile()
{
  if (exists() == true)
    return false;

  RString cpath = getCanonicalPath()->convertToNative();
  int erg = topen(ACDK_API_CONSTCHARPTR(cpath->native_c_str()), O_CREAT | O_EXCL, S_IREAD | S_IWRITE);
  if (erg == -1)
    return false;
  close(erg);
  return true;
}


//virtual 
bool 
FileStandardImpl::deleteFile()
{
  if (exists() == false)
    return false;
  RString canp = getCanonicalPath()->convertToNative();

  if (isDirectory())
    return trmdir(ACDK_API_CONSTCHARPTR(canp->native_c_str())) == 0; 
  return tunlink(ACDK_API_CONSTCHARPTR(canp->native_c_str())) == 0;
}


RStringArray
FileStandardImpl::_listFiles(IN(RString) dir) 
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  RString tdir = dir + File::separator() + "*";
  WIN32_FIND_DATA fd;
  tdir = tdir->convertToNative();
  HANDLE fh = FindFirstFile(ACDK_API_CONSTCHARPTR(tdir->native_c_str()), &fd);
  if (fh == NULL)
    return new ObjectArrayImpl<RString>(0);
  RStringArray files = new ObjectArrayImpl<RString>(20);
  int count = 0;
  do {
    files->resize(count + 1);
    files[count] = SCS((const uc2char*)fd.cFileName);
    ++count;
  } while (FindNextFile(fh, &fd));
  FindClose(fh);
  files->resize(count);
  return files;
#endif // defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
  DIR* dp;
  RString tdir = dir->convertToNative();
  dp = opendir(dir->native_c_str());
  if (dp == NULL)
    return new ObjectArrayImpl<RString>(0);
  
  RStringArray files = new ObjectArrayImpl<RString>(20);
  struct dirent* dirp;
  int i = 0;
  for (; (dirp = readdir(dp)) != NULL; i++) {
    files->ensureCapacity(i + 1);
    files[i] = SCS(dirp->d_name);
  }
  closedir(dp);
  files->resize(i);
  return files;
#endif //ACDK_OS_UNIX
}


//virtual 
RStringArray 
FileStandardImpl::list(IN(RFilenameFilter) filter/* = Nil*/, int listFlags/* = FileListBoth*/)
{
  if (isDirectory() == false)
    return new StringArray(0);
  RString cp = getCanonicalPath();
  
  RFileArray files = getFileSystem()->listFiles(cp, listFlags);
  
  RStringArray erglist = new StringArray(0);
  int ergcount = 0;
  int i;
  File tf(this);
  for (i = 0; i < files->length(); i++) 
  {
    RString name = files[i]->getName();
    if (filter == Nil || filter->accept(&tf, name) == true) 
    {
      RString fqn = files[i]->getCanonicalPath();
      name = fqn->substr(cp->length() + 1);
      erglist->append(name);
    }
  }
  return erglist;
}

//virtual 
RFileArray 
FileStandardImpl::listFiles(IN(RFileFilter) filter/* = Nil*/, int listFlags/* = FileListBoth*/)
{
  if (isDirectory() == false)
    return Nil;
  RString cp = getCanonicalPath();
  RFileArray files = getFileSystem()->listFiles(cp, listFlags);
  if (filter == Nil)
    return files;
  RFileArray erglist = new FileArray(0);
  int ergcount = 0;
  for (int i = 0; i < files->length(); i++) 
  {
    if (filter->accept(files[i]) == true) 
    {
      erglist->append(files[i]);
    }
  }
  return erglist;
}

//virtual 
jlong 
FileStandardImpl::lastModified()
{
  if (exists() == false)
    return 0;
  return FileStatus(getCanonicalPath()).lastModified();
}

//virtual 
jlong 
FileStandardImpl::fileCreated()
{
  if (exists() == false)
    return 0;
  return FileStatus(getCanonicalPath()).created();
}

//virtual 
bool 
FileStandardImpl::mkdir(int mode/* = 0777*/) 
{
  if (exists() == true)
    return false;
  RString cpath = getCanonicalPath()->convertToNative();
#if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
  return ::tmkdir(cpath->native_c_str(), mode) == 0;
#else
  return ::tmkdir(ACDK_API_CONSTCHARPTR(cpath->native_c_str())) == 0;
#endif
}

//virtual 
bool 
FileStandardImpl::renameTo(IN(RFile) dest)
{
  //### check if same file implementation
  if (exists() == false)
    return false;
  if (isDirectory()) {
    _throwNotImplementedYet("File::renameTo()");
  }
  if (dest->exists())
    return false;
  RString cpath = getCanonicalPath()->convertToNative();
  RString destcpath = dest->getCanonicalPath()->convertToNative();
  return ::trename(ACDK_API_CONSTCHARPTR(cpath->native_c_str()), ACDK_API_CONSTCHARPTR(destcpath->native_c_str())) == 0;
}

//virtual 
bool 
FileStandardImpl::setLastModified(jlong time)
{
  if (exists() == false)
    return false;
  return FileStatus(getCanonicalPath()).lastModified(time);
}

//virtual 
bool 
FileStandardImpl::setFileCreated(jlong time)
{
  if (exists() == false)
    return false;
  return FileStatus(getCanonicalPath()).created(time);
}

//virtual 
RReader 
FileStandardImpl::getReader()
{
  return new FileReader(_path);
}

//virtual 
RWriter
FileStandardImpl::getWriter()
{
  return new FileWriter(_path);
}

//virtual 
RFileSystem 
FileStandardImpl::getFileSystem()
{
  return StandardFileSystem::standarFileSystem();
}

RFileInfo 
FileStandardImpl::getFileInfo()
{
#if defined(ACDK_OS_WIN32)
  return FileStatus(getPath()).getFileInfo();
#else
  return FileImpl::getFileInfo();
#endif
}

bool 
FileStandardImpl::setFileAttributes(int mask, int flags)
{
  return FileStatus(getPath()).setFileAttributes(mask, flags);
}

//static 
RString 
FileStandardImpl::fileUrlToFileName(IN(RString) furl)
{
  if (furl->startsWith("file:///") == false)
    return furl;
  RString fname = furl;
  fname = fname->substr(8);
  if (File::separatorChar() != '/')
    fname = fname->replace('/', File::separatorChar());
  return fname;
}

} // io
} // acdk




