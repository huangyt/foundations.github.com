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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/StandardFileSystem.cpp,v 1.12 2005/03/17 11:30:35 kommer Exp $

#include <acdk.h>
#include <acdk/io/IOException.h>
#include <acdk/lang/System.h>
#include "FileSystem.h"
#include "FileStatus.h"
#include "FileStandardImpl.h"
#include "RessourceFileSystem.h"

#include  <acdk/util/HashMap.h>
#include <acdk/lang/ref/WeakReference.h>
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#  include <direct.h>
#  include <io.h>
#  include <windows.h>
#endif
#if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
#  include <dirent.h>
#endif


namespace acdk {
namespace io {


//static 
RFileSystem 
StandardFileSystem::standarFileSystem()
{
  static StandardFileSystem standardFileSystem;
  return &standardFileSystem;
}


//virtual 
bool 
StandardFileSystem::ownsFile(IN(RString) fname)
{
#ifdef ACDK_OS_UNIX
  if (fname->startsWith("/") == true)
    return true;
  else
    return false;
#endif
#ifdef ACDK_OS_WIN32
  if (fname->startsWith("\\\\") == true)
    return true;
  if (fname->length() > 1 && fname->charAt(1) == ':')
    return true;
  return false;
#endif
}

//virtual 
RString 
StandardFileSystem::getRootName()
{
#ifdef ACDK_OS_UNIX
  return "/";
#endif
#ifdef ACDK_OS_WIN32
  return "\\\\";
#endif
}
#if defined(UNICODE) && defined(ACDK_OS_WIN32)
#define tstrcmp wcscmp
#else
#define tstrcmp strcmp
#endif

RFileArray 
StandardFileSystem::listFiles(IN(RString) dir, int listflags)
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  RString tdir = dir + File::separator() + "*";
  WIN32_FIND_DATA fd;
  RString nstr = tdir->convertToNative();
  HANDLE fh = FindFirstFile(ACDK_API_CONSTCHARPTR(nstr->native_c_str()), &fd);
  if (fh == INVALID_HANDLE_VALUE)
  {
    THROW1(IOException, "Error accessing dir=[" + tdir + "]: " + System::getLastError());
  }
  RFileArray files = new FileArray(0);

  if (fh == NULL)
    return files;
  int count = 0;
  do {
    if (tstrcmp(fd.cFileName, _T(".")) == 0 || tstrcmp(fd.cFileName, _T("..")) == 0)
      continue;
    RString s = SCS((const uc2char*)fd.cFileName);
    s = s->narrow();
    RString fqs = dir + File::separator() + s;

    bool isdir = FileStatus(fqs).isDirectory();
    if (isdir == true)
    {
      if ((listflags & FileListDirectories) == FileListDirectories)
       files->append(new File(new FileStandardImpl(fqs)));
    }
    else
    {
      if ((listflags & FileListFiles) == FileListFiles)
       files->append(new File(new FileStandardImpl(fqs)));
    }
    if (isdir && (listflags & FileListRecursive))
    {
      try {
        RFileArray sa = listFiles(fqs, listflags);
        files->concat(sa);
      } catch (RIOException ex) {
        if (FileListAllReadable & listflags)
          ; //System::out->println("StandardFileSystem::listFiles: " + ex->getMessage());
        else
          throw ex;
      }
    }
  } while (FindNextFile(fh, &fd));
  FindClose(fh);
  return files;
#endif // defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)

  DIR* dp = opendir(dir->c_str());
  
  RFileArray files = new FileArray(0);

  if (dp == NULL)
    return files;
  
  struct dirent* dirp;
  int i = 0;
  for (; (dirp = readdir(dp)) != NULL; i++) 
  {

    RString s = SCS(dirp->d_name);
    if (s->equals(".") == true || s->equals("..") == true)
      continue;

    RString fqs = dir + File::separator() + s;
    
    bool isdir = FileStatus(fqs).isDirectory();

    if (isdir == true)
    {
      if ((listflags & FileListDirectories) == FileListDirectories)
       files->append(new File(new FileStandardImpl(fqs)));
    }
    else
    {
      if ((listflags & FileListFiles) == FileListFiles)
       files->append(new File(new FileStandardImpl(fqs)));
    }

    if (isdir && (listflags & FileListRecursive))
    {
      try {
        RFileArray sa = listFiles(fqs, listflags);
        files->concat(sa);
      } catch (RIOException ex) {
        if (FileListAllReadable & listflags)
          ; //System::out->println("StandardFileSystem::listFiles: " + ex->getMessage());
        else
          throw ex;
      }

    }
  }
  closedir(dp);
  return files;
#endif //ACDK_OS_UNIX
}

//virtual 
RFile 
StandardFileSystem::file(IN(RString) path)
{
  return new File(new FileStandardImpl(FileStandardImpl::fileUrlToFileName(path)));
}

 
RFileImpl 
StandardFileSystem::getFileImpl(IN(RString) fqpath)
{
  return new FileStandardImpl(FileStandardImpl::fileUrlToFileName(fqpath));
}

} // io
} // acdk




