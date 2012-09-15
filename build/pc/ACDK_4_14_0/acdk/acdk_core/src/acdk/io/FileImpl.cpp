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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileImpl.cpp,v 1.12 2005/03/17 11:27:52 kommer Exp $


#include <acdk.h>
#include "FileImpl.h"
#include "File.h"

namespace acdk {
namespace io {
//virtual 
RStringArray 
FileImpl::list(IN(RFilenameFilter) filter/* = Nil*/, int listFlags/* = FileListBoth*/)
{
  if (isDirectory() == false)
    return Nil;
  RString cp = getPath();
  
  acdk::io::RFileArray files = getFileSystem()->listFiles(cp, listFlags);
  
  RStringArray erglist = new StringArray(0);
  int ergcount = 0;
  int i;
  acdk::io::File tf(this);
  for (i = 0; i < files->length(); i++) 
  {
    if (files[i]->getName()->length() == 0)
      continue;
    if (filter == Nil || filter->accept(&tf, files[i]->getName()) == true) {
      erglist->append(files[i]->getName());
    }
  }
  return erglist;
}

RFileArray 
FileImpl::listFiles(IN(RFileFilter) filter/* = Nil*/, int listFlags/* = FileListBoth*/)
{
  if (isDirectory() == false)
    return Nil;
  RString cp = getPath();
  acdk::io::RFileArray files = getFileSystem()->listFiles(cp, listFlags);
  
  acdk::io::RFileArray erglist = new acdk::io::FileArray(0);
  int ergcount = 0;
  for (int i = 0; i < files->length(); i++) {
    if (filter == Nil || filter->accept(files[i]) == true) {
      erglist->append(files[i]);
    }
  }
  return erglist;
}

RFileInfo 
FileImpl::getFileInfo()
{
  RString pname = getParentFile()->getCanonicalPath();
  RString name = getName();
  int flags = 0;
  if (exists() == true)
    flags |= FileInfoExists;
  if (isFile() == true)
    flags |= FileInfoIsFile;
  if (isDirectory() == true)
    flags |= FileInfoIsDir;

  if (canRead() == true)
    flags |= FileInfoCanRead;
  if (canWrite() == true)
    flags |= FileInfoCanWrite;
  jlong modt = lastModified();
  jlong crt = fileCreated();
  jlong fl = length();
#if defined(ACDK_OS_UNIX)
    if (name->startsWith(".") == true)
      flags |= FileInfoIsHidden;
#endif

  return new FileInfo(flags, pname, name, fl, crt, modt);
}

AbstractFileImpl::AbstractFileImpl(IN(RFileSystem) fsys, IN(RString) fpath)
: _fsys(fsys)
, _filePath(fpath)
{
  RString tp = fpath->replace(File::separatorChar(), '/');
  int idx = tp->lastIndexOf('/');
  if (idx != -1)
  {
    _dir = tp->substr(0, idx);
    _name = tp->substr(idx + 1);
  }
  else
  {
    _dir = "";
    _name = fpath;
  }
}


} // io
} // acdk



