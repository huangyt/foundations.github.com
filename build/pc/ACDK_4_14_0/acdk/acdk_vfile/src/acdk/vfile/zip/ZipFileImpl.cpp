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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/zip/ZipFileImpl.cpp,v 1.8 2005/02/05 10:45:33 kommer Exp $


#include <acdk.h>
#include "ZipFileImpl.h"
#include "ZipFileReader.h"

namespace acdk {
namespace vfile {
namespace zip {


RString 
ZipFileImpl::getCanonicalPath()
{
  if (_fsys == Nil)
    return "";
  return _fsys->getRootName() + "@" + _lfh->filename;
}
  
RString 
ZipFileImpl::getAbsolutePath()
{
  return getCanonicalPath();
}

RString 
ZipFileImpl::getName()
{
  RString fname = _lfh->filename;
  int idx = fname->lastIndexOf("/");
  if (idx == -1)
    return fname;
  fname = fname->substr(idx + 1);
  return fname;
}


RString 
ZipFileImpl::getPath()
{
  return _lfh->filename;
}


acdk::io::RFile 
ZipFileImpl::getParentFile()
{
  RString fname = _lfh->filename;
  int idx = fname->lastIndexOf("/");
  if (idx == -1)
    return new acdk::io::File(_fsys->getRootName());
  RString nfilename = fname->substr(0, idx);
  RLocalFileHeader plfh = _fsys->_find(nfilename);
  return new acdk::io::File(new ZipFileImpl(_fsys, plfh));
}


acdk::io::RFile 
ZipFileImpl::makeChild(IN(RString) subfile)
{
  RString p = _lfh->filename;
  if (p->length() > 0)
    p = p + "/" + subfile;
  else
    p = subfile;
  RLocalFileHeader plfh = _fsys->_find(p);
  return new acdk::io::File(new ZipFileImpl(_fsys, plfh));
}

bool  
ZipFileImpl::isAbsolute()
{
  return true;
}

bool 
ZipFileImpl::exists()
{
  return _fsys != Nil && _lfh != Nil;
}

bool 
ZipFileImpl::canRead()
{
  return exists();
}

bool 
ZipFileImpl::canWrite()
{
  return false;
}

bool 
ZipFileImpl::isDirectory()
{
  return _lfh->isDirectory();
}

bool 
ZipFileImpl::isFile()
{
  return _lfh->isFile();
}

bool 
ZipFileImpl::isHidden()
{
  return false; //### to implement
}

jlong 
ZipFileImpl::length()
{
  return _lfh->size;
}

bool 
ZipFileImpl::createNewFile()
{
  return false;
}

bool 
ZipFileImpl::deleteFile()
{
  return false;
}



jlong 
ZipFileImpl::lastModified()
{
  return 0;
}

jlong 
ZipFileImpl::fileCreated()
{
  return 0;
}

bool 
ZipFileImpl::mkdir(int mode/* = 0777*/)
{
  return false;
}

bool 
ZipFileImpl::renameTo(IN(acdk::io::RFile) dest)
{
  return false;
}

bool 
ZipFileImpl::setLastModified(jlong time)
{
  return false;
}


acdk::io::RReader 
ZipFileImpl::getReader()
{
  return new ZipFileReader(this);
}

acdk::io::RWriter 
ZipFileImpl::getWriter()
{
  return Nil;
}

acdk::io::RFileSystem 
ZipFileImpl::getFileSystem()
{
  return &_fsys;
}

} // zip
} // vfile
} // acdk




