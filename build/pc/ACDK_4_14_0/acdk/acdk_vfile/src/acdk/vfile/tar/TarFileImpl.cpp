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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/tar/TarFileImpl.cpp,v 1.14 2005/03/17 12:28:04 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/SubReader.h>

#include <acdk/util/ArrayList.h>
#include "TarFileImpl.h"
#include "TarFileSystem.h"

namespace acdk {
namespace vfile {
namespace tar {



RString 
TarFileImpl::getCanonicalPath()
{
  return _fs->getRootName() + _fh->filename;
}


RString 
TarFileImpl::getAbsolutePath()
{
  return getCanonicalPath();
}

RString 
TarFileImpl::getName()
{
  RString name = _fh->filename;
  int idx = name->lastIndexOf("/");
  if (idx != -1)
    name = name->substr(idx + 1);
  return name;
}

RString 
TarFileImpl::getPath()
{
  return _fh->filename;
}

RFile 
TarFileImpl::getParentFile()
{
  return new acdk::io::File(getParent());
}
  

//virtual 
RString 
TarFileImpl::getParent()
{
  RString name = _fh->filename;
  if (name->length() == 0) 
  {
     RString rn = _fs->getRootName();
     return rn->substr(0, rn->length() - 1);
  }
     

  int idx = name->lastIndexOf("/");
  if (idx != -1)
      return _fs->getRootName();
  return _fs->getRootName() + name->substr(0, idx - 1);
}

bool 
TarFileImpl::isAbsolute()
{
  return true;
}
  

//virtual 
RFile 
TarFileImpl::makeChild(IN(RString) sf)
{
  RString path = _fh->filename;
  RString subfile = sf;
  if (path->length() > 0) 
    subfile = path + "/" + subfile;
  RTarFileHeader fh = _fs->_find(subfile);
  if (fh == Nil)
    return new acdk::io::File(new acdk::io::AbstractFileImpl(&_fs, subfile));
  return new acdk::io::File(new TarFileImpl(_fs, fh));
}

//virtual 
bool 
TarFileImpl::exists()
{
  return _fs != Nil && _fh != Nil;
}

//virtual 
bool 
TarFileImpl::canRead()
{
  return exists() && _fh->canRead();
}

//virtual 
bool 
TarFileImpl::isDirectory()
{
  return exists() && _fh->isDirectory();
}


//virtual 
bool 
TarFileImpl::isFile()
{
  return exists() && _fh->isDirectory() == false;
}

//virtual 
bool 
TarFileImpl::isHidden()
{
  return false;
}

//virtual 
jlong 
TarFileImpl::length()
{
  if (exists() == false || _fh->isDirectory() == true)
    return 0;
  return _fh->length();
}
  
//virtual 
bool 
TarFileImpl::createNewFile()
{
  return false;
}

//virtual 
bool 
TarFileImpl::deleteFile()
{
  return false;
}



//virtual 
jlong 
TarFileImpl::lastModified()
{
  return 0;
}

//virtual 
jlong 
TarFileImpl::fileCreated()
{
  return 0;
}

//virtual 
bool 
TarFileImpl::mkdir(int mode/* = 0777*/)
{
  return false;
}

//virtual 
bool 
TarFileImpl::renameTo(IN(RFile) dest)
{
  return false;
}

//virtual 
bool 
TarFileImpl::setLastModified(jlong time)
{
  return false;
}


//virtual 
RReader 
TarFileImpl::getReader()
{
  RReader r = File(_fs->_fileName).getReader();
  return new acdk::io::SubReader(r, _fh->offset, _fh->size);
}

//virtual 
RWriter 
TarFileImpl::getWriter()
{
  return Nil;
}

RFileSystem 
TarFileImpl::getFileSystem()
{
  return &_fs;
}

} // tar
} // vfile
} // acdk




