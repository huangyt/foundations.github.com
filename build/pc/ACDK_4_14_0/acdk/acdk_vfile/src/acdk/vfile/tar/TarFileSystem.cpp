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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/tar/TarFileSystem.cpp,v 1.17 2005/03/17 12:28:04 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileStandardImpl.h>
#include <acdk/util/ArrayList.h>
#include "TarFileImpl.h"
#include "TarFileSystem.h"

namespace acdk {
namespace vfile {
namespace tar {

ACDK_REGISTER_FILESYSTEM(TarFileSystemFactory);


TarFileSystem::TarFileSystem(IN(RString) tf)
: Object()
, _fileHeaders(0)
{
  RString tarfile = tf;
  int idx = tarfile->toLowerCase()->lastIndexOf(".tar@");
  if (idx != -1)
    tarfile = tarfile->substr(0, idx + 4);
 
  _fileName = tarfile;
  ACDK_SAFE_CONSTRUCTOR();
  _openTarFile();
  FileSystem::registerFileSystem(this);
}

bool 
TarFileSystem::ownsFile(IN(RString) fname)
{
  return fname->startsWith(_fileName + "@");
}

RString 
TarFileSystem::getRootName()
{
  return _fileName + "@";
}

acdk::io::RFileArray 
TarFileSystem::listFiles(IN(RString) dir, int listflags)
{
  RString directory = dir;
  if (directory == Nil)
    directory = "";
  RFileArray fa = new FileArray(fileCount());
  fa->resize(0);
  RString tfn;
  for (int i = 0; i < fileCount(); ++i) 
  {
    if (directory->length() > 0) {
      if (_fileHeaders[i]->filename->startsWith(directory + "/") == false) 
        continue;
    }
    if (directory->equals(_fileHeaders[i]->filename) == true)
      continue;
    bool doAppend = false;
    bool isdir = _fileHeaders[i]->isDirectory();
    if (isdir == true)
    {
      if ((listflags & acdk::io::FileListDirectories) == acdk::io::FileListDirectories)
       doAppend = true;
    }
    else
    {
      if ((listflags & acdk::io::FileListFiles) == acdk::io::FileListFiles)
        doAppend = true;
    }

    if ((listflags & acdk::io::FileListRecursive) == false) 
    {
      int len = directory->length();
      if (len > 0) ++len;
      tfn = _fileHeaders[i]->filename->substr(len);
      if (tfn->indexOf('/') != -1)
        continue;
    }
    if (doAppend == true)
      fa->append(new File(new TarFileImpl(this, _fileHeaders[i])));
  }
  return fa;
  /*
  
  if (directory == Nil)
    directory = "";
  RFileArray fa = new FileArray(0);
  for (int i = 0; i < fileCount(); ++i) 
  {
    if (_fileHeaders[i]->filename->startsWith(directory) == false) 
      continue;
    bool isdir = _fileHeaders[i]->isDirectory();
    if (!(listflags & ListDirectories) && isdir == false)
      continue;
    if (!(listflags & ListFiles) && isdir == true)
      continue;
    if ((listflags & ListRecursive) == false) {
      RString tfn = _fileHeaders[i]->filename->substr(directory->length());
      if (tfn->indexOf('/') != -1)
        continue;
    }
    fa->append(new File(new TarFileImpl(this, _fileHeaders[i])));
  }
  return fa;
  */
}

acdk::io::RFile 
TarFileSystem::file(IN(RString) path)
{
  RTarFileHeader tfh = _find(path);
  if (tfh == Nil)
    return new acdk::io::File(new acdk::io::AbstractFileImpl(this, path));

  return new acdk::io::File(new TarFileImpl(this, tfh));
}

acdk::io::RFileImpl 
TarFileSystem::getFileImpl(IN(RString) fqpath)
{
  if (ownsFile(fqpath) == false) 
    THROW1(IOException, "File " + fqpath + " is not part of the FileSystem: " + _fileName);
  
  RString sf = fqpath->substr(_fileName->length() + 1);
  RTarFileHeader tfh = _find(sf);
  if (tfh == Nil)
    return new acdk::io::AbstractFileImpl(this, fqpath);
  return new TarFileImpl(this, tfh);
}


RTarFileHeader 
TarFileSystem::_find(IN(RString) path)
{
  for (int i = 0; i < _fileHeaders.length(); ++i)
  {
    RTarFileHeader tfh = _fileHeaders[i];
    if (tfh->filename->equals(path) == true)
      return tfh;
  }
  return Nil;
}

void 
TarFileSystem::_openTarFile()
{
  _readEntries();
  _addDerived();

}

void 
TarFileSystem::_addDirLfh(IN(RString) dirname)
{
  RTarFileHeader lfh = new TarFileHeader();
  lfh->derived = true;
  lfh->filename = dirname;
  lfh->linkflag = Directory;
  _fileHeaders.append(lfh);
}


void 
TarFileSystem::_addDerived()
{
   int count = _fileHeaders.length();
  
  for (int i = 0; i < count; ++i)
  {
    if (_fileHeaders[i]->derived == true)
      continue;
    RString fname = _fileHeaders[i]->filename;
    int idx = -1;
    do {
      idx = fname->indexOf('/', idx + 1);
      if (idx == -1) 
        break;

      RString path = fname->substr(0, idx);
      if (_find(path) == Nil) 
        _addDirLfh(path);
    } while (idx != -1);
  }
  if (_find("") == Nil)
    _addDirLfh("");
}

void 
TarFileSystem::_readEntries()
{
  RReader reader = File(_fileName).getReader();
  Reader& fin = *reader;
  jlong offset = 0;
  int lvail = 0;
  try {
    while ((lvail = fin.available()) > 0) {
      RTarFileHeader th = new TarFileHeader();
      if (th->read(offset, fin) == false)
        break;
      if (th->filename->length() > 0) 
      {
        _fileHeaders.append(th);
        offset = th->getEndOffset();
      } else {
        offset += 512;
      }
    }
  } catch (RThrowable ex) {

  }
}

//static 
void 
TarFileSystem::loadFileSystem()
{
  // does nothing
}


} // tar
} // vfile
} // acdk




