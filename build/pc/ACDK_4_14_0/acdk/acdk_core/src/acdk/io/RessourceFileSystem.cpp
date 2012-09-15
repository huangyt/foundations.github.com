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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/RessourceFileSystem.cpp,v 1.15 2005/02/05 10:44:54 kommer Exp $


#include <acdk.h>
#include "RessourceFileSystem.h"
#include "RessourceFileImpl.h"
#include "IOException.h"
#include "Writer.h"

namespace acdk {
namespace io {

RessourceFile::RessourceFile(IN(RRessourceDir) pdir, IN(RString) fname, IN(RbyteArray) cont/* = Nil*/)
: _pdir(pdir)
, _fname(fname)
, _cont(cont)
{
}

RessourceFile::RessourceFile(IN(RRessourceDir) pdir, IN(RString) fname, const byte* cont, int contlen)
: _pdir(pdir)
, _fname(fname)
, _cont(Nil)
{
  if (cont != 0)
    _cont = new byteArray(cont, contlen);
}

RString 
RessourceFile::getFqName()
{
  if (_pdir == Nil)
    return _fname;
  StringBuffer sb(_pdir->getFqName());
  if (sb.length() > 0)
    sb.append("/");
  sb.append(_fname);
  return sb.toString();
}


RRessourceDir 
RessourceFile::getParent() 
{ 
  return _pdir; 
}


RRessourceDir 
RessourceDir::findDir(IN(RString) fname)
{
  int idx = fname->indexOf('/');
  if (idx == -1) {
    for (int i = 0; i < _dirs.length(); ++i)
    {
      if (_dirs[i]->_fname->equals(fname) == true)
        return _dirs[i];
    }
    return Nil;
  }
  RString p = fname->substr(0, idx);
  RRessourceDir sdir = findDir(p);
  if (sdir == Nil)
    return Nil;
  return sdir->findDir(fname->substr(idx + 1));
}

RRessourceFile 
RessourceDir::findFile(IN(RString) fname)
{
  int idx = fname->indexOf('/');
  if (idx == -1) {
    int i;
    for (i = 0; i < _dirs.length(); ++i)
    {
      if (_dirs[i]->_fname->equals(fname) == true)
        return &_dirs[i];
    }
    for (i = 0; i < _files.length(); ++i)
    {
      if (_files[i]->_fname->equals(fname) == true)
        return _files[i];
    }
    return Nil;
  }
  RString p = fname->substr(0, idx);
  RRessourceDir sdir = findDir(p);
  if (sdir == Nil)
    return Nil;
  return sdir->findFile(fname->substr(idx + 1));
}

RRessourceDir 
RessourceDir::createDir(IN(RString) fname)
{
  int idx = fname->indexOf('/');
  if (idx == -1) {
    for (int i = 0; i < _dirs.length(); ++i)
    {
      if (_dirs[i]->_fname->equals(fname) == true)
        THROW1(IOException, "Ressource already exists: " + getFqName());
    }
    RRessourceDir rd = new RessourceDir(this, fname);
    _dirs.append(rd);
    return rd;
  }
  RString p = fname->substr(0, idx);

  RRessourceDir sdir = findDir(p);
  if (sdir != Nil) 
    return sdir->createDir(fname->substr(idx + 1));
  sdir = createDir(p);
  if (sdir == Nil)
    THROW1(IOException, "cannot create dir: " + getFqName() + "/" + fname->substr(idx + 1));
  return sdir->createDir(fname->substr(idx + 1));

}
/*
RRessourceFile 
RessourceDir::createFile(IN(RString) fname, IN(RString) content)
{
  int idx = fname->indexOf('/');
  if (idx == -1) {
    int i;
    for (i = 0; i < _dirs.length(); ++i)
    {
      if (_dirs[i]->_fname->equals(fname) == true)
        THROW1(IOException, "Ressource already exists: " + getFqName());
    }
    for (i = 0; i < _files.length(); ++i)
    {
      if (_files[i]->_fname->equals(fname) == true)
        THROW1(IOException, "Ressource already exists: " + getFqName());
    }
    RRessourceFile rd = new RessourceFile(this, fname, content);
    _files.append(rd);
    return rd;
  }
  RString p = fname->substr(0, idx);

  RRessourceDir sdir = findDir(p);
  if (sdir != Nil) 
    return sdir->createFile(fname->substr(idx + 1));
  sdir = createDir(p);
  if (sdir == Nil)
    THROW1(IOException, "cannot create dir: " + getFqName() + "/" + fname->substr(idx + 1));
  return sdir->createFile(fname->substr(idx + 1), content);
}
*/

RRessourceFile 
RessourceDir::createFile(IN(RString) fname, IN(RbyteArray) ba)
{
  int idx = fname->indexOf('/');
  if (idx == -1) {
    int i;
    for (i = 0; i < _dirs.length(); ++i)
    {
      if (_dirs[i]->_fname->equals(fname) == true)
        THROW1(IOException, "Ressource already exists: " + getFqName());
    }
    for (i = 0; i < _files.length(); ++i)
    {
      if (_files[i]->_fname->equals(fname) == true)
        THROW1(IOException, "Ressource already exists: " + getFqName());
    }
    RRessourceFile rd = new RessourceFile(this, fname, ba);
    _files.append(rd);
    return rd;
  }
  RString p = fname->substr(0, idx);

  RRessourceDir sdir = findDir(p);
  if (sdir != Nil) 
    return sdir->createFile(fname->substr(idx + 1));
  sdir = createDir(p);
  if (sdir == Nil)
    THROW1(IOException, "cannot create dir: " + getFqName() + "/" + fname->substr(idx + 1));
  return sdir->createFile(fname->substr(idx + 1), ba);
}

RRessourceFile 
RessourceDir::createFile(IN(RString) fname, const byte* ba, int len)
{
  int idx = fname->indexOf('/');
  if (idx == -1) {
    int i;
    for (i = 0; i < _dirs.length(); ++i)
    {
      if (_dirs[i]->_fname->equals(fname) == true)
        THROW1(IOException, "Ressource already exists: " + getFqName());
    }
    for (i = 0; i < _files.length(); ++i)
    {
      if (_files[i]->_fname->equals(fname) == true)
        THROW1(IOException, "Ressource already exists: " + getFqName());
    }
    RRessourceFile rd = new RessourceFile(this, fname, ba, len);
    _files.append(rd);
    return rd;
  }
  RString p = fname->substr(0, idx);

  RRessourceDir sdir = findDir(p);
  if (sdir != Nil) 
    return sdir->createFile(fname->substr(idx + 1));
  sdir = createDir(p);
  if (sdir == Nil)
    THROW1(IOException, "cannot create dir: " + getFqName() + "/" + fname->substr(idx + 1));
  return sdir->createFile(fname->substr(idx + 1), ba, len);
}

namespace {

RessourceFileSystem&
getRessourceFileSystem()
{
  static RessourceFileSystem _fs;
  return _fs;
}
} // anon namespace

RessourceFileSystem::RessourceFileSystem()
: _root(Nil, "")
{
}

void
RessourceFileSystem::listFiles(IN(RRessourceDir) rd, int listflags, FileArray& fa)
{
  
  int i;
  if (listflags & FileListDirectories)
  {
    for (int i = 0; i < rd->_dirs.length(); ++i)
    {
      fa.append(new File(new RessourceFileImpl(this, &rd->_dirs[i])));
    }
  }
  if (listflags & FileListFiles)
  {
    for (int i = 0; i < rd->_files.length(); ++i)
    {
      fa.append(new File(new RessourceFileImpl(this, rd->_files[i])));
    }
  }
}


RFileArray 
RessourceFileSystem::listFiles(IN(RString) directory, int listflags)
{
  RFileArray fa = new FileArray(0);
  RString dir = directory;
  if (dir->startsWith(".ressource@") == true)
    dir = dir->substr(strlen(".ressource@"));
  RRessourceDir rd = _root.findDir(dir);
  if (rd == Nil)
    return fa;
  listFiles(rd, listflags, *fa);
  return fa;
}

RFile 
RessourceFileSystem::file(IN(RString) path)
{
  RRessourceFile rf = _root.findFile(path);
  if (rf == Nil)
    return Nil;
  return new File(new RessourceFileImpl(this, rf));
}

RFileImpl 
RessourceFileSystem::getFileImpl(IN(RString) fqpath)
{
  if (fqpath->startsWith(".ressource@") == false)
    THROW1(IOException, "File is not part of the ressource file system: " + fqpath);
  
  RRessourceFile rf = _root.findFile(fqpath->substr(strlen(".ressource@")));
  if (rf != Nil)
    return new RessourceFileImpl(this, rf);
  // ### check @ at 
  return Nil;
  
}

RRessourceFileSystem 
RessourceFileSystem::ressourceFileSystem()
{
  return &getRessourceFileSystem();
}



} // io
} // acdk


