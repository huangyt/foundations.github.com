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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/zip/ZipFileSystem.cpp,v 1.20 2005/03/17 12:32:44 kommer Exp $

#include <acdk.h>
#include "ZipFileSystem.h"
#include "ZipFileImpl.h"


#include <acdk/io/BinaryDataReader.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/lang/System.h>

namespace acdk {
namespace vfile {
namespace zip {

USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::io::, FileSystem);
USING_CLASS(::acdk::io::, FilterReader);
USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Storage);


ACDK_REGISTER_FILESYSTEM(ZipFileSystemFactory);

ZipFileSystem::ZipFileSystem(IN(RString) zipfile)
: _fileName(zipfile)
, _loaded(false)
{
  int idx = zipfile->toLowerCase()->lastIndexOf(".zip@");
  if (idx != -1)
    _fileName = zipfile->substr(0, idx + 4);
  _load();
  ACDK_SAFE_CONSTRUCTOR();
  FileSystem::registerFileSystem(this);
}

ZipFileSystem::~ZipFileSystem()
{
}

void 
ZipFileSystem::_load()
{
  RReader fin = File(_fileName).getReader();
  _lfheaders = new LocalFileHeaderArray(0);
  _scanLocalHeader(*fin);
  fin->close();
  _addDerived();
  _loaded =  true;
}


void 
ZipFileSystem::_scanLocalHeader(acdk::io::Reader& in)
{
  acdk::io::BinaryDataReader bin(&in, ::acdk::lang::LittleEndian);
  while (true) 
  {
    RLocalFileHeader lfh = new LocalFileHeader();
    lfh->read(bin);
    if (lfh->valid == true) {
      _lfheaders->append(lfh);
      bin.seek(acdk::io::SeekCur, lfh->cmprsize);
    } else {
      break;
    }
      
  }
}

void 
ZipFileSystem::_readCentralDirectory(acdk::io::Reader& in)
{
  _findCentralDirectory(in);
  CentralDirectory cdir;
  acdk::io::BinaryDataReader bin(&in, ::acdk::lang::LittleEndian);
  cdir.read(bin);
}

void 
ZipFileSystem::_findCentralDirectory(acdk::io::Reader& in)
{
  const int BufSize = 256;
  byte buffer[BufSize];
  jlong pos = in.seek(acdk::io::SeekSet, 0);
  bool found = false;
  while (found == false) 
  {
    int readed = in.read(buffer, 0, BufSize);
    for (int i = readed; i >= 0; --i)
    {
      if (*((int*)&buffer[i])  == CentralDirectory::CDStart) { // EO CD
        in.seek(acdk::io::SeekCur, -(BufSize - i));
        return;
      }
    }
    if (readed <= 0)
      break;
  }
  if (found == false) 
    THROW1_FQ(acdk::io::, IOException, "Central directory not found in ZIP: " + _fileName);
  
}

//virtual 
bool 
ZipFileSystem::ownsFile(IN(RString) fname)
{
  if (fname->startsWith(_fileName + "@") == true)
    return true;
  return false;
}


RString 
ZipFileSystem::getRootName()
{
  return _fileName;
}


RFileArray 
ZipFileSystem::listFiles(IN(RString) dir, int listflags)
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
      if (_lfheaders[i]->filename->startsWith(directory + "/") == false) 
        continue;
    }
    if (directory->equals(_lfheaders[i]->filename) == true)
      continue;
 
    bool isdir = _lfheaders[i]->isDirectory();
    bool doAppend = false;
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
      tfn = _lfheaders[i]->filename->substr(len);
      if (tfn->indexOf('/') != -1)
        doAppend = false;
    }
    if (doAppend == true)
      fa->append(new acdk::io::File(new ZipFileImpl(this, _lfheaders[i])));
  }
  return fa;
}

RFile 
ZipFileSystem::file(IN(RString) path)
{
  RLocalFileHeader lfh = _find(path);
  if (lfh == Nil)
    return new acdk::io::File(new acdk::io::AbstractFileImpl(this, path));
  return new acdk::io::File(new ZipFileImpl(this, lfh));
}

acdk::io::RFileImpl 
ZipFileSystem::getFileImpl(IN(RString) fqpath)
{
  RString fn;
  if (fqpath->startsWith(_fileName) == true) {
    fn = fqpath->substr(_fileName->length());
    if (fn->startsWith("@") == true)
      fn = fn->substr(1); 
    if (fn->startsWith("/") == true)
      fn = fn->substr(1);
  } else {
    THROW1_FQ(acdk::io::, IOException, "File " + fqpath + " is not part of the FileSystem: " + _fileName);
  }
  RLocalFileHeader lfh = _find(fn);
  if (lfh == Nil)
    return new acdk::io::AbstractFileImpl(this, fqpath);
  
  return new ZipFileImpl(this, lfh);
}

void 
ZipFileSystem::dump(IN(acdk::io::RPrintWriter) out)
{
  int count = _lfheaders->length();
  out->println(RCS("Entries: ") + count);
  for (int i = 0; i < count; ++i)
  {
    out->println(_lfheaders[i]->filename);
  }
}


void 
ZipFileSystem::_addDirLfh(IN(RString) dirname)
{
  RLocalFileHeader lfh = new LocalFileHeader();
  lfh->signature = LocalFileHeader::DerivedDir;
  lfh->fnameLength = dirname->length();
  lfh->filename = dirname;
  _lfheaders->append(lfh);
}

void 
ZipFileSystem::_addDerived()
{
  int count = _lfheaders->length();
  
  for (int i = 0; i < count; ++i)
  {
    RLocalFileHeader lfh = _lfheaders[i];
    if (lfh->isDerivedDir() == true)
      continue;
    RString fname = lfh->filename;
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

RLocalFileHeader 
ZipFileSystem::_find(IN(RString) fname)
{
  int count = _lfheaders->length();
  for (int i = 0; i < count; ++i)
  {
    RLocalFileHeader lfh = _lfheaders[i];
    if (fname->equals(lfh->filename) == true)
      return lfh;
  }
  return Nil;
}

//static 
void 
ZipFileSystem::loadFileSystem()
{
  // does nothing, but ensure this so/dll will be loaded
}

} // zip
} // vfile
} // acdk



