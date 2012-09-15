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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileSystem.cpp,v 1.19 2005/04/28 14:58:14 kommer Exp $

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

namespace {

OUT(acdk::util::RHashMap)
getHashMap()
{
  static acdk::util::RHashMap hm;
  if (hm == Nil)
  {
    hm = new acdk::util::HashMap();
    System::registerStaticReference(hm);
  }
  return hm;
}

OUT(RFileSystemFactoryArray)
getFactories()
{
  static RFileSystemFactoryArray fa;
  if (fa == Nil)
  {
    fa = new FileSystemFactoryArray(0);
    System::registerStaticReference(fa);
  }
  return fa;
}
} // anon namespace


FileSystem::FileSystem()
{
  
}



//static 
void 
FileSystem::registerFileSystem(IN(RFileSystem) fs)
{
  getHashMap()->put(&fs->getRootName(), new acdk::lang::ref::WeakReference((RObject)fs));
}

//static
void 
FileSystem::registerFileSystemFactory(IN(RFileSystemFactory) fsf)
{
  getFactories()->append(fsf);
}

//static 
void 
FileSystem::unRegisterFileSystemFactory(IN(RFileSystemFactory) fsf)
{
  RFileSystemFactoryArray& fsfa = getFactories();
  for (int i = 0; i < fsfa->length(); ++i)
  {
    if (fsfa[i] == fsf)
    {
      fsfa->remove(i);
      return;
    }
  }
}


//static 
RFileSystem 
FileSystem::findFileSystem(IN(RString) file)
{
  acdk::util::RHashMap& hm = getHashMap();
  RString fname = FileStandardImpl::fileUrlToFileName(file);
  loadPropertyFileSystemHandler();

  bool triedToLoad = false;
restart:
  acdk::util::RIterator it = hm->iterator();
  while (it->hasNext() == true)
  {
    acdk::util::RMapEntry bk = (acdk::util::RMapEntry)it->next();
    acdk::lang::ref::RReference ref = (acdk::lang::ref::RReference) bk->getValue();
    if (ref->get() == Nil) 
    {
      hm->remove(bk->getKey());
      goto restart;
    }
    RFileSystem fs = (RFileSystem)ref->get();

    if (fs != Nil && fs->ownsFile(fname) == true)
      return fs;
  }
  RFileSystemFactoryArray& fsfa = getFactories();
  RFileSystemFactory fsf = Nil;
  int bestidx = -1;
  for (int i = 0; i < fsfa->length(); ++i)
  {
    int idx = fsfa[i]->handleFile(fname);
    if (idx != -1)
    {
      if (bestidx < idx)
      {
        bestidx = idx;
        fsf = fsfa[i];
      }
    }
  }
  
  if (fsf != Nil)
    return fsf->create(fname);
  
  if (RessourceFileSystem::ressourceFileSystem()->ownsFile(fname) == true)
    return &RessourceFileSystem::ressourceFileSystem();

  return StandardFileSystem::standarFileSystem();
}

int 
ConfigFileSystemFactory::handleFile(IN(RString) file)
{
  if (_isUrl == true)
  {
    if (file->startsWith(_detector) == true)
    {
      return 0;
    }
    return -1;
  }
  else
  {
    return file->indexOf(_detector);
  }
}
  
RFileSystem 
ConfigFileSystemFactory::create(IN(RString) file)
{
  FileSystem::unRegisterFileSystemFactory(this);
  RClass cls = Class::findClass(_factoryClassName);
  if (cls == Nil)
    THROW1(IOException, "cannot load file factor: " + _factoryClassName);
  return FileSystem::findFileSystem(file);
}


//static 
void
FileSystem::loadPropertyFileSystemHandler()
{
  static bool initialized = false;
  if (initialized == true || System::configurationLoaded() == false)
    return;

  initialized = true;
  using namespace acdk::util;
  
  RProperties props = System::getProperties();

  RStringArray m = props->getArrayProperty("acdk.io.filesystem.protocol.container");
  int maxIdx = -1;
  RString containerProtocollClassName = Nil;
  int i;
  for (i = 0; i < m->length(); ++i)
  {
    RString entry = m[i];
    int idx = entry->indexOf('=');
    if (idx == -1)
      continue;
    RString k = entry->substr(0, idx);
    RString v = entry->substr(idx + 1);
    registerFileSystemFactory(new ConfigFileSystemFactory(false, k, v));
  }
  
  m = props->getArrayProperty("acdk.io.filesystem.protocol.url");
  for (i = 0; i < m->length(); ++i)
  {
    RString entry = m[i];
    int idx = entry->indexOf('=');
    if (idx == -1)
      continue;
    RString k = entry->substr(0, idx);
    RString v = entry->substr(idx + 1);
    registerFileSystemFactory(new ConfigFileSystemFactory(true, k, v));
  }
}


} // io
} // acdk




