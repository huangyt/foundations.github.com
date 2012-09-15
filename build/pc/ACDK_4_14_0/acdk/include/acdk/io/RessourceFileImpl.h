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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/RessourceFileImpl.h,v 1.9 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_RessourceFileImpl_h
#define acdk_io_RessourceFileImpl_h

#include <acdk.h>
#include "FileImpl.h"
#include "RessourceFileSystem.h"
#include "MemReader.h"
#include "Writer.h"

namespace acdk {
namespace io {

ACDK_DECL_CLASS(RessourceFile);
ACDK_DECL_CLASS(RessourceFileImpl);
ACDK_DECL_CLASS(RessourceFileSystem);

/**
  Provides a File implementation for the FileImpl interface
  which can be embedded into C++ source code
*/
class ACDK_CORE_PUBLIC RessourceFileImpl
: extends ::acdk::lang::Object
, implements FileImpl
{
  ACDK_WITH_METAINFO(RessourceFileImpl)
private:
  RRessourceFileSystem _fs;
  RRessourceFile _fh;
public:
  RessourceFileImpl(IN(RRessourceFileSystem) fs, IN(RRessourceFile) fh)
  : Object()
  , _fs(fs)
  , _fh(fh)
  {
  }
  foreign RString getCanonicalPath() { return _fs->getRootName() + _fh->getFqName(); }
  foreign RString getAbsolutePath() { return getCanonicalPath(); }
  foreign RString getName() { return _fh->getName(); }
  foreign RString getPath() { return getCanonicalPath(); }
  foreign RFile getParentFile() 
  { 
    RRessourceDir pd = _fh->getParent();
    if (pd == Nil)
      return Nil;
    return new File(new RessourceFileImpl(_fs, &pd));
  }
  foreign RFile makeChild(IN(RString) subfile)
  {
    if (isDirectory() == false)
      return Nil;
    RRessourceFile fh = RRessourceDir(_fh)->findFile(subfile);
    if (fh == Nil)
      return Nil;
    return new File(new RessourceFileImpl(_fs, fh));
  }
  foreign bool isAbsolute() { return true; }
  foreign bool exists() { return _fh != Nil; }
  foreign bool canRead() { return exists(); }
  foreign bool canWrite() { return false; }
  foreign bool isDirectory() { return exists() && instanceof(_fh, RessourceDir) == true; }
  foreign bool isFile() { return exists() && instanceof(_fh, RessourceDir) == false; }
  foreign bool isHidden() { return false; }
  foreign jlong length() { if (exists() == false) return false; return _fh->length(); }
  /**
    May change later. Now only manipulation over FileSystem is possible
  */
  foreign bool createNewFile() { return false; }
  foreign bool deleteFile() { return false; }
  foreign jlong lastModified() { return 0; }
  foreign jlong fileCreated()  { return 0; }
  foreign bool mkdir(int mode = 0777)  { return false; }
  foreign bool renameTo(IN(RFile) dest) { return false; }
  foreign bool setLastModified(jlong time) { return false; }
  /**
    @todo change this to StringReader
  */
  foreign RReader getReader() { return new MemReader(_fh->content()); }
  /** may change later */
  foreign RWriter getWriter() { return Nil; }
  foreign RFileSystem getFileSystem() { return &_fs; }
};

} // io
} // acdk


#endif //acdk_io_RessourceFileImpl_h

