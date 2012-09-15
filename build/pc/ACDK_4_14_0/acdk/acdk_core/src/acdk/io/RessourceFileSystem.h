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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/RessourceFileSystem.h,v 1.13 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_RessourceFileSystem_h
#define acdk_io_RessourceFileSystem_h

#include <acdk.h>
#include "File.h"
#include "FileImpl.h"
#include "FileSystem.h"


namespace acdk {
namespace io {

ACDK_DECL_CLASS(RessourceDir);
ACDK_DECL_CLASS(RessourceFile);
/**
  Internal class implementing in C++ source embedded file
  @see RessourceFileSystem
*/
class ACDK_CORE_PUBLIC RessourceFile
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(RessourceFile)
private:
  RRessourceDir _pdir;
  RString _fname;
  //RString _cont;
  RbyteArray _cont;
  friend class RessourceDir;
  friend class RessourceFileSystem;
  
public:
  RessourceFile(IN(RRessourceDir) pdir, IN(RString) fname, IN(RbyteArray) cont = Nil);
  RessourceFile(IN(RRessourceDir) pdir, IN(RString) fname, const byte* cont, int contlen);
  RString getFqName();
  RString getName() { return _fname; }
  RRessourceDir getParent();
  RbyteArray content() { return _cont; }
  int length() { if (_cont == Nil) return 0; return _cont->length(); }
};



/**
  Internal class implementing in C++ source embedded directory
  @see RessourceFileSystem
*/
class ACDK_CORE_PUBLIC RessourceDir
: extends RessourceFile
{
  ACDK_WITH_METAINFO(RessourceDir)
private:
  friend class RessourceFileSystem;
  RessourceDirArray _dirs;
  RessourceFileArray _files;
public:
  RessourceDir(IN(RRessourceDir) pdir, IN(RString) fname)
  : RessourceFile(pdir, fname)
  , _dirs(0)
  , _files(0)
  {
  }
  RRessourceDir findDir(IN(RString) fname);
  RRessourceFile findFile(IN(RString) fname);
  RRessourceDir createDir(IN(RString) fname);
  //RRessourceFile createFile(IN(RString) fname, IN(RString) content = Nil);
  RRessourceFile createFile(IN(RString) fname, IN(RbyteArray) ba = Nil);
  foreign RRessourceFile createFile(IN(RString) fname, const byte* ba, int len);
  
};


ACDK_DECL_CLASS(RessourceFileSystem);


/**
  Implements internal ressource storage embedded into C++ source code.
  @author Roger Rene Kommer
  @seealso FileImpl
*/
class ACDK_CORE_PUBLIC RessourceFileSystem
: extends ::acdk::lang::Object
, implements FileSystem
{
  ACDK_WITH_METAINFO(RessourceFileSystem)
private:
  RessourceDir _root; 
public:
  RessourceFileSystem();
  bool ownsFile(IN(RString) fname)
  {
    return fname->startsWith(".ressource@") == true; 
  }

  foreign RString getRootName() { return ".ressource@"; }
  foreign RFileArray listFiles(IN(RString) directory, int listflags);
  foreign RFile file(IN(RString) path);
  
  RFileImpl getFileImpl(IN(RString) fqpath);
  static RRessourceFileSystem ressourceFileSystem();
  RRessourceDir root() { return &_root; }
private:
  foreign void listFiles(IN(RRessourceDir) rd, int listflags, FileArray& fa);
  
};

ACDK_DECL_CLASS(RessourceFileSystemFactory);
/**
  Factory for the acdk::io::RessourceFileSystem
*/
class ACDK_CORE_PUBLIC RessourceFileSystemFactory
: extends ::acdk::lang::Object
, implements FileSystemFactory
{
  ACDK_WITH_METAINFO(RessourceFileSystemFactory)

public:
  virtual bool isRootFileSystem() { return true; }
  virtual int handleFile(IN(RString) file)
  {
    if (file->startsWith(".ressource@") == true)
      return strlen(".ressource@");
    return -1;
  }
  virtual RFileSystem create(IN(RString) file) { return &RessourceFileSystem::ressourceFileSystem(); }
};


} // io
} // acdk



#endif //acdk_io_RessourceFileSystem_h

