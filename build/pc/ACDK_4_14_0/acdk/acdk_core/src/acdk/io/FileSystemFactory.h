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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileSystemFactory.h,v 1.11 2005/04/28 14:58:14 kommer Exp $

#ifndef acdk_io_FileSystemFactory_h
#define acdk_io_FileSystemFactory_h

#include <acdk.h>


namespace acdk {
namespace io {

/**
  Interface to a factory for file system by name

  Please refer also to gw_ref[acdk_io_FileSystem].
*/
class ACDK_CORE_PUBLIC FileSystemFactory
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(FileSystemFactory)
public:
  /**
    If the file system doesn't handle this file
    it should return -1
    otherwise the index of the start of the 
    root name.
    Sample:
    ".ressource@tests/acdk/vfile/tar/acdk_dmi-src.tar@/dir/fname.c"
    The RessourceFileSystem returns strlen(.ressource@), 
    The TarFileSytem returns ".ressource@tests/acdk/vfile/tar/acdk_dmi-src.tar@"
  */
  virtual int handleFile(IN(RString) file) = 0;
  /**
    create a new FileImplementation of the 
    given file name
  */
  virtual RFileSystem create(IN(RString) file) = 0;
};

/** little macro to load a file system at load time of shared library */
#define ACDK_REGISTER_FILESYSTEM(cname) \
struct Register##cname { Register##cname() { ::acdk::io::FileSystem::registerFileSystemFactory(new cname()); } }; \
static Register##cname register##cname


ACDK_DECL_CLASS(ConfigFileSystemFactory);

/**
  helper class for file system factories loaded from configuration
*/
final
class ACDK_CORE_PUBLIC ConfigFileSystemFactory
: extends acdk::lang::Object
, implements FileSystemFactory
{
  ACDK_WITH_METAINFO(ConfigFileSystemFactory)
private:
  bool _isUrl;
  RString _detector;
  RString _factoryClassName;
public:
  ConfigFileSystemFactory(bool isUrl, IN(RString) part, IN(RString) fc)
    : _isUrl(isUrl)
    , _detector(part)
    , _factoryClassName(fc)
  {
  }
  virtual int handleFile(IN(RString) file);
  
  virtual RFileSystem create(IN(RString) file);
};


} // io
} // acdk



#endif //acdk_io_FileSystemFactory_h

