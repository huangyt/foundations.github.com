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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/StandardFileSystem.h,v 1.7 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_StandardFileSystem_h
#define acdk_io_StandardFileSystem_h

#include <acdk.h>
#include "FileSystem.h"

namespace acdk {
namespace io {

/**
  The StandardFileSystem implements the FileSystem for 
  standard OS files.
  Please refer also to gw_ref[acdk_io_FileSystem].
*/
class ACDK_CORE_PUBLIC StandardFileSystem
: extends acdk::lang::Object
, implements FileSystem
{
public:
  foreign bool ownsFile(IN(RString) fname);
  foreign RString getRootName();
  foreign RFileArray listFiles(IN(RString) directory, int listflags);
  foreign RFile file(IN(RString) path);
  foreign RFileImpl getFileImpl(IN(RString) fqpath);
  static RFileSystem standarFileSystem();
};

} // io
} // acdk



#endif //acdk_io_StandardFileSystem_h

