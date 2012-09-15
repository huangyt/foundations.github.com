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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileAbstractImpl.h,v 1.9 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_FileAbstractImpl_h
#define acdk_io_FileAbstractImpl_h

#include <acdk.h>
#include <acdk/io/File.h>

#include "FileImpl.h"

namespace acdk {
namespace io {

ACDK_DECL_CLASS(FileAbstractImpl);

/**
  Abstract/incomplete implementation 
  of the FileImpl interface
*/
class ACDK_CORE_PUBLIC FileAbstractImpl
: extends ::acdk::lang::Object
, implements ::acdk::io::FileImpl
{
public:
  FileAbstractImpl(IN(RString) path)
  : ::acdk::lang::Object()
  , _path(path)
  {
  }
  virtual RString getCanonicalPath();
  virtual RString getAbsolutePath();
  virtual RString getName();
  virtual RString getPath() { return _path; }
  virtual bool isAbsolute();
  virtual RString getParent();
  virtual RFile getParentFile();
  virtual RFile makeChild(IN(RString) subfile); 
  static RString concatNames(IN(RString) parent, IN(RString) child);
protected:
  RString _path;
};

} // io
} // acdk


#endif //acdk_io_FileAbstractImpl


