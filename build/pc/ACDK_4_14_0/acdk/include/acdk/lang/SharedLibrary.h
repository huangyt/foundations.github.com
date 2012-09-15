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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/SharedLibrary.h,v 1.11 2005/04/09 19:26:50 kommer Exp $

#ifndef acdk_lang_SharedLibrary_h
#define acdk_lang_SharedLibrary_h

#include <acdk.h>

#include <acdk/io/File.h>

namespace acdk {
namespace lang {


ACDK_DECL_CLASS(SharedLibrary);

/**
  Loads shared libraries/DLLs at runtime.

  API: Java<br>
  
  In the shared library:
  @code
  extern "C" Object* getComponentFactory()
  {
    return new MyLibComponentFactory();
  }
  typedef Object* (*GetComponentFactoryCall)();
#ifdef ACDK_OS_WIN32
  SharedLibrary slib("libmylib.so"); 
#else
  SharedLibrary slib("mylib.dll"); 
#endif
  slib.loadLibrary();
  GetComponentFactoryCall fptr = (GetComponentFactoryCall)slib.locateFunction("getComponentFactory"); 
  if (fptr == 0)
    return;
  RObject obj = (*fptr)();
  @endcode
  @author Roger Rene Kommer
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:50 $

*/
class ACDK_CORE_PUBLIC SharedLibrary
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(SharedLibrary)
private:
  acdk::lang::RString _library;
  foreign void* _libReference;
  
public:
  SharedLibrary(IN(RString) libName)
  : _library(libName),
    _libReference(0)
  {
  }
  SharedLibrary(IN(acdk::io::RFile) library)
  : _library(library),
    _libReference(0)
  {
  }
  void loadLibary();
  void unloadLibrary();
  foreign void* locateFunction(const String& name);
  foreign void* locateFunction(IN(RString) name) { return locateFunction(*name); }
  bool loaded() { return _libReference != 0; }
};

} // lang
} // acdk

#endif //acdk_lang_SharedLibrary_h

