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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_ClassLoader_Test.cpp,v 1.12 2005/04/21 08:28:29 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/ClassLoader.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( ClassLoader_Test )
  DECLARE_TEST( findExistant )
  DECLARE_TEST( findToLoad )
  DECLARE_TEST( unicode )
  DECLARE_TEST( aliase )
END_DECLARE_TEST( ClassLoader_Test  )

BEGIN_DEFINE_TEST( ClassLoader_Test )
  ADD_TEST( ClassLoader_Test, findExistant ) 
  ADD_TEST( ClassLoader_Test, findToLoad   ) 
  ADD_TEST( ClassLoader_Test, unicode ) 
  ADD_TEST( ClassLoader_Test, aliase ) 
  
END_DEFINE_TEST( ClassLoader_Test )

using namespace acdk::lang;


void ClassLoader_Test::findExistant()
{
  RClassLoader cl = ClassLoader::getSystemClassLoader();
  RClass cls = cl->findClass("acdk.lang.StringBuffer");
  testAssert(cls != Nil);
  testAssert(cls == StringBuffer::GetClass());
}

void ClassLoader_Test::findToLoad()
{
#if !defined(ACDK_STATIC_LIB)
  RClassLoader cl = ClassLoader::getSystemClassLoader();
  RClass gseclass = cl->findClass("acdk.security.GeneralSecurityException");
  testAssert(gseclass != Nil);
  RObject obj = gseclass->newInstance();
  testAssert(obj != Nil);
#endif
}


void
ClassLoader_Test::unicode()
{
  RString clsname = "acdk/lang/StringBuffer";
  clsname = clsname->convertToNative();
  Class::forName(clsname);
}

void
ClassLoader_Test::aliase()
{
  RString clsname = "org.w3c.dom.Comment";
  RClass cls = Class::forName(clsname);
  bool erg = ClassLoader::getSystemClassLoader()->loadMetaInfoLibrary("org.w3c.dom.Comment");
  testAssert(erg == true);
}


} // namespace lang 
} //namespace acdk 
} //namespace tests




