
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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_File_Test.cpp,v 1.11 2005/02/05 10:45:08 kommer Exp $

#include <acdk.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/Writer.h>

namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( File_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( listFiles )
  DECLARE_TEST( tempFiles )
END_DECLARE_TEST( File_Test )

BEGIN_DEFINE_TEST( File_Test )
  ADD_TEST( File_Test, standard ) 
  ADD_TEST( File_Test, listFiles );
ADD_TEST( File_Test, tempFiles );

END_DEFINE_TEST( File_Test )


void
File_Test::standard()
{
  testAssert(true); 
}

void
File_Test::listFiles()
{
  RString dirname = System::getProperties()->getProperty("ACDKHOME", ".");
  ::acdk::io::File f(dirname);
  RStringArray sa = f.list();
  int i;
  for (i = 0; i < sa->length(); ++i)
  {
    RString ts = sa[i];
    System::out->println(ts);
  }
  ::acdk::io::RFileArray fa = f.listFiles();
  for (i = 0; i < fa->length(); ++i)
  {
    RString ts = fa[i]->getName();
    System::out->println(ts);
  }
  testAssert(sa->length() == fa->length());
}

void
File_Test::tempFiles()
{
  try {
    ::acdk::io::RFileArray fa = new ::acdk::io::FileArray(5);
    int i;
    for (i = 0; i < fa->length(); ++i)
    {
      fa[i] = ::acdk::io::File::createTempFile("acdk_test", Nil);
      fa[i]->getWriter()->write((const byte*)"just a test", 0, strlen("just a test"));
    }
    for (i = 0; i < fa->length(); ++i)
    {
      fa[i]->deleteFile();
    }
  } catch (::acdk::io::RIOException ex) {
    System::out->println("Exception in File_Test::tempFiles: " + ex->getMessage());
  }
}


} // namespace io
} // namespace acdk
} // namespace tests
