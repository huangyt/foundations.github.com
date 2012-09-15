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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/tests/acdk/vfile/zip/acdk_vfile_zip_ZipFile_Test.cpp,v 1.12 2005/03/17 12:23:10 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/File.h>
#include <acdk/io/BinaryDataReader.h>
#include <acdk/io/NullWriter.h>
#include <acdk/io/RessourceFileSystem.h>

#include <acdk/vfile/zip/LocalFileHeader.h>
#include <acdk/vfile/zip/ZipFileSystem.h>

namespace tests {
namespace acdk {
namespace vfile {
namespace zip {
  
BEGIN_DECLARE_TEST( ZipFile_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( externFile )
END_DECLARE_TEST( ZipFile_Test  )

BEGIN_DEFINE_TEST( ZipFile_Test )
  ADD_TEST( ZipFile_Test, standard ) 
  ADD_TEST( ZipFile_Test, externFile ) 
  
END_DEFINE_TEST( ZipFile_Test )

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::vfile::zip;

#include "acdk_dmi.rc"
void setup_rc()
{
  static bool inited = false;
  if (inited  == true)
    return;
  RessourceFileSystem::ressourceFileSystem()->root()
    ->createFile("tests/acdk/vfile/zip/acdk_sol.zip", 
      (const byte*)ressource_array, ressource_size);
  
  inited = true;
}

void listRecursive(RFile f, RString ident = "")
{

  RStringArray sa = f->list();
  for (int i = 0; i < sa->length(); ++i)
  {
    RFile sf = new File(f, sa[i]); 
    if (sf->isDirectory() == true) {
      System::out->println(ident + "d " + sa[i]);
      listRecursive(sf, ident + " ");
    } else {
      System::out->println(ident + "f " + sa[i]);
      ::acdk::io::NullWriter nw;
      RReader r = sf->getReader();
      r->trans(&nw);
      //r->trans(&System::out);
    }
  }
}



void 
ZipFile_Test::standard()
{
  setup_rc();
  ZipFileSystem::loadFileSystem();
  File f(".ressource@tests/acdk/vfile/zip/acdk_sol.zip@");
  listRecursive(&f);
  File f2(".ressource@tests/acdk/vfile/zip/acdk_sol.zip@acdk-1.01/acdk_dmi/Makefile");
  testAssert(f2.exists() == true);

}

void
ZipFile_Test::externFile()
{
  RString baseFileName = System::getAcdkHome() + "/cfg/tests/acdk/vfile/TestContainer.zip";
  RString fn = baseFileName + "@dir1/subdir1/TextFile.txt";
  
  {
    File f(fn);
    testAssert(f.isFile() == true);
    testAssert(f.exists() == true);
  }
  
  {
    fn = baseFileName + "@/dir1/subdir1/TextFile.txt";
    File f(fn);
    testAssert(f.isFile() == true);
    testAssert(f.exists() == true);
  }
  
  {
    fn = baseFileName + "@/dir1/subdir23/ThisFileDoesnotExists.txt";
    File f(fn);
    testAssert(f.isFile() == false);
    testAssert(f.exists() == false);
  }
  {
    fn = baseFileName + "@nested_container/TestContainer.tar@tsub/tsub2/TarredFile.txt";
    File f(fn);
    testAssert(f.isFile() == true);
    testAssert(f.exists() == true);
  }
}


} // namespace zip
} // namespace vfile
} //namespace acdk 
} //namespace tests 



