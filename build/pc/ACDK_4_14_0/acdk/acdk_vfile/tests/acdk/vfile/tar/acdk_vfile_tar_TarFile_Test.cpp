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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/tests/acdk/vfile/tar/acdk_vfile_tar_TarFile_Test.cpp,v 1.14 2005/03/17 12:23:09 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/io/RessourceFileSystem.h>

#include <acdk/vfile/tar/TarFileSystem.h>
#include <acdk/io/NullWriter.h>

namespace tests {
namespace acdk {
namespace vfile {
namespace tar {
  
BEGIN_DECLARE_TEST( TarFile_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( externFile )
END_DECLARE_TEST( TarFile_Test  )

BEGIN_DEFINE_TEST( TarFile_Test )
  ADD_TEST( TarFile_Test, standard ) 
  ADD_TEST( TarFile_Test, externFile ) 
  
END_DEFINE_TEST( TarFile_Test )





using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::vfile::tar;

#include "acdk_dmi-src.tar.c"
void setup_rc()
{
  static bool inited = false;
  if (inited  == true)
    return;

  RessourceFileSystem::ressourceFileSystem()->root()
    ->createFile("tests/acdk/vfile/tar/acdk_dmi-src.tar", 
      ressource_array, ressource_size);
  
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
    }
  }
}

void 
TarFile_Test::standard()
{
  setup_rc();   
  acdk::vfile::tar::TarFileSystem::loadFileSystem();
  RString tfname = ".ressource@tests/acdk/vfile/tar/acdk_dmi-src.tar@";
  //tfname = "D:\\archive\\incoming\\zipios++-0.1.5.tar@";
  //TarFileSystem tfs(tfname);
  ::acdk::io::RFile tarfile = new ::acdk::io::File(tfname);
  listRecursive(tarfile);
  
}

void
TarFile_Test::externFile()
{
  RString baseFileName = System::getAcdkHome() + "/cfg/tests/acdk/vfile/TestContainer.tar";
  RString subfile = "@tsub/tsub2/TarredFile.txt";
  RString fn = baseFileName + subfile;
  File f(fn);
  testAssert(f.exists() == true);
  
}


} // namespace tar
} // namespace vfile
} //namespace acdk 
} //namespace tests 



