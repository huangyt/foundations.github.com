
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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_RessourceFileImpl_Test.cpp,v 1.8 2005/02/05 10:45:08 kommer Exp $

#include <acdk.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/System.h>
#include <acdk/io/RessourceFileImpl.h>
#include <acdk/io/MemWriter.h>
#include <acdk/locale/Encoding.h>

namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( RessourceFileImpl_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( listFiles )
END_DECLARE_TEST( RessourceFileImpl_Test )

BEGIN_DEFINE_TEST( RessourceFileImpl_Test )
  ADD_TEST( RessourceFileImpl_Test, standard ) 
  ADD_TEST( RessourceFileImpl_Test, listFiles );
END_DEFINE_TEST( RessourceFileImpl_Test )

using namespace ::acdk::io;

void
RessourceFileImpl_Test::standard()
{
  RRessourceFileSystem rfs = RessourceFileSystem::ressourceFileSystem();
  RString filecontent = "This is the content of the file";
  rfs->root()->createFile("tests/acdk/io/RessourceFileSystem/FirstTry", (const byte*)filecontent->c_str(), filecontent->length());
  
  File f(".ressource@tests/acdk/io/RessourceFileSystem/FirstTry");
  testAssert(f.exists() == true);
  RReader in = f.getReader();
  MemWriter out;
  in->trans(&out);
  RbyteArray ch = out.getBuffer();
  RString t = new String(ch, ::acdk::locale::Encoding::getAsciiEncoding()->getDecoder());
  testAssert(t->equals(filecontent) == true);
}

void
RessourceFileImpl_Test::listFiles()
{
  File f(".ressource@tests/acdk/io/RessourceFileSystem");
  RFileArray fa = f.listFiles();
  testAssert(fa->length() > 0);
}


} // namespace io
} // namespace acdk
} // namespace tests
