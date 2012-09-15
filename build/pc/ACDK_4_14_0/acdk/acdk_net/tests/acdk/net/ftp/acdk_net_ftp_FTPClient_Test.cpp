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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/ftp/acdk_net_ftp_FTPClient_Test.cpp,v 1.5 2005/02/05 10:45:30 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/util/Arrays.h>
#include <acdk/io/RessourceFileSystem.h>

#include <acdk/net/ftp/FTPClient.h>

namespace tests {
namespace acdk {
namespace net {
namespace ftp {
  
BEGIN_DECLARE_TEST( FTPClient_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( FTPClient_Test  )

BEGIN_DEFINE_TEST( FTPClient_Test )
  ADD_TEST( FTPClient_Test, standard ) 
END_DEFINE_TEST( FTPClient_Test )

using namespace ::acdk::lang;
using namespace ::acdk::net::ftp;


RString join(IN(RObjectArray) arr, IN(RString) joiner)
{
  StringBuffer sb;
  for (int i = 0; i < arr->length(); ++i)
  {
    if (i > 0)
      sb.append(joiner);
    sb.append(arr[i]->toString());
  }
  return sb.toString();
}

void
FTPClient_Test::standard()
{
  try {

  FTPClient client;
  client.connect("localhost", 21);
  client.login("acdktest", "testacdk");
  RString cwd = client.getCwd();
  /*
  RStringArray erg = client.listFiles();
  
  System::out->println(join(erg, "\n"));
  erg = client.listFiles();
  System::out->println(join(erg, "\n"));
  */
  ::acdk::io::RFileInfoArray fia = client.listFileInfos();
  System::out->println(join((RObjectArray)fia, "\n"));
  client.mkdir("acdk_net_ftp_test");
  client.setCwd("acdk_net_ftp_test");
  ::acdk::io::RRessourceFileSystem rfs = ::acdk::io::RessourceFileSystem::ressourceFileSystem();
  RString filecontent = "This is the content of the file";
  rfs->root()->createFile("tests/acdk/net/ftp/test1.file", (const byte*)filecontent->c_str(), filecontent->length());
  
  ::acdk::io::File f(".ressource@tests/acdk/net/ftp/test1.file");
  client.sendFile(&f, "test1.file");
  client.rename("test1.file", "test1a.file");
  client.deleteFile("test1a.file");
  client.setCwdUp();
  client.deleteDirectory("acdk_net_ftp_test");
  } catch (::acdk::io::RIOException ex) {
    System::out->println("FTPClient test failed: you need a local ftp server and a user acdktest pass testacdk "
                         "with writing access on the default ftp login directory for this test");

  }
}

} // namespace ftp 
} // namespace net
} // namespace acdk
} // namespace tests



