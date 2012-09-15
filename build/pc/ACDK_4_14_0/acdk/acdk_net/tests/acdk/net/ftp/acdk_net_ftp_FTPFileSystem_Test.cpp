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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/ftp/acdk_net_ftp_FTPFileSystem_Test.cpp,v 1.9 2005/04/21 14:39:00 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/net/ftp/FTPFileImpl.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>

namespace tests {
namespace acdk {
namespace net {
namespace ftp {
  
BEGIN_DECLARE_TEST( FTPFileSystem_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( fileOps )
  DECLARE_TEST( timeOut )
END_DECLARE_TEST( FTPFileSystem_Test  )

BEGIN_DEFINE_TEST( FTPFileSystem_Test )
  ADD_TEST( FTPFileSystem_Test, standard ) 
  ADD_TEST( FTPFileSystem_Test, fileOps ) 
  ADD_TEST( FTPFileSystem_Test, timeOut ) 
  
END_DEFINE_TEST( FTPFileSystem_Test )

using namespace ::acdk::lang;
using namespace ::acdk::io;
//using namespace ::acdk::net::ftp;


void listRecursive(IN(RFile) file)
{
  RFileArray fa = file->listFiles();
  for (int i = 0; i < fa->length(); ++i)
  {
    RFile f = fa[i];
    /*
    RFileImpl rfimpl = f->getFileImpl();
    ::acdk::net::ftp::RFTPFileImpl fimpl = (::acdk::net::ftp::RFTPFileImpl)f->getFileImpl();
    */
    if (f->isDirectory() == true)
    {
      System::out->println("Dir: " + f->getCanonicalPath());
      listRecursive(f);
    }
    else
    {
      System::out->println("File: " + f->getCanonicalPath());
    }
  }
}

void
FTPFileSystem_Test::standard()
{
  
  ::acdk::util::logging::RLogger ftplogger = new ::acdk::util::logging::Logger("acdk.net.ftp");
  ::acdk::util::logging::LogManager::MinLevel = ::acdk::util::logging::LogManager::Threshold 
      = ::acdk::util::logging::Trace;
  ftplogger->addConsumer(new ::acdk::util::logging::ConsoleConsumer(new ::acdk::util::logging::SimpleFormatter()));
  
  File f("ftp://www.artefaktur.com/acdk/acdk_xml-src-1.01.0.zip");
  testAssert(f.isFile() == true);
  RReader rin = f.getReader();
  MemWriter mout;
  rin->trans(&mout);
  
  //File file("ftp://acdktest:testacdk@localhost");
  //listRecursive(&file);
  {
    
  }
}

void
FTPFileSystem_Test::fileOps()
{
  try {

  ::acdk::util::logging::RLogger ftplogger = new ::acdk::util::logging::Logger("acdk.net.ftp");
  ::acdk::util::logging::LogManager::MinLevel = ::acdk::util::logging::LogManager::Threshold 
      = ::acdk::util::logging::Trace;
  ftplogger->addConsumer(new ::acdk::util::logging::ConsoleConsumer(new ::acdk::util::logging::SimpleFormatter()));
  
  File dir("ftp://acdktest:testacdk@localhost/acdk_test");
  if (dir.exists() == false)
    dir.mkdir();
  File ffile("ftp://acdktest:testacdk@localhost/acdk_test/testData.dat");
  RWriter out = ffile.getWriter();
  const char* text = "Hello this is is a test file to test FTPFileSystem.\nThis file can be deleted.";
  int textlen = strlen(text);
  out->write((const byte*)text, 0, textlen);
  out->close();
  RReader in = ffile.getReader();
  RbyteArray ba = in->readAll();
  in->close();
  byteArray baorg((const byte*)text, textlen);
  testAssert(baorg.equals(ba) == true);
  File ffile2(&dir, "testData2.dat");
  ffile.renameTo(&ffile2);
  /*
  ffile2.deleteFile();
  dir.deleteFile();
*/
   } catch (RIOException ex) {
    System::out->println("FTPClient test failed: you need a local ftp server and a user acdktest pass testacdk "
                         "with writing access on the default ftp login directory for this test");

  }
  
}

void
FTPFileSystem_Test::timeOut()
{
  
/*
  run only in interactive mode
  ::acdk::util::logging::RLogger ftplogger = new ::acdk::util::logging::Logger("acdk.net.ftp");
  ::acdk::util::logging::LogManager::MinLevel = ::acdk::util::logging::LogManager::Threshold 
      = ::acdk::util::logging::Trace;
  ftplogger->addConsumer(new ::acdk::util::logging::ConsoleConsumer(new ::acdk::util::logging::SimpleFormatter()));
  {
    File d("ftp://www.artefaktur.com/acdk");
    testAssert(d.isDirectory() == true);
    Thread::sleep(1000 * 60 * 5); // 5 minutes
    File f(&d, "acdk_xml-src-1.01.0.zip");
    f.getReader();
  }
  {
    File d("ftp://www.artefaktur.com/acdk");
  }
  */
}

} // namespace ftp 
} // namespace net
} // namespace acdk
} // namespace tests



