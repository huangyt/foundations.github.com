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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/acdk_net_URL_Test.cpp,v 1.13 2005/03/26 15:01:18 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/Writer.h>
#include <acdk/net/URL.h>
#include <acdk/net/SocketException.h>
#include <acdk/net/HttpURLConnectionImpl.h>
#include <acdk/net/ftp/FTPClient.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
namespace tests {
namespace acdk {
namespace net {
  


BEGIN_DECLARE_TEST( URL_Test )
  DECLARE_TEST( http )
  DECLARE_TEST( file )
  DECLARE_TEST( inet )
  DECLARE_TEST( httpUrlConnection )
  DECLARE_TEST( urlFileSystem )
END_DECLARE_TEST( URL_Test  )

BEGIN_DEFINE_TEST( URL_Test )
  ADD_TEST( URL_Test, http ) 
  ADD_TEST( URL_Test, file ) 
  ADD_TEST( URL_Test, inet ) 
  ADD_TEST( URL_Test, httpUrlConnection ) 
  ADD_TEST( URL_Test, urlFileSystem ) 
END_DEFINE_TEST( URL_Test )

using namespace ::acdk::lang;
using namespace ::acdk::net;
using namespace ::acdk::net::ftp;


void urlTest(IN(RURL) url, IN(RString) protocol, IN(RString) host)
{
  System::out->println("Test URL=[" + url->toString() + "]");
  testAssert(url->getProtocol()->equals(protocol) == true);
  System::out->println(host + " == " + url->getHost());
  //### not working now under linux testAssert(url->getHost()->equals(host) == true);
  try {
    RReader in = url->openStream();
    in->trans(&System::out->getWriter());
  } catch (::acdk::net::RSocketException ex) {
    System::out->println("Cannot reach URL: " + url->toString());
  } catch (::acdk::io::RIOException ex) {
      System::out->println("Cannot reach URL: " + url->toString() + " reason: " + ex->getMessage());
  }
}

void URL_Test::http()
{
  RURL httpurl = new URL("http://acdk.sourceforge.net");
  urlTest(httpurl, "http", "acdk.sourceforge.net");
}

void URL_Test::file()
{
  RString ahome = System::getProperties()->getProperty("ACDKHOME");
  testAssertComment(ahome != Nil, "Environmnet variable ACDKHOME has to be set");
  RString acdkhome = ::acdk::io::File(ahome).getCanonicalPath();
  
  acdkhome = acdkhome->replace(':', '|') + ::acdk::io::File::separator() + "bin" + 
                                          ::acdk::io::File::separator() + "install.sh";
  RURL fileurl = new URL("file://" + acdkhome);
  urlTest(fileurl, "file", acdkhome);
}

void 
URL_Test::inet()
{
  {
    URL url("inet:localhost:8888");
    testAssert(url.getProtocol()->equals("inet") == true);
    testAssert(url.getHost()->equals("localhost") == true);
    testAssert(url.getPort() == 8888);
  }
  {
    URL url("http://auser@acdk.sourceforge.net/acdk");
    testAssert(url.getProtocol()->equals("http") == true);
    testAssert(url.getHost()->equals("acdk.sourceforge.net") == true);
    testAssert(url.getUser()->equals("auser") == true);
  }
  {
    URL url("http://auser:apass@acdk.sourceforge.net:80/acdk");
    testAssert(url.getProtocol()->equals("http") == true);
    testAssert(url.getHost()->equals("acdk.sourceforge.net") == true);
    testAssert(url.getPort() == 80);
    testAssert(url.getUser()->equals("auser") == true);
    testAssert(url.getPassword()->equals("apass") == true);
  }
}

void
URL_Test::httpUrlConnection()
{
  RURL url = new URL("http://sourceforge.net/docman/?group_id=1"); // about 16bk
  RHttpURLConnectionImpl urlconnection = new HttpURLConnectionImpl(&url);
  urlconnection->connect();
  int headercount = urlconnection->getHeaderFieldCount();
  StringBuffer sb;
  for (int i = 0; i < headercount; ++i)
  {
    sb << urlconnection->getHeaderFieldKey(i) << ": " << urlconnection->getHeaderField(i) << "\n";
  }
  sb << "\n";
  RReader in = urlconnection->getInputStream();
  RLineNumberCharReaderImpl lin = new LineNumberCharReaderImpl(new ByteToCharReader(in));
  RString l = " ";
  while (l->length() != 0)
  {
    l = lin->readLine();
    if (l == Nil)
      break;
    sb << l;
    System::out->print(l);
  }
  
  File("page.outhttp.tmp").getWriter()->getCharWriter()->writeString(sb.toString());
  urlconnection->disconnect();

  in = url->openStream();
  in->trans(File("page.outhurl.tmp").getWriter());


}


void
URL_Test::urlFileSystem()
{
  ::acdk::util::logging::RLogger ftplogger = new ::acdk::util::logging::Logger("acdk.net.ftp");
  ::acdk::util::logging::LogManager::MinLevel = ::acdk::util::logging::LogManager::Threshold 
      = ::acdk::util::logging::Trace;
  ftplogger->addConsumer(new ::acdk::util::logging::ConsoleConsumer(new ::acdk::util::logging::SimpleFormatter()));
  
  {
    File f("file:///" + System::getAcdkHome() + "/cfg/acdk.cfg");
    RReader rin = f.getReader();
    MemWriter mout;
    rin->trans(&mout);
  }
  {
    File f("http://www.artefaktur.com/corporate/corporate-en.html");
    RReader rin = f.getReader();
    RFile p = f.getParentFile()->getParentFile();
    RString fname = p->getCanonicalPath();
    rin = f.getReader();
    MemWriter mout;
    rin->trans(&mout);
  }
  {
    File f("ftp://www.artefaktur.com/acdk/acdk_xml-src-1.01.0.zip");
    RReader rin = f.getReader();
    {
      MemWriter mout;
      rin->trans(&mout);
      rin->close();
    }
    
    RFile p = f.getParentFile()->getParentFile();
    RString fname = p->getCanonicalPath();
    File f2("ftp://www.artefaktur.com/acdk/acdk_xml-src-1.01.0.zip");

  }
  
  {
    /* this is currently not supported, because a SocketReader is not seek'able
    File f("ftp://www.artefaktur.com/acdk/acdk_xml-src-1.01.0.zip@/");
    bool isDir = f.isDirectory();
    */
  }
  
  
}

} // namespace net
} // namespace acdk
} // namespace tests



