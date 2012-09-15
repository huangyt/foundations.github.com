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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdkx/net/ssl/acdkx_net_ssl_SSLConnection_Test.cpp,v 1.2 2005/03/30 17:30:16 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/StringWriter.h>

#include <acdk/net/URL.h>
#include <acdk/net/URLConnection.h>
#include <acdkx/net/ssl/SSLSocket.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>

namespace tests {
namespace acdkx {
namespace net {
namespace ssl {
  
BEGIN_DECLARE_TEST( SSLConnection_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( urlConnection )
END_DECLARE_TEST( SSLConnection_Test  )

BEGIN_DEFINE_TEST( SSLConnection_Test )
  ADD_TEST( SSLConnection_Test, standard ) 
  ADD_TEST( SSLConnection_Test, urlConnection )
END_DEFINE_TEST( SSLConnection_Test )

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::net;
using namespace ::acdkx::net::ssl;

void readHttps(IN(RString) surl)
{
  RURL url = new URL(surl);

  RSSLSocket sock = new SSLSocket(url->getHost(), 443);
  RReader rin = sock->getReader();
  RWriter rout = sock->getWriter();
  RString str = "GET " + url->getFile() + " HTTP/1.0\r\n\r\n";
  System::out->println("=========================================");
  System::out->println(str);
  rout->write((const byte*)str->byte_begin(), 0, str->length());
  int ic;
  StringBuffer sb;
  while ((ic = rin->read()) != -1)
  {
    sb.append((char)ic);
  }
  System::out->println(sb.toString());
}

void
SSLConnection_Test::standard()
{
  readHttps("https://www.openssl.org/");
}

void
SSLConnection_Test::urlConnection()
{
  RURL url = new URL("https://www.openssl.org");
  acdk::io::RWriter rout = ::acdk::lang::System::out->getWriter();
  url->openConnection()->getInputStream()->trans(rout);
  rout->flush();

}

} // namespace ssl 
} // namespace net
} // namespace acdkx
} // namespace tests



