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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/HttpURLConnection.cpp,v 1.11 2005/03/08 18:54:43 kommer Exp $


#include <acdk_all.h>

#include "URLConnection.h"
#include "URL.h"
#include "HttpURLConnection.h"
#include "UnknownServiceException.h"
#include "ProtocolException.h"

#include "ProtocolException.h"
#include "URLStreamHandler.h"

namespace acdk {
namespace net {

//  virtual 
void
HttpURLConnection::setRequestMethod(IN(RString) method)
{
  if (RString(__valid_methods)->indexOf("|" + method->toUpperCase() + "|") == -1) {
    THROW1(ProtocolException, method->toUpperCase());
  }
  this->method = method;

}

//  virtual 
RReader
HttpURLConnection::getErrorStream()
{

  if (!connected)
    return(Nil);

  int code;
  try {
    code = getResponseCode();
  } catch(RIOException e) {
      code = -1;
  }

  if (code == -1)
    return(Nil);

  if (((code / 100) != 4) || ((code/ 100) != 5))
    return(Nil);

  try {
    RPushbackReader pbis = new PushbackReader(getInputStream());

    int i = pbis->read();
    if (i == -1)
      return(Nil);
 
    pbis->unread(i);
    return (RReader)pbis;
  } catch (RIOException e) {
    return Nil;
  }
}

// private:
//  static 
ACDK_NET_PUBLIC bool HttpURLConnection::_follow_redirects = true;

//  static 
ACDK_NET_PUBLIC char* HttpURLConnection::__valid_methods = "|GET|POST|HEAD|OPTIONS|PUT|DELETE|TRACE|";

} // namespace acdk
} // namespace net
