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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLStreamHandler.cpp,v 1.10 2005/02/05 10:45:29 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/NumberFormatException.h>
#include <acdk/io/IOException.h>

#include "net.h"
#include "URL.h"
#include "URLStreamHandler.h"
#include "MalformedURLException.h"

namespace acdk {
namespace net {

//  virtual 
void
URLStreamHandler::parseURL(IN(RURL) url, IN(RString) urlstr)
{
  RString resturl = urlstr;
  /*
  rfc1738
  ( 'http://' | 'ftp://' | 'inet://' | 'file://' ) [ user [ ':' pass ] @ ] [ host [ ':' port ] ] [ '/' urlpath ] [ '#' ref ]
  */

  int idx;
  RString host = Nil;
  int port = -1;
  RString file = Nil;
  RString anchor = Nil;
  RString user = Nil;
  RString pass = Nil;
  if (resturl->startsWith("//") == true)
    resturl = resturl->substring(2);
  
  int colon_index = resturl->indexOf(':');
  int slash_index = resturl->indexOf('/');
  int at_index = resturl->indexOf('@');
  if (at_index != -1 && (at_index < slash_index || slash_index == -1))
  {
    RString userpass = resturl->substr(0, at_index);
    resturl = resturl->substr(at_index + 1);
    if ((idx = userpass->indexOf(':')) != -1)
    {
      user = userpass->substr(0, idx);
      pass = userpass->substr(idx + 1);
    }
    else
    {
      user = userpass;
    } 
  }

  colon_index = resturl->indexOf(':');
  slash_index = resturl->indexOf('/');
  RString hostport;
  if (slash_index != -1)
  {
    hostport = resturl->substr(0, slash_index);
    resturl = resturl->substr(slash_index);
  }
  else
  {
    hostport = resturl;
    resturl = "";
  }
  if ((idx = hostport->indexOf(':')) != -1)
  {
    host = hostport->substr(0, idx);
    try {
      port = Integer::parseInt(hostport->substr(idx + 1));
    } catch (RNumberFormatException ex) {
      THROW1(MalformedURLException, urlstr);
    }
  }
  else
  {
    host = hostport;
  }
  slash_index = resturl->indexOf("/");
  if (slash_index != -1)
  {
    file = resturl;
  }
  else
    file = "/" + resturl;
  if ((idx = file->indexOf('#')) != -1)
  {
    anchor = file->substr(idx + 1);
    file = file->substr(0, idx);
  }
  /*  
  int slash_index = url_string->indexOf("/");
  int colon_index = url_string->indexOf(":");

  else if (slash_index == -1)
    slash_index = url_string->length();
  
  if ((colon_index == -1) || (colon_index > slash_index)) 
  {
    host = url_string->substring(0, slash_index);
  } else {
    host = url_string->substring(0, colon_index);
    
    RString port_str = url_string->substring(colon_index + 1, slash_index);
    try {
      port = Integer::parseInt(port_str);
    } catch (RNumberFormatException e) {
      return;
    }
  }
  if (slash_index < (url_string->length() - 1))
    url_string = url_string->substring(slash_index + 1);
  else
    url_string = "";
  
  if (end == 0) {
    file = "/" + url_string;
    anchor = Nil;
  } else {
    file = "/" + url_string->substring(0, url_string->length() - end);
    
    if (url_string->charAt(url_string->length() - end) == '#')
      anchor = url_string->substring((url_string->length() - end) + 1,
             url_string->length());
    else
      anchor = Nil;
  }
  if ((file == Nil) || (file->compareTo("") == 0))
    file = "/";
  */
  setURL(url, url->getProtocol(), host, port, user, pass, file, anchor); 
  
}
  
//  virtual 
RString
URLStreamHandler::toExternalForm(IN(RURL) url)
{
  RString protocol = url->getProtocol();
  RString host = url->getHost();
  int port = url->getPort();
  RString file = url->getFile();
  RString anchor = url->getRef();
  RString user = url->getUser();
  RString pass = url->getPassword();

  StringBuffer result;
  if (protocol != Nil)
    result << protocol << "://";
  if (user != Nil)
  {
    result << user;
    if (pass != Nil)
      result << ":" << pass;
    result << "@";
  }
  if (host != Nil)
    result << host;
  if (port != -1)
    result << ":" << String::valueOf(port);
  result << ((file != Nil) ? file : RString("/"));
  if (anchor != Nil)
    result << "#" << anchor;
  return result.toString();
}

//  virtual 
void 
URLStreamHandler::setURL(IN(RURL) url, IN(RString) protocol, IN(RString) host, int port, 
                         IN(RString) user, IN(RString) password, IN(RString) file, IN(RString) anchor)
{
  url->set(protocol, host, port, user, password, file, anchor);
}

  // private:
} // namespace acdk
} // namespace net
