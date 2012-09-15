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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URL.cpp,v 1.20 2005/04/08 10:53:20 kommer Exp $

#include <acdk.h>
#include "net.h"

#include "TCPSocket.h"
#include "SocketException.h"
#include "Socket.h"

#include "HttpURLConnection.h"
#include "URL.h"
#include "URLConnection.h"
#include "URLStreamHandlerFactory.h"
#include "URLStreamHandler.h"
#include "MalformedURLException.h"
#include "HeaderFieldHelper.h"
#include "HttpURLConnection.h"

#include <acdk/lang/Exception.h>
#include <acdk/io/IOException.h>
#include "MalformedURLException.h"
#include "ProtocolException.h"
#include "FileURLConnection.h"
#include "HttpURLConnectionImpl.h"
#include "InetURLConnection.h"

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;






ACDK_DECL_CLASS(URLDefaultStreamHandler_http);
/** @internal */
class URLDefaultStreamHandler_http
  : extends URLStreamHandler
{
protected:
  RURLConnection openConnection(IN(RURL) url) 
  {
    return(new HttpURLConnectionImpl(url));
  }
  virtual void parseURL(IN(RURL) url, IN(RString) url_string)
  {
    URLStreamHandler::parseURL(url, url_string);
    if (url->getPort() == -1)
      url->setPort(80);
  }

  RURLStreamHandler createInstance()
  {
    return new URLDefaultStreamHandler_http();
  }
};


ACDK_DECL_CLASS(URLDefaultStreamHandler_file);
/** @internal */
class URLDefaultStreamHandler_file
  : extends URLStreamHandler
{
protected:
  RURLConnection openConnection(IN(RURL) url) 
  {
    return(new FileURLConnection(url));
  }
  
  RURLStreamHandler createInstance()
  {
    return new URLDefaultStreamHandler_file();
  }
};


ACDK_DECL_CLASS(URLDefaultStreamHandler_inet);
/** @internal */
class URLDefaultStreamHandler_inet
  : extends URLStreamHandler
{
protected:
  RURLConnection openConnection(IN(RURL) url) 
  {
    return(new InetURLConnection(url));
  }
  RURLStreamHandler createInstance()
  {
    return new URLDefaultStreamHandler_inet();
  }
};


ACDK_DECL_CLASS(URLDefaultStreamHandlerFactory);
/** @internal */
class ACDK_NET_PUBLIC URLDefaultStreamHandlerFactory
: extends Object,
  implements URLStreamHandlerFactory
{
public:
  virtual RURLStreamHandler createURLStreamHandler(IN(RString) protocol) 
  {
    if (protocol->compareTo("http") == 0)
      return new URLDefaultStreamHandler_http();
    if (protocol->compareTo("file") == 0)
      return new URLDefaultStreamHandler_file();
    if (protocol->compareTo("inet") == 0)
      return new URLDefaultStreamHandler_inet();
    return Nil;
  }
};

namespace {
/** @internal */
struct RegisterStreamHandlerFactory
{
  RegisterStreamHandlerFactory(IN(RString) name, IN(RURLStreamHandlerFactory) factory)
  {
    URLStreamHandlerFactory::registerFactory(name, factory);
  }
};

/** @internal */
RegisterStreamHandlerFactory _httpRegisterFactory("http", new URLDefaultStreamHandlerFactory());
/** @internal */
RegisterStreamHandlerFactory _fileRegisterFactory("file", new URLDefaultStreamHandlerFactory());
/** @internal */
RegisterStreamHandlerFactory _inetRegisterFactory("inet", new URLDefaultStreamHandlerFactory());

} // anon namespace

void
URL::_URLinit(IN(RString) protocol, IN(RString) host, int port, IN(RString) user, IN(RString) pass, IN(RString) file, IN(RURLStreamHandler) ph)
{

  this->protocol = protocol->toLowerCase();
  this->host = host;
  this->port = port;
  this->file = file;
  this->user = user;
  this->password = pass;

  if (ph != Nil) {
    this->ph = ph;
  } else {
    this->ph = URLStreamHandlerFactory::getProtocolHandler(protocol);
  }

}



void
URL::_URLbyContext(IN(RURL) context, IN(RString) url, IN(RURLStreamHandler) ph)
{
/*
  rfc1738
  ( 'http://' | 'ftp://' | 'inet://' | 'file://' ) [ user [ ':' pass ] @ ] [ host [ ':' port ] ] [ '/' urlpath ] [ '#' ref ]
  'inet:' hostname ':' port
  */

  RString resturl = url;
  int idx;
  
  int colon_index = resturl->indexOf(':');
  int slash_index = resturl->indexOf('/');
  
  if (colon_index != -1)
  {
    if (slash_index == -1 || colon_index < slash_index)
    {
      protocol = url->substring(0, colon_index);
      resturl = resturl->substr(colon_index + 1);
    }
  }
  int s3_index = resturl->indexOf(":///");
  if (s3_index != -1)
  {
      protocol = resturl->substr(0, s3_index);
      resturl = resturl->substr(s3_index + 2);
  }
  if (protocol == Nil)
    if (context == Nil)
      THROW1(MalformedURLException, url);
    else 
      protocol = context->getProtocol();
   protocol = protocol->toLowerCase();

  if (context != Nil)
  {
    if (context->getProtocol()->toLowerCase()->equals(protocol) == true) 
    {
        host = context->getHost();
        port = context->getPort();
        file = context->getFile();
        user = context->getUser();
        password = context->getPassword();
        if (file == Nil)
          file = "";
    }
  }
  if (ph != Nil) 
    this->ph = ph;
  else
    this->ph = URLStreamHandlerFactory::getProtocolHandler(protocol);

  this->ph->parseURL(this, resturl);
  /*
  if (context == Nil) 
  {
    this->ph->parseURL(this, url);
  } 
  else 
  {
    int idx = file->lastIndexOf("/"); 
    if (idx == -1) {
      file = url;
    } else if (idx == (file->length() - 1)) {
      file = file + url;
    } else {
      file = file->substring(0, idx+1) + url;
    }
  }
  // System::out->println(url);
  _hashCode = toExternalForm()->hashCode();
  */
}


//  virtual 
RURLConnection 
URL::openConnection()
{
 return(ph->openConnection(this));
}

//  virtual 
RString 
URL::toExternalForm()
{
  return(ph->toExternalForm(this));
}

//  virtual
RReader
URL::openStream()
{
  return(openConnection()->getInputStream());
}

//  virtual 
RObject 
URL::getContent()
{
  return(openConnection()->getContent());
}

//  virtual 
bool
URL::equals(IN(RObject) url)
{
  if (url == Nil)
    return(false);

  if (!(instanceof(url, URL)))
    return(false);

  RURL u = (RURL)url;

  if (sameFile((RURLInterface)u) == false)
    return false;

  RString s = u->getRef();
  if (s != Nil) {
    if (!s->equals(getRef())) {
      return false;
    } else if (getRef() != Nil) {
      return false;
    }
  }
  return(true);
}

//  virtual 
bool
URL::sameFile(IN(RURLInterface) url)
{

  if (url == Nil)
    return(false);

  RString s = url->getProtocol();
  if (s != Nil) {
    if (!s->equals(getProtocol())) {
      return(false);
    }
  } else if (getProtocol() != Nil) {
    return(false);
  }

  s = url->getHost();
  if (s != Nil) {
    if (!s->equals(getHost())) {
      return(false); 
    }
  } else if (getHost() != Nil) 
    return(false);

  if (url->getPort() != getPort())
    return(false);

  s = url->getFile();
  if (s != Nil) {
    if (!s->equals(getFile())) {
      return(false); 
    }
  } else if (getFile() != Nil) {
    return(false);
  }
  return(true);
}


// protected:
//  virtual 
void
URL::set(IN(RString) protocol, IN(RString) host, int port, IN(RString) user, IN(RString) password, IN(RString) file, IN(RString) ref)
{

  this->protocol = protocol;
  if (host != Nil)
    this->host = host;
  if (port != -1)
    this->port = port;
  if (file != Nil)
    this->file = file;
  if (ref != Nil)
    this->ref = ref;
  if (user != Nil)
    this->user = user;
  if (password != Nil)
    this->password = password;
}

//static
RString 
URL::fileAsUrlName(IN(RString) fileName)
{
  RString p = fileName;
  if (p->indexOf("://") != -1)
    return p;
  p = p->replace('\\', '/');
  return "file:///" + p;
}

//static 
RString 
URL::fileAsUrlName(IN(acdk::io::RFile) file)
{
   RString p = file->getCanonicalPath();
  if (p->indexOf("://") != -1)
    return p;
  p = p->replace('\\', '/');
  if (file->isDirectory() == true)
    p = p + "/";
  return "file:///" + p;
}

} // namespace acdk
} // namespace net
