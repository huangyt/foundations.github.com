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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/HttpURLConnection.h,v 1.11 2005/03/30 17:30:13 kommer Exp $

#ifndef acdk_net_HttpURLConnection_h
#define acdk_net_HttpURLConnection_h

#include "net.h"

#include "URLConnection.h"

namespace acdk {
namespace net {

enum HttpResponceCode
{
    HTTP_UNKNOWN = -1,
    HTTP_OK = 200,    
    HTTP_CREATED = 201,
    HTTP_ACCEPTED = 202,
    HTTP_NOT_AUTHORITATIVE = 203,
    HTTP_NO_CONTENT = 204,    
    HTTP_RESET = 205,
    HTTP_PARTIAL = 206,

    HTTP_MULT_CHOICE = 300,
    HTTP_MOVED_PERM = 301,
    HTTP_MOVED_TEMP = 302,
    HTTP_SEE_OTHER = 303,
    HTTP_NOT_MODIFIED = 304,
    HTTP_USE_PROXY = 305,

    HTTP_BAD_REQUEST = 400,
    HTTP_UNAUTHORIZED = 401,
    HTTP_PAYMENT_REQUIRED = 402,
    HTTP_FORBIDDEN = 403,
    HTTP_NOT_FOUND = 404,
    HTTP_BAD_METHOD = 405,
    HTTP_NOT_ACCEPTABLE = 406,
    HTTP_PROXY_AUTH = 407,
    HTTP_CLIENT_TIMEOUT = 408,
    HTTP_CONFLICT = 409,
    HTTP_GONE = 410,
    HTTP_LENGTH_REQUIRED = 411,
    HTTP_PRECON_FAILED = 412,
    HTTP_ENTITY_TOO_LARGE = 413,
    HTTP_REQ_TOO_LONG = 414,
    HTTP_UNSUPPORTED_TYPE = 415,

    HTTP_SERVER_ERROR = 500,
    HTTP_INTERNAL_ERROR = 500,
    HTTP_BAD_GATEWAY = 502,
    HTTP_UNAVAILABLE = 503,
    HTTP_GATEWAY_TIMEOUT = 504,
    HTTP_VERSION = 505 
};
ACDK_DEF_LIB_ENUM(ACDK_NET_PUBLIC, HttpResponceCode);

ACDK_DECL_CLASS(URL);
ACDK_DECL_CLASS(HttpURLConnection);

/**
  @see HttpURLConnectionImpl for implementation details
*/
class ACDK_NET_PUBLIC HttpURLConnection
: extends URLConnection
{
  ACDK_WITH_METAINFO(HttpURLConnection)
public:
  
protected:
  RString method;
  HttpResponceCode responseCode;
  RString responseMessage;
public:
  static bool _follow_redirects;
  foreign static char* __valid_methods;

public:
  HttpURLConnection(IN(RURL) url)
  : URLConnection(url)
  , method("GET")
  , responseCode(HTTP_UNKNOWN)
  {
  }

  static void setFollowRedirects(bool follow) 
  {
    _follow_redirects = follow;
  }

  static bool getFollowRedirects() 
  {
    return _follow_redirects;
  }

  virtual void setRequestMethod(IN(RString) method);

  virtual RString getRequestMethod()
  {
    return method;
  }

  virtual HttpResponceCode getResponseCode() 
  {
    return responseCode;
  }

  virtual RString getResponseMessage() 
  {
    return responseMessage;
  }

  virtual RReader getErrorStream();

  virtual void disconnect() = 0;

  virtual bool usingProxy() = 0;
  virtual int getStandardPort() = 0;
  virtual RSocket createSocket(IN(RString) host, int port) = 0;
};

} // namespace acdk
} // namespace net

#endif //acdk_net_HttpURLConnection_h

