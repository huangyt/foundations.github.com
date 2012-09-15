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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLConnection.h,v 1.18 2005/03/17 12:44:14 kommer Exp $

#ifndef acdk_net_URLConnection
#define acdk_net_URLConnection

#include "Config.h"
#include <acdk/util/HashMap.h>


#include "URL.h"
#include "FileNameMap.h"

namespace acdk {
namespace net {

//using namespace acdk::util;
using namespace acdk::lang;
USING_CLASS(acdk::util::, HashMap);

ACDK_DECL_CLASS(URL);
ACDK_DECL_INTERFACE(ContentHandlerFactory);
ACDK_DECL_CLASS(URLConnection);

class ACDK_NET_PUBLIC URLConnection
  : extends Object
{
  ACDK_WITH_METAINFO(URLConnection)
protected:
  bool allowUserInteraction;
  bool connected;
  bool doInput;
  bool doOutput;
  bool useCaches;
  jlong ifModifiedSince;
  RURL url;

private:
  static RFileNameMap fileNameMap;
  static RContentHandlerFactory factory;
  static bool def_allow_user_inter;
  static bool def_use_caches;
  static RHashMap def_req_props;
  RHashMap req_props;
  friend class _URLConnection_init;
public:
  URLConnection(IN(RURL) url);

  static void setContentHandlerFactory(IN(RContentHandlerFactory) fac);

  static bool getDefaultAllowUserInteraction() 
  {
    return(def_allow_user_inter);
  }

  static void setDefaultAllowUserInteraction(bool allow) 
  {
    def_allow_user_inter = allow;
  }

  static RString getDefaultRequestProperty(IN(RString) key) {
    return((RString)def_req_props->get((RObject)key->toLowerCase()));
  }

  static void setDefaultRequestProperty(IN(RString) key, RString value) 
  {
    def_req_props->put((RObject)key->toLowerCase(), (RObject)value);
  }

  static RString guessContentTypeFromStream(IN(RReader) is) 
  {
    return("application/octet-stream");
  }

  static RFileNameMap getFileNameMap() 
  {
    return(fileNameMap);
  }

  static void setFileNameMap(IN(RFileNameMap) fileNameMap) {
    URLConnection::fileNameMap = fileNameMap;  
  }

  static bool getDefaultUseCaches() 
  {
    return(def_use_caches);
  }

  static void setDefaultUseCaches(bool use) 
  {
    def_use_caches = use;
  }

  virtual bool getAllowUserInteraction() 
  {
    return(allowUserInteraction);
  }

  virtual void setAllowUserInteraction(bool allow) 
  {
    allowUserInteraction = allow;
  }

  virtual bool getDoInput() 
  {
    return(doInput);
  }

  virtual void setDoInput(bool input) 
  {
    doInput = input;
  }

  virtual bool getDoOutput() 
  {
    return(doOutput);
  }

  virtual void setDoOutput(bool output) 
  {
    doOutput = output;
  }

  virtual bool getUseCaches() { return useCaches;  }

  virtual void setUseCaches(bool use_caches)  
  {
    useCaches = use_caches;
  }

  virtual jlong getIfModifiedSince() 
  {
    return ifModifiedSince;
  }

  virtual void setIfModifiedSince(jlong modified_since)  
  {
    ifModifiedSince = modified_since;
  }

  virtual RString getRequestProperty(IN(RString) key) 
  {
    return((RString)req_props->get((RObject)key->toLowerCase()));
  }

  virtual void setRequestProperty(IN(RString) key, IN(RString) value) 
  {
    req_props->put((RObject)key->toLowerCase(), (RObject)value);
  }

  virtual RURL getURL() { return url; }

  virtual void connect() = 0;
 

  virtual RReader getInputStream() 
  {
    return(Nil);
  }

  virtual RWriter getOutputStream() 
  {
    return(Nil);
  }
  
  virtual RString getContentEncoding() 
  {
    return(getHeaderField("content-encoding"));
  }

  virtual int getContentLength() 
  {
    return(getHeaderFieldInt("content-length", -1));
  }

  virtual RString getContentType();

  virtual jlong getDate() 
  {
    return(getHeaderFieldDate("date", 0));
  }

  virtual jlong getExpiration() 
  {
    return(getHeaderFieldDate("expires", 0));
  }

  virtual jlong getLastModified() 
  {
    return(getHeaderFieldDate("last-modified", 0));
  }

  virtual RString getHeaderFieldKey(int index) 
  {
    return(Nil);
  }

  virtual RString getHeaderField(int index) 
  {
    return(Nil);
  }

  virtual RString getHeaderField(IN(RString) name);

  virtual jlong getHeaderFieldDate(IN(RString) key, jlong def);

  virtual int getHeaderFieldInt(IN(RString) key, int def);

  virtual RObject getContent();

  virtual RString toString();

protected:
  static RString guessContentTypeFromName(IN(RString) filename) 
  {
    return(fileNameMap->getContentTypeFor(filename->toLowerCase()));
  }
};

} // namespace acdk
} // namespace net

#endif //acdk_net_URLConnection


