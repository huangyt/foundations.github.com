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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLConnection.cpp,v 1.16 2005/04/08 10:53:20 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Exception.h>
#include <acdk/lang/System.h>
#include <acdk/util/Set.h>

#include "URL.h"
#include "URLConnection.h"
#include "URLStreamHandler.h"
#include "ContentHandler.h"
#include "ContentHandlerFactory.h"
#include "UnknownServiceException.h"
#include "MimeTypeMapper.h"


#include "UnknownServiceException.h"

namespace acdk {
namespace net {

USING_CLASS(::acdk::util::, Set);
USING_CLASS(::acdk::util::, Iterator);

ACDK_DECL_CLASS(DefaultContentHandler);

class DefaultContentHandler
  : extends Object,
  implements ContentHandler
{
  RObject getContent(IN(RURLConnection) urlcon) 
  {
    RReader rd = urlcon->getInputStream();
    RbyteArray ar;
    
    int length = urlcon->getContentLength();
    if (length <= 0)
      length = 1;
    
    ar = new byteArray(length);
    
    int count = rd->read(ar, 0, length);
    ar->resize(count);
    //    ar[count] = 0;
    return (RObject)ar;
  }
};

ACDK_DECL_CLASS(DefaultContentHandlerFactory);
class DefaultContentHandlerFactory  
  : extends Object,
  implements ContentHandlerFactory
{
  RContentHandler createContentHandler(IN(RString) mime_type) {
    return new DefaultContentHandler();
  }
};

//  static 
void
URLConnection::setContentHandlerFactory(IN(RContentHandlerFactory) fac)
{
  
  if (factory != Nil)
    THROW1(Exception, "The ContentHandlerFactory is already set");

  factory = fac;

}

//  virtual 
RString
URLConnection::getContentType()
{
  
  RString type = getHeaderField("content-type");
  if (type == Nil)
    type = guessContentTypeFromName(getURL()->getFile());

  return(type);

}

//  virtual 
RString 
URLConnection::toString()
{
  return(url->toString());
}

//  virtual 
RString
URLConnection::getHeaderField(IN(RString) name)
{
  
  for (int i=0; ; i++) {
    RString key = getHeaderFieldKey(i);
    if (key == Nil)
      return(Nil);
    
    if (key->toLowerCase()->equals(name->toLowerCase()))
      return(getHeaderField(i));
  }
}

//  virtual 
jlong
URLConnection::getHeaderFieldDate(IN(RString) key, jlong def)
{
  
  RString value = getHeaderField(key);
  if (value == Nil)
    return(0);
  // ### not implemented
  return 0;       
}

//  virtual 
int
URLConnection::getHeaderFieldInt(IN(RString) key, int def)
{
  
  RString value = getHeaderField(key);
  if (value == Nil)
    return(def);

  int retval = def;
  try {
    retval = Integer::parseInt(value);
  } catch (RNumberFormatException e) { 
    return(def);
  }

  return(retval);

}

//  virtual 

//  virtual 
RObject
URLConnection::getContent()
{
  
  RString type = getContentType();

  RContentHandler ch = Nil;
  if (factory == Nil) 
    setContentHandlerFactory(new DefaultContentHandlerFactory);

  if (factory != Nil)
    ch = factory->createContentHandler(type);

  if (ch != Nil)
    return(ch->getContent(this));

  THROW1(UnknownServiceException, type);
  return Nil;
}

//  virtual 
URLConnection::URLConnection(IN(RURL) url)
: connected(false),
  ifModifiedSince(0)
{
  
  this->url = url;
  allowUserInteraction = def_allow_user_inter;
  useCaches = def_use_caches;
  req_props = new HashMap;
  RSet s = def_req_props->keySet();
  RIterator e = s->iterator();
  while (e->hasNext()) 
  {
    RString key = (RString)e->next();
    RString value = (RString)def_req_props->get((RObject)key);
    
    req_props->put((RObject)key, (RObject)value);
  }
}

// private:
//  static 
RFileNameMap URLConnection::fileNameMap;

//  static 
RContentHandlerFactory URLConnection::factory;

//  static 
bool URLConnection::def_allow_user_inter;

//  static 
bool URLConnection::def_use_caches;

//  static 
RHashMap URLConnection::def_req_props;

class ACDK_NET_PUBLIC _URLConnection_init 
: implements ::acdk::lang::Object
, extends Initializer
{
public:
  void beforeMain() 
  {
    URLConnection::fileNameMap = new MimeTypeMapper();
    URLConnection::def_req_props = new HashMap();
  }
  void afterMain() {
  }
};

/** @internal */
ACDK_NET_PUBLIC SystemInitializer<_URLConnection_init> __URLC_init; 

} // namespace acdk
} // namespace net
