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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ContentHandlerFactory.h,v 1.9 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_ContentHandlerFactory_h
#define acdk_net_ContentHandlerFactory_h

#include "Config.h"

#include "ContentHandler.h"

namespace acdk {
namespace net {

ACDK_DECL_INTERFACE(ContentHandlerFactory);

class ACDK_NET_PUBLIC ContentHandlerFactory
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ContentHandlerFactory)
public:
/**
  * This method is passed a MIME type as a string and is responsible for
  * returning the appropriate ContentType object.
  *
  * @param mime_type The MIME type to map to a ContentHandler
  *
  * @return The ContentHandler for the passed in MIME type
  */

  virtual RContentHandler createContentHandler(IN(RString) mime_type) = 0;
};

} // namespace acdk
} // namespace net

#endif //acdk_net_ContentHandlerFactory_h


