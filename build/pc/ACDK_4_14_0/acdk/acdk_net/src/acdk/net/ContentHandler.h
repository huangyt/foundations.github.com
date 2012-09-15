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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ContentHandler.h,v 1.10 2005/04/13 13:41:33 kommer Exp $

#ifndef acdk_net_ContentHandler_h
#define acdk_net_ContentHandler_h

#include "Config.h"

namespace acdk {
namespace net {

ACDK_DECL_CLASS(URLConnection);
ACDK_DECL_INTERFACE(ContentHandler);

class ACDK_NET_PUBLIC ContentHandler
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ContentHandler)
public:
/*************************************************************************/

/**
  * This method reads from the InputStream of the passed in URL connection
  * and uses the data downloaded to create an Object represening the
  * content.  For example, if the URL is pointing to a GIF file, this 
  * method might return an Image object.  This method should be overridden
  * by subclasses.
  *
  * @param urlcon A URLConnection object to read data from
  *
  * @return An object representing the data read
  *
  * @exception IOException If an error occurs
  */

  virtual RObject getContent(IN(RURLConnection) urlcon) = 0;
protected:
private:
};

} // namespace acdk
} // namespace net

#endif //acdk_net_ContentHandler_h

