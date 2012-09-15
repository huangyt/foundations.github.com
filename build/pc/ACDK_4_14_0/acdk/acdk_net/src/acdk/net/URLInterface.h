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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLInterface.h,v 1.10 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_net_URLInterface_h
#define acdk_net_URLInterface_h

#include "Config.h"

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_INTERFACE(URLInterface);

/** 
  Interface to an URL.
  API: ACDK<br>
    To seperate core packages from acdk::net, URLInterface is introduced for usage in acdk.core 
    to seperate it from URL, which is implemented in acdk::net
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/04/08 10:53:20 $
*/
class ACDK_NET_PUBLIC URLInterface
      ACDK_INTERFACEBASE
{

  ACDK_WITH_METAINFO(URLInterface)

public: 
  /**
    
    API: Java<br>
    @param obj The leading argument
    @param ch The buffer to write in
    @return returns given converter as string
  */
  virtual RObject getContent() = 0;
  virtual RString getFile() = 0;
  virtual RString getHost() = 0;
  virtual int getPort() = 0;
  virtual RString getProtocol() = 0;
  virtual RString getUser() = 0;
  virtual RString getPassword() = 0;
  virtual RString getRef() = 0;
  //virtual RURLConnection openConnection() = 0;
  virtual RReader openStream() = 0;
  virtual bool sameFile(IN(RURLInterface) other) = 0;
  virtual RString toExternalForm() = 0;
};


} // namespace net 
} // namespace acdk 

#endif //acdk_net_URLInterface_h

