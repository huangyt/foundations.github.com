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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/MalformedURLException.h,v 1.8 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_MalformedURLException_h
#define acdk_net_MalformedURLException_h

#include "Config.h"

#include <acdk/io/IOException.h>

namespace acdk {
namespace net {

using namespace acdk::io;

ACDK_DECL_THROWABLE(MalformedURLException, IOException);

class ACDK_NET_PUBLIC MalformedURLException
  : extends IOException
{
  ACDK_WITH_METAINFO(MalformedURLException)
public:
/*
 * Constructors
 */

/**
  * Constructs a new MalformedURLException with no descriptive message.
  */

  MalformedURLException()
    : IOException()
  {
  }


/*************************************************************************/

/**
  * Constructs a new MalformedURLException with a descriptive message 
  * passed in as an argument.
  *
  * @param message A message describing the error that occurs
  */

  MalformedURLException(IN(RString) message) 
    : IOException(message) 
  {
  }
protected:
private:
};

} // namespace acdk
} // namespace net

#endif //acdk_net_MalformedURLException_h


