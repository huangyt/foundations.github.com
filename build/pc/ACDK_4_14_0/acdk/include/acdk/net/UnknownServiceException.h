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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/UnknownServiceException.h,v 1.10 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_UnknownServiceException_h
#define acdk_net_UnknownServiceException_h

#include <acdk.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/NumberFormatException.h>
#include <acdk/io/IOException.h>

#include "net.h"

namespace acdk {
namespace net {

using namespace acdk::io;

ACDK_DECL_THROWABLE(UnknownServiceException, IOException);

class ACDK_NET_PUBLIC UnknownServiceException
: extends ::acdk::io::IOException
{
  ACDK_WITH_METAINFO(UnknownServiceException)
public:
/*
 * Constructors
 */

/**
  * Constructs a new UnknownServiceException with no descriptive message.
  */

  UnknownServiceException()
    : IOException()
  {
  }


/*************************************************************************/

/**
  * Constructs a new UnknownServiceException with a descriptive message (such as the
  * text from strerror(3)) passed in as an argument
  *
  * @param message A message describing the error that occurs
  */

  UnknownServiceException(IN(RString) message)
    : IOException(message)
  {
  }
protected:
private:
};

} // namespace acdk
} // namespace net

#endif //acdk_net_UnknownServiceException_h

