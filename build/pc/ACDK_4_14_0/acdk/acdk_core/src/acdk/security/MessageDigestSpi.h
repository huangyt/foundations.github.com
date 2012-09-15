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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/security/MessageDigestSpi.h,v 1.10 2005/03/08 12:45:39 kommer Exp $

#ifndef acdk_security_MessageDigestSpi_h
#define acdk_security_MessageDigestSpi_h


#include "security.h"

#include <acdk/lang/CloneNotSupportedException.h>

namespace acdk {
namespace security {


ACDK_DECL_CLASS(MessageDigestSpi);

class ACDK_SECURITY_LIB_PUBLIC MessageDigestSpi
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(MessageDigestSpi)
public:
  MessageDigestSpi()
  : Object()
  {
  }
  foreign virtual RObject clone()
  { 
    THROW0(CloneNotSupportedException);
    return Nil;
  }
  virtual RbyteArray engineDigest() = 0;
  virtual int engineDigest(IN(RbyteArray) buf, int offset, int len) = 0;
  virtual int engineGetDigestLength() { return 0; }
  
  virtual void engineReset() = 0;
  virtual void engineUpdate(byte input) = 0;
  virtual void engineUpdate(IN(RbyteArray) buf, int offset, int len) = 0;

};

} // Security
} // acdk



#endif //acdk_security_MessageDigestSpi_h

