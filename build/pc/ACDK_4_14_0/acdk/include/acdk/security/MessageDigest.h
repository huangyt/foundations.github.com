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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/security/MessageDigest.h,v 1.11 2005/02/05 10:45:03 kommer Exp $

#ifndef acdk_security_MessageDigest_h
#define acdk_security_MessageDigest_h

#include <acdk.h>
#include <acdk/lang/Cloneable.h>

#include "MessageDigestSpi.h"

namespace acdk {
namespace security {

using namespace acdk::lang;

ACDK_DECL_CLASS(MessageDigest);

class ACDK_SECURITY_LIB_PUBLIC MessageDigest
: extends MessageDigestSpi
, implements acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(MessageDigest)
private:
  RString _algorithm;
protected:
  MessageDigest(IN(RString) algorithm) 
  : _algorithm(algorithm)
  {
  }
public:
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc);
  // re-declare that acdkmc detects abstract class
  virtual void engineReset() = 0;

  RbyteArray digest() 
  { 
    return engineDigest(); 
  }
  int digest(IN(RbyteArray) buf, int offset, int len) 
  { 
    return engineDigest(buf, offset, len);
  }
  int getDigestLength()
  {
    return engineGetDigestLength();
  }
  void reset()
  {
    engineReset();
  }
  void update(byte input)
  {
    engineUpdate(input);
  }
  void update(IN(RbyteArray) buf, int offset = 0, int len = -1)
  {
    if (len == -1)
      len = buf->length() - offset;
    engineUpdate(buf, offset, len);
  }

  RString getAlgorithm() { return _algorithm; }

  static RMessageDigest getInstance(IN(RString) algorithm);
  //static RMessageDigest getInstance(String algorithm, String provider);
  //Provider getProvider() 
  static bool isEqual(IN(RbyteArray) digesta, IN(RbyteArray) digestb);
  virtual RString toString();
};

} // Security
} // acdk

#endif //acdk_security_MessageDigest_h

