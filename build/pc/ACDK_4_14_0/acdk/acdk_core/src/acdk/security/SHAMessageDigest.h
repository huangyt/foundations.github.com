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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/security/SHAMessageDigest.h,v 1.10 2005/02/05 10:45:03 kommer Exp $

#ifndef acdk_security_SHAMessageDigest_h
#define acdk_security_SHAMessageDigest_h

#include "MessageDigest.h"

namespace acdk {
namespace security {

using namespace acdk::lang;

ACDK_DECL_CLASS(SHAMessageDigest);

class ACDK_SECURITY_LIB_PUBLIC SHAMessageDigest
: extends MessageDigest/*,
  implements acdk::lang::Cloneable*/
{
  ACDK_WITH_METAINFO(SHAMessageDigest)
public:
  SHAMessageDigest();
  SHAMessageDigest(SHAMessageDigest& other);
  
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc);
  foreign virtual RbyteArray engineDigest();
  foreign virtual int engineDigest(IN(RbyteArray) buf, int offset, int len);
  foreign virtual int engineGetDigestLength();
  
  foreign virtual void engineReset();
  foreign virtual void engineUpdate(byte input);
  foreign virtual void engineUpdate(IN(RbyteArray) buf, int offset, int len);

private:
  int _digest[5];         /* message digest */
  int _count_lo;
  int _count_hi;        /* 64-bit bit count */
  int _data[16];          /* SHA data buffer */
  int _local;  
  
  void _init();
  void _final();
  void _transform();
  void _update(byte *buffer, int count);
};
  

} // Security
} // acdk

#endif //acdk_security_SHAMessageDigest_h

