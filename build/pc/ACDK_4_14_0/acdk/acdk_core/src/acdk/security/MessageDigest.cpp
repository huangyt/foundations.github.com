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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/security/MessageDigest.cpp,v 1.9 2005/03/08 12:45:39 kommer Exp $


#include <acdk.h>
#include "security.h"

#include "MessageDigest.h"
#include "NoSuchAlgorithmException.h"
#include "SHAMessageDigest.h"

#include <acdk/lang/CloneNotSupportedException.h>
#include "NoSuchAlgorithmException.h"
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace security {

using namespace acdk::lang;

//virtual 
RObject 
MessageDigest::clone(sys::Allocator* alc)
{
  THROW0(CloneNotSupportedException);
  return Nil;
}

//static 
RMessageDigest 
MessageDigest::getInstance(IN(RString) algorithm)
{
  if (algorithm->equals(RString("SHA")) == true)
    return new SHAMessageDigest();
  THROW1(NoSuchAlgorithmException, algorithm);
  return Nil;
}

//static 
bool 
MessageDigest::isEqual(IN(RbyteArray) digesta, IN(RbyteArray) digestb)
{
  if (digesta->length() != digestb->length())
    return false;
  for (int i = digesta->length() - 1; i >= 0; -- i)
    if (digesta[i] != digestb[i])
      return false;
  return true;
}

//virtual 
RString 
MessageDigest::toString()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

} // Security
} // acdk
