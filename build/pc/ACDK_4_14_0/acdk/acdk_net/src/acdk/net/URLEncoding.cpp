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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLEncoding.cpp,v 1.6 2005/04/08 10:53:20 kommer Exp $

#include "URLEncoding.h"

namespace acdk {
namespace net {

//static 
acdk::locale::REncoding 
URLEncoding::getURLEncoding()
{
  return new URLEncoding();
}

acdk::locale::REncoder 
URLEncoding::getEncoder(acdk::locale::CodingErrorAction onMalformed, acdk::locale::CodingErrorAction onUnmappable)
{
  return new URLEncoder(this, onMalformed, onUnmappable);
}

acdk::locale::RDecoder 
URLEncoding::getDecoder(acdk::locale::CodingErrorAction onMalformed, acdk::locale::CodingErrorAction onUnmappable)
{
  return new URLDecoder(this, onMalformed, onUnmappable);
}

namespace {
/** @internal */
struct RegisterEncs
{
  RegisterEncs()
  {
    acdk::locale::Encoding::registerEncoding("URL", &URLEncoding::getURLEncoding);
  }
};
/** @internal */
RegisterEncs _registerEncs;

} // anon namespace


} // namespace acdk
} // namespace net





