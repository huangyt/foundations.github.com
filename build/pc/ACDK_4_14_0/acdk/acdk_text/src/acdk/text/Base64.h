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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/Base64.h,v 1.7 2005/02/05 10:45:32 kommer Exp $

#ifndef acdk_text_Base64_h
#define acdk_text_Base64_h

#include <acdk.h>

#include "Config.h"

namespace acdk {
namespace text {

ACDK_DECL_CLASS(Base64);

class ACDK_TEXT_PUBLIC Base64
: public ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(Base64)
public:
  static RbyteArray encode(IN(RbyteArray) in);
  static RbyteArray decode(IN(RbyteArray) in);
};

} // text
} // acdk

#endif //acdk_text_Base64_h

