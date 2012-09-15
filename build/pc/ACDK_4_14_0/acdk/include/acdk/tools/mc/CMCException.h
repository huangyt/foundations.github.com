
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


#ifndef acdk_tools_mc_CMCException_h
#define acdk_tools_mc_CMCException_h

#include "mc.h"
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_THROWABLE(CMCException, Exception);

class CMCException
: public acdk::lang::Exception
{
public:
  CMCException(IN(acdk::io::RStreamTokenizer) in, IN(RString) msg)
  : Exception()
  {
    _what = in->getStreamPos() + ": " + msg;
  }
};


#define THROW_CMC(ex, in, msg) \
  do {  \
  ACDK_LOG(Fatal, msg); \
  THROW2(ex, in, msg); \
  } while (false)

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_CMCException_h
