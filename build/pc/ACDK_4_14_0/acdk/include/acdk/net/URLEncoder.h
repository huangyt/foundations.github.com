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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLEncoder.h,v 1.9 2005/04/30 14:06:55 kommer Exp $

#ifndef acdk_net_URLEncoder_h
#define acdk_net_URLEncoder_h

#include <acdk.h>
#include "Config.h"
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace net {

USING_CLASS(::acdk::locale::, Encoder);
ACDK_DECL_ENUM_FQ(acdk::locale::, CodingErrorAction);

ACDK_DECL_CLASS(URLEncoder);

final
class ACDK_NET_PUBLIC URLEncoder
: extends acdk::locale::Encoder
{
  ACDK_WITH_METAINFO(URLEncoder)
public:
  URLEncoder(IN(acdk::locale::REncoding) encoding, 
              acdk::locale::CodingErrorAction onMalformed = acdk::locale::ReportCodingError, 
              acdk::locale::CodingErrorAction onUnmappable = acdk::locale::ReplaceCodingError)
    : Encoder(encoding, onMalformed, onUnmappable)
  {
  }
  URLEncoder(acdk::locale::CodingErrorAction onMalformed = acdk::locale::ReportCodingError, 
             acdk::locale::CodingErrorAction onUnmappable = acdk::locale::ReplaceCodingError)
    : Encoder(Nil, onMalformed, onUnmappable)
  {
  }
  foreign virtual void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1);
  foreign virtual void encode(IN(acdk::io::RWriter) out, uc2char ch);
  foreign RString encode(IN(RString) source);
  
};

} // namespace acdk
} // namespace net

#endif //acdk_net_URLEncoder_h


