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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLEncoding.h,v 1.5 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_URLEncoding_h
#define acdk_net_URLEncoding_h

#include <acdk.h>
#include "Config.h"
#include <acdk/locale/Encoding.h>
#include "URLEncoder.h"
#include "URLDecoder.h"

namespace acdk {
namespace net {

USING_CLASS(::acdk::locale::, Encoding);

ACDK_DECL_CLASS(URLEncoding);

class ACDK_NET_PUBLIC URLEncoding
: extends acdk::locale::Encoding
{
  ACDK_WITH_METAINFO(URLEncoding)
protected:
  URLEncoding() : Encoding("URL") {}
public:
  static acdk::locale::REncoding getURLEncoding();
  static acdk::locale::REncoding getURLEncoding(IN(RString) ) { return getURLEncoding(); }
  
  foreign float averageBytesPerChar() { return 1.5; }
  foreign float maxBytesPerChar()  { return 3.0; }
  foreign virtual acdk::locale::REncoder getEncoder(acdk::locale::CodingErrorAction onMalformed = acdk::locale::ReportCodingError, acdk::locale::CodingErrorAction onUnmappable = acdk::locale::ReplaceCodingError);
  foreign virtual acdk::locale::RDecoder getDecoder(acdk::locale::CodingErrorAction onMalformed = acdk::locale::ReportCodingError, acdk::locale::CodingErrorAction onUnmappable = acdk::locale::ReplaceCodingError);
};


} // namespace acdk
} // namespace net

#endif //acdk_net_URLEncoding_h



