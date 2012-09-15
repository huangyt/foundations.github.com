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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLDecoder.h,v 1.9 2005/04/30 14:06:55 kommer Exp $

#ifndef acdk_net_URLDecoder_h
#define acdk_net_URLDecoder_h

#include "Config.h"
#include <acdk.h>
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace net {

USING_CLASS(::acdk::locale::, Decoder);
ACDK_DECL_ENUM_FQ(acdk::locale::, CodingErrorAction);

ACDK_DECL_CLASS(URLDecoder);

class ACDK_NET_PUBLIC URLDecoder
: extends acdk::locale::Decoder
{
  ACDK_WITH_METAINFO(URLDecoder)
public:
  URLDecoder(IN(acdk::locale::REncoding) encoding, 
              acdk::locale::CodingErrorAction onMalformed = acdk::locale::ReportCodingError, 
              acdk::locale::CodingErrorAction onUnmappable = acdk::locale::ReportCodingError)
  : Decoder(encoding, onMalformed, onUnmappable)
  {
  }
  URLDecoder(acdk::locale::CodingErrorAction onMalformed = acdk::locale::ReportCodingError, 
             acdk::locale::CodingErrorAction onUnmappable = acdk::locale::ReportCodingError)
  : Decoder(Nil, onMalformed, onUnmappable)
  {
  }
  /**
    decodes one char
    @return return ucchar or -1 if end of file, -2 if input is not mappable
    @see acdk::locale::Decoder::decodeToChar
  */
  virtual int decodeToChar(IN(acdk::io::RReader) in);
  /**
    decodes from a byte stream to a string.
    In normal cases the String contains a Ascii encoding if only 7bit character
    are in the byte stream.
    @param in reader to read from
    @param stopOn
            -1 = until end of string
            -2 = until internal 0 terminating character (normally '\0'
            >= 0 character number to write
    @see acdk::locale::Decoder::decodeToString
  */
  virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1);
  /**
    decodes a string from one encoding to another
    @see acdk::locale::Decoder::decode
  */
  virtual RString decode(IN(RString) str);
private:
  static int unhex(char c);
};

} // namespace acdk
} // namespace net

#endif //acdk_net_URLDecoder_h


