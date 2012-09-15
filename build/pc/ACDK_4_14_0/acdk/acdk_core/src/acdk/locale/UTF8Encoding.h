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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/UTF8Encoding.h,v 1.6 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_UTF8Encoding_h
#define acdk_locale_UTF8Encoding_h

#include "Encoding.h"

namespace acdk {
namespace locale {

enum CodingErrorAction;

/**
  encode a string to UTF8 byte encoding
*/
class ACDK_CORE_PUBLIC UTF8Encoder
: extends Encoder
{
  DECL_ACDK_DEFAULT_METACLASS(Encoder)
public:
  UTF8Encoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  UTF8Encoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Encoder(encoding, onMalformed, onUnmappable)
  {
  }
  void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1);
  void encode(IN(acdk::io::RWriter) out, ucchar ch);
  virtual RString encode(IN(RString) str);
};

/**
  decode UTF8 byte encoding to a string
*/
class ACDK_CORE_PUBLIC UTF8Decoder
: extends Decoder
{
  DECL_ACDK_DEFAULT_METACLASS(Decoder)
public:
  UTF8Decoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  UTF8Decoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Decoder(encoding, onMalformed, onUnmappable)
  {
  }
  
  foreign virtual int decodeToChar(IN(acdk::io::RReader) in);
  foreign virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1);
  foreign virtual RString decode(IN(RString) str);
};

ACDK_DECL_CLASS(UTF8Encoding);
/**
  Class to encode/decode UTF8 encoded strings
*/
class ACDK_CORE_PUBLIC UTF8Encoding
: extends Encoding   
{
  ACDK_WITH_METAINFO(UTF8Encoding)
protected:
  UTF8Encoding();

public:
  static REncoding getUTF8Encoding();
  static REncoding getUTF8Encoding(IN(RString) ) { return getUTF8Encoding(); }
  
  foreign float averageBytesPerChar() { return 1.5; }
  foreign float maxBytesPerChar()  { return 4.0; }
  virtual REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  virtual RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
};

inline
UTF8Encoder::UTF8Encoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
: Encoder(UTF8Encoding::getUTF8Encoding(), onMalformed, onUnmappable)
{
}

inline
UTF8Decoder::UTF8Decoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
: Decoder(UTF8Encoding::getUTF8Encoding(), onMalformed, onUnmappable)
{
}

} // locale
} // acdk

#endif //acdk_locale_UTF8Encoding_h

