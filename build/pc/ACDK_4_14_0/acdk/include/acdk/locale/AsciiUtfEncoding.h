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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/AsciiUtfEncoding.h,v 1.12 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_AsciiUtfEncoding_h
#define acdk_locale_AsciiUtfEncoding_h

#include "Encoding.h"


namespace acdk {
namespace locale {

ACDK_DECL_CLASS(AsciiUtfEncoder);

/**
  Please refer to acdk::locale::AsciiUtfEncoding
*/
class ACDK_CORE_PUBLIC AsciiUtfEncoder
: extends Encoder
{
  DECL_ACDK_DEFAULT_METACLASS(Encoder)
protected:
  bool _withCEscapes;
public:
  AsciiUtfEncoder(bool withCEscapes, IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Encoder(encoding, onMalformed, onUnmappable)
  , _withCEscapes(withCEscapes)
  {
  }
  foreign void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1);
  foreign void encode(IN(acdk::io::RWriter) out, uc2char ch);
  foreign virtual RString encode(IN(RString) str) { return encodeString(str); }
  static RString encodeString(IN(RString) str);
};


ACDK_DECL_CLASS(AsciiUtfDecoder);

/**
  Please refer to acdk::locale::AsciiUtfEncoding
*/
class ACDK_CORE_PUBLIC AsciiUtfDecoder
: extends Decoder
{
  DECL_ACDK_DEFAULT_METACLASS(Decoder)
protected:
  bool _withCEscapes;
public:
  AsciiUtfDecoder(bool withCEscapes, IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Decoder(encoding, onMalformed, onUnmappable)
  , _withCEscapes(withCEscapes)
  {
  }
  
  foreign virtual int decodeToChar(IN(acdk::io::RReader) in);
  foreign virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1);
  foreign virtual RString decode(IN(RString) str) { return decodeString(str); }
  static RString decodeString(IN(RString) str);
  foreign static int _decode(byte* bytes);
};

ACDK_DECL_CLASS(AsciiUtfEncoding);

/**
  Encodes/Decodes ASCII with UTF escapes.
  There are 2 variants:
  - AsciiUtfEncoding::getAsciiUtfEncoding() returns an Encoding which
    only tranlate unicode characters, which are not covered by US-ASCII
    into hex notation: \\uABCD.
  - AsciiUtfEncoding::getAsciiUtfEncoding() return an Encoding which
    also translates characters like new line to the C escaped notation \\n 
    or " to \\" so the resulting string can be uses as C literal

  This implements the Java Literal encoding, which is also be used in
  properties files
  
  Used in ASCII source code to decode UCS2 streams
*/
class ACDK_CORE_PUBLIC AsciiUtfEncoding
: extends Encoding   
{
  ACDK_WITH_METAINFO(AsciiUtfEncoding)
protected:
  bool _withCEscapes;
protected:
  /**
    if withCEscapes = true also \n are encodes as \\n
  */
  AsciiUtfEncoding(bool withCEscapes = true);

public:
    
  static REncoding getAsciiUtfEncoding();
  static REncoding getAsciiUtfCEscapeEncoding();
  static REncoding getAsciiUtfCEscapeEncoding(IN(RString) ) { return getAsciiUtfCEscapeEncoding(); }
  static REncoding getAsciiUtfEncoding(IN(RString) ) { return getAsciiUtfEncoding(); }
  
  foreign float averageBytesPerChar() { return 1.2; }
  foreign float maxBytesPerChar()  { return 5.0; }
  virtual REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  virtual RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
};


} // locale
} // acdk

#endif //acdk_locale_AsciiUtfEncoding_h

