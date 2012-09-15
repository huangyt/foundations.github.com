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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/AsciiEncoding.h,v 1.7 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_AsciiEncoding_h
#define acdk_locale_AsciiEncoding_h

#include "Encoding.h"

namespace acdk {
namespace locale {

enum CodingErrorAction;
/**
  encoded character to 7 bit ascii
*/
class ACDK_CORE_PUBLIC AsciiEncoder
: extends Encoder
{
  DECL_ACDK_DEFAULT_METACLASS(Encoder)
public:
  AsciiEncoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Encoder(encoding, onMalformed, onUnmappable)
  {
  }
  void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1);
  void encode(IN(acdk::io::RWriter) out, ucchar ch);
  virtual RString encode(IN(RString) str);
};

/**
  decode character from 7 bit ascii
*/
class ACDK_CORE_PUBLIC AsciiDecoder
: extends Decoder
{
  DECL_ACDK_DEFAULT_METACLASS(Decoder)
public:
  AsciiDecoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Decoder(encoding, onMalformed, onUnmappable)
  {
  }
  
  foreign virtual int decodeToChar(IN(acdk::io::RReader) in);
  foreign virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1);
  foreign virtual RString decode(IN(RString) str);
};

ACDK_DECL_CLASS(AsciiEncoding);
/**
  Class to encode/decode strings for US-ASCII
*/
class ACDK_CORE_PUBLIC AsciiEncoding
: extends Encoding   
{
  ACDK_WITH_METAINFO(AsciiEncoding)
protected:
  AsciiEncoding();

public:
  static REncoding getAsciiEncoding();
  static REncoding getAsciiEncoding(IN(RString) ) { return getAsciiEncoding(); }
  foreign float averageBytesPerChar() { return 1.0; }
  foreign float maxBytesPerChar()  { return 1.0; }
  virtual REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  virtual RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  /**
     just force a byte buffer to be a ascii string
  */
  foreign static RString decodeToString(const byte* buffer, int len = -1);
};


} // locale
} // acdk

#endif //acdk_locale_AsciiEncoding_h

