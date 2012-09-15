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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/ByteAsciiEncoding.h,v 1.5 2005/02/05 10:45:01 kommer Exp $

#ifndef acdk_locale_ByteAsciiEncoding_h
#define acdk_locale_ByteAsciiEncoding_h

#include "Encoding.h"

namespace acdk {
namespace locale {

enum CodingErrorAction;

class ACDK_CORE_PUBLIC ByteAsciiEncoder
: extends Encoder
{
  DECL_ACDK_DEFAULT_METACLASS(Encoder)
public:
  ByteAsciiEncoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Encoder(encoding, onMalformed, onUnmappable)
  {
  }
  void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1);
  void encode(IN(acdk::io::RWriter) out, ucchar ch);
  virtual RString encode(IN(RString) str);
};

class ACDK_CORE_PUBLIC ByteAsciiDecoder
: extends Decoder
{
  DECL_ACDK_DEFAULT_METACLASS(Decoder)
public:
  ByteAsciiDecoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Decoder(encoding, onMalformed, onUnmappable)
  {
  }
  
  foreign virtual int decodeToChar(IN(acdk::io::RReader) in);
  foreign virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1);
  foreign virtual RString decode(IN(RString) str);
};

ACDK_DECL_CLASS(ByteAsciiEncoding);
/**
  ByteAsciiEncoding holds uninterpreted 8 bit charachter
*/
class ACDK_CORE_PUBLIC ByteAsciiEncoding
: extends Encoding   
{
  ACDK_WITH_METAINFO(ByteAsciiEncoding)
protected:
  ByteAsciiEncoding();

public:
  static REncoding getByteAsciiEncoding();
  static REncoding getByteAsciiEncoding(IN(RString) ) { return getByteAsciiEncoding(); }
  foreign float averageBytesPerChar() { return 1.0; }
  foreign float maxBytesPerChar()  { return 1.0; }
  virtual REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  virtual RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
};


} // locale
} // acdk

#endif //acdk_locale_ByteAsciiEncoding_h

