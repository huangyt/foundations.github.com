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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/UCS2Encoding.h,v 1.7 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_UCS2Encoding_h
#define acdk_locale_UCS2Encoding_h

#include "Encoding.h"

namespace acdk {
namespace locale {

/**
  byte order of the encoded character
*/
enum UCSEndianess
{
  NativeEndian = 0,
  BigEndian,
  LittleEndian
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, UCSEndianess);

enum CodingErrorAction;

ACDK_DECL_INTERFACE(UCS2Encoder);
/**
  Encode String to 2 bytes unicode character with given endian order
*/
class ACDK_CORE_PUBLIC UCS2Encoder
: extends Encoder
{
  DECL_ACDK_DEFAULT_METACLASS(Encoder)
  UCSEndianess _endian;
public:
  UCS2Encoder(IN(REncoding) encoding, UCSEndianess endian = NativeEndian, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Encoder(encoding, onMalformed, onUnmappable)
  , _endian(endian)
  {
  }
  UCSEndianess getEndian() { return _endian; }
  void setEndian(UCSEndianess endian) { _endian = endian; }

  foreign virtual void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1);
  foreign virtual void encode(IN(acdk::io::RWriter) out, ucchar ch);
  foreign virtual RString encode(IN(RString) str);

};

/**
  decode 2 bytes unicode character with given endian order to String
*/
class ACDK_CORE_PUBLIC UCS2Decoder
: extends Decoder
{
  DECL_ACDK_DEFAULT_METACLASS(Decoder)
  UCSEndianess _endian;
public:
  UCS2Decoder(IN(REncoding) encoding, UCSEndianess endian = NativeEndian, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Decoder(encoding, onMalformed, onUnmappable)
  , _endian(endian)
  {
  }
  UCSEndianess getEndian() { return _endian; }
  void setEndian(UCSEndianess endian) { _endian = endian; }

  foreign virtual int decodeToChar(IN(acdk::io::RReader) in);
  foreign virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1);
  foreign virtual RString decode(IN(RString) str);
};


ACDK_DECL_CLASS(UCS2Encoding);
/**
  encode/decode 2 bytes unicode character with given endian order from/to String
*/
class ACDK_CORE_PUBLIC  UCS2Encoding
: extends Encoding   
{
  ACDK_WITH_METAINFO(UCS2Encoding)
protected:
  UCS2Encoding(IN(RString) name, UCSEndianess endian);
  UCSEndianess _endian;
public:
  /** return encoding with native endianess */
  static REncoding getUCS2NativeEncoding();
  /** return encoding with little endianess */
  static REncoding getUCS2LeEncoding();
  /** return encoding with big endianess */
  static REncoding getUCS2BeEncoding();
  /**
    understands "UCS2", "UCS2-LE", "UCS2-BE"
  */
  static REncoding getUCS2Encoding(IN(RString) str);
  
  foreign float averageBytesPerChar() { return 2.0; }
  foreign float maxBytesPerChar()  { return 2.0; }
  virtual REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  virtual RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
};


} // locale
} // acdk

#endif //acdk_locale_UCS2Encoder_h

