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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/CEscapeEncoding.h,v 1.7 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_CEscapeEncoding_h
#define acdk_locale_CEscapeEncoding_h

#include <acdk.h>
#include "Encoding.h"

namespace acdk {
namespace locale {

enum CodingErrorAction;

/**
  Please refer to acdk::util::CEscapeEncoding
*/
class ACDK_CORE_PUBLIC CEscapeEncoder
: extends Encoder
{
  DECL_ACDK_DEFAULT_METACLASS(Encoder)
public:
  CEscapeEncoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Encoder(encoding, onMalformed, onUnmappable)
  {
  }
  foreign virtual void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1);
  foreign virtual void encode(IN(acdk::io::RWriter) out, ucchar ch);
  foreign virtual RString encode(IN(RString) str);
  /**
    encode a char.
    @param c character to encode
    @param buffer buffer to write in. need at least 2 bytes
    @return return number of bytes needed
  */
  static int encodeChar(ucchar c, byte* buffer);
};

/**
  Please refer to acdk::util::CEscapeEncoding
*/
class ACDK_CORE_PUBLIC CEscapeDecoder
: extends Decoder
{
  DECL_ACDK_DEFAULT_METACLASS(Decoder)
public:
   CEscapeDecoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Decoder(encoding, onMalformed, onUnmappable)
  {
  }
  
  foreign virtual int decodeToChar(IN(acdk::io::RReader) in);
  foreign virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1);
  foreign virtual RString decode(IN(RString) str);

  /**
    decodes a byte escaped via leading '\'
    @return a ucchar
  */
  static int decodeEscapeByte(byte b);
};

/**
  implements encoding C literals like \\n for newline
  Only ASCII charactes are allowed
*/
class ACDK_CORE_PUBLIC CEscapeEncoding
: extends Encoding
{
public:
  CEscapeEncoding();
  static REncoding getCEscapeEncoding(IN(RString) ) { return &_encoder; }
  foreign float averageBytesPerChar() { return 1.2; }
  foreign float maxBytesPerChar()  { return 2.0; }

  virtual REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError) { return new CEscapeEncoder(this, onMalformed, onUnmappable); }
  virtual RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError){ return new CEscapeDecoder(this, onMalformed, onUnmappable); }

  virtual int getMaxByteCount(int charcount);
  virtual int getMaxCharCount(int bytecount);
  /*
  virtual int encode(String::utfiterator sb, String::utfiterator se, 
                     byte* ts, byte* te, bool flush) ;
  */
  virtual int encode(const ucchar* begin, const ucchar* end, 
                     byte* ts, byte* te, bool flush);
  /*
  virtual int getByteCount(String::utfiterator sb, String::utfiterator se, bool flush);
  */
  virtual int getByteCount(const ucchar* sb, const ucchar* se, bool flush);
  virtual int decode(const byte* sb, const byte* se, ucchar* tb, ucchar* te);
  //virtual int decode(const byte* sb, const byte* se, String::utfiterator tb, String::utfiterator te);

  virtual int getCharCount(const byte* sb, const byte* se);
  

  static CEscapeEncoding _encoder;
};

} // locale
} // acdk

#endif //acdk_locale_CEscapeEncoding_h

