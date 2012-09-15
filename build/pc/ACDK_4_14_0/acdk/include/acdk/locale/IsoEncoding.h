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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/IsoEncoding.h,v 1.6 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_IsoEncoding_h
#define acdk_locale_IsoEncoding_h

#include "Encoding.h"


namespace acdk {
namespace locale {

enum CodingErrorAction;

struct IsoUnicodeMapping 
{
  char* name;
  unsigned short* iso2unicode;
  unsigned short** unicode2iso;
  inline uc2char iso2uc(char ch) { return iso2unicode[(unsigned char)ch]; }
  inline uc2char uc2iso(uc2char ch) { return unicode2iso[ch >> 8][ch & 0x00FF]; }
};

/**
  Encodes String ISO byte encoding
*/
class ACDK_CORE_PUBLIC IsoEncoder
: extends Encoder
{
  DECL_ACDK_DEFAULT_METACLASS(Encoder)
private:
  foreign IsoUnicodeMapping* _mapping;
public:
  IsoEncoder(IN(REncoding) encoding, IsoUnicodeMapping* mapping, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Encoder(encoding, onMalformed, onUnmappable)
  , _mapping(mapping)
  {
  }
  foreign void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1);
  foreign void encode(IN(acdk::io::RWriter) out, ucchar ch);
  foreign virtual RString encode(IN(RString) str);
};

/**
  Decode ISO byte to String
*/
class ACDK_CORE_PUBLIC IsoDecoder
: extends Decoder
{
  DECL_ACDK_DEFAULT_METACLASS(Decoder)
private:
  foreign IsoUnicodeMapping* _mapping;
public:
  IsoDecoder(IN(REncoding) encoding, IsoUnicodeMapping* mapping, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  : Decoder(encoding, onMalformed, onUnmappable)
  , _mapping(mapping)
  {
  }
  
  foreign virtual int decodeToChar(IN(acdk::io::RReader) in);
  foreign virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1);
  foreign virtual RString decode(IN(RString) str);
};

ACDK_DECL_CLASS(IsoEncoding);

/**
  Encodes/Decodes ASCII with UTF escapes.
  Used in ASCII source code to decode UCS2 streams
*/
class ACDK_CORE_PUBLIC IsoEncoding
: extends Encoding   
{
  ACDK_WITH_METAINFO(IsoEncoding)

protected:
  foreign IsoUnicodeMapping* _mapping;
  IsoEncoding(IN(RString) enc);

public:
  static REncoding getIsoEncoding(IN(RString) enc);
  static RStringArray getAvailableEncodings();
  foreign float averageBytesPerChar() { return 1; }
  foreign float maxBytesPerChar()  { return 1; }
  virtual REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  virtual RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
};


} // locale
} // acdk

#endif //acdk_locale_IsoEncoding_h

