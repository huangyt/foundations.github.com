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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/Encoding.h,v 1.15 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_Encoding_h
#define acdk_locale_Encoding_h

#include <acdk.h>
#include "Encoder.h"
#include "Decoder.h"
#include "IllegalCharsetNameException.h"

namespace acdk {
namespace locale {

// acdkmc hint
enum CodingErrorAction;

ACDK_DECL_CLASS(Encoding);

typedef REncoding (*EncodingCreator)(IN(RString) name);

/**
  Class to encode/decode strings
*/
class ACDK_CORE_PUBLIC Encoding
: extends ::acdk::lang::Object      
{
  ACDK_WITH_METAINFO(Encoding)
protected:
  RString _name;
  Encoding(IN(RString) name)
  : _name(name)
  {
  }

public:
  /**
   return the standard encoding
   This will use the system property user.encoding to determine
   the encoding.
   if this is not set returns ASCII
   @throw IllegalCharsetNameException if user.encoding contains 
          unknown encoding
  */
  static REncoding getEncoding() THROWS1(RIllegalCharsetNameException);
  /**
    set the standard encoding
  */
  static void setEncoding(IN(REncoding) encoding);
  /**
    retrive encoding by name
  */
  static REncoding getEncoding(IN(RString) name) THROWS1(RIllegalCharsetNameException);
  /**
    same as getEncoding, but does not throw an exception if encoding
    was not found
  */
  static REncoding findEncoding(IN(RString) name);
  /**
    see CEscapeEncoding
  */
  static REncoding getCEscapeEncoding();
  /**
    see AsciiEncoding
  */
  static REncoding getAsciiEncoding();
  /**
    see AsciiUtfEncoding::getAsciiUtfEncoding
  */
  static REncoding getUnicodeEscapeEncoding();
  /**
    see AsciiUtfEncoding::getAsciiUtfCEscapeEncoding()
  */
  static REncoding getUnicodeCEscapeEncoding();

  /**
    returns all available encodings
  */
  static RStringArray getAvailableEncodings();

  virtual REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError) = 0;
  virtual RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError) = 0;
  
  
  
  /**
    name of this encoding
  */
  RString getName() { return _name; }
  /**
    return the average bytes used to encode one character
  */
  virtual float averageBytesPerChar() = 0;
  /**
    return the maximum count of bytes use to encode one character
  */
  virtual float maxBytesPerChar() = 0;
  /**
    register known Encodings
  */
  foreign static void registerEncoding(IN(RString) name, EncodingCreator creator);
};


inline REncoding Encoder::getEncoding() { return _encoding; }
inline REncoding Decoder::getEncoding() { return _encoding; }

} // locale
} // acdk

#endif //acdk_locale_Encoding_h

