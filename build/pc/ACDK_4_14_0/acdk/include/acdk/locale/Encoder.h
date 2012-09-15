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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/Encoder.h,v 1.11 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_Encoder_h
#define acdk_locale_Encoder_h

#include <acdk.h>
#include "CodingErrorAction.h"

namespace acdk {
namespace locale {

// hint for acdkmc
enum CodingErrorAction;

ACDK_DECL_CLASS(Encoding);


ACDK_DECL_CLASS(Encoder);
/**
  Base interface to encode a character into a byte sequence
*/
class ACDK_CORE_PUBLIC Encoder
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(Encoder)
protected:
  REncoding _encoding;
  CodingErrorAction _onMalformedInput;
  CodingErrorAction _onUnmappableCharacter;
  RbyteArray _encodingReplacement;
  int _bytesWritten;
public:
  /**
    Please refer to REncoder getEncoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  */
  Encoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError);
  /**
    return the encoding for this Encoder
  */
  virtual REncoding getEncoding();
  /**
    return the bytes which should be written in case of unmappable 
    characters
    The returned bytes must be valid encodable/decodable 
  */
  RbyteArray getEncodingReplacement() { return _encodingReplacement; }
  /**
    set the replace byte sequence for unmappable characters
  */
  void setEncodingReplacement(IN(RbyteArray) replacement) { _encodingReplacement = replacement; }

  CodingErrorAction malformedInputAction() { return _onMalformedInput; }
  CodingErrorAction unmappableCharacterAction() { return _onUnmappableCharacter; }
  void onUnmappableCharacter(CodingErrorAction newAction) { _onUnmappableCharacter = newAction;  }
  void onMalformedInput(CodingErrorAction newAction) { _onMalformedInput = newAction; }
  /**
    return the bytes written by this Encoder
  */
  int bytesWritten() { return _bytesWritten; }
  /**
    resets the counter for the bytes written by this Encoder
    @see bytesWritten()
  */
  virtual void reset() { _bytesWritten = 0; }

  /**
    @param target writer
    @param the string to encode
    @param stopOn
            -1 = until end of string
            -2 = until internal 0 terminating character (normally '\0'
            >= 0 character number to write
  */
  virtual void encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn = -1) = 0;
  /**
    Encode one character to writer
  */
  virtual void encode(IN(acdk::io::RWriter) out, uc2char ch) = 0;
  /**
    encode the current string into this encoding
  */
  virtual RString encode(IN(RString) str) = 0;
  /**
    in case an Encoder cannot map an character
    this method will be called, which performces the CodingErrorAction
  */
  void handleUnmappable(IN(acdk::io::RWriter) out, uc2char ch);
};






} // locale
} // acdk

#endif //acdk_locale_Encoder_h

