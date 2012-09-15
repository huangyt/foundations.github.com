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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/Decoder.h,v 1.12 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_Decoder_h
#define acdk_locale_Decoder_h

#include "Encoder.h"

namespace acdk {
namespace locale {

// hint for acdkmc
enum CodingErrorAction;

ACDK_DECL_CLASS(Decoder);

/**
  Base class interface for decoding character from bytes
*/
class ACDK_CORE_PUBLIC Decoder
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(Decoder)
protected:
  CodingErrorAction _onMalformedInput;
  CodingErrorAction _onUnmappableCharacter;
  RString _decodingReplacement;
  REncoding _encoding;
  int _bytesReaded;
public:
  /**
    Please refer to 
      RDecoder getDecoder(CodingErrorAction onMalformed = ReportCodingError, CodingErrorAction onUnmappable = ReplaceCodingError)
  */
  Decoder(IN(REncoding) encoding, CodingErrorAction onMalformed = ReportCodingError, 
                                  CodingErrorAction onUnmappable = ReportCodingError);
  /**
    return the matchin encoder to this decoder
  */
  virtual REncoding getEncoding();
  /**
    return the bytes which should be written in case of unmappable 
    characters
  */
  RString getDecodingReplacement() { return _decodingReplacement; }
  /**
    set the replace string for unmappable character
  */
  void setDecodingReplacement(IN(RString) replacement) { _decodingReplacement = replacement; }

  CodingErrorAction malformedInputAction() { return _onMalformedInput; }
  CodingErrorAction unmappableCharacterAction() { return _onUnmappableCharacter; }
  void onUnmappableCharacter(CodingErrorAction newAction) { _onUnmappableCharacter = newAction;  }
  void onMalformedInput(CodingErrorAction newAction) { _onMalformedInput = newAction; }
  /**
    return the bytes readed by this Decoder
  */
  int bytesReaded() { return _bytesReaded; }
  /** 
    reset the read byte counter 
    @see bytesReaded()
  */
  virtual void reset() { _bytesReaded = 0; }
  /**
    decodes one char
    @return return ucchar or -1 if end of file, -2 if input is not mappable
  */
  virtual int decodeToChar(IN(acdk::io::RReader) in) = 0;
  /**
    decodes from a byte stream to a string.
    In normal cases the String contains a Ascii encoding if only 7bit character
    are in the byte stream.
    @param in reader to read from
    @param stopOn
            -1 = until end of string
            -2 = until internal 0 terminating character (normally '\0'
            >= 0 character number to write
  */
  virtual RString decodeToString(IN(acdk::io::RReader) in, int stopOn = -1) = 0;
  /**
    decodes a string from one encoding to another
  */
  virtual RString decode(IN(RString) str) = 0;
  /**
    implementation to handle unmappable character/bytes
  */
  RString handleUnmappable(uc2char ch);
};


} // locale
} // acdk

#endif //acdk_locale_Decoder_h

