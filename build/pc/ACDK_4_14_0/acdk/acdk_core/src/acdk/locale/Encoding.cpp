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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/Encoding.cpp,v 1.20 2005/04/28 14:58:14 kommer Exp $


#include "Encoding.h"
#include "CEscapeEncoding.h"
#include "AsciiUtfEncoding.h"

#include "AsciiEncoding.h"
#include "UnmappableCharacterException.h"

#include <acdk/io/Writer.h>
#include <acdk/lang/System.h>

namespace acdk {
namespace locale {


Encoder::Encoder(IN(REncoding) encoding, CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
: _encoding(encoding)
, _onMalformedInput(onMalformed)
, _onUnmappableCharacter(onUnmappable)
, _bytesWritten(0)
{
}

void 
Encoder::handleUnmappable(IN(acdk::io::RWriter) out, uc2char ch)
{
  switch (unmappableCharacterAction())
  {
  case IgnoreCodingError:
    return;
  case ReplaceCodingError:
  {
    RbyteArray ba = getEncodingReplacement();
    if (ba != Nil)
      out->write(ba);
    break;
  }
  case ReportCodingError:
    THROW1(UnmappableCharacterException, getEncoding()->getName() + ": cannot encode : " + String::valueOf(ch));
  }
}

RString 
Decoder::handleUnmappable(uc2char ch)
{
  switch (unmappableCharacterAction())
  {
  case IgnoreCodingError:
    return "";
  case ReplaceCodingError:
  {
    RString s = getDecodingReplacement();
    if (s != Nil)
      return s;
    return "";
  }
  case ReportCodingError:
    THROW1(UnmappableCharacterException, getEncoding()->getName() + ": cannot encode : " + String::valueOf(ch));
  }
  return "";
}


Decoder::Decoder(IN(REncoding) encoding, CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
: _onMalformedInput(onMalformed)
, _onUnmappableCharacter(onUnmappable)
, _encoding(encoding)
, _bytesReaded(0)
{
}

namespace {

struct EncSlot
{
  RString name; 
  EncodingCreator creator;
  EncSlot(IN(RString) n, EncodingCreator c) : name(n), creator(c) {}
  EncSlot() : name(Nil), creator(0) {}
};

typedef ::acdk::lang::sys::core_vector<EncSlot> Encoders;


Encoders& 
getEncoders()
{
  static Encoders _encoders(0);
  return _encoders;
}


} // anon namespace

//static 
void 
Encoding::registerEncoding(IN(RString) name, EncodingCreator creator)
{
  getEncoders().push_back(EncSlot(name, creator));
  System::registerStaticReference(getEncoders().back().name);
}

OUT(REncoding)
Encoding_getStaticEncoding()
{
  static REncoding staticEncoding;
  return staticEncoding;
}

//static 
REncoding 
Encoding::getEncoding() THROWS1(RIllegalCharsetNameException)
{
  OUT(REncoding) enc =  Encoding_getStaticEncoding();
  if (enc != Nil)
    return enc;
  RString encName = System::getProperties()->getProperty("user.encoding");
  if (encName == Nil)
    enc = getAsciiEncoding();
  else
  {
    enc = findEncoding(encName);
    if (enc == Nil && encName->startsWith("1") == true)
      enc = findEncoding("CP" + encName);
    if (enc == Nil)
    {
      RString encodings = String::join((RObjectArray)getAvailableEncodings(), ", "); /** add template function String::join for ObjectArrays */
      THROW1(IllegalCharsetNameException, "Unknown Encoding: " + encName + ". Select one of: " + encodings);
    }
  }
  return enc;
}

void
Encoding::setEncoding(IN(REncoding) encoding)
{
  Encoding_getStaticEncoding() = encoding;
}

REncoding 
Encoding::findEncoding(IN(RString) name)
{
  Encoders& encs = getEncoders();
  Encoders::iterator it = encs.begin();
  Encoders::iterator end = encs.end();
  for (; it != end; ++it)
  {
    if (it->name->equalsIgnoreCase(name) == true)
      return it->creator(name);
  }
  return Nil;
}


//static 
REncoding 
Encoding::getEncoding(IN(RString) name) THROWS1(RIllegalCharsetNameException)
{
  REncoding enc = findEncoding(name);
  if (enc != Nil)
    return enc;
  RString encodings = String::join((RObjectArray)getAvailableEncodings(), ", ");
  THROW1(IllegalCharsetNameException, "Unknown Encoding: " + name + ". Select one of: " + encodings);
  return Nil;
}

//static 
RStringArray 
Encoding::getAvailableEncodings()
{
  Encoders& encs = getEncoders();
  RStringArray erg = new StringArray(encs.size());
  Encoders::iterator it = encs.begin();
  Encoders::iterator end = encs.end();
  for (int i = 0; it != end; ++it, ++i)
  {
    erg[i] = it->name;
  }
  return erg;
}

/*
RString 
Encoding::decode(IN(RString) str)
{
  RuccharArray ba = _decoder->decode((byte*)str->byte_begin(), (byte*)str->byte_end());
  return new String(ba);
}

RString 
Encoding::encode(IN(RString) str)
{
  // ### TODO RbyteArray ca = _encoder->encode(str->begin(), str->end(), true);
  //return new String(ca);
  return Nil;
}





*/

//static 
REncoding 
Encoding::getCEscapeEncoding()
{
  return &CEscapeEncoding::_encoder;
}

//static 
REncoding 
Encoding::getAsciiEncoding()
{
  return AsciiEncoding::getAsciiEncoding();
}

REncoding 
Encoding::getUnicodeEscapeEncoding()
{
  return AsciiUtfEncoding::getAsciiUtfEncoding();
}

//static 
REncoding 
Encoding::getUnicodeCEscapeEncoding()
{
  return AsciiUtfEncoding::getAsciiUtfCEscapeEncoding();
}


UnmappableCharacterException::UnmappableCharacterException() 
: CharacterCodingException() 
{
    //int* ptr = 0;
    //*ptr = 42;
}

UnmappableCharacterException::UnmappableCharacterException(IN(RString) what) 
: CharacterCodingException(what) 
{
    //int* ptr = 0;
    //*ptr = 42;
}



} // locale
} // acdk



