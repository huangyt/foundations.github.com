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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/UTF8Encoding.cpp,v 1.8 2005/03/19 12:07:54 kommer Exp $


#include "UTF8Encoding.h"
#include "UnmappableCharacterException.h"
#include <acdk/io/MemWriter.h>
#include <acdk/lang/Integer.h>

namespace acdk {
namespace locale {


void
encodeToUtf8(byteArray& target, uc2char ucc)
{
  //  ucc = Number::toLittleEndian(ucc);
  static const int firstByteMark[7] = {0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC};
	static const int byteMask = 0x3F;
	static const int byteMark = 0x80; 
  if (ucc < 0x80)
  {
    target.append((byte)ucc);
    return;
  }
  int byteSpaceNeeded;
  if (ucc < 0x80) byteSpaceNeeded = 1;
  else if (ucc < 0x800)    byteSpaceNeeded = 2;
  else byteSpaceNeeded = 3;
      /*if (ucc < 0x10000)  byteSpaceNeeded = 3; // ### TODO use uc4char, other always false
	else if (ucc < 0x200000) byteSpaceNeeded = 4;
	else if (ucc < 0x400000) byteSpaceNeeded = 5;
	else byteSpaceNeeded = 6;
      */
  target.resize(target.length() + byteSpaceNeeded);
  byte* tptr = target.data() + target.length();
  switch (byteSpaceNeeded)
	{
	case 6:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 5:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 4:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 3:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 2:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 1:	*--tptr =  ucc | firstByteMark[byteSpaceNeeded];
	}
}

void
encodeToUtf8(byteArray& target, IN(RString) str, int stopOn)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; stopOn != 0 && it < end; ++it)
  {
    if (stopOn == -2 && *it == 0)
      break;
    encodeToUtf8(target, *it);
    if (stopOn > 0)
      --stopOn;
  }
}

void 
UTF8Encoder::encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn)
{
  if (stopOn <= 0 && str->characterClass() == CCAscii)
  {
     out->write(str->byte_begin(), 0, str->length());
     _bytesWritten += str->length();
     return;
  }
  byteArray ba(0);
  ba.ensureCapacity(int(str->length() * UTF8Encoding::getUTF8Encoding()->averageBytesPerChar()));
  encodeToUtf8(ba, str, stopOn);
  out->write(&ba);
  _bytesWritten += ba.length();
}

void 
UTF8Encoder::encode(IN(acdk::io::RWriter) out, ucchar ch)
{
  byteArray ba(0);
  encodeToUtf8(ba, ch);
  out->write(&ba);
  _bytesWritten += ba.length();
}

RString 
UTF8Encoder::encode(IN(RString) str)
{
  return str;
}


int 
UTF8Decoder::decodeToChar(IN(acdk::io::RReader) in)
{
retry_with_next_char:
  int ch = in->read();
  if (ch == -1)
    return -1;

  int char_byte_length;
  int byte_mask = 0;
  if (ch < 128) { char_byte_length = 1; byte_mask = 0x7f; }
  else if ((ch & 0xe0) == 0xc0) { char_byte_length = 2; byte_mask = 0x1f; }
  else if ((ch & 0xf0) == 0xe0) { char_byte_length = 3; byte_mask = 0x0f; }
  else if ((ch & 0xf8) == 0xf0) { char_byte_length = 4; byte_mask = 0x07; }
  else if ((ch & 0xfc) == 0xf8) { char_byte_length = 5; byte_mask = 0x03; }
  else if ((ch & 0xfe) == 0xfc) { char_byte_length = 6; byte_mask = 0x01; }
  else 
  { 
    if (malformedInputAction() == IgnoreCodingError)
      goto retry_with_next_char;
    if (malformedInputAction() == ReplaceCodingError)
      return getDecodingReplacement()->charAt(0);
    THROW1(UnmappableCharacterException, "Invalid Unicode character: " + Integer::toHexString(ch));
    //char_byte_length = -1; 
  }

  uc2char ret = ch & byte_mask;
  int i;
  for (i = 1; i < char_byte_length; ++i)
  {
    ch = in->read();
    if ((ch & 0xc0) != 0x80)
    {
	    ret = 0xFFFF;
	    break;
	  }
    ret <<= 6;
    ret |= (ch & 0x3f);
  }
  _bytesReaded += char_byte_length;
  return ret;
}


RString 
UTF8Decoder::decodeToString(IN(acdk::io::RReader) in, int stopOn)
{
  StringBuffer sb;
  int c;
  while (stopOn != 0 && (c = decodeToChar(in)) != -1)
  {
    if (stopOn == -2 && c == 0)
      break;
    sb.append((uc2char)c);
    if (stopOn > 0)
      --stopOn;
  }
  return sb.toString();
}

RString 
UTF8Decoder::decode(IN(RString) str)
{
  return str->convert(CCUtf8);
}


UTF8Encoding::UTF8Encoding()
: Encoding("UTF8")
{
}

//static 
REncoding 
UTF8Encoding::getUTF8Encoding()
{
  static RUTF8Encoding _encoding;
  if (_encoding == Nil)
    _encoding = new UTF8Encoding();
  return &_encoding;
}

REncoder 
UTF8Encoding::getEncoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new UTF8Encoder(this, onMalformed, onUnmappable);
}

RDecoder 
UTF8Encoding::getDecoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new UTF8Decoder(this, onMalformed, onUnmappable);
}

namespace {

struct RegisterEncs
{
  RegisterEncs()
  {
    Encoding::registerEncoding("UTF8", &UTF8Encoding::getUTF8Encoding);
    Encoding::registerEncoding("UTF-8", &UTF8Encoding::getUTF8Encoding);
  }
};
RegisterEncs _registerEncs;

} // anon namespace


} // locale
} // acdk

