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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/AsciiUtfEncoding.cpp,v 1.12 2005/04/28 14:58:14 kommer Exp $


#include "AsciiUtfEncoding.h"
#include "CEscapeEncoding.h"

#include <acdk/lang/Short.h>
#include <acdk/lang/System.h>
#include <acdk/io/MemWriter.h>

namespace acdk {
namespace locale {

void 
AsciiUtfEncoder::encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; stopOn != 0 && it < end; ++it)
  {
    if (stopOn == -2 && *it == 0)
      break;
    encode(out, *it);
    if (stopOn > 0)
      --stopOn;
  }
}

namespace {

byte encodeToHex(char ch)
{
  if (ch < 10)
    return '0' + ch;
  return 'A' + (ch - 10);
}

void _encode(ucchar ch, byte* bytes)
{
  // char = 16, 
  if (ch & 0xF000)
    bytes[0] = encodeToHex((ch & 0xF000) >> 12);
  else
    bytes[0] = '0';
  if (ch & 0x0F00)
    bytes[1] = encodeToHex((ch & 0x0F00) >> 8);
  else
    bytes[1] = '0';
  if (ch & 0x00F0)
    bytes[2] = encodeToHex((ch & 0x00F0) >> 4);
  else
    bytes[2] = '0';
  bytes[3] = encodeToHex((ch & 0x000F));
}




} // anon namespace


/**
  returns -1 on decoding errot
*/
int AsciiUtfDecoder::_decode(byte* source)
{
   const char* XS = "0123456789abcdef";
  uc2char erg = 0;
  for (int i = 0; i < 4; ++i)
  {
    const char* s = strchr(XS, ::tolower(*(source + i)));
    if (s == 0)
      return -1;
    erg += (s - XS) << (4 * (4 - i - 1));
  }
  return erg;
}

void 
AsciiUtfEncoder::encode(IN(acdk::io::RWriter) out, uc2char ch)
{
  if (ch < 0x80)
  {
    byte buffer[2];
    int num = ch;
    if (_withCEscapes == true)
      num = CEscapeEncoder::encodeChar(ch, buffer);
    _bytesWritten += num;
    out->write(buffer, 0, num);
    return;
  }
  out->write((byte*)"\\u", 0, 2);
  // "\\uFFFF"
  byte buffer[4];
  _encode(ch, buffer);
  out->write(buffer, 0, 4);
  _bytesWritten += 6;
}

RString 
AsciiUtfEncoder::encodeString(IN(RString) str)
{
  return str->encodeAscUnicode();
}


int 
AsciiUtfDecoder::decodeToChar(IN(acdk::io::RReader) in)
{
  int c = in->read();
  if (c == -1)
    return -1;
  ++_bytesReaded;
  if (c == '\\')
  {
    c = in->read();
    ++_bytesReaded;
    if (c == 'u')
    {
      byte buffer[4];
      int count = in->read(buffer, 0, 4);
      if (count < 4)
        return -2;
      _bytesReaded += 4;
      int erg = _decode(buffer);
      if (erg == -1)
        return -2;
      return erg;
    }
    else
    {
      if (_withCEscapes == false)
        return c;
      return CEscapeDecoder::decodeEscapeByte(c);
    }
  }
  if (c < 0x80)
    return c;
DecodeErrror: // ### TODO exception
  return c;
}

RString 
AsciiUtfDecoder::decodeToString(IN(acdk::io::RReader) in, int stopOn)
{
  
  int i;
  StringBuffer sb;
  while (stopOn != 0 && (i = in->read()) != -1)
  {
    if (stopOn == -2)
    {
      if (i == 0)
        return sb.toString();
    } 
    sb.append(decodeToChar(in));
    if (stopOn > 0)
      --stopOn;
  }
  return sb.toString();
}

RString 
AsciiUtfDecoder::decodeString(IN(RString) str)
{
  return String::decodeAscUnicode(str->c_str()); // #### move this code to here
}


AsciiUtfEncoding::AsciiUtfEncoding(bool withCEscapes)
: Encoding("AsciiUtf")
, _withCEscapes(withCEscapes)
{
}

//static 
REncoding 
AsciiUtfEncoding::getAsciiUtfEncoding()
{
  static RAsciiUtfEncoding _encoding;
  if (_encoding == Nil)
  {
    _encoding = new AsciiUtfEncoding(false);
    System::registerStaticReference(_encoding);
  }
  return &_encoding;
}

//static 
REncoding 
AsciiUtfEncoding::getAsciiUtfCEscapeEncoding()
{
  static RAsciiUtfEncoding _encoding;
  if (_encoding == Nil)
  {
    _encoding = new AsciiUtfEncoding(true);
    System::registerStaticReference(_encoding);
  }
  return &_encoding;
}


REncoder 
AsciiUtfEncoding::getEncoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new AsciiUtfEncoder(_withCEscapes, this, onMalformed, onUnmappable);
}

RDecoder 
AsciiUtfEncoding::getDecoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new AsciiUtfDecoder(_withCEscapes, this, onMalformed, onUnmappable);
}


namespace {

struct RegisterEncs
{
  RegisterEncs()
  {
    Encoding::registerEncoding("AsciiUtf", &AsciiUtfEncoding::getAsciiUtfEncoding);
    Encoding::registerEncoding("AsciiCEscUtf", &AsciiUtfEncoding::getAsciiUtfCEscapeEncoding);
  }
};
RegisterEncs _registerEncs;

} // anon namespace


} // locale
} // acdk

