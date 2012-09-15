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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/UCS2Encoding.cpp,v 1.7 2005/04/28 14:58:14 kommer Exp $


#include "UCS2Encoding.h"
#include "../lang/StringInternals.h"
#include <acdk/io/Writer.h>
#include <acdk/lang/System.h>

namespace acdk {
namespace locale {

void 
UCS2Encoder::encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn)
{
  if (stopOn <= 0 && str->characterClass() == CCUcs2)
  {
     out->write(str->byte_begin(), 0, str->length() * 2);
     _bytesWritten += str->length() * 2;
     return;
  }
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

void 
UCS2Encoder::encode(IN(acdk::io::RWriter) out, ucchar ch)
{
  byte buf[2];
#ifdef ACDK_BIGENDIAN
  buf[1] = (byte)ch;
  buf[0] = ch >> 8;
#else
  buf[0] = (byte)ch;
  buf[1] = ch >> 8;
#endif
  out->write(buf, 0, 2);
  _bytesWritten += 2;
}

RString 
UCS2Encoder::encode(IN(RString) str)
{
  return str;
}

int 
UCS2Decoder::decodeToChar(IN(acdk::io::RReader) in)
{
  char buf[2];
  int c = in->read();
  if (c == -1)
    return -1;
  buf[0] = c;
  c = in->read();
  if (c == -1)
    return -1;
  buf[1] = c;
  // ### TODO endian
  _bytesReaded += 2;
  return *(ucchar*)buf; // ### TODO check for unmappable
}

RString 
UCS2Decoder::decodeToString(IN(acdk::io::RReader) in, int stopOn)
{
  StringBuffer sb;
  int c;
  while (stopOn != 0 && (c = decodeToChar(in)) != -1)
  {
    if (stopOn == -2 && c == 0)
      break;
    sb.append((ucchar)c);
    if (stopOn > 0)
      --stopOn;
  }
  return sb.toString();
}

RString 
UCS2Decoder::decode(IN(RString) str)
{
  return str;
}


UCS2Encoding::UCS2Encoding(IN(RString) name, UCSEndianess endian)
: Encoding(name)
, _endian(endian)
{
}

//static 
REncoding 
UCS2Encoding::getUCS2NativeEncoding()
{
  static RUCS2Encoding _encoding;
  if (_encoding == Nil)
  {
    _encoding = new UCS2Encoding("UCS2", NativeEndian);
    System::registerStaticReference(_encoding);
  }
  return &_encoding;
}

//static 
REncoding 
UCS2Encoding::getUCS2Encoding(IN(RString) str) 
{ 
  if (str->equals("UCS2-LE") == true)
    return getUCS2LeEncoding();
  if (str->equals("UCS2-BE") == true)
    return getUCS2BeEncoding();
  return getUCS2NativeEncoding(); 
}

//static 
REncoding 
UCS2Encoding::getUCS2LeEncoding()
{
  static RUCS2Encoding _encoding;
  if (_encoding == Nil)
  {
    _encoding = new UCS2Encoding("UCS2-LE", LittleEndian);
    System::registerStaticReference(_encoding);
  }
  return &_encoding;
}

//static 
REncoding 
UCS2Encoding::getUCS2BeEncoding()
{
  static RUCS2Encoding _encoding;
  if (_encoding == Nil)
  {
    _encoding = new UCS2Encoding("UCS2-BE", BigEndian);
    System::registerStaticReference(_encoding);
  }
  return &_encoding;
}

REncoder 
UCS2Encoding::getEncoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new UCS2Encoder(this, _endian, onMalformed, onUnmappable);
}

RDecoder 
UCS2Encoding::getDecoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new UCS2Decoder(this, _endian, onMalformed, onUnmappable);
}
namespace {

struct RegisterEncs
{
  RegisterEncs()
  {
    Encoding::registerEncoding("UCS2", &UCS2Encoding::getUCS2Encoding);
    Encoding::registerEncoding("UCS2-LE", &UCS2Encoding::getUCS2Encoding);
    Encoding::registerEncoding("UCS2-BE", &UCS2Encoding::getUCS2Encoding);
  }
};
RegisterEncs _registerEncs;

} // anon namespace


} // locale
} // acdk


