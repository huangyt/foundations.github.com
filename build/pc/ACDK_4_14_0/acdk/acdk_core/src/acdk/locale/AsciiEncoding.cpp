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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/AsciiEncoding.cpp,v 1.7 2005/03/21 13:40:03 kommer Exp $


#include "AsciiEncoding.h"
#include <acdk/io/MemWriter.h>
#include <acdk/io/BytePtrReader.h>

namespace acdk {
namespace locale {

void 
AsciiEncoder::encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn)
{
  if (stopOn <= 0 && str->characterClass() == CCAscii)
  {
     out->write(str->byte_begin(), 0, str->length());
     _bytesWritten += str->length();
     return;
  }
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it < end; ++it)
  {
    if (stopOn == -2 && *it == 0)
      return;
    else if (stopOn > 0)
      --stopOn;
    else if (stopOn == 0)
      return;
    else
      encode(out, *it);
  }
}

void 
AsciiEncoder::encode(IN(acdk::io::RWriter) out, ucchar ch)
{

  if (ch < 0x80)
  {
    out->write((byte)ch);
    ++_bytesWritten;
  }
  else
    handleUnmappable(out, ch);
}

RString 
AsciiEncoder::encode(IN(RString) str)
{
  // ### TODO implement
  return str;
}

int 
AsciiDecoder::decodeToChar(IN(acdk::io::RReader) in)
{
  int c = in->read();
  ++_bytesReaded;
  if (c == -1)
    return -1;
  if (c < 0x80)
    return c;
  RString s = handleUnmappable(c);
  if (s->length() > 0)
    return s->charAt(0);
  return 0;
}

RString 
AsciiDecoder::decodeToString(IN(acdk::io::RReader) in, int stopOn)
{
  if (stopOn == -1)
  {
    acdk::io::MemWriter mout;
    in->trans(&mout);
    RbyteArray buffer = mout.getBuffer();
    _bytesReaded = buffer->length();
    byteArray::iterator it = buffer->begin();
    byteArray::iterator end = buffer->end();
    StringBuffer sb;
    for (; it != end; ++it)
    {
      if (*it > 0x80)
        sb.append(handleUnmappable((uc2char)*it));
      else
        sb.append((char)*it);
    }
    return sb.toString();
  }
  int i = 0;
  StringBuffer sb;
  while (stopOn != 0 && (i = in->read()) != -1)
  {
    ++_bytesReaded;
    if (stopOn == -2 && i == 0)
      return sb.toString();
    if (i > 0x80)
      sb.append(handleUnmappable((uc2char)i));
    else
      sb.append((char)i);
    --stopOn;
  }
  return sb.toString();
}

RString 
AsciiDecoder::decode(IN(RString) str)
{
  if (str->characterEncoding() == CCAscii)
    return str;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  StringBuffer sb;
  for (; it != end; ++it)
  {
    if (*it >= 0x80)
      sb.append(handleUnmappable((uc2char)*it));
    else
      sb.append((char)*it);
  }
  return sb.toString();
}


AsciiEncoding::AsciiEncoding()
: Encoding("US-ASCII")
{
}

//static 
REncoding 
AsciiEncoding::getAsciiEncoding()
{
  static RAsciiEncoding _encoding;
  if (_encoding == Nil)
    _encoding = new AsciiEncoding();
  return &_encoding;
}

REncoder 
AsciiEncoding::getEncoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new AsciiEncoder(this, onMalformed, onUnmappable);
}

RDecoder 
AsciiEncoding::getDecoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new AsciiDecoder(this, onMalformed, onUnmappable);
}


//static
RString
AsciiEncoding::decodeToString(const byte* buffer, int len)
{
  if (len == -1)
      len = strlen((const char*)buffer);
  acdk::io::BytePtrReader in(buffer, len);
  return AsciiEncoding::getAsciiEncoding()->getDecoder()->decodeToString(&in);
}

namespace {

struct RegisterEncs
{
  RegisterEncs()
  {
    Encoding::registerEncoding("US-ASCII", &AsciiEncoding::getAsciiEncoding);
    Encoding::registerEncoding("ASCII", &AsciiEncoding::getAsciiEncoding);
  }
};
RegisterEncs _registerEncs;

} // anon namespace


} // locale
} // acdk

