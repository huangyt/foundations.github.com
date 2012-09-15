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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/ByteAsciiEncoding.cpp,v 1.5 2005/02/05 10:45:01 kommer Exp $


#include "ByteAsciiEncoding.h"
#include <acdk/io/MemWriter.h>

namespace acdk {
namespace locale {

void 
ByteAsciiEncoder::encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn)
{
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
ByteAsciiEncoder::encode(IN(acdk::io::RWriter) out, ucchar ch)
{
  if (ch < 0xFF)
  {
    out->write((byte)ch);
    ++_bytesWritten;
  }
  else
    handleUnmappable(out, ch);
}

RString 
ByteAsciiEncoder::encode(IN(RString) str)
{
  // ### TODO implement
  return str;
}

int 
ByteAsciiDecoder::decodeToChar(IN(acdk::io::RReader) in)
{
  int c = in->read();
  if (c == -1)
    return -1;
  ++_bytesReaded;
  return c;
/*
  if (c < 0xFF)
    return c;
  RString s = handleUnmappable(c);
  if (s->length() > 0)
    return s->charAt(0);
  return 0;
*/
}

RString 
ByteAsciiDecoder::decodeToString(IN(acdk::io::RReader) in, int stopOn)
{
  if (stopOn == -1)
  {
    acdk::io::MemWriter mout;
    in->trans(&mout);
    RbyteArray buffer = mout.getBuffer();
    _bytesReaded += buffer->length();
    byteArray::iterator it = buffer->begin();
    byteArray::iterator end = buffer->end();
    StringBuffer sb;
    for (; it != end; ++it)
    {
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
    /*if (i > 0x80)
      sb.append(handleUnmappable((uc2char)i));
      else*/
    sb.append((char)i);
    --stopOn;
  }
  return sb.toString();
}

RString 
ByteAsciiDecoder::decode(IN(RString) str)
{
  if (str->characterEncoding() == CCAscii)
    return str;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  StringBuffer sb;
  for (; it != end; ++it)
  {
    if (*it > 0xFF)
      sb.append(handleUnmappable((uc2char)*it));
    else
      sb.append((char)*it);
  }
  return sb.toString();
}


ByteAsciiEncoding::ByteAsciiEncoding()
: Encoding("ByteAscii")
{
}

//static 
REncoding 
ByteAsciiEncoding::getByteAsciiEncoding()
{
  static RByteAsciiEncoding _encoding;
  if (_encoding == Nil)
    _encoding = new ByteAsciiEncoding();
  return &_encoding;
}

REncoder 
ByteAsciiEncoding::getEncoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new ByteAsciiEncoder(this, onMalformed, onUnmappable);
}

RDecoder 
ByteAsciiEncoding::getDecoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new ByteAsciiDecoder(this, onMalformed, onUnmappable);
}

namespace {

struct RegisterEncs
{
  RegisterEncs()
  {
    Encoding::registerEncoding("ByteAscii", &ByteAsciiEncoding::getByteAsciiEncoding);
  }
};
RegisterEncs _registerEncs;

} // anon namespace


} // locale
} // acdk

