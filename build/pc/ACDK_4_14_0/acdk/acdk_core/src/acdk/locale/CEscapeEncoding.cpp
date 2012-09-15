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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/CEscapeEncoding.cpp,v 1.6 2005/02/05 10:45:01 kommer Exp $


#include "CEscapeEncoding.h"
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>

namespace acdk {
namespace locale {



int
CEscapeEncoder::encodeChar(ucchar c, byte* ts)
{
  int count = 0;
    switch(c)
    {
    case '\t':
      *ts++ = '\\'; *ts = 't';
      count += 2;
      break;
    case '\n':
      *ts++ = '\\'; *ts = 'n';
      count += 2;
      break;
    case '\r':
      *ts++ = '\\'; *ts = 'r';
      count += 2;
      break;
    case '\b':
      *ts++ = '\\'; *ts = 'b';
      count += 2;
      break;
    case '\v':
      *ts++ = '\\'; *ts = 'v';
      count += 2;
      break;
    case '\a':
      *ts++ = '\\'; *ts = 'a';
      count += 2;
      break;
    case '\f':
      *ts++ = '\\'; *ts = 'f';
      count += 2;
      break;
    case '\0':
      *ts++ = '\\'; *ts = '0';
      count += 2;
      break;
    case '"':
      *ts++ = '\\'; *ts = '"';
      count += 2;
      break;
    case '\\':
      *ts++ = '\\'; *ts = '\\';
      count += 2;
      break;
    default:
      *ts = c;
      ++count;
      break;
    }
  return count;
}

int _decode(IN(acdk::io::RReader) in, int& _bytesReaded)
{
  int c = in->read();
  if (c == -1)
    return -1;
  ++_bytesReaded;
  if (c == '\\')
  {
    c = in->read();
    if (c == -1)
      return -1;
    ++_bytesReaded;
    switch(c)
    {
    case 't':
      return '\t';
    case '\n':
      return '\n';
    case '\r':
      return '\r';
    case '\b':
      return '\b';
    case '\v':
      return '\v';
    case '\a':
      return '\a';
    case '\f':
      return '\f';
    case '\0':
      return '\0';
    case '"':
      return '"';
    case '\\':
      return '\\';
    default:
      return c;
    }
  }
  else
  {
    return c;
  }
  return c;
}



void
CEscapeEncoder::encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end && stopOn != 0; ++it)
  {
    if (stopOn == -2 && *it == 0)
      break;
    encode(out, *it);
    if (stopOn > 0)
      --stopOn;
  }
}

void
CEscapeEncoder::encode(IN(acdk::io::RWriter) out, ucchar ch)
{
  byte buffer[3];
  int len = encodeChar(ch, buffer);
  out->write(buffer, 0, len);
  _bytesWritten += len;
}

RString
CEscapeEncoder::encode(IN(RString) str)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  StringBuffer sb(str->length());
  byte buf[3]; buf[2] = 0;
  for (; it != end; ++it)
  {
    int count = CEscapeEncoder::encodeChar(*it, buf);
    sb.append((char)buf[0]);
    if (count == 2)
      sb.append((char)buf[1]);
  }
  return sb.toString();
}


int
CEscapeDecoder::decodeToChar(IN(acdk::io::RReader) in)
{
  return _decode(in, _bytesReaded);

}

RString
CEscapeDecoder::decodeToString(IN(acdk::io::RReader) in, int stopOn)
{
  StringBuffer sb;
  int c;
  while (stopOn != 0 && (c = _decode(in, _bytesReaded)) != -1)
  {
    if (stopOn == -2 && c == 0)
      break;
    if (c < 0x80)
      sb.append((char)c);
    else
      sb.append((ucchar)c);
    if (stopOn > 0)
      --stopOn;
  }
  return sb.toString();
}

RString
CEscapeDecoder::decode(IN(RString) str)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  StringBuffer sb(str->length());
  for (; it != end; ++it)
  {
    if (*it == '\\')
    {
      ++it;
      sb.append((char)decodeEscapeByte((byte)*it));
    }
    else
      sb.append(*it);
  }
  return sb.toString();
}

int
CEscapeDecoder::decodeEscapeByte(byte b)
{
  switch(b)
  {
    case 't': return '\t';
    case 'n': return '\n';
    case 'r': return '\r';
    case 'b': return '\b';
    case 'v': return '\v';
    case 'a': return '\a';
    case 'f': return '\f';
    case '0': return '\0';
    case '"': return '\"';
    case '\\': return '\\';
    default: return b;
  }
}


CEscapeEncoding::CEscapeEncoding()
: Encoding("C-Literal")
{
}

int
CEscapeEncoding::getMaxByteCount(int charcount)
{
  return charcount * 2;
}

int
CEscapeEncoding::getMaxCharCount(int bytecount)
{
  return bytecount;
}


int
CEscapeEncoding::encode(const ucchar* begin, const ucchar* end,
                     byte* ts, byte* te, bool flush) // ## unneeded
{
  return -1;
  // return CEscapeEncoding_encode(begin, end, ts, te, flush);
}

template <class Iterator>
int
CEscapeEncoding_getByteCount(Iterator sb, Iterator se, bool flush)
{
  Iterator it = sb;
  int count = 0;
  for (; it < se; ++it)
  {
    switch(*it)
    {
    case '\t':
    case '\n':
    case '\r':
    case '\b':
    case '\v':
    case '\a':
    case '\f':
    case '\0':
    case '"':
    case '\\':
      count += 2;
      break;
    default:
      ++count;
      break;
    }
  }
  return count;
}
/*
int
CEscapeEncoding::getByteCount(String::utfiterator sb, String::utfiterator se, bool flush)
{
  return CEscapeEncoding_getByteCount(sb, se, flush);
}
*/

int
CEscapeEncoding::getByteCount(const ucchar* sb, const ucchar* se, bool flush)
{
  return CEscapeEncoding_getByteCount(sb, se, flush);
}



int
CEscapeEncoding::decode(const byte* sb, const byte* se, ucchar* tb, ucchar* te)
{
  const byte* it = sb;
  int count = 0;
  for (; it < se; ++it, ++tb)
  {
    if (*it == '\\')
    {
      ++it;
      *tb = CEscapeDecoder::decodeEscapeByte(*it);
    }
    else
    {
      *tb = *it;
    }
    ++count;
  }
  return count;
}

int
CEscapeEncoding::getCharCount(const byte* sb, const byte* se)
{
  const byte* it = sb;
  int count = 0;
  for (; it < se; ++it)
  {
    if (*it != '\\')
    {
      ++count;
    }
  }
  return count;
}


CEscapeEncoding CEscapeEncoding::_encoder;


namespace {

struct RegisterEncs
{
  RegisterEncs()
  {
    Encoding::registerEncoding("C-Escape", CEscapeEncoding::getCEscapeEncoding);
  }
};
RegisterEncs _registerEncs;

} // anon namespace



} // locale
} // acdk



