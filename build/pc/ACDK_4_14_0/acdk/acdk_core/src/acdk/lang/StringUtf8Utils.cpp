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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringUtf8Utils.cpp,v 1.12 2005/03/15 15:06:58 kommer Exp $
#include <acdk.h>
#include "StringUtf8Utils.h"
#include "Number.h"
#include <acdk/locale/UnmappableCharacterException.h>
#include <acdk/locale/CEscapeEncoding.h>

#if defined(ACDK_OS_DARWIN)
size_t wcslen(const wchar_t* ptr);
#else
#include <wchar.h>
#endif

namespace acdk {
namespace lang {



void
StringUtf8Utils::incUtfPtr(const char*& ptr, const char* endptr)
{
  if (endptr != 0)
	  for (++ptr; ptr < endptr && (*ptr & 0xc0) == 0x80; ++ptr)
	    ;
  else
	  for (++ptr; (*ptr & 0xc0) == 0x80; ++ptr)
	    ;
}

void
StringUtf8Utils::decUtfPtr(const char*& ptr, const char* beginptr)
{
  while (ptr > beginptr)
  {
    ptr--;
    if ((*ptr & 0xc0) != 0x80)
	    return;
  }
}

size_t
StringUtf8Utils::utfDiff(const char* end, const char* begin)
{
  size_t erg;
  for (erg = 0; begin < end; incUtfPtr(begin, end), ++erg)
    ;
  return erg;
}

bool
StringUtf8Utils::isAscii(const char* begin, const char* end)
{
  for (; begin < end; ++begin)
    if ((unsigned char)*begin > 127)
      return false;
  return true;
}

ucchar
StringUtf8Utils::fetchWideChar(const char*& it, const char* end)
{
  if (it >= end)
    return 0;
  int char_byte_length;
  int byte_mask = 0;
  unsigned short c = *it;
  if (c < 128) { char_byte_length = 1; byte_mask = 0x7f; }
  else if ((c & 0xe0) == 0xc0) { char_byte_length = 2; byte_mask = 0x1f; }
  else if ((c & 0xf0) == 0xe0) { char_byte_length = 3; byte_mask = 0x0f; }
  else if ((c & 0xf8) == 0xf0) { char_byte_length = 4; byte_mask = 0x07; }
  else if ((c & 0xfc) == 0xf8) { char_byte_length = 5; byte_mask = 0x03; }
  else if ((c & 0xfe) == 0xfc) { char_byte_length = 6; byte_mask = 0x01; }
  else
  {
    it += 1;
    return *(it - 1);
  }

  ucchar ret = it[0] & byte_mask;
  int i;
  for (i = 1; i < char_byte_length; ++i)
  {
    if ((it[i] & 0xc0) != 0x80)
    {
	    ret = 0xFFFF;
	    break;
	  }
    ret <<= 6;
    ret |= (it[i] & 0x3f);
  }
  it += char_byte_length;
  //  ret = Number::fromLittleEndian(ret);
  return ret;
}

int
StringUtf8Utils::getByteLength(const char* it)
{

  unsigned char c = *((unsigned char*)it);
  if (c < 128) return 1;
  if ((c & 0xe0) == 0xc0) return 2;
  if ((c & 0xf0) == 0xf0) return 3;
  if ((c & 0xf8) == 0xf0) return 4;
  if ((c & 0xfc) == 0xf8)	return 5;
  if ((c & 0xfe) == 0xfc) return 6;
  return 1; // error
}

int
StringUtf8Utils::utflength(uc2char ucc)
{
  int byteSpaceNeeded;
  if (ucc < 0x80) byteSpaceNeeded = 1;
  else if (ucc < 0x800)    byteSpaceNeeded = 2;
  else
      byteSpaceNeeded = 3;
/*
      if (ucc < 0x10000)  byteSpaceNeeded = 3; // ### TODO always true
	else if (ucc < 0x200000) byteSpaceNeeded = 4;
	else if (ucc < 0x400000) byteSpaceNeeded = 5;
	else byteSpaceNeeded = 6;
*/
  return byteSpaceNeeded;
}

int
StringUtf8Utils::utflength(const ucchar* begin, const ucchar* end)
{
  int erg = 0;
  for (const ucchar* it = begin; it < end; ++it)
  {
    erg += utflength(*it);
  }
  return erg;
}

int
StringUtf8Utils::uclength(const ucchar* ch)
{
  const ucchar* begin = ch;
  while (*ch++ != 0)
    ;
  return ch - begin - 1;
}

int
StringUtf8Utils::writeUcToUtf8(byte*& it, byte* end, ucchar ucc)
{
  static const int firstByteMark[7] = {0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC};
	static const int byteMask = 0x3F;
	static const int byteMark = 0x80;
  if (ucc < 0x80)
  {
    if (it >= end)
      return 1;
    *it++ = (unsigned char)ucc;
    return 0;
  }
  int byteSpaceNeeded;
  if (ucc < 0x80) byteSpaceNeeded = 1;
  else if (ucc < 0x800)    byteSpaceNeeded = 2;
  else
      byteSpaceNeeded = 3;
      /*if (ucc < 0x10000)  byteSpaceNeeded = 3; // ### TODO always true
	else if (ucc < 0x200000) byteSpaceNeeded = 4;
	else if (ucc < 0x400000) byteSpaceNeeded = 5;
	else byteSpaceNeeded = 6;
      */
  if (it + byteSpaceNeeded >= end)
    return byteSpaceNeeded;
  it += byteSpaceNeeded;
	byte* tptr = it;
	switch (byteSpaceNeeded)
	{
	case 6:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 5:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 4:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 3:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 2:	*--tptr = (ucc & byteMask) | byteMark; ucc >>= 6;
	case 1:	*--tptr =  ucc | firstByteMark[byteSpaceNeeded];
	}
	return 0;
}

int
StringUtf8Utils::writeUcToUtf8(byte*& it, byte* end,
                   const ucchar* ucbegin, const ucchar* ucend
                   )
{
  int erg = 0;
  for (const ucchar* uit = ucbegin; uit < ucend; ++uit)
  {
    int terg = writeUcToUtf8(it, end, *uit);
    if (terg == -1)
      return -1;
    if (terg > 0)
      erg += terg;
  }
  return erg;
}

ucchar*
StringUtf8Utils::uc4touc(ucchar* buffer, const uc4char* source, int length)
{
  if (length == -1)
    length = uc4length(source);
  ucchar* it = buffer;
  for (const uc4char* end = source + length; source < end; ++source, ++it)
  {
    *it = (ucchar)*source; // ## TODO check for overflow
  }
  *it = 0;
  return buffer;
}

ucchar*
StringUtf8Utils::wcchartouc(ucchar* buffer, const wchar_t* source, int length)
{
  if (length == -1)
    length = wcslen(source);
  ucchar* it = buffer;
  for (const wchar_t* end = source + length; source < end; ++source, ++it)
  {
    *it = (ucchar)*source; // ## TODO check for overflow
  }
  *it = 0;
  return buffer;
}


StringUtf8Utils::UtfStreamType
StringUtf8Utils::validUtf8Stream(const byte* begin, const byte* end, bool throwOnFail)
{
  UtfStreamType type = StreamIsAscii;
  /* ### FIXME
  for (const byte* it = begin; it < end; )
  {
    ucchar c = fetchWideChar(it, end);
    if (c == -1)
    {
      if (throwOnFail == true)
      {
        RString str = new String(begin, end);
        THROW1(Exception, "Invalid UTF8 stream: [" + str + "]"); //## TODO better exception
      }
      return StreamIsError;
    }
    if (c >= 0x80)
      type = StreamIsUtf8;
	}*/
	return type;
}

int
StringUtf8Utils::uc4length(const uc4char* ptr)
{
  const uc4char* begin = ptr;
  while (*ptr++ != 0)
    ;
  return ptr - begin - 1;
}

template <typename IT>
RString _decodeAscUnicodeT(IT begin, IT end)
{

  StringBuffer sb;
  IT ptr = begin;
  const char* XS = "0123456789abcdef";
  for (; ptr != end; ++ptr)
  {
    if (*ptr == '\\' && *(ptr + 1) == 'u')
    {
      ptr += 2;
      if ((end - ptr) < 4)
        THROW0_FQ(acdk::locale::, UnmappableCharacterException);
      uc2char erg = 0;
      for (int i = 0; i < 4; ++i)
      {
        const char* s = strchr(XS, ::tolower(*(ptr + i)));
        if (s == 0)
          THROW0_FQ(acdk::locale::, UnmappableCharacterException);
        erg += (s - XS) << (4 * (4 - i - 1));
      }
      sb.append(erg);
      ptr += 3;
    }
    else
    {
      sb.append(*ptr);
    }
  }
  return sb.toString();
}



RString 
String::decodeAscUnicode()
{
  if (indexOf("\\u") == -1)
    return this;
  return _decodeAscUnicodeT(begin(), end());
}

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


RString 
String::encodeAscUnicode()
{
  if (characterClass() == CCAscii)
    return this;
  StringBuffer sb;
  String::iterator it = begin();
  String::iterator itend = end();
  for (; it != itend; ++it)
  {
    uc2char ch = *it;
    if (ch < 0x80)
    {
      byte buffer[2];
      int num = acdk::locale::CEscapeEncoder::encodeChar(ch, buffer);
      sb.append((char)buffer[0]);
      if (num == 2)
        sb.append((char)buffer[1]);
    }
    else
    {
      byte buffer[5];
      buffer[4] = 0;
      _encode(ch, buffer);
      sb.append("\\u");
      sb.append((const char*)buffer);
    }
  }
  return sb.toString();
}

//static
RString
String::_decodeAscUnicode(const char* source, const char* end)
{
  return _decodeAscUnicodeT(source, end);
  /*
  // Use StringBuffer to avoid unneeded CharacterClass
  using ::acdk::lang::sys::ObjectHeap;
  int buflen = (end - source) * sizeof(uc2char);
  uc2char* buffer = (uc2char*)ObjectHeap::allocator()->allocate(buflen, ::acdk::lang::sys::DumpBufferMem);
  uc2char* target = buffer;
  const char* XS = "0123456789abcdef";
  for (const char* ptr = source; ptr < end; ++ptr)
  {
    if (*ptr == '\\' && *(ptr + 1) == 'u')
    {
      ptr += 2;
      if ((end - ptr) < 4)
        THROW0_FQ(acdk::locale::, UnmappableCharacterException);
      uc2char erg = 0;
      for (int i = 0; i < 4; ++i)
      {
        const char* s = strchr(XS, ::tolower(*(ptr + i)));
        if (s == 0)
          THROW0_FQ(acdk::locale::, UnmappableCharacterException);
        erg += (s - XS) << (4 * (4 - i - 1));
      }
      *target = erg;
      ++target;
      ptr += 3;
    }
    else
    {
      *target = *ptr;
      ++target;
    }
  }
  *target = 0;
  return new String((byte*)buffer, buflen, (byte*)buffer, (byte*)target, CCUcs2 | NormalSST);
  */
}


//static
uc2char
String::_decodeAscUnicodeCharacter(const char* source)
{
  if (*source != '\\' || *(source + 1) != 'u')
    return *source;
  source += 2;
  const char* XS = "0123456789abcdef";
  uc2char erg = 0;
  for (int i = 0; i < 4; ++i)
  {
    const char* s = strchr(XS, ::tolower(*(source + i)));
    if (s == 0)
      THROW0_FQ(acdk::locale::, UnmappableCharacterException);
    erg += (s - XS) << (4 * (4 - i - 1));
  }

  return erg;
}

} // namespace lang
} // namespace acdk




