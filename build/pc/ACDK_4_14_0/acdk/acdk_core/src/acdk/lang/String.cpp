/// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*-
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

// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/String.cpp,v 1.82 2005/05/02 23:06:38 kommer Exp $



#include <acdk.h>


#include "Math.h"
#include "Integer.h"
#include "Character.h"
#include "UnicodeCharacter.h"
#include "Long.h"
#include "Float.h"
#include "Double.h"

#include "IllegalArgumentException.h"
#include "StringIndexOutOfBoundsException.h"
#include "NullPointerException.h"
#include "RuntimeException.h"
#include "System.h"
#include "sys/Allocator.h"
#include <acdk/locale/Encoding.h>
#include <acdk/io/MemReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/BytePtrReader.h>

#include <acdk/locale/UnmappableCharacterException.h>
#include <acdk/util/WeakHashMap.h>
#include <acdk/lang/ref/WeakReference.h>
#include <acdk/util/StringTokenizer.h>

#include <stdio.h>
#include <acdk/lang/sys/core_alloca.h>
#if !defined(ACDK_OS_DARWIN)
#include <wchar.h>
#endif

#include "StringInternals.h"
#if defined(ACDK_OS_DARWIN)
size_t wcslen(const wchar_t* ptr)
{
  int i;
  for (i = 0; *ptr != 0; ++i, ++ptr)
    ;
  return i;
}
#endif

namespace acdk {
namespace lang{

int uc2strlen(const uc2char* ptr) { return StringUtf8Utils::uc2length(ptr); } // ## somewhere prominent

/**
  refer to:
  F:\programr\lang\c++\lib\unicode\icu-2.2\icu\source\common\unicode\ustring.h
  F:\programr\lang\c++\lib\unicode\libiconv-1.5.1\lib\java.h for java literal encodings

*/

void
String::_throwIncompatibleString(const String& s)
{
  THROW1_FQ(acdk::locale::, UnmappableCharacterException, RString("Incompatible String encoding: ") + s.stringFlags());
}


/*

String::utfiterator
find(String::utfiterator sit, String::utfiterator send, ucchar ch)
{
  while (sit < send)
  {
    if (*sit == ch)
      return sit;
    ++sit;
  }
  return send;
}
String::utfiterator
findr(String::utfiterator sit, String::utfiterator send, ucchar ch)
{
  String::utfiterator realend = send;

  while (sit < send) {
    --send;
    if (*send == ch)
      return send;
  }
  return realend;
}


ACDK_CORE_PUBLIC
String::utfiterator
find(String::utfiterator bit, String::utfiterator eit,
     String::utfiterator bs, String::utfiterator se)
{
  String::utfiterator i = bs;
  String::utfiterator startf = eit;
  while (bit < eit && i < se)
  {
    if (*i != *bit)
    {
      i = bs;
      if (startf != eit)
      {
        bit = startf;
        startf = eit;
      }
    } else {
      if (startf == eit)
        startf = bit;
      ++i;
    }
    ++bit;
  }
  if (i != se)
    return eit;
  return bit - (se - bs);
}

ACDK_CORE_PUBLIC
String::utfiterator
findr(String::utfiterator bit, String::utfiterator eit,
      String::utfiterator bs, String::utfiterator se)
{
  String::utfiterator realend = eit;

  String::utfiterator findit = realend;
  String::utfiterator spe = se;

  while (spe > bs && eit > bit)
  {
    --spe;
    --eit;

    if (*spe != *eit)
    {
      spe = se;
      if (findit != realend)
      {
        eit = findit;
        findit = realend;
      }

    } else {
      if (findit == realend)
        findit = eit;
    }
  }
  if (spe != bs)
    return realend;
  return eit;
}
*/


/*
template <typename Iterator, typename CharType>
Iterator
find(Iterator sit, Iterator send, CharType ch)
{
  while (sit < send) {
    if (*sit == ch)
      return sit;
    ++sit;
  }
  return send;
}

template <typename Iterator, typename CharType>
Iterator
findr(Iterator sit, Iterator send, CharType ch)
{
  Iterator realend = send;

  while (sit < send) {
    --send;
    if (*send == ch)
      return send;
  }
  return realend;
}




template <typename Iterator>
Iterator
find(Iterator bit, Iterator eit,
     Iterator bs, Iterator se)
{
  Iterator i = bs;
  Iterator startf = eit;
  while (bit < eit && i < se) {
    if (*i != *bit)
    {
      i = bs;
      if (startf != eit)
      {
        bit = startf;
        startf = eit;
      }
    } else {
      if (startf == eit)
        startf = bit;
      ++i;
    }
    ++bit;
  }
  if (i != se)
    return eit;
  return bit - (se - bs);
}

template <typename  Iterator>
Iterator
findr(Iterator bit, Iterator eit,
      Iterator bs, Iterator se)
{
  Iterator realend = eit;
  Iterator findit = realend;
  Iterator spe = se;

  while (spe > bs && eit > bit)
  {
    --spe;
    --eit;

    if (*spe != *eit)
    {
      spe = se;
      if (findit != realend)
      {
        eit = findit;
        findit = realend;
      }

    } else {
      if (findit == realend)
        findit = eit;
    }
  }
  if (spe != bs)
    return realend;
  return eit;
}

template <typename  Iterator>
Iterator
startsWithT(Iterator bit, Iterator eit,
      Iterator bs, Iterator se)
{
  Iterator realend = eit;
  Iterator findit = realend;
  Iterator spe = se;

  while (spe > bs && eit > bit)
  {
    --spe;
    --eit;

    if (*spe != *eit)
    {
      spe = se;
      if (findit != realend)
      {
        eit = findit;
        findit = realend;
      }

    } else {
      if (findit == realend)
        findit = eit;
    }
  }
  if (spe != bs)
    return realend;
  return eit;
}
*/

static char* empty_string = "";
String::String()
: Object()
, _stringFlags(NormalSST | CCAscii)
, _buffer(0)
, _bufferLength(0)
, _begin((byte*)empty_string)
, _end((byte*)empty_string)
, _hashCode(-1)
{
}

String::String(int buffersize)
: Object()
, _stringFlags(NormalSST | CCAscii)
, _buffer(0)
, _bufferLength(buffersize)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _begin = _end = _buffer = (byte*)allocate(_bufferLength, acdk::lang::sys::DumpBufferMem);
}

String::String(const char* ptr, int len, int flags)
: Object()
, _stringFlags(flags)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _init(ptr, len, flags);
}



String::String(const uc2char* ptr, int flags)
: Object()
, _stringFlags(flags)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _init(ptr, -1, flags);
}

String::String(const uc4char* ptr)
: Object()
, _stringFlags(NormalSST | CCUcs2)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  if (ptr != 0)
  {
    const uc2char* cptr =  ((const uc2char*)::acdk::lang::String(ptr).uc2c_str());
    int length = StringUtf8Utils::uclength(cptr);
    _init(cptr, length, NormalSST | CCUcs2);
  }
  else
    _init((const uc2char*)"\0\0", 0, ConstSST | CCUcs2);
}

#if ACDK_UCLITERAL_WIDE!=ACDK_UCCHAR_SIZE ||  defined(__BORLANDC__)
String::String(const wchar_t* ptr, int flags/* = ConstSST | CCOsNative*/)
: Object()
, _stringFlags(flags)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
#if ACDK_UCLITERAL_WIDE==ACDK_UCCHAR_SIZE && !defined(__BORLANDC__)
    _init((const uc2char*)ptr), StringUtf8Utils::uclength(ptr))
#else
  if (ptr != 0)
  {
    int charlen = wcslen(ptr);
    uc2char* ucptr = StringUtf8Utils::wcchartouc((uc2char*)core_alloca((charlen + 1) * sizeof(uc2char)), ptr, charlen);
    _init(ucptr, charlen, NormalSST | CCUcs2);
    core_freea(ucptr);
  }
  else
    _init((const uc2char*)"\0", 0, ConstSST | CCUcs2);
#endif
}
String::String(const wchar_t* ptr, int length, int flags)
: Object()
, _stringFlags(flags)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
#if ACDK_UCLITERAL_WIDE==ACDK_UCCHAR_SIZE && !defined(__BORLANDC__)
    _init((const uc2char*)ptr), length, CCUcs2 | NormalSST)
#else
  if (ptr != 0)
  {
    int charlen = length;
    uc2char* ucptr = StringUtf8Utils::wcchartouc((uc2char*)core_alloca((charlen + 1) * sizeof(uc2char)), ptr, charlen);
    _init(ucptr, charlen, CCUcs2 | NormalSST);
    core_freea(ucptr);
  }
  else
  _init((const uc2char*)"\0", 0, CCUcs2 | NormalSST);
#endif
}
#endif //ACDK_UCLITERAL_WIDE!=ACDK_UCCHAR_SIZE

String::String(const ucchar* ptr, int length, int flags)
: Object()
, _stringFlags(flags)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _init(ptr, length, flags);
  //_detectUtf();
}

String::String(const char* ptr, int flags/* = ConstSST*/)
: Object()
, _stringFlags(flags)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _init(ptr, ptr == 0 ? 0 : strlen(ptr), flags);
}

/*
char*
String::_init(const ucchar* ptr, int length, const char* buffer, char*& end, int targetoffset)
{
  int size = targetoffset + (length * 1.3);
  char* nbuffer;
  char* nbufferstart;
  do {
    nbuffer = (char*)allocate(size + 1, acdk::lang::sys::DumpBufferMem);
    if (targetoffset > 0)
      memcpy(nbuffer, buffer, targetoffset);
    nbufferstart = nbuffer + targetoffset;
    int erg = StringUtf8Utils::writeUcToUtf8(nbufferstart, (char*)nbuffer + size + 1, ptr, ptr + length);
    if (erg == 0)
    {
      end = nbufferstart;
      *end = 0;
      return nbuffer;
    }
    deallocate(nbuffer, acdk::lang::sys::DumpBufferMem);
    size = size + erg;
    if (erg < 0)
      return 0;
  } while (true);
  return 0;
}*/

void checkAscii(byte* b, int len)
{
  for (int i = 0; i < len; ++i)
    if (b[i] > 0x80)
      THROW0_FQ(acdk::locale::, UnmappableCharacterException);
}

void String_initUtf8(String& str, const char* ptr, int len, byte*& buffer, int& buf_len, byte*& buf_begin, byte*& buf_end, unsigned short& flags)
{
  if (len == -1)
    len = strlen(ptr);
  buf_len = (len + 1) * sizeof(uc2char);
  buffer = (byte*)str.allocate(buf_len, acdk::lang::sys::DumpBufferMem);
  buf_begin = buf_end = buffer;
  const char* cend = ptr + len;
  flags = CCUcs2 | NormalSST;
  uc2char* target = (uc2char*)buffer;
  int i;
  for (i = 0; *ptr != 0 && ptr < cend; ++i)
  {
    target[i] = StringUtf8Utils::fetchWideChar(ptr, cend);
  }
  target[i] = 0;
  buf_end = ((byte*)&target[i]);
}



void
String::_init(const char* ptr, int length, int flags)
{
  if (length == -1)
    length = strlen(ptr);
  if (length < 0)
    THROW1(IllegalArgumentException, "Constructor of String with with length < 0 && length != -1");
  //char onstack = 0;
  if ((flags & CharacterClassMask) == 0)
  {
    flags |= CCAscii;
  }
  else if (isCharacterClass(flags, CCUtf8))
  {
    if (StringUtf8Utils::isAscii(ptr, ptr + length) == false)
    {
      String_initUtf8(*this, ptr, length, _buffer, _bufferLength, _begin, _end, _stringFlags);
      return;
    }
    flags &= ~CCUtf8;
    flags |= CCAscii;
    _stringFlags &= ~CCUtf8;
    _stringFlags |= CCAscii;
  }
  if (storageType(flags) == ConstSST && (isCharacterClass(flags, CCAscii)))
  {
// borland: because always report pointer arithmetic error
// solaris not on sparc, distance to small
#if !defined(__BORLANDC__) && (!defined(ACDK_OS_SOLARIS) || defined(__sparc__)) 
    if (isStackRef() == false && ptr != 0 && sys::core_system::isPtrInStack((void*)ptr) == true)
    {
      System::out->println("Initializing Const String with a stack pointer");
      System::printStackTrace();
      // force segfault
      char* ptr = 0;
      *ptr = 0;
    }
#endif //__BORLANDC__ //
    _begin = (byte*)ptr;
    _end = _begin + length;
    _stringFlags = flags;
#ifdef ACDK_DEBUG
    checkAscii(_begin, length);
#endif //ACDK_DEBUG
    return;
  }
  if (storageType(flags) != NormalSST && (isCharacterClass(flags, CCAscii)))
  {
    THROW1(IllegalArgumentException, "Constructor of String with wrong CharacterClass args");
  }
  if (ptr == 0)
    return;

  byte* buffer = (byte*)allocate(length + 1, acdk::lang::sys::DumpBufferMem);
  memcpy(buffer, ptr, length + 1);
  _buffer = buffer;
  _bufferLength = length + 1;
  _begin = _buffer;
  _end = _buffer + length;

}

void
String::_init(const uc2char* ptr, int len, int flags) 
{
  if (len == -1)
    len = ptr == 0 ? 0 : StringUtf8Utils::uclength(ptr);
  if (len < 0)
    THROW1(IllegalArgumentException, "Constructor of String with with length < 0 && length != -1");

  if (flags & ConstSST)
  {
    _begin = (byte*)ptr;
    _end = (byte*)(ptr + len);
    _stringFlags = ConstSST | CCUcs2;
    return;
  }
  else
  {
    ::initT(*this, Uc2Iterator(this, ptr), Uc2Iterator(this, ptr + len), _buffer, _end);
    _begin = _buffer;
    _stringFlags = NormalSST | CCUcs2;
  }
  /*
  _bufferLength = (length + 1) * sizeof(uc2char);
  _buffer = (byte*)allocate(_bufferLength, acdk::lang::sys::DumpBufferMem);
  memcpy(_buffer, ptr, length * sizeof(uc2char));
  _begin = _buffer;
  _end = _begin + (length * sizeof(uc2char));
  *((uc2char*)_end) = 0;
  _stringFlags = NormalSST | CCUcs2;
  */
}

String::String(IN(RbyteArray) ba, int offset /* = 0*/, int size /* = -1 */)
: Object()
, _stringFlags(NormalSST)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{

  _throwNotImplementedYet("String::String(IN(RbyteArray) ba, int offset = 0, int size = -1 )");
  /*
  ### TODO
  if (size == -1)
    size = ba->length() - offset;
  char* buffer = (char*)allocate(size + 1, acdk::lang::sys::DumpBufferMem);
  copy((const char*)ba->data() + offset, (const char*)ba->data() + offset + size, buffer);
  buffer[size] = 0;
  _buffer = buffer;
  _bufferLength = size + 1;
  _begin = buffer;
  _end = buffer + size;
  _detectUtf();
  */
}

String::String(IN(RcharArray) ba, int offset /* = 0*/, int size /* = -1 */)
: Object()
, _stringFlags(NormalSST | CCAscii)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  if (size == -1)
    size = ba->length() - offset;
  byte* buffer = (byte*)allocate(size + 1, acdk::lang::sys::DumpBufferMem);
  memcpy(buffer, (byte*)ba->data() + offset, size);
  buffer[size] = 0;
  _buffer = buffer;
  _bufferLength = size + 1;
  _begin = buffer;
  _end = buffer + size;
}

String::String(IN(RuccharArray) ba, int offset, int size)
: Object()
, _stringFlags(NormalSST | CCUcs2)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  if (size == -1)
    size = ba->length() - offset;
  _bufferLength = (Uc2Iterator::getMaxByteCount() * size) + Uc2Iterator::getTerminatorByteSize();
  _begin = _buffer = (byte*)allocate(_bufferLength, acdk::lang::sys::DumpBufferMem);
  memcpy(_begin, ba->data() + offset, size * Uc2Iterator::getMaxByteCount());
  _end = _begin + (Uc2Iterator::getMaxByteCount() * size);
  Uc2Iterator::writeTerminator(_end);

}

String::String(StringBuffer& buf, int start/* = 0*/, int end/* = -1*/)
: Object()
, _stringFlags(NormalSST)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _init(*buf.toString(), start, end);
}

String::String(IN(RStringBuffer) buf, int start/* = 0*/, int end/* = -1*/)
: Object()
, _stringFlags(NormalSST)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _init(*buf->toString(), start, end);
}

String::String(String* parent, byte* bit, byte* eit)
: Object()
, _stringFlags(SubSST | parent->characterEncoding())
, _buffer(reinterpret_cast<byte*>(parent))
, _bufferLength(-1)
, _begin(bit)
, _end(eit)
, _hashCode(-1)
{
  parent->addRef();
}

foreign
String::String(iterator begin, iterator end)
: Object()
, _stringFlags(SubSST | begin.getString()->characterEncoding())
, _buffer(reinterpret_cast<byte*>(begin.getString()))
, _bufferLength(-1)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  byte* tbegin = begin.getString()->byte_begin();
  begin.getString()->addRef();
  DISPATCH14(::getByteIteratorT, *this, tbegin, tbegin, begin.getIndex(), end.getIndex(), _begin, _end)
}

String::String(IN(RbyteArray) ba, IN(acdk::locale::RDecoder) dec, int offset, int size)
: Object()
, _stringFlags(NormalSST)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  acdk::io::MemReader min(ba, offset, size == -1 ? size : offset + size);
  RString t = dec->decodeToString(&min);
  _stringFlags = t->_stringFlags;
  _buffer = t->_buffer;
  _bufferLength = t->_bufferLength;
  _begin = t->_begin;
  _end = t->_end;
  t->_stringFlags = ConstSST;
}


String::String(const byte* cptr, IN(acdk::locale::RDecoder) decoder, int offset, int size) // ### FIXME size must not -1, offset make no sense
: Object()
, _stringFlags(NormalSST)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  acdk::io::BytePtrReader in(cptr, size);
  RString t = decoder->decodeToString(&in);
  _stringFlags = t->_stringFlags;
  _buffer = t->_buffer;
  _bufferLength = t->_bufferLength;
  _begin = t->_begin;
  _end = t->_end;
  t->_stringFlags = ConstSST;
}

// substring constructor
void
String::_init(const String& str, int start/* = 0*/, int length/* = -1*/)
{
  DISPATCH18(::initsubstrT, str, str.byte_begin(), str.byte_end(), str, start, length, _buffer, _bufferLength, _begin, _end, _stringFlags);

}

//virtual
String::~String()
{
  switch (storageType()) {
  case NormalSST:
    if (_buffer)
      deallocate(_buffer, acdk::lang::sys::DumpBufferMem);
    break;
  case SubSST:
    reinterpret_cast<String*>(_buffer)->releaseRef();
    break;
  case ConstSST:
    //nothing
  case HashSST:
    //nothing
  default:
    //nothing
    break;
  }
}


void
String::_calcHashCode()
{
  int ret = 0;
  iterator eit = end();
  for (iterator it = begin(); it != eit; ++it)
    ret = ret * 31 + *it;
  _hashCode  = ret;
}

// static
int 
String::calcHashCode(const char* t)
{
  if (t == 0)
    return 0;
  int ret = 0;
  while (*t != 0)
  {
    ret = ret * 31 + *t;
    ++t;
  }
  return ret;
}

//static 
int 
String::calcHashCode(const char* t, int len)
{
  if (t == 0)
    return 0;
  int ret = 0;
  for (int i = 0; i < len; ++i)
    ret = ret * 31 + t[i];
  return ret;
}

int
String::getCharCapacity() const
{
  int erg;
  DISPATCH11(::capacityT, *this, _begin, _begin + _bufferLength, erg)
  return erg;
}

RString
String::getNormalized() const
{
  if (_stringFlags & SubSST)
  {
    return (RString)const_cast<String*>(this)->clone();
  }
  return this;
}


RObject
String::clone(sys::Allocator* alc)
{
  return new (alc) String(toString(), 0, -1);
}


uc2char
String::charAt(int idx) const
{
  uc2char ret;
  DISPATCH12(::charAtT, *this, _begin, _end, idx, ret);
  return ret;
}


int
String::length()  const
{
  if (isCharacterClass(CCAscii) || isCharacterClass(CCAnsi))
      return _end - _begin;
  if (isCharacterClass(CCUcs2))
      return (_end - _begin) >> 1;
  if (isCharacterClass(CCUtf8))
    return Utf8CharIterator((const char*)_end) - Utf8CharIterator((const char*)_begin);
  THROW1(Exception, "Unknown String character encoding");
  return -1;
}

const char*
String::c_str() const
{
  if (isCharacterClass(CCAscii) || isCharacterClass(CCAnsi) || isCharacterClass(CCUtf8))
  {
    normalize();
    return (const char*)_begin;
  }
  THROW1(Exception, RString("Incompatible String character Type to retrieve c- char* string: ") + characterClass());
  return 0;
}

const uc2char*
String::uc2c_str() const
{
  if (isCharacterClass(CCUcs2))
  {
    normalize();
    return (const uc2char*)_begin;
  }
  THROW1(Exception, RString("Incompatible String character Type to retrieve c- char* string: ") + characterClass());
  return 0;
}

void
String::_normalize()
{
  if ((_stringFlags & SubSST) == 0 || (_stringFlags & NormalSST && _buffer != 0))
    return;
  DISPATCH14(::unshareT, *this, _begin, _end, *this, const_cast<String*>(this)->_begin, _end, _bufferLength);
  reinterpret_cast<String*>(_buffer)->releaseRef();
  _buffer = _begin;
  _stringFlags = NormalSST | (characterClass() | codePage());
}

/// @internal
template <class I>
class StringByteWriter
: extends acdk::io::AbstractWriter
{
  typedef acdk::io::AbstractWriter Super;
public:
  byte* buf_begin;
  byte* buf_end;
  int buf_size;
  String& _string;

  StringByteWriter(String& s, int size)
    : buf_begin(0)
    , buf_end(0)
    , buf_size(0)
    , _string(s)
  {
    overflow(size);
  }
  virtual void write(byte c)
  {
    if ((buf_end - buf_begin) + I::getTerminatorByteSize() >= buf_size)
      overflow();
    *buf_end = c;
    ++buf_end;
  }
  void write(const byte* cstr, int offset, int len)
  {
    Super::write(cstr, offset, len);
  }
  void flush() {}
  void close() {}

  void overflow(int size = -1)
  {
    if (size == -1)
      size = buf_size / 2;
    if (size == 0)
      size = 124;
    size += I::getTerminatorByteSize();
    int curbufused = 0;
    if (buf_begin != 0)
      curbufused = buf_end - buf_begin;
    byte* newbuf = (byte*)_string.allocate(buf_size + size, acdk::lang::sys::DumpBufferMem);
    
    if (buf_begin != 0)
    {
      memcpy(newbuf, buf_begin, curbufused);
      _string.deallocate(buf_begin, acdk::lang::sys::DumpBufferMem);
    }
    buf_begin = newbuf;
    buf_end = buf_begin + curbufused;
    buf_size = buf_size + size - I::getTerminatorByteSize();
  }
};

template <typename I1, typename I2>
void convertT2(I1 begin1, I1 end1, const String& s, I2 dummy, byte*& buf_begin, byte*& buf_end, int& buf_size, int& flags,
               acdk::locale::CodingErrorAction onMalformed, acdk::locale::CodingErrorAction onUnmappable)
{
  typedef I2 IteratorType;
  typedef typename IteratorType::CharType CharType;
  int newcharsize = (end1 - begin1);
  int newbufsize = int(newcharsize * IteratorType::getAverageByteCount() + IteratorType::getTerminatorByteSize());
  StringByteWriter<IteratorType> out(const_cast<String&>(s), newbufsize);

  acdk::locale::REncoder enc = IteratorType::getEncoding()->getEncoder(onMalformed, onUnmappable);
  byteArray repl(1); repl[0] = '?';
  enc->setEncodingReplacement(&repl);
  enc->encode(&out, &s);

  
  byte b[4];
  IteratorType::writeTerminator(b);
  out.write(b, 0, IteratorType::getTerminatorByteSize());
  buf_end = out.buf_end - IteratorType::getTerminatorByteSize();
  buf_begin = out.buf_begin;
  buf_size = out.buf_size;
  flags = CharacterIteratorTraits<IteratorType>::StringFlags;
}


RString
String::_convert(int flags, acdk::locale::CodingErrorAction onMalformed,
                             acdk::locale::CodingErrorAction onUnmappable) const
{
  if (characterClass() & flags &&  codePage() & flags)
    return this;


  byte* buf_begin = 0;
  byte* buf_end;
  int buf_size = 0;
  int nflags = 0;
  if (isCharacterClass(flags, CCAscii))
  {
    if (isCharacterClass(CCUcs2))
    {
      convertT2(Uc2Iterator(*this, _begin), Uc2Iterator(*this, _end), *this, AsciiIterator(*this, (byte*)0), buf_begin, buf_end, buf_size, nflags, onMalformed, onUnmappable);
    }
    else if (isCharacterClass(CCUtf8))
    {
      convertT2(Utf8CharIterator(*this, _begin), Utf8CharIterator(*this, _end), *this, AsciiIterator(*this, (byte*)0), buf_begin, buf_end, buf_size, nflags, onMalformed, onUnmappable);
    }
    else
      STRING_THROW_INCOMPATIBLE_CHARTYPE(*this);
  }
  else if (isCharacterClass(flags, CCUcs2))
  {
    if (isCharacterClass(CCAscii))
    {
      convertT2(AsciiIterator(*this, _begin), AsciiIterator(*this, _end), *this, Uc2Iterator(*this, (byte*)0), buf_begin, buf_end, buf_size, nflags, onMalformed, onUnmappable);
    }
    else if (isCharacterClass(CCUtf8))
    {
      convertT2(Utf8CharIterator(*this, _begin), Utf8CharIterator(*this, _end), *this, Uc2Iterator(*this, (byte*)0), buf_begin, buf_end, buf_size, nflags, onMalformed, onUnmappable);
    }
    else
      STRING_THROW_INCOMPATIBLE_CHARTYPE(*this);
  }
  else if (isCharacterClass(flags, CCUtf8))
  {
    if (isCharacterClass(CCAscii) || isCharacterClass(CCUtf8))
      ; // noting
    else if (isCharacterClass(CCUcs2))
    {
      convertT2(Uc2Iterator(*this, _begin), Uc2Iterator(*this, _end), *this, Utf8CharIterator(*this, (byte*)0), buf_begin, buf_end, buf_size, nflags, onMalformed, onUnmappable);
    }
    else
      STRING_THROW_INCOMPATIBLE_CHARTYPE(*this);
  }
  else
    STRING_THROW_INCOMPATIBLE_CHARTYPE(*this);
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_size, buf_begin, buf_end, NormalSST | nflags);
}

RString
String::_narrow() const
{
  if (isCharacterClass(CCAscii))
    return this;
  if (isCharacterClass(CCUcs2))
  {
    uc2char* it = (uc2char*)_begin;
    uc2char* end = (uc2char*)_end;
    for (; it < end; ++it)
    {
      if (*it > 0x80)
        return this;
    }
    return _convert(CCAscii, ::acdk::locale::ReportCodingError, ::acdk::locale::ReportCodingError);
  }
  return this;
}

RString
String::concat(const String& str) const
{
  byte* buf_begin;
  byte* buf_end;
  int flags;
  DISPATCH24(::concatT, *this, _begin, _end, str, str.byte_begin(), str.byte_end(), *this, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | flags);
}


RString
String::concat(const char* str) const
{
  if (str == 0)
    return this;
  byte* buf_begin;
  byte* buf_end;
  int flags;
  int len = strlen(str);
  DISPATCH16(::concatT, *this, _begin, _end, AsciiIterator(0, str), AsciiIterator(0, str + len), *this, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | flags);
}


RString
String::concat(const uc2char* str) const
{
  if (str == 0)
    return this;
  byte* buf_begin;
  byte* buf_end;
  int flags;
  int len = StringUtf8Utils::uc2length(str);
  DISPATCH16(::concatT, *this, _begin, _end, Uc2Iterator(0, str), Uc2Iterator(0, str + len), *this, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | flags);
}


RbyteArray
String::getBytes(IN(RString) enc) const
{
  acdk::locale::REncoding encoding = acdk::locale::Encoding::getEncoding(enc);
  return getBytes(encoding->getEncoder());

}

RbyteArray
String::getBytes(IN(acdk::locale::REncoder) enc) const
{
  acdk::io::MemWriter mout;
  enc->encode(&mout, this);
  return mout.getBuffer();
}

RcharArray
String::getChars(IN(acdk::locale::REncoder) enc) const 
{
  if (enc == Nil)
    return getChars();
   acdk::io::MemWriter mout;
  enc->encode(&mout, this);
  return mout.getBufferAsChars();
}

void
String::getUc2Chars(int srcBegin, int srcEnd, IN(RuccharArray) dst, int dstBegin) const
{
  int len = length();
  if (srcBegin >= len)
    THROW3(StringIndexOutOfBoundsException, this, srcBegin, len);
  if (srcEnd >= len)
    THROW3(StringIndexOutOfBoundsException, this, srcEnd, len);
  String::iterator it = begin() + srcBegin;
  String::iterator end = begin() + srcEnd;
  for (; it < end; ++it, ++dstBegin)
    dst[dstBegin] = *it;
}

void
String::getChars(int srcBegin, int srcEnd, IN(RcharArray) dst, int dstBegin, IN(acdk::locale::REncoder) enc) const // ### probably remove this
{
  _throwNotImplementedYet();
  // ### TODO Implement me copy(byte_begin() + srcBegin, byte_begin() + srcBegin + srcEnd, const_cast<char*>(dst->data()) + dstBegin);
}



RcharArray
String::getChars() const // ### probably remove this
{
  if (isCharacterClass(CCAscii) || isCharacterClass(CCAnsi) || isCharacterClass(CCUtf8))
  {
    RcharArray ch(new charArray(length()));
    char* ptr = ch->data();
    int i = 0;
    for (byte_iterator it = _begin; it < _end; ++it, ++i)
      ptr[i] = *it;
    return ch;
  }
  THROW1(IllegalArgumentException, "String::getChars() called with incompatible String character class");
  return Nil;
}



//inline
RbyteArray
String::getBytes() const // ### encoder missing
{
  if (isCharacterClass(CCAscii) == false && isCharacterClass(CCUtf8) == false)
  {
    RString tstr = convert(CCAscii);
    return tstr->getBytes();
  }
  RbyteArray ch(new byteArray(length()));
  memcpy(ch->data(), _begin, _end - _begin);
  return ch;
}




RuccharArray
String::getUcChars() const
{
  RuccharArray ch = new uccharArray(length());
  String::iterator it = begin();
  String::iterator endit = end();
  for (int i = 0; it < endit; ++it, ++i)
    ch[i] = *it;
  return ch;
}


bool
String::regionMatches(int toffset, const String& other, int otheroffset, int len, bool ignoreCase) const
{
  bool erg;
  DISPATCH25(regionMatchesT, *this, _begin, _end, other, other._begin, other._end, toffset, otheroffset, len, ignoreCase, erg);
  return erg;
}

bool
String::regionMatches(int toffset, const char* other, int otheroffset, int len, bool ignoreCase) const
{
  return regionMatches(toffset, String(other), otheroffset, len, ignoreCase);
}

bool
String::regionMatches(int toffset, const uc2char* other, int otheroffset, int len, bool ignoreCase) const
{
  return regionMatches(toffset, String(other), otheroffset, len, ignoreCase);
}


int
String::indexOf(char ch, int fromIndex/* = 0*/) const
{
  int erg;
  DISPATCH13(::indexOfCharT, *this, _begin, _end, fromIndex, ch, erg)
  return erg;
}

int
String::indexOf(uc2char ch, int fromIndex) const
{


  int erg;
  DISPATCH13(::indexOfCharT, *this, _begin, _end, fromIndex, ch, erg)
  return erg;
}

int
String::lastIndexOf(char ch, int fromIndex/* = -1*/) const
{
  int erg;
  DISPATCH13(::lastIndexOfCharT, *this, _begin, _end, fromIndex, ch, erg)
  return erg;
}

int
String::lastIndexOf(uc2char ch, int fromIndex) const
{
  int erg;
  DISPATCH13(::lastIndexOfCharT, *this, _begin, _end, fromIndex, ch, erg)
  return erg;
}

int
String::indexOf(const String& str, int fromIndex/* = 0*/) const
{
  int erg;
  DISPATCH21(::indexOfT, *this, _begin + fromIndex, _end, str, str.byte_begin(), str.byte_end(), erg)
  if (erg == -1)
    return erg;
  return erg + fromIndex;
}

int
String::indexOf(const char* str, int fromIndex)  const
{
  int erg;
  if (fromIndex < 0 || fromIndex > length())
    THROW3(StringIndexOutOfBoundsException, this, fromIndex, length());
  int len = strlen(str);
  DISPATCH13(::indexOfT, *this, _begin + fromIndex, _end, AsciiIterator(0, str), AsciiIterator(0, str + len), erg)
  if (erg == -1)
    return erg;
  return erg + fromIndex;
}

int
String::indexOf(const uc2char* str, int fromIndex)  const
{
  int erg;
  if (fromIndex < 0 || fromIndex > length())
    THROW3(StringIndexOutOfBoundsException, this, fromIndex, length());
  int len = uc2strlen(str);
  DISPATCH13(::indexOfT, *this, _begin + fromIndex, _end, Uc2Iterator(0, str), Uc2Iterator(0, str + len), erg)
  if (erg == -1)
    return erg;
  return erg + fromIndex;
}

int
String::lastIndexOf(const String& str, int fromIndex/* = -1*/) const
{
  int erg;
  DISPATCH22(::lastIndexOfT, *this, _begin, _end, str, str.byte_begin(), str.byte_end(), fromIndex, erg)
  return erg;
}

int
String::lastIndexOf(const char* str, int fromIndex)  const
{
  int erg;
  int len = strlen(str);
  DISPATCH14(::lastIndexOfT, *this, _begin, _end, AsciiIterator(0, str), AsciiIterator(0, str + len), fromIndex, erg)
  return erg;
}

int
String::lastIndexOf(const uc2char* str, int fromIndex)  const
{
  int erg;
  int len = uc2strlen(str);
  DISPATCH14(::lastIndexOfT, *this, _begin, _end, Uc2Iterator(0, str), Uc2Iterator(0, str + len), fromIndex, erg)
  return erg;
}


RString
String::substr(int startidx, int endidx/* = -1*/) const
{
  RString ret;
  DISPATCH14(::substrT, *this, _begin, _end, *this, startidx, endidx, ret);
  return ret;
}


bool
String::isLowerCase() const
{
  bool erg;
  DISPATCH11(::isLowerT, *this, _begin, _end, erg)
  return erg;
}


bool
String::isUpperCase() const
{
  bool erg;
  DISPATCH11(::isUpperT, *this, _begin, _end, erg)
  return erg;
}

#if defined(__GNUC__) && __GNUC__ < 3

inline
bool dotrim(uc2char ch, int flags)
{
  bool ret = true;
  if ((flags & TrimControl) == TrimControl && UnicodeCharacter::isControl(ch) == true)
    return true;
  if ((flags & TrimSpace) == TrimSpace && UnicodeCharacter::isSpace(ch) == true)
    return true;
  if ((flags & TrimNewLines) == TrimNewLines && (ch == '\n' || ch == '\r'))
    return true;
  return false;
}

RString dotrim(const String& s, String::iterator begin, String::iterator end, int trimflags)
{
  String::iterator b = begin;
  String::iterator e = end;

  if (trimflags & TrimLeft)
  {
    while (b < e)
    {
      if (dotrim(*b, trimflags) == false)
        break;
      ++b;
    }
  }
  if (trimflags & TrimRight)
  {
    while (e > b)
    {
      --e;
      if (dotrim(*e, trimflags) == false) {
        ++e;
        break;
      }
    }
  }
  if (b == begin && e == end)
    return  const_cast<String*>(&s);
  else
  {
   String* sptr = const_cast<String*>(&s);
    //return new (sptr->allocator()) String(*s._getSubstrBase(), b - begin, e - b);
   return new (sptr->allocator()) String(/*s._getSubstrBase(), */b, e);
  }
}

#endif
RString
String::trim(int trimflags/* = TrimBoth*/) const
{
  RString ret;
#if defined(__GNUC__) && __GNUC__ < 3
  return dotrim(*this, begin(), end(), trimflags);
#else
  DISPATCH13(trimT, *this, _begin, _end, *this, ret, trimflags);
#endif
  return ret;
}



RString
String::replace(char oldChar, char newChar) const
{
  byte* buf_begin;
  byte* buf_end;
  int flags;
  DISPATCH16(::replaceCharT, *this, _begin, _end, *this, oldChar, newChar, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, _stringFlags & ~StorageMaskSST | NormalSST);
}

RString
String::replace(uc2char oldChar, uc2char newChar) const
{
  byte* buf_begin;
  byte* buf_end;
  int flags;
  DISPATCH16(::replaceCharT, *this, _begin, _end, *this, oldChar, newChar, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, _stringFlags & ~StorageMaskSST | NormalSST);
}



int
String::elementCount(char c) const
{
  int erg;
  DISPATCH12(::elementCountT, *this, _begin, _end, (ucchar)c, erg)
  return erg;
}

int
String::elementCount(uc2char c) const
{
  int erg;
  DISPATCH12(::elementCountT, *this, _begin, _end, (ucchar)c, erg)
  return erg;
}

int
String::elementCount(const char* str)  const
{
  return elementCount(String(str));
}
int
String::elementCount(const uc2char* str)  const
{
  return elementCount(String(str));
}

RString
String::valueOf(bool z)
{
  const char* ptr = "false";
  if (z)
    ptr = "true";
  return new String(ptr);
}




//static
RString
String::_itoa(int i, int radix)
{
  static char digits[] =
    { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
      'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
      'u', 'v', 'w', 'x', 'y', 'z'
  };
  StringBuffer strbuf(33);
  bool neg = false;
  if (radix < 2 || radix > 36)
    THROW1(IllegalArgumentException, RString("invalid radix ") + Integer::toString(radix) + ". it has to be within the range 2 ... 36 (inclusive).");
  if (i < 0) {
    neg = true;
    i = -i;
  }
  do {
    strbuf.append(digits[Math::abs(i % radix)]);
  } while ((i /= radix) != 0);
  if (neg == true)
    strbuf.append('-');
  return strbuf.reverse()->toString();
}

//static
RString
String::_jltoa(jlong l, int radix)
{
  static char digits[] =
    { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
      'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
      'u', 'v', 'w', 'x', 'y', 'z'
  };
  StringBuffer strbuf(65);
  bool neg = false;
  if (radix < 2 || radix > 36)
    THROW1(IllegalArgumentException, RString("invalid radix ") + Integer::toString(radix) + ". it has to be within the range 2 ... 36 (inclusive).");
  if (l < 0) {
    neg = true;
    l = -l;
  }
  do {
    strbuf.append(digits[Math::abs(l % radix)]);
  } while ((l /= radix) != 0);
  if (neg == true)
    strbuf.append('-');
  return strbuf.reverse()->toString();
}


//static
RString
String::valueOf(int z)
{
  return Integer::toString(z);
}


//static
RString
String::valueOf(char z)
{
  return Character(z).toString();
}

//static
RString
String::valueOf(ucchar z)
{
  return UnicodeCharacter(z).toString();
}

//static
//RString
//String::valueOf(long z)
//{
//  return Long::toString(z);
//}


//static
RString
String::valueOf(jlong z)
{
  return Long::toString(z);
}


//static
RString
String::valueOf(float z)
{
  return Float::toString(z);
}


//static
RString
String::valueOf(double z)
{
  return Double::toString(z);
}


//static
RString
String::valueOf(IN(RObject) obj)
{
  return obj == Nil ? RCS("null") : obj->toString();
}


//static
RString
String::valueOf(const char* ptr)
{
  return new String(ptr, NormalSST | CCAscii);
}

//static
RString
String::valueOf(const ucchar* ptr)
{
  return new String(ptr);
}

//static
RString
String::valueOf(void* ptr)
{
  char buf[16];
  sprintf(buf, "%p", ptr);
  return new String((const char*)buf, NormalSST);
}


//static
RString
String::valueOf(size_t s)
{
  return valueOf(int(s));
}

char*
String::c_strdup(sys::Allocator* alloc/* = 0*/) const
{
  if (alloc == 0)
    alloc = const_cast<String*>(this)->allocator();
  char* nptr = (char*)alloc->allocate(length() + 1);
  memcpy(nptr, _begin, length());
  nptr[length()] = 0;
  return nptr;
}

void
String::_throwIndexOutOfBound(const char* text, int idx, int maxl)
{
  THROW3(StringIndexOutOfBoundsException, text, idx, maxl);
}


RString String::operator+(IN(RString) other) const { return concat(other); }
RString String::operator+(IN(RObject) o) const { return concat((o == Nil ? RString("Nil") : o->toString())); }
RString String::operator+(bool v) const { return concat(String::valueOf(v)); }
RString String::operator+(char v) const { return concat(String::valueOf(v)); }
RString String::operator+(ucchar v) const { return concat(String::valueOf(v)); }
RString String::operator+(short v) const { return concat(String::valueOf(v)); }
RString String::operator+(int v) const { return concat(String::valueOf(v)); }
RString String::operator+(jlong v) const { return concat(String::valueOf(v)); }
RString String::operator+(float v) const { return concat(String::valueOf(v)); }
RString String::operator+(double v) const { return concat(String::valueOf(v)); }


bool
String::equalsNoHash(const String& str)
{
  bool erg;
  DISPATCH21(::equalsT, *this, byte_begin(), byte_end(), str, str.byte_begin(), str.byte_end(), erg)
  if (erg == false)
    System::out->println(RString("not equal with same hash ") + this + "<=>" + str);
  return erg;
}

bool
String::equals(const char* cstr)
{
  if (cstr == 0)
    return false;
  bool erg;
  DISPATCH12(::equalsT, *this, _begin, _end, cstr, erg)
  return erg;
}

bool 
String::equals(const char* cstr, int len)
{
   if (cstr == 0)
    return false;
  bool erg;
  DISPATCH13(::equalsT, *this, _begin, _end,
                        AsciiIterator(cstr), AsciiIterator(cstr + len),
                        erg)
  return erg;
}

bool
String::equals(const uc2char* cstr)
{
  if (cstr == 0)
    return false;
  bool erg;
  //int len = uc2strlen(cstr);

  DISPATCH12(::equalsT, *this, _begin, _end, cstr, erg)
  return erg;
}

bool
String::equalsIgnoreCase(const String& str) const
{
  bool erg;
  DISPATCH21(::equalsIgnoreCaseT, *this, byte_begin(), byte_end(), str, str.byte_begin(), str.byte_end(), erg)
  return erg;
}

bool
String::equalsIgnoreCase(const char* other)  const
{
  if (other == 0)
    return false;
  bool erg;
  DISPATCH12(::equalsIgnoreCaseT, *this, _begin, _end, other, erg)
  return erg;

}

bool
String::equalsIgnoreCase(const uc2char* other)  const
{
  if (other == 0)
    return false;
  bool erg;
  DISPATCH12(::equalsIgnoreCaseT, *this, _begin, _end, other, erg)
  return erg;
}

int
String::compareTo(const String& other)
{
  int erg;
  DISPATCH21(::compareToT, *this, byte_begin(), byte_end(), other, other.byte_begin(), other.byte_end(), erg)
  return erg;
}

int
String::compareTo(const char* cstr)
{
  if (cstr == 0)
    return 1;
  int erg;
  int len = strlen(cstr);
  DISPATCH13(::compareToT, *this, byte_begin(), byte_end(),
                            AsciiIterator(cstr), AsciiIterator(cstr + len),
                            erg)
  return erg;
}

int
String::compareTo(const uc2char* cstr)
{
  if (cstr == 0)
    return 1;

  int erg;
  int len = uc2strlen(cstr);
  DISPATCH13(::compareToT, *this, byte_begin(), byte_end(),
                            Uc2Iterator(cstr), Uc2Iterator(cstr + len),
                            erg)
  return erg;
}


int
String::compareToIgnoreCase(const String& other) const
{
  int erg;
  DISPATCH21(::compareToIgnoreCaseT, *this, byte_begin(), byte_end(), other, other.byte_begin(), other.byte_end(), erg)
  return erg;
}
int
String::compareToIgnoreCase(const char* other)  const
{
  if (other == 0)
    return 1;
  int erg;
  int len = strlen(other);
  DISPATCH13(::compareToIgnoreCaseT, *this, byte_begin(), byte_end(),
                            AsciiIterator(other), AsciiIterator(other + len),
                            erg)
  return erg;
}

int
String::compareToIgnoreCase(const uc2char* other)  const
{
  if (other == 0)
    return 1;

  int erg;
  int len = uc2strlen(other);
  DISPATCH13(::compareToIgnoreCaseT, *this, byte_begin(), byte_end(),
                            Uc2Iterator(other), Uc2Iterator(other + len),
                            erg)
  return erg;
}

int
String::elementCount(const String& other) const
{
  int erg;
  DISPATCH21(::elementCountT, *this, byte_begin(), byte_end(), other, other.byte_begin(), other.byte_end(), erg)
  return erg;
}

bool
String::startsWith(const String& prefix, int toffset/* = 0*/) const
{
  bool erg;
  DISPATCH21(::startsWithT, *this, byte_begin() + toffset, byte_end(), prefix, prefix.byte_begin(), prefix.byte_end(), erg)
  return erg;
}

bool
String::startsWith(const char* prefix, int toffset)  const
{
  if (prefix == 0)
    return false;
  bool erg;
  int len = strlen(prefix);
  DISPATCH13(::startsWithT, *this, byte_begin() + toffset, byte_end(),
                            AsciiIterator(prefix), AsciiIterator(prefix + len),
                            erg)
  return erg;
}
bool
String::startsWith(const uc2char* prefix, int toffset)  const
{
  if (prefix == 0)
    return false;
  bool erg;
  int len = uc2strlen(prefix);
  DISPATCH13(::startsWithT, *this, byte_begin() + toffset, byte_end(),
                            Uc2Iterator(prefix), Uc2Iterator(prefix + len),
                            erg)
  return erg;
}


bool
String::endsWith(const String& suffix) const
{
  bool erg;
  DISPATCH21(::endsWithT, *this, byte_begin(), byte_end(), suffix, suffix.byte_begin(), suffix.byte_end(), erg)
  return erg;
}

bool
String::endsWith(const char* suffix)  const
{
  if (suffix == 0)
    return false;
  bool erg;
  int len = strlen(suffix);
  DISPATCH13(::endsWithT, *this, byte_begin(), byte_end(),
                          AsciiIterator(suffix), AsciiIterator(suffix + len),
                          erg)
  return erg;
}

bool
String::endsWith(const uc2char* suffix)  const
{
  if (suffix == 0)
    return false;
  bool erg;
  int len = uc2strlen(suffix);
  DISPATCH13(::endsWithT, *this, byte_begin(), byte_end(),
                          Uc2Iterator(suffix), Uc2Iterator(suffix + len),
                          erg)
  return erg;
}


RString
String::toLowerCase() const
{
  byte* buf_begin;
  byte* buf_end;
  int flags;
  DISPATCH14(::toLowerCaseT, *this, _begin, _end, *this, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | flags);
}

RString
String::toUpperCase() const
{
  byte* buf_begin;
  byte* buf_end;
  int flags;
  DISPATCH14(::toUpperCaseT, *this, _begin, _end, *this, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | flags);
}


RString
String::replace(const String& find, const String& repl) const
{
  byte* buf_begin;
  byte* buf_end;
  int flags;
  DISPATCH34(::replaceT, *this, _begin, _end, find, find.byte_begin(), find.byte_end(), repl, repl.byte_begin(), repl.byte_end(), *this, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | flags);
}



RString
String::replace(const char* find, const char* repl) const
{
  byte* buf_begin;
  byte* buf_end;
  int findlen = strlen(find);
  int repllen = strlen(repl);
  int flags;
  DISPATCH18(::replaceT, *this, _begin, _end,
                  AsciiIterator(find), AsciiIterator(find + findlen),
                  AsciiIterator(repl), AsciiIterator(repl + repllen),
                  *this, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | flags);
}


RString
String::replace(const ucchar* find, const ucchar* repl)  const
{
  byte* buf_begin;
  byte* buf_end;
  int findlen = uc2strlen(find);
  int repllen = uc2strlen(repl);
  int flags;
  DISPATCH18(::replaceT, *this, _begin, _end,
                  Uc2Iterator(find), Uc2Iterator(find + findlen),
                  Uc2Iterator(repl), Uc2Iterator(repl + repllen),
                  *this, buf_begin, buf_end, flags)
  if (buf_begin == 0)
    return this;
  return new (const_cast<String*>(this)->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | flags);
}


RString
String::replace(int startidx, int endidx, const String& replstr) const
{
  RString erg;
  DISPATCH24(::replaceByIndexT, *this, _begin, _end, replstr, replstr._begin, replstr._end, *this, startidx, endidx, erg);
  return erg;
}

//static
RString
String::fomUtfEscapedLiteral(const char* text)
{
  return decodeAscUnicode(text);
}

//static
RString
String::fomUtfEscapedLiteral(const uc2char* text)
{
   _throwNotImplementedYet();
  return Nil;
}

int 
String::getFirstIndexOf(IN(RString) chars) const
{
  String::iterator fit = chars->begin();
  String::iterator fend = chars->end();
  int idx = -1;
  for (; fit != fend; ++fit)
  {
    int tidx = indexOf(*fit);
    if (tidx != -1)
    {
      if (idx == -1)
        idx = tidx;
      else if (tidx < idx)
        idx = tidx;
    }
  }
  return idx;
}

RString 
String::peekLine(int lineNo) const
{
  if (lineNo < 0)
    return Nil;
  int curline = 0;
  String::iterator it = begin();
  String::iterator eit = end();
  String::iterator lit = eit;
  for (; it != eit; ++it)
  {
    if (lineNo == 0 || curline == lineNo - 1)
    {
      RString bt = substr(it - begin());
      int idx = bt->getFirstIndexOf("\r\n");
      if (idx == -1)
        return bt;
      return bt->substr(0, idx);
    }
    if (*it == '\n')
    {
      ++it;
      if (it == eit)
        return Nil;
      if (*it != '\r')
        --it;
      lit = it;
      ++curline;
    }
  }
  return Nil;
}

int
StringOrCString::size() const
{
  if (type == StringClass)
    return rstring()->length();
  else if (type == CCUcs2)
    return StringUtf8Utils::uc2length(str.uccstr);
  else
    return strlen(str.cstr);
}


template <typename I1, typename I2>
void copyCastedT(I1 it, I1 end, I2& ins)
{
  for (; it < end; ++it, ++ins)
  {
    ins.set(I2::castTo(*it));
  }
}

template <typename I>
byte*
fill_StringOrCString(const StringOrCString& This, I& ins)
{
  CharacterClass scc = CCAscii;

  switch (This.type)
  {
  case StringOrCString::StringClass:
  {
    RString str = This.rstring();
    DISPATCH11(copyCastedT, *str, str->byte_begin(), str->byte_end(), ins);
    break;
  }
  case StringOrCString::CString:
  {
    const char* cstr = This.str.cstr;
    copyCastedT(AsciiIterator(cstr), AsciiIterator(cstr + strlen(cstr)), ins);
    break;
  }
  case StringOrCString::CCUcs2:
  {
    const uc2char* cstr = This.str.uccstr;
    copyCastedT(Uc2Iterator(cstr), Uc2Iterator(cstr + StringUtf8Utils::uc2length(cstr)), ins);
    break;
  }
  default:
    THROW1(IllegalArgumentException, "Unsupported StringConcanatorType");
  }
  return ins.getByteIterator();
}

template <typename I>
RString
StringConcenator_merge(StringConcenator& This, int charlen, const I&)
{
  int bytec = I::getMaxByteCount();
  int bufsize  = (I::getMaxByteCount() * charlen) + I::getTerminatorByteSize();
  RString str = new String(bufsize);
  byte* buffer = const_cast<byte*>(str->buffer());
  byte* ptr = buffer;
  StringConcenator::const_iterator it = This._strings.begin();
  StringConcenator::const_iterator end = This._strings.end();
  for (; it != end; ++it)
  {
    I insIt((typename I::CharType*)ptr);
    ptr = fill_StringOrCString(*it, insIt);
  }
  I::writeTerminator(ptr);

  str->_set(NormalSST | CharacterIteratorTraits<I>::StringFlags, str->buffer(), bufsize, str->byte_begin(), ptr);
  return str;
}

ACDK_CORE_PUBLIC
RString
StringConcenator::merge(StringConcenator& This)
{
  const_iterator it = This._strings.begin();
  const_iterator end = This._strings.end();
  int alllen = 0;
  CharacterClass scc = CCAscii;
  for (; it != end; ++it)
  {
    alllen += it->size();
    CharacterClass oscc = CCAscii;
    if (it->type == StringOrCString::CString)
      oscc = CCAscii;
    else if (it->type == StringOrCString::StringClass)
      oscc = it->rstring()->characterClass();
    else if (it->type == StringOrCString::CCUcs2)
      oscc = CCUcs2;
    if (scc == CCAscii)
    {
      if (oscc == CCUcs2)
        scc = CCUcs2;
    }
  }
  if (scc == CCAscii)
    return StringConcenator_merge(This, alllen, AsciiIterator(0));
  else if (scc == CCUcs2)
    return StringConcenator_merge(This, alllen, Uc2Iterator(0));
  THROW1(IllegalArgumentException, "Unsupported StringConcanatorType");
  return Nil;
}

OUT(acdk::util::RWeakHashMap) _getInternalStrings()
{
  static acdk::util::RWeakHashMap _internalStrings = new acdk::util::WeakHashMap();
  return _internalStrings;
}


RString 
String::intern() const
{
  acdk::util::RWeakHashMap& whm = _getInternalStrings();
  SYNCOBJECT(whm);
  RString normalizedString = getNormalized();
  acdk::lang::ref::RWeakReference ref = (acdk::lang::ref::RWeakReference)whm->get((RObject)(Object*)&normalizedString);
  if (ref != Nil)
  {
    RString s = (RString) ref->get();
    if (s != Nil)
      return s;
    
  }
  whm->put((RObject)(Object*)&normalizedString, new acdk::lang::ref::WeakReference((RObject)(Object*)&normalizedString));
  return this;
}

//static 
OUT(RString)
String::emptyString()
{
  static RString emptyString;
  if (emptyString != Nil)
    return emptyString;
  emptyString = new String();
  System::registerStaticReference(emptyString);
  return emptyString;
}

//static 
bool 
String::isBlank(IN(RString) str)
{
  if (str == Nil)
    return true;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isWhitespace(*it) == false)
      return false;
  }
  return true;
}

//static 
bool 
String::isAlpha(IN(RString) str)
{
  if (str == Nil)
    return false;
   String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isLetter(*it) == false)
      return false;
  }
  return true;
}

//static 
bool 
String::isAlphaSpace(IN(RString) str)
{
  if (str == Nil)
    return false;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isWhitespace(*it) == false && 
        UnicodeCharacter::isLetter(*it) == false)
      return false;
  }
  return true;
}

//static 
bool 
String::isAlphanumeric(IN(RString) str)
{
  if (str == Nil)
    return false;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isLetterOrDigit(*it) == false)
      return false;
  }
  return true;
}

//static 
bool 
String::isAlphanumericSpace(IN(RString) str)
{
  if (str == Nil)
    return false;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isLetterOrDigit(*it) == false &&
        UnicodeCharacter::isWhitespace(*it) == false)
      return false;
  }
  return true;
}

//static 
bool 
String::isNumeric(IN(RString) str)
{
  if (str == Nil)
    return false;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isDigit(*it) == false)
      return false;
  }
  return true;
}

//static 
bool 
String::isNumericSpace(IN(RString) str)
{
  if (str == Nil)
    return false;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isDigit(*it) == false &&
        UnicodeCharacter::isWhitespace(*it) == false)
      return false;
  }
  return true;
}

RString 
String::substringBefore(IN(RString) find) const
{
  if (find == Nil)
    return this;
  int idx = indexOf(find);
  if (idx == -1)
    return this;
  return substr(0, idx);
}

RString 
String::substringAfter(IN(RString) find) const
{
  if (find == Nil)
    return emptyString();
  int idx = indexOf(find);
  if (idx == -1)
    return emptyString();
  if (idx + find->length() > length())
    return emptyString();
  return substr(idx + find->length());
}

RString 
String::substringBeforeLast(IN(RString) find) const
{
  if (find == Nil)
    return this;
  int idx = lastIndexOf(find);
  if (idx == -1)
    return this;
  return substr(0, idx);
}

RString 
String::substringAfterLast(IN(RString) find) const
{
  if (find == Nil)
    return emptyString();
  int idx = lastIndexOf(find);
  if (idx == -1)
    return emptyString();
  if (idx + find->length() > length())
    return emptyString();
  return substr(idx + find->length());
}

//static 
bool 
String::containsOnly(IN(RString) str, IN(RString) chars)
{
  if (str == Nil || chars == Nil)
    return false;
  String::iterator sit = chars->begin();
  String::iterator send = chars->end();
  for (; sit != send; ++sit)
  {
    if (str->indexOf(*sit) == -1)
      return false;
  }
  return true;
}

//static 
bool 
String::containsNone(IN(RString) str, IN(RString) chars)
{
  if (str == Nil || chars == Nil)
    return true;
   String::iterator sit = chars->begin();
  String::iterator send = chars->end();
  for (; sit != send; ++sit)
  {
    if (str->indexOf(*sit) != -1)
      return false;
  }
  return true;
}

//static 
int 
String::indexOfAny(IN(RString) str, IN(RuccharArray) chars)
{
  if (str == Nil || chars == Nil)
    return -1;
  int foundIdx = -1;
  uccharArray::iterator it = chars->begin();
  uccharArray::iterator end = chars->end();
  for (; it != end; ++it)
  {
    int idx = str->indexOf(*it);
    if (idx != -1 && (foundIdx == -1 || idx < foundIdx))
      foundIdx = idx;
  }
  return foundIdx;
}

//static 
int 
String::indexOfAny(IN(RString) str, IN(RString) chars)
{
   if (str == Nil || chars == Nil)
    return -1;
  int foundIdx = -1;
  String::iterator it = chars->begin();
  String::iterator end = chars->end();
  for (; it != end; ++it)
  {
    int idx = str->indexOf(*it);
    if (idx != -1 && (foundIdx == -1 || idx < foundIdx))
      foundIdx = idx;
  }
  return foundIdx;
}

//static 
int 
String::indexOfAny(IN(RString) str, IN(RStringArray) strings)
{
  if (str == Nil || strings == Nil)
    return -1;
  StringArray::array_iterator sit = strings->begin();
  StringArray::array_iterator send = strings->end();
  int foundIdx = -1;
  for (; sit != send; ++sit)
  {
    int idx = str->indexOf(*sit);
    if (idx != -1 && (foundIdx == -1 || idx < foundIdx))
      foundIdx = idx;
  }
  return foundIdx;
}

//static 
int 
String::lastIndexOfAny(IN(RString) str, IN(RString) chars)
{
   if (str == Nil || chars == Nil)
    return -1;
  int foundIdx = -1;
  String::iterator it = chars->begin();
  String::iterator end = chars->end();
  for (; it != end; ++it)
  {
    int idx = str->lastIndexOf(*it);
    if (idx != -1 && (foundIdx == -1 || idx > foundIdx))
      foundIdx = idx;
  }
  return foundIdx;
}

//static 
int 
String::lastIndexOfAny(IN(RString) str, IN(RuccharArray) chars)
{
  if (str == Nil || chars == Nil)
    return -1;
  int foundIdx = -1;
  uccharArray::iterator it = chars->begin();
  uccharArray::iterator end = chars->end();
  for (; it != end; ++it)
  {
    int idx = str->indexOf(*it);
    if (idx != -1 && (foundIdx == -1 || idx > foundIdx))
      foundIdx = idx;
  }
  return foundIdx;
}

//static 
int 
String::lastIndexOfAny(IN(RString) str, IN(RStringArray) strings)
{
  if (str == Nil || strings == Nil)
    return -1;
  int foundIdx = -1;
  StringArray::array_iterator it = strings->begin();
  StringArray::array_iterator end = strings->end();
  for (; it != end; ++it)
  {
    int idx = str->indexOf(*it);
    if (idx != -1 && (foundIdx == -1 || idx > foundIdx))
      foundIdx = idx;
  }
  return foundIdx;
}

//static 
RString 
String::join(IN(acdk::util::RIterator) it)
{
  return join(it, String::emptyString());
}

//static 
RString 
String::join(IN(acdk::util::RIterator) it, uc2char delimiter)
{
  if (it == Nil)
    return Nil;
  return join(it, Character::toString(delimiter));
}

//static 
RString 
String::join(IN(acdk::util::RIterator) it, IN(RString) delimiter)
{
  if (it == Nil)
    return Nil;
  RString del = defaultString(delimiter);
  bool isFirst = true;
  StringBuffer sb;
  while (it->hasNext() == true)
  {
    if (isFirst == true)
      isFirst = false;
    else
      sb << del;
    RObject obj = it->next();
    if (obj != Nil)
      sb << obj->toString();
  }
  return sb.toString();
}

//static 
RString 
String::join(IN(RObjectArray) oa)
{
  return join(oa, String::emptyString());
}
//static 
RString 
String::join(IN(RObjectArray) oa, uc2char delimiter)
{
  return join(oa, Character::toString(delimiter));
}

//static 
RString 
String::join(IN(RObjectArray) oa, IN(RString) delimiter)
{
  if (oa == Nil)
    return Nil;
  RString del = defaultString(delimiter);
  ObjectArray::array_iterator it = oa->begin();
  ObjectArray::array_iterator end = oa->end();
  bool isFirst = true;
  StringBuffer sb;
  for (; it != end; ++it)
  {
    if (isFirst == true)
      isFirst = false;
    else
      sb << del;
    RObject o = *it;
    if (o != Nil)
      sb << o;
  }
  return sb.toString();
}

RStringArray 
String::split() const
{
  return acdk::util::StringTokenizer(this).allToken();
}

RStringArray 
String::split(uc2char delimiter) const
{
  return acdk::util::StringTokenizer(this, Character::toString(delimiter)).allToken();
}

RStringArray 
String::split(IN(RString) delimiterChars) const
{
  return acdk::util::StringTokenizer(this, delimiterChars).allToken();
}

RString 
String::repeat(int ntimes) const
{
  StringBuffer sb(length() * ntimes);
  for (int i = 0; i < ntimes; ++i)
    sb.append(*this);
  return sb.toString();
}

RString 
String::rightPad(int size, uc2char padchar) const
{
  if (length() >= size)
    return this;
  StringBuffer sb(size);
  sb.append(*this);
  int len = size - length();
  for (int i = 0; i < len; ++i)
    sb << padchar;
  return sb.toString();
}

RString 
String::leftPad(int size, uc2char padchar) const
{
  if (length() >= size)
    return this;
  StringBuffer sb(size);
  int len = size - length();
  for (int i = 0; i < len; ++i)
    sb << padchar;
  sb.append(*this);
  return sb.toString();
}


RString 
String::center(int size, uc2char padchar) const
{
  if (length() >= size)
    return this;
  
  StringBuffer sb(size);
  int len = size - length();
  int len1 = len / 2;
  int i;
  for (i = 0; i < len1; ++i)
    sb << padchar;
  sb.append(*this);
  for (i = 0; i < len1; ++i)
    sb << padchar;
  if (sb.length() < size)
    sb.append(padchar);
  return sb.toString();
} 

//static 
RString 
String::deleteWhitespace(IN(RString) str)
{
  if (str == Nil)
    return Nil;
  StringBuffer sb(str->length());
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (UnicodeCharacter::isWhitespace(*it) == false)
      sb.append(*it);
  }
  if (sb.length() == str->length())
    return str;
  return sb.toString();
}

//static 
int 
String::indexOfDifference(IN(RString) first, IN(RString) second)
{
  if (first == Nil || second == Nil)
    return -1;
  String::iterator fit = first->begin();
  String::iterator fend = first->end();
  String::iterator sit = second->begin();
  String::iterator send = second->end();
  int i;
  for (i = 0; fit != fend && sit != send; ++fit, ++sit, ++i)
  {
    if (*fit != *sit)
      return i;
  }
  if (fit == fend && sit == send)
    return -1;
  return i;
}


//static 
int 
String::getDistance(IN(RString) first, IN(RString) second)
{
  // Implementation from by Anders Sewerin Johansen
  // http://www.merriampark.com/ldcpp.htm
  if (first == Nil || second == Nil)
    THROW0(IllegalArgumentException);

  int n = first->length();
  int m = second->length();
  if (n == 0)
    return m;
  if (m == 0)
    return n;

  int i; // iterates through s
  int j; // iterates through t
  uc2char s_i; // ith character of s
  uc2char t_j; // jth character of t
  int cost; // cost
  
  // Step 1	
  sys::core_vector<sys::core_vector<int> > table(n + 1);
  for (i = 0; i <= n; i++) 
    table[i].resize(m + 1);


  for (i = 0; i <= n; i++) 
    table[i][0] = i;
  
  for (j = 0; j <= m; j++) 
    table[0][j] = j;
  
  for (i = 1; i <= n; i++) 
  {
    uc2char s_i = first->charAt(i - 1);
    for (j = 1; j <= m; j++) 
    {
      uc2char t_j = second->charAt(j - 1);
      int cost = 1;
      if (s_i == t_j) 
        cost = 0;
    
      int above = table[i - 1][j];
      int left = table[i][j - 1];
      int diag = table[i - 1][j - 1];
      int cell = Math::min( above + 1, Math::min(left + 1, diag + cost));

      // Step 6A: Cover transposition, in addition to deletion,
      // insertion and substitution. This step is taken from:
      // Berghel, Hal ; Roach, David : "An Extension of Ukkonen's 
      // Enhanced Dynamic Programming ASM Algorithm"
      // (http://www.acm.org/~hlb/publications/asm/asm.html)

      if (i > 2 && j > 2) 
      {
        int trans = table[i - 2][j - 2] + 1;
        if (first->charAt(i - 2) != t_j) 
          trans++;
        if (s_i != second->charAt(j - 2)) 
          trans++;
        if (cell > trans) 
          cell = trans;
      }

      table[i][j] = cell;
    }
  }
  return table[n][m];
}

StaticAsciiLiteral::StaticAsciiLiteral(const char* ptr)
: hashCode(0)
, length(0)
, text(ptr)
{
  while (*ptr != 0)
  {
    ++length;
    hashCode = hashCode * 31 + *ptr;
    ++ptr;
  }
}

} // namespace lang
} // namespace acdk




