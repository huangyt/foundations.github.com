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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringBuffer.cpp,v 1.30 2005/03/08 12:45:38 kommer Exp $


#include <acdk.h>

#include "StringBuffer.h"
#include "Math.h"

#include "IndexOutOfBoundsException.h"
#include "StringInternals.h"
#include "System.h"

namespace acdk {
namespace lang {


//old #define ACDK_SB_CKL() if (_str->length() != int(strlen(_str->_data()))) sys::coreout << "TIRITSOD: " << __FILE__ << ":" << __LINE__ << sys::eofl
#define ACDK_SB_CKL()


#define INIT_SB_BUFFERSIZE 256

StringBuffer::StringBuffer() 
: Object()
, _str(new (allocator()) String((byte*)allocate(INIT_SB_BUFFERSIZE, acdk::lang::sys::DumpBufferMem), INIT_SB_BUFFERSIZE, 0, 0, NormalSST | CCAscii))
, _shared(false)
 
{ 
  String& str = *_str;
  memset(str._buffer, '\0', INIT_SB_BUFFERSIZE);
  str._end = str._begin = str._buffer;

}

StringBuffer::StringBuffer(size_t cap)
: Object()
, _str(new (allocator()) String((byte*)allocate(cap == 0 ? 1 : cap, acdk::lang::sys::DumpBufferMem), cap, 0, 0, NormalSST | CCAscii))
, _shared(false)
{
  memset(_str->_buffer, '\0', _str->_bufferLength);
  String& str = *_str;
  str._end = str._begin = str._buffer;
}



StringBuffer::StringBuffer(IN(RString) str)
: Object()
, _str(str)
, _shared(true)
{
}

void
StringBuffer::reset()
{
  _unShare();
  String& str = *_str;
  memset(str._buffer, 0, str._bufferLength); // not really needed
  str._end = str._begin = str._buffer;
}
 

RStringBuffer 
StringBuffer::append(const String& str)
{
  if (str.byte_begin() == str.byte_end())
    return this;
#if defined(__GNUC__)  && (__GNUC__ < 3)
  bool shared = _shared;
  if (isCChar())
  {
    if (str.isCChar())
      SB_APPEND_T(AsciiIterator, AsciiIterator, 
                AsciiIterator(*_str, _str->byte_begin()), 
                AsciiIterator(*_str, _str->byte_end()), 
                AsciiIterator(str, str.byte_begin()), 
                AsciiIterator(str, str.byte_end()), _str, shared);
    else if (str.isUc2Char())
      SB_APPEND_T(AsciiIterator, Uc2Iterator, 
                AsciiIterator(*_str, _str->byte_begin()), 
                AsciiIterator(*_str, _str->byte_end()), 
                Uc2Iterator(str, str.byte_begin()), 
                Uc2Iterator(str, str.byte_end()), _str, shared);
  }
  else if (isUc2Char())
  {
    if (str.isCChar())
      SB_APPEND_T(Uc2Iterator, AsciiIterator, 
                Uc2Iterator(*_str, _str->byte_begin()), 
                Uc2Iterator(*_str, _str->byte_end()), 
                AsciiIterator(str, str.byte_begin()), 
                AsciiIterator(str, str.byte_end()), _str, shared);
    else if (str.isUc2Char())
      SB_APPEND_T(Uc2Iterator, Uc2Iterator, 
                Uc2Iterator(*_str, _str->byte_begin()), 
                Uc2Iterator(*_str, _str->byte_end()), 
                Uc2Iterator(str, str.byte_begin()), 
                Uc2Iterator(str, str.byte_end()), _str, shared);
  }

#else
  DISPATCH22(::sb_appendT, *_str, _str->_begin, _str->_end, str, str.byte_begin(), str.byte_end(), _str, _shared);
#endif
  return this; 
}

RStringBuffer 
StringBuffer::append(String::iterator begin, String::iterator end)
{
  return append(begin.substr(end)); // ## TODO better performance
}

RStringBuffer 
StringBuffer::append(const char* cptr, int len) 
{ 
#if defined(__GNUC__)  && (__GNUC__ < 3)
  return append(String(cptr, len, ConstSST | CCAscii));
#else
  if (len == -1)
    len = strlen(cptr);
  DISPATCH14(::sb_appendT, *_str, _str->_begin, _str->_end, AsciiIterator(0, cptr), AsciiIterator(0, cptr + len), _str, _shared);
  return this;
#endif
}

RStringBuffer 
StringBuffer::append(const uc2char* cptr, int len) 
{ 
  if (len == -1)
    len = StringUtf8Utils::uc2length(cptr);
#if defined(__GNUC__)  && (__GNUC__ < 3)
  return append(String(cptr, len, ConstSST | CCUcs2));
#else
  DISPATCH14(::sb_appendT, *_str, _str->_begin, _str->_end, Uc2Iterator(0, cptr), Uc2Iterator(0, cptr + len), _str, _shared);
  return this;
#endif
}

#if defined(__BORLANDC__)
RStringBuffer 
StringBuffer::append(const wchar_t* cptr, int len)
{
  return append((const uc2char*)cptr, len);
}
#endif //defined(__BORLANDC__)

RStringBuffer 
StringBuffer::reverse()
{
  SYNCHRONIZETHIS();
  _unShare();
  DISPATCH10(::sb_reverseT, *_str, _str->byte_begin(), _str->byte_end())
  return this;
}

//virtual 
bool 
StringBuffer::equals(IN(RObject) o)
{
  if (instanceof(o, StringBuffer) == false && instanceof(o, String) == false)
    return false;
  return _str->equals(o->toString());
}


RStringBuffer 
StringBuffer::deleteRegion(int start, int end)  THROWS1(RIndexOutOfBoundsException)
{

  SYNCHRONIZETHIS();
  ACDK_SB_CKL();
  

  int oldlen = length();
  if (start < 0 || start > oldlen)
    THROW0(IndexOutOfBoundsException);
  if (end > oldlen)
    end = oldlen;
  if (start > end && end != -1)
    THROW0(IndexOutOfBoundsException);

  _unShare();

  DISPATCH13(::sb_removeT, *_str, _str->_begin, _str->_end, start, end, _str->_end);
  _shared = false;
  ACDK_SB_CKL();
  return this;

}

RStringBuffer 
StringBuffer::deleteCharAt(int index) THROWS1(RIndexOutOfBoundsException)
{
  SYNCHRONIZETHIS();
  ACDK_SB_CKL();
  if ((index < 0) || (index >= length()))
    THROW0(IndexOutOfBoundsException);
  _unShare();

  DISPATCH13(::sb_removeT, *_str, _str->_begin, _str->_end, index, index + 1, _str->_end);
  _shared = false;

  ACDK_SB_CKL();
  return this;
}

void 
StringBuffer::setCharAt(int idx, char c)
{
  SYNCHRONIZETHIS();
  _unShare();
  DISPATCH12(::sb_setCharAtT, *_str, _str->byte_begin(), _str->byte_end(), idx, c);
  _shared = false;
}


void 
StringBuffer::setCharAt(int idx, uc2char c)
{
  SYNCHRONIZETHIS();
  _unShare();
  DISPATCH12(::sb_setCharAtT, *_str, _str->byte_begin(), _str->byte_end(), idx, c); // ### FIXME what if current is ascii and insert an UCS2?
  _shared = false;
}

void 
StringBuffer::setLength(int newLength) THROWS1(RIndexOutOfBoundsException)
{
  SYNCHRONIZETHIS();
  int chsize = _str->getMaxCharacterSize();
  int newbufsize = newLength * chsize;
  if (newbufsize == (_str->_bufferLength - 1))
    return;

  _unShare(newbufsize + chsize);
  
  if (newbufsize < _str->length()) 
  {
    DISPATCH12(::sb_setTerminatorAtT, *_str, _str->_begin, _str->_end, newLength, _str->_end);
  }

}

RString 
StringBuffer::substring(int start, int end) THROWS1(RIndexOutOfBoundsException)
{
  return _str->substring(start, end);
}

RStringBuffer 
StringBuffer::replace(int start, int end, IN(RString) str) THROWS1(RIndexOutOfBoundsException)
{
  SYNCHRONIZETHIS();
  ACDK_SB_CKL();

  if ((start < 0) || (start >= length()) || (end < start) || end > length())
    THROW0(IndexOutOfBoundsException);
  RString sicstr = _str;
  _str = sicstr->replace(start, end, str);
  if (_str != sicstr)
    _shared = false;
  ACDK_SB_CKL();
  return this;
}

RStringBuffer 
StringBuffer::set(IN(RString) str)
{
  SYNCHRONIZETHIS();
  _unShare();

  if (str == Nil)
  {
    _str = String::emptyString();
    _shared = true;
  }
  else 
  {
    _str = str;
    _str->normalize();
    _shared = true;
  }
  return this;
}

#ifdef max 
#undef max
#endif 
/*
void
rcopy(const char* sb, const char* se, char* t)
{
  t += se - sb - 1;
  --se;
  while (se >= sb) 
  {
    *t = *se;
    --se;
    --t;
  }
}
*/

RStringBuffer 
StringBuffer::insert(int index, const String& str)
{
  SYNCHRONIZETHIS();
  ACDK_SB_CKL();
  if (index > _str->length())
    THROW0(IndexOutOfBoundsException);
#if defined(__GNUC__)  && (__GNUC__ < 3)
  bool shared = _shared;
  if (isCChar())
  {
    if (str.isCChar())
      SB_INSERT_T(AsciiIterator, AsciiIterator, 
                AsciiIterator(*_str, _str->byte_begin()), 
                AsciiIterator(*_str, _str->byte_end()), 
                AsciiIterator(str, str.byte_begin()), 
                AsciiIterator(str, str.byte_end()), _str, index, shared);
    else if (str.isUc2Char())
      SB_INSERT_T(AsciiIterator, Uc2Iterator, 
                AsciiIterator(*_str, _str->byte_begin()), 
                AsciiIterator(*_str, _str->byte_end()), 
                Uc2Iterator(str, str.byte_begin()), 
                Uc2Iterator(str, str.byte_end()), _str, index, shared);
  }
  else if (isUc2Char())
  {
    if (str.isCChar())
      SB_INSERT_T(Uc2Iterator, AsciiIterator, 
                Uc2Iterator(*_str, _str->byte_begin()), 
                Uc2Iterator(*_str, _str->byte_end()), 
                AsciiIterator(str, str.byte_begin()), 
                AsciiIterator(str, str.byte_end()), _str, index, shared);
    else if (str.isUc2Char())
      SB_INSERT_T(Uc2Iterator, Uc2Iterator, 
                Uc2Iterator(*_str, _str->byte_begin()), 
                Uc2Iterator(*_str, _str->byte_end()), 
                Uc2Iterator(str, str.byte_begin()), 
                Uc2Iterator(str, str.byte_end()), _str, index, shared);
  }

#else
  DISPATCH23(::sb_insertT, *_str, _str->byte_begin(), _str->byte_end(), str, str.byte_begin(), str.byte_end(), _str, index, _shared);
#endif
  _shared = false;
  return this;
}

RStringBuffer 
StringBuffer::insert(int index, const char* str, int offset, int len)
{
  if (len == -1)
    len = strlen(str) - offset;
#if defined(__GNUC__)  && (__GNUC__ < 3)
  String tstr(str + offset, len);
  return insert(index, tstr);
#else
  DISPATCH15(::sb_insertT, *_str, _str->byte_begin(), _str->byte_end(), AsciiIterator(0, str), AsciiIterator(0, str + len), _str, index, _shared);
  _shared = false;
#endif
  return this;
}

RStringBuffer 
StringBuffer::insert(int index, const char* str) 
{ 
  return insert(index, str, 0, -1);
}

RStringBuffer 
StringBuffer::insert(int index, const uc2char* str, int offset, int len)
{
  if (len == -1)
    len = StringUtf8Utils::uc2length(str) - offset;
#if defined(__GNUC__)  && (__GNUC__ < 3)
  String tstr(str + offset, len);
  return insert(index, tstr);
#else
  DISPATCH15(::sb_insertT, *_str, _str->byte_begin(), _str->byte_end(), Uc2Iterator(str + offset), Uc2Iterator(str + offset + len), _str, index, _shared);
  _shared = false;
#endif
  return this;
}

RStringBuffer 
StringBuffer::insert(int index, const uc2char* str) 
{ 
  int len = StringUtf8Utils::uc2length(str);
#if defined(__GNUC__)  && (__GNUC__ < 3)
  String tstr(str);
  return insert(index, tstr);
#else
  DISPATCH15(::sb_insertT, *_str, _str->byte_begin(), _str->byte_end(), Uc2Iterator(str), Uc2Iterator(str + len), _str, index, _shared);
  _shared = false;
#endif
  return this;
}


RStringBuffer 
StringBuffer::insert(int offset, IN(RString) str) THROWS1(RIndexOutOfBoundsException)
{
  return insert(offset, *str);
}
  
RStringBuffer
StringBuffer::insert(int offset, IN(RObject) obj) THROWS1(RIndexOutOfBoundsException)
{
  return  insert(offset, *String::valueOf(obj));
}

void 
StringBuffer::ensureCapacity(int mincap)
{
  SYNCHRONIZETHIS();
  int bytecap = mincap * _str->getMaxCharacterSize();
  if (_str->_bufferLength - 1 >= bytecap)
    return;
  _unShare((bytecap > _str->_bufferLength * 2 + 2) ? bytecap : (_str->_bufferLength * 2 + 2));
}

void 
StringBuffer::_unShare2(int mincap)
{
  
  int bufsize = _str->_bufferLength;
  int bytesize = _str->byte_end() - _str->byte_begin();
  if (mincap <= bufsize) {
    byte* nbuf = (byte*)allocate(bufsize, acdk::lang::sys::DumpBufferMem);
    memcpy(nbuf, _str->_begin, bufsize);
    _str = new String(nbuf, bufsize, nbuf, nbuf + bytesize, _str->stringFlags());
    _shared = false;
    return;
  }
  mincap = mincap + (mincap / 2 == 0 ? 128 : mincap / 2); 
  byte* nbuf = (byte*)allocate(mincap, acdk::lang::sys::DumpBufferMem);
  memset(nbuf, '\0', mincap);
  memcpy((char*)nbuf, (char*)_str->byte_begin(), bytesize);
  _str = new String(nbuf, mincap, nbuf, nbuf + bytesize, _str->stringFlags());
  _shared = false;

}

} // lang
} // acdk

