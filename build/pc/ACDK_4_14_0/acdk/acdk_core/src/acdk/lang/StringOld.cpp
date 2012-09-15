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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringOld.cpp,v 1.9 2005/03/08 12:45:38 kommer Exp $


#include <acdk.h>
#if defined(USE_OLD_STRING)
#include <string>

#include "StringBuffer.h"
#include "Math.h"
#include "Integer.h"
#include "Character.h"
#include "Long.h"
#include "Float.h"
#include "Double.h"

#include "IllegalArgumentException.h"
#include "IndexOutOfBoundsException.h"
#include "NullPointerException.h"
#include "RuntimeException.h"

#include "sys/Allocator.h"

#include <stdio.h>

namespace acdk {
namespace lang {

 void __bcctestdummy()
{
  RString _sadf = "asdf";
  RString __tstr = ::acdk::lang::RString("SystemError") + 
      ". thrown in " "./src\\acdk/lang/ThreadLocalImpl.h" ":" /*+ 
        ::acdk::lang::String::valueOf(47)*/; 
  
}

//using namespace std;

ACDK_CORE_PUBLIC
String* 
createNewString(const char* str)
{
  return new String(str);
}

static void foo()
{
  RObject o = (RObject)(RString)"asdf";
}

//inline
String::String()
: Object(),
  _buffer(0),
  _size(0)
{
}

//inline
String::String(RString str)
: Object(),
  _buffer(0),
  _size(0)
{
  if (str == Nil)
    return;
  _init(str->length(), str->c_str());
}

String::String(String& str)
: Object(),
  _buffer(0),
  _size(0)
{
  _init(str.length(), str.c_str());
}

//inline
String::String(StringBuffer& buf)
: Object(),
  _buffer(0),
  _size(0)
{
  _init(buf.length(), buf.c_str());
}

//inline
String::String(RStringBuffer buf)
: Object(),
  _buffer(0),
  _size(0)
{
  if (buf == Nil)
    return;
  _init(buf->length(), buf->c_str());
}


void
String::_init(int strsize, const char* buf)
{
  _size = strsize + 1;
  _buffer = (char*)allocate(_size);
  memcpy(_buffer, buf, _size);
}

#ifdef ACDK_STD_CPP_CONVERTERS
//inline
String::String(const std::string& str)
: Object(),
  _buffer(0),
  _size(0)
{
  _size = str.length() + 1;
  _buffer = (char*)allocate(_size);
  memcpy(_buffer, str.c_str(), str.length() + 1);
}
#endif //ACDK_STD_CPP_CONVERTERS

//inline
String::String(int size, char* takethis)
: Object(),
  _buffer(takethis),
  _size(size)
{
}




//virtual 
//inline
int 
String::compareTo(RObject o)
{
  RString str = RString(o);
  if (str == Nil)
    return 1;
  if (_buffer == 0)
    return -1;
  return strcmp(_buffer, str->c_str());
}


//virtual 
int 
String::compareTo(RString s)
{
  if (s == Nil)
    return 1;
  if (_buffer == 0)
    return -1;
  return strcmp(_buffer, s->c_str());
}

//virtual 
//inline
RString 
String::toString()
{
  return RString(const_cast<String*>(this));
}

//virtual
RObject
String::clone()
{
  return new String(c_str());
}

//virtual
RObject 
String::clone(sys::Allocator* alloc)
{
  return new (alloc) String(c_str());
}



RString 
String::intern()
{ 
  return RString(const_cast<String*>(this)); 
}

//virtual 
//inline 
char 
String::charAt(int idx) 
{ 
  if (idx >= length() || idx < 0)
    THROW3(IndexOutOfBoundsException, "String::charAt()", idx, length());
  return _buffer[idx];
}


ACDK_CORE_PUBLIC 
RString  
operator+(const char* cstr, RObject obj)
{
  return RString(cstr) + obj->toString();
}

ACDK_CORE_PUBLIC 
RString 
operator+(const char* cstr, RString obj)
{
  return RString(cstr) + obj->toString();
}


RString::RString(RStringBuffer& strbuffer)
{
  *this = strbuffer->toString();
}
  
  
String::String(RcharArray ch)
  : Object(),
    _buffer(0),
    _size(9)
{
  if (ch == Nil)
    THROW0(NullPointerException);

  const char* d = ch->data();
  if (d == 0)
    THROW0(NullPointerException);
  int length = Math::min((int)strlen(d), (int)ch->length());
  _size = length + 1 ;
  _buffer = (char*)allocate(_size);
  memcpy(_buffer, d, length);
  _buffer[_size - 1] = 0;
}

String::String(const char* str, int startidx, int length)
  : Object(),
    _buffer(0),
    _size(0)
{
  if (str == 0)
    THROW0(RuntimeException);
  
  if (length == -1)
    length = strlen(str);
  if (length < 0)
    THROW0(IndexOutOfBoundsException);
  _size = length + 1;
  _buffer = (char*)allocate(_size);
  memcpy(_buffer, str + startidx, length);
  _buffer[_size - 1] = 0;
}

String::String(const char* str)
: Object(),
  _buffer(0),
  _size(0)
{
  if (str == 0) {
    _size = 1;
    _buffer = (char*)allocate(_size);
  } else {
    int length = strlen(str);
    _size = length + 1;
    _buffer = (char*)allocate(_size);
    memcpy(_buffer, str, length);
  }
  _buffer[_size - 1] = 0;
}

//virtual
String::~String()
{
  if (_buffer) { 
    deallocate(_buffer);
  }
  _buffer = 0;
  _size = 0;
}

//virtual 
int 
String::hashCode()
{
  int erg = 0;
  int ct = length();
  const char* cptr = c_str();
  for (int i = 0; i < ct; i++)
    erg = erg * 31 + cptr[i];
  return erg;
}


//static 
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
String::valueOf(RObject obj)
{
  return obj == Nil ? RString("null") : obj->toString();
}

//static 
RString 
String::valueOf(char* ptr)
{
  return RString(ptr);
}

//static 
RString 
String::valueOf(void* ptr)
{
  char buf[16];
  sprintf(buf, "%p", ptr);
  return RString((const char*)buf);
}

//static 
RString 
String::valueOf(size_t s)
{
  return valueOf(int(s));
}

//virtual 
bool 
String::equals(RObject o)
{
  if (o == Nil)
     return false;
  if (ISINSTANCEOF(String, o) == false)
    return false;
  return equals(RString(o)->c_str());
}

bool 
String::equals(RString str) 
{ 
   if (str == Nil)
      return false;
   return equals(str->c_str());
}

// virtual
bool
String::equalsIgnoreCase(RString o)
{
  return compareToIgnoreCase(o) == 0;
}

#ifdef ACDK_STD_CPP_CONVERTERS
std::string
toUpper(const std::string& str)
{
  std::string ret = str;
  for (size_t i = 0; i < ret.length(); i++)
    ret[i] = char(toupper(ret[i]));
  return ret;
}

std::string
toLower(const std::string& str)
{
  std::string ret = str;
  for (size_t i = 0; i < ret.length(); i++)
    ret[i] = char(tolower(ret[i]));
  return ret;
}
#endif

void
toUpper(char* ptr)
{
  while (*ptr) {
    *ptr = char(toupper(*ptr));
    ++ptr;
  }
}

void
toLower(char* ptr)
{
  while (*ptr) {
    *ptr = char(tolower(*ptr));
    ++ptr;
  }
}


//virtual 
int
String::compareToIgnoreCase(RString o)
{
  if (o == Nil)
    return 1;
  const char* ptr1 = c_str();
  const char* ptr2 = o->c_str();
  while (*ptr1 && *ptr2) {
    if (tolower(*ptr1) != tolower(*ptr2))
      break;

    ptr1++;

    ptr2++;
  }
  return tolower(*ptr1) - tolower(*ptr2);
}

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

RString 
RString::operator+(RString str)
{
  return getIPtr()->concat(str);
}



RString 
String::concat(RString str)
{
  if (str == Nil)
    return const_cast<String*>(this);
  return concat(*str);
  
}


RString 
String::concat(String& str)
{
  int newsize = length() + str.length() + 1;
  char* nbuf = (char*)allocate(newsize);
  strcat(strcpy(nbuf, c_str()), str.c_str());
  return new String(newsize, nbuf);
}

RString 
String::concat(const char* str)
{
  int newsize = length() + strlen(str) + 1;
  char* nbuf = (char*)allocate(newsize);
  strcat(strcpy(nbuf, c_str()), str);
  return new String(newsize, nbuf);
}


//static 
RString 
String::copyValueOf(RcharArray data, int offset, int count)
{
  return new String(data->data(), offset, count);
}

  
bool 
String::startsWith(RString prefix, int toffset)
{
  if (prefix == Nil)
    return false;
  return startsWith(*prefix, toffset);
}

bool 
String::startsWith(String& prefix, int toffset/* = 0*/)
{
  const char* ptr = c_str() + toffset;
  const char* optr = prefix.c_str();
  return strncmp(ptr, optr, prefix.length()) == 0;
}

bool 
String::startsWith(const char* prefix, int toffset/* = 0*/)
{
  const char* ptr = c_str() + toffset;
  return strncmp(ptr, prefix, strlen(prefix)) == 0;
}

bool 
String::endsWith(String& suffix)
{
  const char *ss = suffix.c_str();
  const char *se = ss + suffix.length() - 1;
  const char* fs = c_str();
  const char* fe = fs + length() - 1;
  while (se >= ss && fe >= fs) {
    if (*se-- != *fe--)
      return false;
  }
  return true;
}

bool 
String::endsWith(RString suffix)
{
  if (suffix == Nil || suffix->length() == 0 || length() == 0)
    return false;
  return endsWith(*suffix);
}

bool 
String::endsWith(const char* suffix)
{
  const char *ss = suffix;
  const char *se = ss + strlen(suffix) - 1;
  const char* fs = c_str();
  const char* fe = fs + length() - 1;
  while (se >= ss && fe >= fs) {
    if (*se-- != *fe--)
      return false;
  }
  return true;
}

void 
String::getChars(int srcBegin, int srcEnd, RcharArray dst, int dstBegin)
{
  if (dst == Nil)
    THROW0(NullPointerException);
  if (srcBegin < 0 
      || srcBegin > length() 
      || srcEnd < srcBegin 
      || srcEnd > length() 
      || dstBegin > int(dst->length())
      || (srcEnd-srcBegin) > (dst->length() - dstBegin))
    THROW0(IndexOutOfBoundsException);
  char* dptr = dst->data() + dstBegin;
  const char* sptr = c_str() + srcBegin;
  const char* eptr = c_str() + srcEnd;
  while (sptr < eptr) {
    *dptr++ = *sptr++;
  }
  //*dptr = 0; // Terminiert
}


RcharArray 
String::getChars()
{
  return new charArray(c_str(), length());
  /*
  RcharArray dst(new charArray(length()));
  getChars(0, length(), dst, 0);
  return dst;
  */
}


RbyteArray 
String::getBytes()
{
  return new byteArray((byte*)c_str(), length());
}

RbyteArray 
String::getBytes(RString enc)
{
  return getBytes();
}

RcharArray 
String::tocharArray()
{
  return getChars();
}

int 
String::indexOf(int ch, int fromIndex)
{
  if (fromIndex >= length())
    return -1;
  const char* ptr = c_str() + fromIndex;
  for (int idx = fromIndex; *ptr != 0; idx++, ptr++)
    if (*ptr == ch)
      return idx;
  return -1;
}  


int 
String::lastIndexOf(int ch, int fromIndex)
{
  if (fromIndex == -1)
    fromIndex = length() - 1;
  const char* sptr = c_str();
  const char* ptr = sptr + length() - 1;

  for (int idx = fromIndex; sptr <= ptr ; idx--, ptr--)
    if (*ptr == ch)
      return idx;
  return -1;
}


int 
String::indexOf(RString str, int fromIndex)
{
  if (str == Nil)
    return -1;
  return indexOf(*str, fromIndex);
}

int 
String::indexOf(String& str, int fromIndex)
{
  if (fromIndex > length() - 1)
    return -1;
  const char* tptr = c_str();
  const char* optr = str.c_str();
  const char* ergptr = strstr(tptr + fromIndex, optr);
  if (ergptr == 0)
    return -1;
  return ergptr - tptr;
}

int 
String::indexOf(const char* str, int fromIndex)
{
  if (fromIndex > length() - 1)
    return -1;
  const char* tptr = c_str();
  const char* optr = str;
  const char* ergptr = strstr(tptr  + fromIndex, optr);
  if (ergptr == 0)
    return -1;
  return ergptr - tptr;
}


int 
String::lastIndexOf(RString str, int fromIndex)
{
  if (str == Nil)
    return -1;
  return lastIndexOf(*str, fromIndex);
}


int 
String::lastIndexOf(String& str, int fromIndex)
{
  int rightIndex = length() - str.length();
  if (fromIndex == -1) 
    fromIndex = length() - 1;
  if (fromIndex < 0)
      return -1;
  if (fromIndex > rightIndex) 
      fromIndex = rightIndex;
  if (str.length() == 0) 
      return fromIndex;
  
  const char* sptr = c_str();
  const char* fptr = str.c_str();
  const char* ptr = 0;
  const char* lfptr = 0;
  while (true) {
    ptr = strstr(sptr, fptr);
    if (ptr == 0)
      break;
    if ((ptr - c_str()) > fromIndex)
      break;
    lfptr = ptr;
    sptr = ptr + 1;
  }
  if (lfptr == 0)
    return -1;
  return lfptr - c_str();
}

int 
String::lastIndexOf(const char* str, int fromIndex)
{
  int strlenght = strlen(str);
  int rightIndex = length() - strlenght;
  if (fromIndex == -1) 
    fromIndex = length() - 1;
  if (fromIndex < 0)
    return -1;
  if (fromIndex > rightIndex) 
    fromIndex = rightIndex;
  if (strlenght == 0) 
    return fromIndex;
  
  const char* sptr = c_str();
  const char* fptr = str;
  const char* ptr = 0;
  const char* lfptr = 0;
  while (true) {
    ptr = strstr(sptr, fptr);
    if (ptr == 0)
      break;
    if ((ptr - c_str()) > fromIndex)
      break;
    lfptr = ptr;
    sptr = ptr + 1;
  }
  if (lfptr == 0)
    return -1;
  return lfptr - c_str();
}




bool 
String::regionMatches(int toffset, RString other, int ooffset, int len, bool ignoreCase)
{
  if (other == Nil)
    return false;
  return regionMatches(ignoreCase, toffset, *other, ooffset, len);
}


bool 
String::regionMatches(bool ignoreCase, int toffset, String& other, int ooffset, int len)
{
  const char* ptr = c_str() + toffset;
  const char* optr = other.c_str() + ooffset;
  if (ignoreCase == false) 
    return strncmp(ptr, optr, len) == 0; 
  RString t1 = toLowerCase();
  RString o1 = other.toLowerCase();
  return strncmp(t1->c_str(), o1->c_str(), len) == 0; 
}

bool 
String::regionMatches(bool ignoreCase, int toffset, const char* other, int ooffset, int len)
{
  String tstr(other);
  return regionMatches(ignoreCase, toffset, tstr, ooffset, len);
}

bool 
String::regionMatches(bool ignoreCase, int toffset, RString other, int ooffset, int len)
{
  return regionMatches(toffset, other, ooffset, len, ignoreCase);
}


RString 
SubSSTstring(int startidx, int endidx)
{
  if (endidx == -1)
    endidx = length();
  if (startidx > length() || startidx < 0 || endidx > length())
    THROW0(IndexOutOfBoundsException);
  return new String(c_str(), startidx,  endidx - startidx);
}

RString 
SubSSTstr(int startidx, int endidx)
{
  return substring(startidx, endidx);
}

RString 
String::toLowerCase()
{
  
  int len = length();
  char* buffer = new char[len + 1];
  for (int i = 0; i < len; i++) 
    buffer[i] = tolower(c_str()[i]);
  buffer[len] = 0;
  if (strcmp(buffer, c_str()) == 0) {
    delete[] buffer;
    return this;
  }
  return new String(len + 1, buffer);
}

RString 
String::toUpperCase()
{
  int len = length();
  char* buffer = new char[len + 1];
  for (int i = 0; i < len; i++) 
    buffer[i] = toupper(c_str()[i]);
  buffer[len] = 0;
  if (strcmp(buffer, c_str()) == 0) {
    delete[] buffer;
    return this;
  }
  return new String(len + 1, buffer);
}


RString 
String::trim(String::TrimSide trimside)
{
  int len = length();
  int st = 0;
  const char* ptr = c_str();
  if (trimside == TrimLeft || trimside  == TrimBoth) {
    while ((st < len) && (ptr[st] <= ' ')) 
        st++;
  }
  if (trimside == TrimRight || trimside  == TrimBoth) {
    while ((st < len) && (ptr[len - 1] <= ' ')) 
        len--;
  }
  if  ((st > 0) || (len < length()))
    return substring(st, len);
  return this;
}

RString 
String::trim(int side) 
{ 
  return trim(side == TrimBoth ? TrimBoth : (side == TrimRight ? TrimRight : TrimLeft)); 
}

RString 
String::replace(char oldChar, char newChar)
{
  char *d = const_cast<char*>(c_str());
  RString newString;
  for (int i = 0; i < length(); i++) {
    if (d[i] == oldChar) {
      if (newString == Nil) {
        newString = new String(c_str());
        d = const_cast<char*>(newString->c_str());
      }
      d[i] = newChar;
    }
  }
  return newString == Nil ? RString(const_cast<String*>(this)) : newString;
}

RString 
String::replace(RString find, RString repl)
{
  return replace(*find, *repl);
  
}


RString 
String::replace(String& find, String& repl)
{
  return replace(find.c_str(), repl.c_str());
}

RString 
String::replace(const char* find, const char* repl)
{
  RString str = this;
  int i = 0;
  int fpos;
  int flen = strlen(find);
  int replen = strlen(repl);
  //## optimierung wenn flen == replen
  while ((fpos = str->indexOf(find, i)) != -1) {
    StringBuffer sb(str->length() - flen + replen + 1);  
    sb.append(str->c_str(), fpos);
    sb.append(repl);
    sb.append(str->c_str() + fpos + flen);
    str = sb.toString();
    i = fpos + replen;
  }
  return str;
}

int 
String::elementCount(char c)
{
  const char* ptr = c_str();
  int count = 0;
  while (*ptr) {
    if (*ptr == c)
      ++count;
    ++ptr;
  }
  return count;
}

int 
String::elementCount(RString str)
{
  return elementCount(*str);
}

int 
String::elementCount(String& str)
{
  int count = 0;
  const char* ptr = c_str();
  const char* sptr = str.c_str();
  while ((ptr = strstr(ptr, sptr)) != 0) {
    ++ptr;
    ++count;
  }
  return count;
}

int 
String::elementCount(const char* str)
{
  String tstr(str);
  return elementCount(tstr);
}

char* 
String::c_strdup(sys::Allocator* alloc /* = 0 */)
{
  if (alloc == 0)
    alloc = allocator();
  char* nptr = (char*)alloc->allocate(length() + 1);
  memcpy(nptr, c_str(), length() + 1);
  return nptr;
}



void _foo()
{
  RObject o = new String("asdf");
  RString str = (RString)o;
  RString str2 = new String("asdf");
  str2->compareTo("asdf");
}


} // lang
} // acdk

#else
#if defined(__BORLANDC__)
#include "String.cpp"
#endif
#endif //defined(USE_OLD_STRING)


