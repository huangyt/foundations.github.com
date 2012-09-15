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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringInternals.h,v 1.21 2005/04/14 10:41:54 kommer Exp $

#ifndef acdk_lang_StringInternals_h
#define acdk_lang_StringInternals_h

#ifndef DOXYGENONLY

#include "UnicodeCharacter.h"
#include "StringIndexOutOfBoundsException.h"
#include <acdk/locale/AsciiEncoding.h>
#include <acdk/locale/UCS2Encoding.h>
#include <acdk/locale/UTF8Encoding.h>


ACDK_NO_METAINFO_HEADER // acdkmc should not parse this header


#define AS_CONVERSTRING(TargetChar, text, length) (::acdk::lang::StringUtf8Utils::convertTo((TargetChar*)core_alloca(length * sizeof(TargetChar)), text))
#define CONVSTRINGTO(string, chartype) (typeid(chartype)AS_CONVERSTRING(chartype, (string).begin(), (string).length()))
/**
  @param string reference 
  @param it byte start index
  @param end byte end index
*/
#define STR_BUFFER_TO_UC2(string, it, end) ((string).isCChar() == true ? (uc2char*)(it) : (uc2char*)::acdk::lang::StringUtf8Utils::convertTo((uc2char*)core_alloca(((end - it) + 1) * sizeof(uc2char)), it, end - it));

struct FixexCharMapper
{
  static uc4char toUc4Char(const char* ch) { return (uc4char)*ch; }
  static uc4char toUc4Char(const uc2char* ch) { return (uc4char)*ch; }
  static uc4char toUc4Char(const uc4char* ch) { return *ch; }
  
  static uc2char toUc2Char(const char* ch) { return (uc2char)*ch; }
  static uc2char toUc2Char(const uc2char* ch) { return *ch; }
  static uc2char toUc2Char(const uc4char* ch) { return *ch; }
  
  static char toAsciiChar(const char* ch) { return (char)*ch; }
  static char toAsciiChar(const uc2char* ch) { return *ch; }
  static char toAsciiChar(const uc4char* ch) { return *ch; }
  
  template <class T> T convertTo(char c) { return (T)c; } // conversion
  template <class T> T convertTo(uc2char c) { return (T)c; }// conversion

};

class UnmappableAction;
class CharIterator
{
public:
  typedef CharIterator ThisType;
  virtual ~CharIterator() {}
  virtual void incr(int dif = 1) = 0;
  virtual void decr(int dif = 1) = 0;
  virtual int diff(const CharIterator& other) const = 0;
  virtual bool lessThan(const CharIterator& other) const = 0;
  virtual char getAsciiChar() const  = 0;
  virtual uc4char getUc4Char() const = 0;
  virtual uc2char getUc2Char() const = 0;
  
};

class UnmappableAction
{
public:
  virtual ~UnmappableAction() {}
  virtual uc4char unmappable(uc4char ch) = 0;
};


template <typename T1, typename T2>
struct CharCastTrait
{
  typedef uc4char CharType;
};
template <>
struct CharCastTrait<char, char>
{
  typedef char CharType;
};
template <>
struct CharCastTrait<uc2char, uc2char>
{
  typedef uc2char CharType;
};

template <>
struct CharCastTrait<char, uc2char>
{
  typedef uc2char CharType;
};
template <>
struct CharCastTrait<uc2char, char>
{
  typedef uc2char CharType;
};
template <>
struct CharCastTrait<uc4char, uc4char>
{
  typedef uc4char CharType;
};
template <>
struct CharCastTrait<char, uc4char>
{
  typedef uc4char CharType;
};
template <>
struct CharCastTrait<uc4char, char>
{
  typedef uc4char CharType;
};
template <>
struct CharCastTrait<uc4char, uc2char>
{
  typedef uc4char CharType;
};
template <>
struct CharCastTrait<uc2char, uc4char>
{
  typedef uc4char CharType;
};


template <typename IT> struct CharacterIteratorTraits;


template <typename T>
class SimpleCharIterator
{
public:
  const String* _str;
  typedef T CharType;
  typedef CharType ThisElement;
  typedef SimpleCharIterator<CharType> ThisType;
  typedef ThisType IteratorType;

  const CharType* _ptr;
  inline SimpleCharIterator(const CharType* ptr) : _str(0), _ptr(ptr) {}
  inline SimpleCharIterator(const String* str, const byte* ptr) : _str(str), _ptr((const CharType*)ptr) {}
  inline SimpleCharIterator(const String& str, const byte* ptr) : _str(&str), _ptr((const CharType*)ptr) {}
  inline SimpleCharIterator(const String* str, const CharType* ptr) : _str(str), _ptr(ptr) {}
  inline SimpleCharIterator(const String& str, const CharType* ptr) : _str(&str), _ptr(ptr) {}

  inline ThisType& operator=(const ThisType& other)
  {
    _ptr = other._ptr;
    return *this;
  }
  inline ThisType operator++() { ++_ptr; return *this; }
  inline ThisType operator++(int) { ThisType t(*this); ++_ptr; return t; }
  inline ThisType operator--() { --_ptr; return *this; }
  inline ThisType operator--(int) { ThisType t(*this);  --_ptr; return t; }
  inline ThisType operator+(int offset) 
  { 
    if (offset < 0)
      return operator-(-offset);
    return ThisType(_str, _ptr + offset);
  }
  inline ThisType operator-(int offset) 
  {
    if (offset < 0)
      return operator+(-offset);
    return ThisType(_str, _ptr - offset);
  }
  inline bool operator<(const ThisType& other) const { return _ptr < other._ptr; }
  inline bool operator<=(const ThisType& other) const { return _ptr <= other._ptr; }
  inline bool operator>(const ThisType& other) const { return _ptr > other._ptr; }
  inline bool operator>=(const ThisType& other) const { return _ptr >= other._ptr; }
  inline bool operator==(const ThisType& other) const { return _ptr == other._ptr; }
  inline bool operator!=(const ThisType& other) const { return _ptr != other._ptr; }
    
  inline int operator-(const ThisType& other) const
  {
    return _ptr - other._ptr;
  }
  /**
    operator* should return ascii, uc2, or uc4
  */
  inline ThisElement operator*() const {  return *_ptr; }
  /// returns the byte position of this iterator
  inline byte* getByteIterator() const { return (byte*)_ptr; }
  inline uc2char getUcChar() const { return FixexCharMapper::toUc2Char(_ptr); }

  inline uc4char getUc4Char() const { return FixexCharMapper::toUc4Char(_ptr); }
  inline uc2char getUc2Char() const { return FixexCharMapper::toUc2Char(_ptr); }
  inline char getAsciiChar() const { return FixexCharMapper::toAsciiChar(_ptr); }
  //template <typename OC> void set(OC oc) { *_ptr = FixexCharMapper<OC> ::convertTo(oc); }
  //template <> void set<ThisElement>(ThisElement oc) { *_ptr = oc; }
  /**
    this it must be > than other
  */
  inline int getByteDiff(const ThisType& other) const { return operator-(other) * sizeof(ThisElement); }
  inline static int getMaxByteCount() { return sizeof(ThisElement); }
  inline static float getAverageByteCount() { return sizeof(ThisElement); }
  /**
    copy character to target buffer.
    Move the pointer to the next writing position.
    No checks will done. caller must ensure, that target has enough space
  */
  
  inline static void copyToBytes(byte*& target, CharType el) { *((ThisElement*)target) = el; target += sizeof(ThisElement); }
  inline void copyToBytes(byte*& target) { copyToBytes(target, *_ptr); }
  inline void set(CharType ct) { *const_cast<CharType*>(_ptr) = ct; }
  inline void setTerminator() { *const_cast<CharType*>(_ptr) = 0; }
  inline static void writeTerminator(byte* target) { *((ThisElement*)target) = 0; }
  inline static int getTerminatorByteSize() { return sizeof(CharType); }
  inline static int getStringFlags() { return CharacterIteratorTraits<IteratorType> ::StringFlags; }
  inline int equals(uc2char other, bool compareCase = true) const 
  { 
    return compareCase ? other == operator*() : UnicodeCharacter::toLowerCase(other) == UnicodeCharacter::toLowerCase(operator*());
  }
  inline static CharType toLower(CharType ct) { return CharacterIteratorTraits<IteratorType>::toLower(ct); }
  inline static CharType toUpper(CharType ct) { return CharacterIteratorTraits<IteratorType>::toUpper(ct); }
  inline const String* getString() const { return _str; }
  /**
    cast type from one type to another
    only upcast are allowed
    @todo handle Ansi different, using code table
  */
  template <typename OT> inline static ThisElement castTo(OT other) 
  { 
    return (ThisElement)other; 
  }
  template <typename OT> inline static bool isSameType(const OT& ) { return false; }
#if defined(__GNUG__) || defined(__BORLANDC__)
    inline static bool isSameType(const ThisType&) { return true; }
#else
  template <> inline static bool isSameType<ThisType>(const ThisType& ) { return true; }
#endif
  inline static acdk::locale::REncoding getEncoding() { return CharacterIteratorTraits<ThisType>::getEncoding(); }
};


class Utf8CharIterator
{
public:
  const String* _str;
  typedef uc2char CharType;
  typedef char CharItType;
  typedef CharType ThisElement;
  typedef Utf8CharIterator ThisType;
  typedef ThisType IteratorType;
  const CharItType* _ptr;
  
  inline Utf8CharIterator(const char* ptr) : _str(0), _ptr((const CharItType*)ptr) {}
  inline Utf8CharIterator(const String* str, const byte* ptr) : _str(str), _ptr((const CharItType*)ptr) {}
  inline Utf8CharIterator(const String& str, const byte* ptr) : _str(&str), _ptr((const CharItType*)ptr) {}
  inline Utf8CharIterator(const String* str, const char* ptr) : _str(str), _ptr((const CharItType*)ptr) {}
  inline Utf8CharIterator(const String& str, const char* ptr) : _str(&str), _ptr((const CharItType*)ptr) {}

  inline ThisType& operator=(const ThisType& other)
  {
    _ptr = other._ptr;
    return *this;
  }
  inline void incPtr() { StringUtf8Utils::incUtfPtr(_ptr, _ptr + 6); }
  inline void decPtr() { StringUtf8Utils::decUtfPtr(_ptr, _ptr + 6);  }
  ThisType& operator++() { incPtr(); return *this; }
  ThisType operator++(int) { ThisType t(*this); incPtr(); return t; }
  ThisType& operator--() { decPtr(); return *this; }
  ThisType operator--(int) { ThisType t(*this); decPtr(); return t; }
  ThisType operator+(int offset) 
  { 
    if (offset < 0)
      return operator-(-offset);
    ThisType t(*this);
    while (offset-- > 0)
      ++t;
    return t;
  }
  ThisType operator-(int offset) 
  {
    if (offset < 0)
      return operator+(-offset);
    Utf8CharIterator t(*this);
    while (offset++ < 0)
      --t;
    return t;
  }
  bool operator<(const ThisType& other) const { return _ptr < other._ptr; }
  bool operator<=(const ThisType& other) const { return _ptr <= other._ptr; }
  bool operator>(const ThisType& other) const { return _ptr > other._ptr; }
  bool operator>=(const ThisType& other) const { return _ptr >= other._ptr; }
  bool operator==(const ThisType& other) const { return _ptr == other._ptr; }
  bool operator!=(const ThisType& other) const { return _ptr != other._ptr; }
  
  int operator-(const ThisType& other) const
  {
    return StringUtf8Utils::utfDiff(_ptr, other._ptr);
  }
  
  /**
    operator* should return ascii, uc2, or uc4
  */
  inline ThisElement operator*() const {  return StringUtf8Utils::toWideChar(_ptr, _ptr + 6); }
  /// returns the byte position of this iterator
  inline byte* getByteIterator() const { return (byte*)_ptr; }
  inline uc2char getUcChar() const { return operator*(); }

  inline uc4char getUc4Char() const { return operator*(); }
  inline uc2char getUc2Char() const { return operator*(); }
  inline char getAsciiChar() const { return operator*(); }
  /**
    this it must be > than other
  */
  inline int getByteDiff(const ThisType& other) const { return StringUtf8Utils::utfDiff(_ptr, other._ptr); }
  inline static int getMaxByteCount() { return 6; }
  inline static float getAverageByteCount() { return 1.1; }
  /**
    copy character to target buffer.
    Move the pointer to the next writing position.
    No checks will done. caller must ensure, that target has enough space
  */
  
  inline static void copyToBytes(byte*& target, uc2char el) 
  { 
    StringUtf8Utils::writeUcToUtf8(target, target + 6, el);
  }
  inline void copyToBytes(byte*& target) { copyToBytes(target, *_ptr); }
  inline void set(uc2char ct) 
  { 
    byte* tptr = (byte*)_ptr;
    copyToBytes(tptr, ct);
  }
  inline void setTerminator() { *const_cast<CharItType*>(_ptr) = 0; }
  inline static void writeTerminator(byte* target) { *((ThisElement*)target) = 0; }
  inline static int getTerminatorByteSize() { return sizeof(char); }
  inline static int getStringFlags() { return CCUtf8; }
  inline int equals(uc2char other, bool compareCase = true) const 
  { 
    uc2char ch = getUc2Char();
    return compareCase ? other == ch : UnicodeCharacter::toLowerCase(other) == UnicodeCharacter::toLowerCase(ch);
  }
  inline static uc2char toLower(uc2char ct) { return UnicodeCharacter::toLowerCase(ct); }
  inline static CharType toUpper(CharType ct) { return UnicodeCharacter::toUpperCase(ct); }
  inline const String* getString() const { return _str; }
  /**
    cast type from one type to another
    only upcast are allowed
    @todo handle Ansi different, using code table
  */
  template <typename OT> inline static uc2char castTo(OT other) { return (ThisElement)other; }
  template <typename OT> inline static bool isSameType(const OT& ) { return false; }
#if defined(__GNUG__) || defined(__BORLANDC__)
  inline static bool isSameType(const uc2char&) { return true; }
#else
  template <> inline static bool isSameType<uc2char>(const uc2char& ) { return true; }
#endif
  inline static acdk::locale::REncoding getEncoding() { return acdk::locale::UTF8Encoding::getUTF8Encoding(); }
};

template <typename IT1, typename IT2>
struct IteratorCastTrait
{
  typedef IT1 IteratorType;
};

template <>
struct IteratorCastTrait<SimpleCharIterator<char>, SimpleCharIterator<char> >
{
  typedef SimpleCharIterator<char> IteratorType;
};
template <>
struct IteratorCastTrait<SimpleCharIterator<char>, SimpleCharIterator<uc2char> >
{
  typedef SimpleCharIterator<uc2char> IteratorType;
};
template <>
struct IteratorCastTrait<SimpleCharIterator<char>, SimpleCharIterator<uc4char> >
{
  typedef SimpleCharIterator<uc4char> IteratorType;
};
template <>
struct IteratorCastTrait<SimpleCharIterator<uc2char>, SimpleCharIterator<uc2char> >
{
  typedef SimpleCharIterator<uc2char> IteratorType;
};
template <>
struct IteratorCastTrait<SimpleCharIterator<uc2char>, SimpleCharIterator<uc4char> >
{
  typedef SimpleCharIterator<uc4char> IteratorType;
};

template <>
struct IteratorCastTrait<SimpleCharIterator<uc4char>, SimpleCharIterator<uc4char> >
{
  typedef SimpleCharIterator<uc4char> IteratorType;
};

template <typename IT>
struct CharacterIteratorTraits
{
  typedef  IT Iterator;
  typedef typename IT::CharType CharType;
  enum { StringFlags = 0 };
  inline static CharType toLower(CharType c) { return c; }
  inline static CharType toUpper(CharType c) { return c; }
  inline static acdk::locale::REncoding getEncoding() { return Nil; }
  inline static CharType* getEncodingReplacement(CharType* buffer, uc2char ch) 
  { 
    buffer[0] = 0;
    return buffer; 
  }
};

template <>
struct CharacterIteratorTraits<SimpleCharIterator<char> >
{
  typedef SimpleCharIterator<char> Iterator;
  typedef Iterator::CharType CharType;
  enum { StringFlags = CCAscii };
  inline static CharType toLower(CharType c) { return ::tolower(c); }
  inline static CharType toUpper(CharType c) { return ::toupper(c); }
  inline static acdk::locale::REncoding getEncoding() { return acdk::locale::AsciiEncoding::getAsciiEncoding(); }
  inline static CharType* getEncodingReplacement(CharType* buffer, uc2char ch) 
  { 
    strcpy(buffer, "?");
    return buffer; 
  }
  
};

template <>
struct CharacterIteratorTraits<SimpleCharIterator<uc2char> >
{
  typedef SimpleCharIterator<uc2char> Iterator;
  typedef Iterator::CharType CharType;
  enum { StringFlags = CCUcs2 };
  inline static CharType toLower(CharType c) { return UnicodeCharacter::toLowerCase(c); } 
  inline static CharType toUpper(CharType c) { return UnicodeCharacter::toUpperCase(c); }
  inline static acdk::locale::REncoding getEncoding() { return acdk::locale::UCS2Encoding::getUCS2NativeEncoding(); }
  inline static CharType* getEncodingReplacement(CharType* buffer, uc2char ch) 
  { 
    buffer[0] = '?'; buffer[1] = 0;
    return buffer; 
  }
};
template <>
struct CharacterIteratorTraits<SimpleCharIterator<uc4char> >
{
  typedef SimpleCharIterator<uc4char> Iterator;
  typedef Iterator::CharType CharType;
  enum { StringFlags = CCUcs4 };
  inline static CharType toLower(CharType c) { return UnicodeCharacter::toLowerCase((uc2char)c); } 
  inline static CharType toUpper(CharType c) { return UnicodeCharacter::toUpperCase((uc2char)c); }
  inline static acdk::locale::REncoding getEncoding() { return acdk::locale::UCS2Encoding::getUCS2NativeEncoding(); }
  inline static CharType* getEncodingReplacement(CharType* buffer, uc2char ch) 
  { 
    buffer[0] = '?'; buffer[1] = 0;
    return buffer; 
  }
};

template <>
struct CharacterIteratorTraits<Utf8CharIterator>
{
  typedef Utf8CharIterator Iterator;
  //typedef Iterator::CharType CharType;
  typedef uc2char CharType;
  enum { StringFlags = CCUtf8 };
  inline static CharType toLower(CharType c) { return UnicodeCharacter::toLowerCase((uc2char)c); } 
  inline static CharType toUpper(CharType c) { return UnicodeCharacter::toUpperCase((uc2char)c); }
  inline static acdk::locale::REncoding getEncoding() { return acdk::locale::UTF8Encoding::getUTF8Encoding(); }
  inline static CharType* getEncodingReplacement(CharType* buffer, uc2char ch) 
  { 
    buffer[0] = '?'; buffer[1] = 0;
    return buffer; 
  }
};

typedef SimpleCharIterator<uc4char> Uc4Iterator;
typedef SimpleCharIterator<uc2char> Uc2Iterator;
typedef SimpleCharIterator<char> AsciiIterator;



template <typename I1>
void unshareT(I1 begin1, I1 end1, const String& s, byte*& new_buf, byte*& new_end, int& newbuflen)
{
  int termbytesize = I1::getTerminatorByteSize();
  int contentbytesize = end1.getByteDiff(begin1);
  newbuflen = contentbytesize + termbytesize;
  new_buf = (byte*)const_cast<String&>(s).allocate(newbuflen, acdk::lang::sys::DumpBufferMem);
  memcpy(new_buf, begin1.getByteIterator(), contentbytesize);
  new_end = new_buf + contentbytesize;
  I1::writeTerminator(new_end);
}

template <typename I1>
void getByteIteratorT(I1 begin, I1 end, int startoffset, int endoffset, byte*& byte_begin, byte*& byte_end)
{
  byte_begin = (begin + startoffset).getByteIterator();
  byte_end = (end + endoffset).getByteIterator();
}

template <typename I1>
void 
initsubstrT(I1 begin1, I1 end1, const String& s, int startoffset, int length, byte*& buffer, int& buflen, byte*& buf_begin, byte*& buf_end, String::StringFlags& flags)
{
  s.addRef();
  buffer = (byte*)&s; 
  buflen = 0;
  buf_begin = (begin1 + startoffset).getByteIterator();
  if (length != -1)
    buf_end = (begin1 + startoffset + length).getByteIterator();
  else
    buf_end = end1.getByteIterator();
  flags = s.characterClass() | s.codePage() | SubSST;
}

template <typename I1>
void
charAtT(I1 begin1, I1 end1, int idx, uc2char& ret)
{
  if (idx >= end1 - begin1 || idx < 0)
    THROW3(StringIndexOutOfBoundsException, "String::charAt()", idx, end1 - begin1);
  ret = *(begin1 + idx);
}


/**
  search second in first
*/
template <typename I1, typename I2>
void startsWithT(I1 it1, I1 end1, I2 it2, I2 end2, bool& erg)
{
  erg = false;
  if (end1 - it1 < end2 - it2)
    return;
  for (; it2 < end2; ++it1, ++it2)
    if (it1.getUcChar() != it2.getUcChar())
      return;
  erg = true;
}

template <typename I1, typename I2>
void endsWithT(I1 it1, I1 end1, I2 it2, I2 end2, bool& erg)
{
  erg = false;
  if (end1 - it1 < end2 - it2)
    return;
  it1 = end1 - (end2 - it2);
  for (; it1 < end1; ++it1, ++it2)
    if (it1.getUcChar() != it2.getUcChar())
      return;
  erg = true;
}

template <typename I1, typename I2>
I1
findT(I1 it1, I1 end1, I2 it2, I2 end2)
{
  if (end1 - it1 < end2 - it2)
    return end1;
  I2 i = it2;
  I1 startf = end1;
  while (it1 < end1 && i < end2) 
  {
    if (i.getUcChar() != it1.getUcChar())
    {
      i = it2;
      if (startf != end1)
      {
        it1 = startf;
        startf = end1;
      }
    } else {
      if (startf == end1)
        startf = it1;
      ++i;
    }
    ++it1;
  }
  if (i != end2)
    return it1;
  return it1 - (end2 - it2);
}

template <typename I1>
void
isLowerT(I1 begin1, I1 end1, bool& erg)
{
  erg = false;
  typedef typename I1::CharType CharType;
  for (; begin1 < end1; ++begin1)
  {
    CharType t = *begin1;
    if (I1::toLower(t) != t)
      return;
  }
  erg = true;
}

template <typename I1>
void
isUpperT(I1 begin1, I1 end1, bool& erg)
{
  erg = false;
  typedef typename I1::CharType CharType;
  for (; begin1 < end1; ++begin1)
  {
    CharType t = *begin1;
    if (I1::toUpper(t) != t)
      return;
  }
  erg = true;
}

template <typename I1, typename I2>
I1
findrT(I1 it1, I1 end1, I2 it2, I2 end2)
{
  I1 realend1 = end1;
  I1 find1 = realend1;
  I2 spe2 = end2;

  while (spe2 > it2 && end1 > it1) 
  {
    --spe2;
    --end1;
    
    if (*spe2 != *end1)
    {
      spe2 = end2;
      if (find1 != realend1)
      {
        end1 = find1;
        find1 = realend1;
      }
    } 
    else 
    {
      if (find1 == realend1)
        find1 = end1;
    }
  }
  if (spe2 != it2) 
    return realend1;
  return end1;
}

template <typename I1, typename CT>
I1 
findCharT(I1 it1, I1 end1, CT ch)
{
  for (; it1 < end1; ++it1)
  {
    if (*it1 == ch)
      return it1;
  }
  return end1;
}

template <typename I1, typename CT>
I1 
findrCharT(I1 it1, I1 end1, CT ch)
{
  I1 it = end1 - 1;
  for (; it >= it1; --it)
  {
    if (*it == ch)
      return it;
  }
  return end1;
}


template <typename I1, typename CT>
void 
indexOfCharT(I1 it1, I1 end1, int offset, CT ch, int& erg)
{
  if (end1 - it1 < offset)
    THROW3(StringIndexOutOfBoundsException, "String::indexOf(). offset is out of range", offset, end1 - it1);
  it1 = it1 + offset;
  erg = -1;
  I1 f = findCharT(it1, end1, ch);
  if (f == end1)
    return;
  erg = f - it1 + offset;
}

template <typename I1, typename CT>
void
lastIndexOfCharT(I1 it1, I1 end1, int fromIdx, CT ch, int& erg)
{
  erg = -1;
  if (fromIdx != -1)
    end1 = it1 + fromIdx;
  I1 f = findrCharT(it1, end1, ch);
  if (f == end1)
    return;
  erg = f - it1;
}

template <typename I1, typename I2>
void
indexOfT(I1 begin1, I1 end1, I2 begin2, I2 end2, int& erg)
{
  erg = -1;
  I1 f = findT(begin1, end1, begin2, end2);
  if (f == end1)
    return;
  erg = f - begin1;
}

template <typename I1, typename I2>
void 
lastIndexOfT(I1 begin1, I1 end1, I2 begin2, I2 end2, int offset, int& ret)
{
  if (offset != -1)
    end1 = begin1 + offset;
  I1 f = findrT(begin1, end1, begin2, end2);
  if (f == end1)
    ret = -1;
  else
    ret = f - begin1;
}

template <typename I1, typename I2>
void elementCountT(I1 it1, I1 end1, I2 it2, I2 end2, int& erg)
{
  erg = 0;
  do {
    I1 fpos1 = findT(it1, end1, it2, end2);
    if (fpos1 == end1)
      return;
    it1 = fpos1 + 1;
    ++erg;
  } while (true);
}

template <typename I1>
void elementCountT(I1 it1, I1 end1, ucchar ch, int& erg)
{
  erg = 0;
  do {
    I1 fpos1 = findCharT(it1, end1, ch);
    if (fpos1 == end1)
      return;
    ++erg;
    it1 = fpos1 + 1;
  } while (true);
}

template <typename I1, typename I2>
void 
equalsT(I1 it1, I1 end1, I2 it2, I2 end2, bool& erg)
{
  erg = false;
  if (end1 - it1 != end2 - it2)
    return;
  for (; it1 < end1; ++it1, ++it2)
    if (*it1 != *it2)
      return;
  erg = true; 
}

template <typename I1, typename I2>
void 
equalsT(I1 it1, I1 end1, I2 it2, bool& erg)
{
  erg = false;
  for (; *it2 != 0 && it1 < end1; ++it1, ++it2)
    if (*it1 != *it2)
      return;
  if (*it2 != 0 || it1 != end1)
    return;
  erg = true; 
}

template <typename I1, typename I2>
void equalsIgnoreCaseT(I1 it1, I1 end1, I2 it2, I2 end2, bool& erg)
{
  erg = false;
  if (end1 - it1 != end2 - it2)
    return;
  for (; it1 < end1; ++it1, ++it2)
    if (UnicodeCharacter::toLowerCase(*it1) != UnicodeCharacter::toLowerCase(*it2))
      return;
  erg = true; 
}

template <typename I1, typename I2>
void 
equalsIgnoreCaseT(I1 it1, I1 end1, I2 it2, bool& erg)
{
  erg = false;
  for (; *it2 != 0 && it1 < end1; ++it1, ++it2)
   if (UnicodeCharacter::toLowerCase(*it1) != UnicodeCharacter::toLowerCase(*it2))
      return;
  if (*it2 != 0 || it1 != end1)
    return;
  erg = true; 
}

template <typename I1, typename I2>
void compareToT(I1 it1, I1 end1, I2 it2, I2 end2, int& erg)
{
 
  for (; it1 < end1 && it2 < end2; ++it1, ++it2)
  {
    erg = UnicodeCharacter::compareTo(*it1, *it2);
    if (erg != 0)
      return;
  }
  if (it1 != end1)
    erg = 1;
  else if (it2 != end2)
    erg = -1;
  else
    erg = 0; 
}

template <typename I1, typename I2>
void compareToIgnoreCaseT(I1 it1, I1 end1, I2 it2, I2 end2, int& erg)
{
  for (; it1 < end1 && it2 < end2; ++it1, ++it2)
  {
    erg = UnicodeCharacter::compareToIgnoreCase(*it1, *it2);
    if (erg != 0)
      return;
  }
  if (it1 != end1)
    erg = 1;
  else if (it2 != end2)
    erg = -1;
  else
    erg = 0; 
}

template <typename I1, typename I2>
void regionMatchesT(I1 it1, I1 end1, I2 it2, I2 end2, int firstOffset, int secondOffset, int len, bool ignoreCase, bool& erg)
{
  if (firstOffset > end1 - it1)
    THROW3(StringIndexOutOfBoundsException, "String::regionMatches(). first index is out of range", firstOffset, end1 - it1);
  if (secondOffset + len > end2 - it2)
    THROW3(StringIndexOutOfBoundsException, "String::regionMatches(). second index/length is out of range", secondOffset + len , end2 - it2);
  it1 = it1 + firstOffset;
  it2 = it2 + secondOffset;
  end2 = it2 + len;
  erg = false;
  if (end1 - it1 < end2 - it2)
    return;
  if (ignoreCase == true)
    equalsIgnoreCaseT(it1, end1, it2, end2, erg);
  else
    equalsT(it1, end1, it2, end2, erg);
}

template <typename I>
size_t byteOffset(I it, int logoffset) 
{ 
  return (it + logoffset).getByteDiff(it); 
}

template <typename I>
void copyT(byte*& buffer, I it, I end)
{
  for (; it < end; ++it)
  {
    it.copyToBytes(buffer);
  }
}

/** 
  only valid for fixed char size buffer 
  moves chars from left to write
  @param right target position 
*/
template <typename I>
void moveTo(I begin, I end, int right)
{
  I target = end + right - 1;
  I source = end - 1;
  for (; source >= begin; --source, --target)
  {
    target.set(*source);
  }
}
#define MOVE_TO_T(I, begin, end, right) \
do { \
  I target = end + right - 1; \
  I source = end - 1; \
  for (; source >= begin; --source, --target) \
  { \
    target.set(*source); \
  } \
} while(false)

template <typename I>
void initT(const String& s, I it, I end, byte*& buf_begin, byte*& buf_end)
{
  int size = end.getByteDiff(it) + I::getMaxByteCount();
  buf_begin = (byte*)const_cast<String&>(s).allocate(size, acdk::lang::sys::DumpBufferMem);
  buf_end = buf_begin;
  copyT(buf_end, it, end);
  end.writeTerminator(buf_end);
}

template <typename I1>
void substrT(I1 begin1, I1 end1, const String& s, int startidx, int endidx, RString& ret)
{
  int charlen = end1 - begin1;
  if (startidx == 0 && endidx == charlen)
  {
    ret = &s;
    return;
  }
  if (startidx > charlen)
    THROW3(StringIndexOutOfBoundsException, "SubSSTstr(). startidx is out of range: String=[" + RString(&s) + "]; ", startidx, charlen);
  if (endidx > charlen)
    THROW3(StringIndexOutOfBoundsException, "SubSSTstr(). endidx is out of range: String=[" + RString(&s) + "]; ", endidx, charlen);
  if (endidx >= 0)
    end1 = begin1 + endidx;
  begin1 = begin1 + startidx;
  String* sptr = const_cast<String*>(&s);
  ret = new (sptr->allocator()) String(const_cast<String*>(s._getSubstrBase()), 
                    begin1.getByteIterator(), end1.getByteIterator());
}

template <typename I1, typename I2>
void copyCastedT(byte*& target, I1 it, I1 end, I2 dummy)
{
  for (; it < end; ++it)
  {
    I2::copyToBytes(target, I2::castTo(*it));
  }
}

template <typename I1, typename I2>
void concatT(I1 begin1, I1 end1, I2 begin2, I2 end2, const String& s, byte*& buf_begin, byte*& buf_end, int& flags)
{
  typedef typename IteratorCastTrait< I1, I2>::IteratorType IteratorType;
  typedef typename IteratorType::CharType CharType;
  int newcharsize = (end1 - begin1) + (end2 - begin2);
  int newbufsize = newcharsize * IteratorType::getMaxByteCount() + IteratorType::getTerminatorByteSize();
  buf_begin = (byte*)const_cast<String&>(s).allocate(newbufsize, acdk::lang::sys::DumpBufferMem);
  buf_end = buf_begin;
  IteratorType dummy(0, (const byte*)0);
  copyCastedT(buf_end, begin1, end1, dummy);
  copyCastedT(buf_end, begin2, end2, dummy);
  IteratorType::writeTerminator(buf_end);
  flags = CharacterIteratorTraits<IteratorType>::StringFlags;
}

/**
  converts a string form I1 to I2
*/
template <typename I1, typename I2>
void convertT(I1 begin1, I1 end1, const String& s, I2 dummy, byte*& buf_begin, byte*& buf_end, int& flags)
{

  typedef I2 IteratorType;
  typedef typename IteratorType::CharType CharType;
  int newcharsize = (end1 - begin1);
  int newbufsize = newcharsize * IteratorType::getMaxByteCount() + IteratorType::getTerminatorByteSize();
  buf_begin = (byte*)const_cast<String&>(s).allocate(newbufsize, acdk::lang::sys::DumpBufferMem);
  buf_end = buf_begin;
  copyCastedT(buf_end, begin1, end1, dummy);
  IteratorType::writeTerminator(buf_end);
  flags = CharacterIteratorTraits<IteratorType>::StringFlags;
}

template <typename I1, typename I2>
int 
findCountT(I1 begin1, I1 end1, I2 begin2, I2 end2)
{
  int count = 0;
  I1 f1 = begin1;
  do {
    f1 = findT(f1, end1, begin2, end2);
    if (f1 == end1)
      return count;
    ++count;
    f1 = f1 + (end2 - begin2);
  } while (f1 != end1);
  return count;
}


/**
  1: this
  2: search
  3: replace
*/
template <typename I1, typename I2, typename I3>
void 
replaceT(I1 begin1, I1 end1, I2 begin2, I2 end2, I3 begin3, I3 end3, const String& s, byte*& buf_begin, byte*& buf_end, int& flags)
{
  int foundcount = findCountT(begin1, end1, begin2, end2);
  if (foundcount == 0)
  {
    buf_begin = 0;
    return;
  }
  typedef typename IteratorCastTrait<I1, I2>::IteratorType TempItType;
  typedef typename IteratorCastTrait<TempItType, I3>::IteratorType IteratorType;
  typedef typename IteratorType::CharType CharType;
  int diffindrepl = (end2 - begin2) + (end3 - begin3);
  int difcharsize = diffindrepl * foundcount;
    
  int newcharsize = (end1 - begin1) + difcharsize;
  int newbufsize = newcharsize * IteratorType::getMaxByteCount() + IteratorType::getTerminatorByteSize();
  buf_begin = (byte*)const_cast<String&>(s).allocate(newbufsize, acdk::lang::sys::DumpBufferMem);
  buf_end = buf_begin;
  IteratorType dummy(0, (const byte*)0);
  I1 f1 = begin1;
  do {
    I1 lastf1 = f1;
    f1 = findT(f1, end1, begin2, end2);
    if (f1 == end1)
    {
      copyCastedT(buf_end, lastf1, end1, dummy);
      break;
    }
    copyCastedT(buf_end, lastf1, f1, dummy);
    copyCastedT(buf_end, begin3, end3, dummy);
    f1 = f1 + (end2 - begin2);
  } while (true);
  
  IteratorType::writeTerminator(buf_end);
  flags = CharacterIteratorTraits<IteratorType>::StringFlags;
}


template <typename I1, typename I2>
void 
replaceByIndexT(I1 begin1, I1 end1, I2 begin2, I2 end2, const String& thisstr, int startidx, int endidx, RString& erg)
{
  int chlen = end1 - begin1;
  //if (startidx < 0 || startidx >= chlen || endidx > chlen)
  if (startidx < 0 || startidx >= chlen)
    THROW3(StringIndexOutOfBoundsException, "String::replace()", startidx, chlen);
  if (endidx > chlen)
    THROW3(StringIndexOutOfBoundsException, "String::replace()", endidx, chlen);

  int newcharlen = chlen - (endidx - startidx) + (end2 - begin2);

  typedef typename IteratorCastTrait<I1, I2>::IteratorType IteratorType;
  typedef typename IteratorType::CharType CharType;
  int newbufsize = newcharlen * IteratorType::getMaxByteCount() + IteratorType::getTerminatorByteSize();
  byte* buf_begin = (byte*)const_cast<String&>(thisstr).allocate(newbufsize, acdk::lang::sys::DumpBufferMem);
  byte* buf_end = buf_begin;
  IteratorType dummy(0, (const byte*)0);
  copyCastedT(buf_end, begin1, begin1 + startidx, dummy);
  copyCastedT(buf_end, begin2, end2, dummy);
  copyCastedT(buf_end, begin1 + endidx, end1, dummy);
  IteratorType::writeTerminator(buf_end);
  int nflags = CharacterIteratorTraits<IteratorType>::StringFlags;
  String* sptr = const_cast<String*>(&thisstr);
  erg = new (sptr->allocator()) String(buf_begin, buf_end - buf_begin, buf_begin, buf_end, NormalSST | nflags);
}


template <typename I1, typename CT>
void 
replaceCharT(I1 begin1, I1 end1, const String& s, CT search, CT replace, byte*& buf_begin, byte*& buf_end, int& flags)
{
  typedef I1 IteratorType;
  buf_begin = 0;
  if (findCharT(begin1, end1, search) == end1)
    return;
  int newbufsize = ((end1 - begin1) + 1) * IteratorType::getMaxByteCount();
  buf_begin = (byte*)const_cast<String&>(s).allocate(newbufsize, acdk::lang::sys::DumpBufferMem);
  buf_end = buf_begin;
  
  I1 it = begin1;
   for (; it < end1; ++it)
  {
    if (*it == search)
      IteratorType::copyToBytes(buf_end, replace);
    else
      it.copyToBytes(buf_end);
  }
  IteratorType::writeTerminator(buf_end);
  flags = CharacterIteratorTraits<IteratorType>::StringFlags;
}

template <typename I1, typename ConvFunc>
void convertToT(ConvFunc convfunc, const String& s, I1 begin, I1 end, byte*& buf_begin, byte*& buf_end, int& flags)
{
  buf_begin = 0;
  typedef typename I1::CharType CharType;
restart:
  for (I1 it = begin; it < end; ++it)
  {
    CharType t = convfunc(*it);
    if (buf_begin == 0)
    {
      if (*it != t)
      {
        buf_begin = (byte*)const_cast<String&>(s).allocate(I1::getMaxByteCount() * ((end - begin) + 1), acdk::lang::sys::DumpBufferMem);
        buf_end = buf_begin;
        for (I1 it2 = begin; it2 < it; ++it2)
          I1::copyToBytes(buf_end, *it2);
      }  
    }
    if (buf_begin != 0)
      I1::copyToBytes(buf_end, t);
  }
  if (buf_begin != 0)
    I1::writeTerminator(buf_end);
  flags = I1::getStringFlags();
}

template <typename I1>
void capacityT(I1 begin, I1 end, int& erg)
{
  erg = end - end;
}


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

template <typename I>
void trimT(I begin, I end, const String& s, RString& ret, int trimflags)
{
  I b = begin;
  I e = end;
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
      if (dotrim(*e, trimflags) == false) 
      {
        ++e;
        break;
      }
    }
  }
  if (b == begin && e == end)
    ret = const_cast<String*>(&s);
  else
  {
   String* sptr = const_cast<String*>(&s);
    //ret = new (sptr->allocator()) String(*s._getSubstrBase()/*stringFlags() & SubSST ? *reinterpret_cast<String*>(s.buffer()) : *sptr*/, b - begin, e - b);
   ret = new (sptr->allocator()) String(sptr->_getSubstrBase(), b.getByteIterator(), e.getByteIterator());
  }
}

template <typename I1>
void sb_removeT(I1 begin, I1 end, int startpos, int endpos, byte*& buf_end)
{
  I1 lit = begin + startpos;
  I1 rit = begin + endpos;

  for (;rit < end; ++lit, ++rit) 
  {
    lit.set(*rit);
  }
  lit.setTerminator();
  buf_end = lit.getByteIterator();
}

template <typename I1>
void sb_setCharAtT(I1 begin, I1 end, int pos, typename I1::CharType ch)
{
  I1 lit = begin + pos;
  lit.set(ch);
}

template <typename I1>
void sb_setTerminatorAtT(I1 begin, I1 end, int pos, byte*& buf_it)
{
  I1 lit = begin + pos;
  lit.setTerminator();
  buf_it = lit.getByteIterator();
}

/**
  returns new buffer if size doest fit, String wrong storage type has or
  iterator not have same type
  doesn't copy any
*/
template <typename I1, typename I2>
void sb_unshareT(I1 begin1, I1 end1, I2 begin2, I2 end2, const String& s, byte*& buf_begin, int& nbytebuflen, int& flags, bool& shared)
{
  typedef typename IteratorCastTrait< I1, I2>::IteratorType IteratorType;
  typedef typename IteratorType::CharType CharType;
  int chlen = (end1 - begin1) + (end2 - begin2) + 1;
  int buflen = IteratorType::getMaxByteCount() * chlen;
  if (shared == true ||
     (s.stringFlags() & NormalSST) == false ||
      buflen > s.bufferLength() ||
      // I1::isSameType(begin2) == false
      IteratorType::isSameType(begin1) == false
      )
  {
    nbytebuflen = buflen < 1024 ? 1024 : buflen * 2;
    buf_begin = (byte*)const_cast<String&>(s).allocate(nbytebuflen, acdk::lang::sys::DumpBufferMem);
    flags = IteratorType::getStringFlags() | NormalSST;
    shared = false;
  }
}


template <typename I1, typename I2>
void sb_insertT(I1 begin1, I1 end1, I2 begin2, I2 end2, RString& s, int inspos, bool& shared)
{
  typedef typename IteratorCastTrait< I1, I2>::IteratorType IteratorType;
  typedef typename IteratorType::CharType CharType;
  byte* new_buf = 0;
  int new_buflen = 0;
  int new_flags = 0;

  sb_unshareT(begin1, end1, begin2, end2, *s, new_buf, new_buflen, new_flags, shared);

  if (new_buf != 0)
  {
    byte* new_end = new_buf;
    IteratorType dummy(&s, (byte*)0);
    copyCastedT(new_end, begin1, begin1 + inspos, dummy);
    copyCastedT(new_end, begin2, end2, dummy);
    copyCastedT(new_end, begin1 + inspos, end1, dummy);
    IteratorType::writeTerminator(new_end);
    s = new (s->allocator()) String(new_buf, new_buflen, new_buf, new_end, new_flags);
  }

  else
  {
    // move from insert pos to end
    moveTo(begin1 + inspos, end1, end2 - begin2);
    I1 insit = begin1 + inspos;
    byte* byteit = insit.getByteIterator();
    copyCastedT(byteit, begin2, end2, insit);
    I1 newend = end1 + (end2 - begin2);
    newend.setTerminator();
    s->_setEnd(newend.getByteIterator());
  }
}

#define SB_INSERT_T(I1, I2, begin1, end1, begin2, end2, s, inspos, shared) \
do { \
  typedef IteratorCastTrait< I1, I2>::IteratorType IteratorType; \
  typedef IteratorType::CharType CharType; \
  byte* new_buf = 0; \
  int new_buflen = 0; \
  int new_flags = 0; \
  sb_unshareT(begin1, end1, begin2, end2, *s, new_buf, new_buflen, new_flags, shared); \
  if (new_buf != 0) \
  { \
    byte* new_end = new_buf; \
    IteratorType dummy(&s, (byte*)0); \
    copyCastedT(new_end, begin1, begin1 + inspos, dummy); \
    copyCastedT(new_end, begin2, end2, dummy); \
    copyCastedT(new_end, begin1 + inspos, end1, dummy); \
    IteratorType::writeTerminator(new_end); \
    s = new (s->allocator()) String(new_buf, new_buflen, new_buf, new_end, new_flags); \
  } \
  else \
  { \
    moveTo(begin1 + inspos, end1, end2 - begin2); \
    I1 insit = begin1 + inspos; \
    byte* byteit = insit.getByteIterator(); \
    copyCastedT(byteit, begin2, end2, insit); \
    I1 newend = end1 + (end2 - begin2); \
    newend.setTerminator(); \
    s->_setEnd(newend.getByteIterator()); \
  } \
} while(false)


#define SB_APPEND_T(I1, I2, begin1, end1, begin2, end2, s, shared) \
do { \
  typedef IteratorCastTrait< I1, I2>::IteratorType IteratorType; \
  typedef IteratorType::CharType CharType; \
   byte* new_buf = 0; \
  int new_buflen = 0; \
  int new_flags = 0; \
  sb_unshareT(begin1, end1, begin2, end2, *s, new_buf, new_buflen, new_flags, shared); \
  if (new_buf != 0) \
  { \
    byte* new_end = new_buf; \
    IteratorType dummy(*s, (byte*)0); \
    copyCastedT(new_end, begin1, end1, dummy); \
    copyCastedT(new_end, begin2, end2, dummy); \
    IteratorType::writeTerminator(new_end); \
    s = new (s->allocator()) String(new_buf, new_buflen, new_buf, new_end, new_flags); \
  } \
  else \
  { \
    IteratorType dummy(*s, (byte*)0); \
    byte* new_end = end1.getByteIterator(); \
    copyCastedT(new_end, begin2, end2, dummy); \
    IteratorType::writeTerminator(new_end); \
    s->setEnd(new_end); \
  } \
} while(false)


template <typename I1, typename I2>
void sb_appendT(I1 begin1, I1 end1, I2 begin2, I2 end2, ::acdk::lang::RString& s, bool& shared)
{
  typedef typename IteratorCastTrait< I1, I2>::IteratorType IteratorType;
  typedef typename IteratorType::CharType CharType;
   byte* new_buf = 0;
  int new_buflen = 0;
  int new_flags = 0;
  
  sb_unshareT(begin1, end1, begin2, end2, *s, new_buf, new_buflen, new_flags, shared);
  
  if (new_buf != 0)
  {
    byte* new_end = new_buf;
    IteratorType dummy(*s, (byte*)0);
    copyCastedT(new_end, begin1, end1, dummy);
    copyCastedT(new_end, begin2, end2, dummy);
    IteratorType::writeTerminator(new_end);
    s = new (s->allocator()) String(new_buf, new_buflen, new_buf, new_end, new_flags);
  }
  else
  {
    IteratorType dummy(*s, (byte*)0);
    byte* new_end = end1.getByteIterator();
    copyCastedT(new_end, begin2, end2, dummy);
    IteratorType::writeTerminator(new_end);
    s->setEnd(new_end);
  }
}

template <typename I1>
void sb_reverseT(I1 begin1, I1 end1)
{
  typedef typename I1::CharType CharType;
  I1 it = begin1;
  I1 retit = end1 - 1;
  CharType sic;
  for (; it < retit; ++it, --retit)
  {
    sic = *it;
    it.set(*retit);
    retit.set(sic);
  }
}

template <typename I1>
void toLowerCaseT(I1 begin, I1 end, const String& s, byte*& buf_begin, byte*& buf_end, int& flags)
{
  convertToT(I1::toLower, s, begin, end, buf_begin, buf_end, flags);
}
template <typename I1>
void toUpperCaseT(I1 begin, I1 end, const String& s, byte*& buf_begin, byte*& buf_end, int& flags)
{
  convertToT(I1::toUpper, s, begin, end, buf_begin, buf_end, flags);
}






#define STRING_THROW_INCOMPATIBLE_CHARTYPE(string) \
  String::_throwIncompatibleString(string)

#define DISPATCH10(call, first, fb, fe) \
if ((first).isCChar() == true) \
  call(AsciiIterator(first, fb), AsciiIterator(first, fe)); \
else if ((first).isUc2Char() == true) \
  call(Uc2Iterator(first, fb), Uc2Iterator(first, fe)); \
else if ((first).isUtf8Char() == true) \
  call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe)); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);


#define DISPATCH11(call, first, fb, fe, A) \
if ((first).isCChar() == true) \
  call(AsciiIterator(first, fb), AsciiIterator(first, fe), A); \
else if ((first).isUc2Char() == true) \
  call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), A); \
else if ((first).isUtf8Char() == true) \
  call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), A); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

#define DISPATCH12(call, first, fb, fe, A, B) \
if ((first).isCChar() == true) \
  call(AsciiIterator(first, fb), AsciiIterator(first, fe), A, B); \
else if ((first).isUc2Char() == true) \
  call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), A, B); \
else if ((first).isUtf8Char() == true) \
  call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), A, B); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

#define DISPATCH13(call, first, fb, fe, A, B, C) \
if ((first).isCChar() == true) \
  call(AsciiIterator(first, fb), AsciiIterator(first, fe), A, B, C); \
else if ((first).isUc2Char() == true) \
  call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), A, B, C); \
else if ((first).isUtf8Char() == true) \
  call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), A, B, C); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

#define DISPATCH14(call, first, fb, fe, A, B, C, D) \
if ((first).isCChar() == true) \
  call(AsciiIterator(first, fb), AsciiIterator(first, fe), A, B, C, D); \
else if ((first).isUc2Char() == true) \
  call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), A, B, C, D); \
else if ((first).isUtf8Char() == true) \
  call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), A, B, C, D); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

#define DISPATCH15(call, first, fb, fe, A, B, C, D, E) \
if ((first).isCChar() == true) \
  call(AsciiIterator(first, fb), AsciiIterator(first, fe), A, B, C, D, E); \
else if ((first).isUc2Char() == true) \
  call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), A, B, C, D, E); \
else if ((first).isUtf8Char() == true) \
  call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), A, B, C, D, E); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

#define DISPATCH16(call, first, fb, fe, A, B, C, D, E, F) \
if ((first).isCChar() == true) \
  call(AsciiIterator(first, fb), AsciiIterator(first, fe), A, B, C, D, E, F); \
else if ((first).isUc2Char() == true) \
  call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), A, B, C, D, E, F); \
else if ((first).isUtf8Char() == true) \
  call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), A, B, C, D, E, F); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);


#define DISPATCH18(call, first, fb, fe, A, B, C, D, E, F, G, H) \
if ((first).isCChar() == true) \
  call(AsciiIterator(first, fb), AsciiIterator(first, fe), A, B, C, D, E, F, G, H); \
else if ((first).isUc2Char() == true) \
  call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), A, B, C, D, E, F, G, H); \
else if ((first).isUtf8Char() == true) \
  call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), A, B, C, D, E, F, G, H); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

/*
#define DISPATCHS13(call, A, first, fb, fe, B, C, D) \
if ((first).isCChar() == true) \
  call(A, AsciiIterator(first, fb), AsciiIterator(first, fe), B, C, D); \
else if ((first).isUc2Char() == true) \
  call(A, Uc2Iterator(first, fb), Uc2Iterator(first, fe), B, C, D); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

#define DISPATCHS14(call, A, first, fb, fe, B, C, D, E) \
if ((first).isCChar() == true) \
{ \
  call(A, AsciiIterator(first, fb), AsciiIterator(first, fe), B, C, D, E); \
} \
else if ((first).isUc2Char() == true) \
{ \
  call(A, Uc2Iterator(first, fb), Uc2Iterator(first, fe), B, C, D, E); \
} \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

#define DISPATCHS15(call, A, first, fb, fe, B, C, D, E, F) \
if ((first).isCChar() == true) \
  call(A, AsciiIterator(first, fb), AsciiIterator(first, fe), B, C, D, E, F); \
else if ((first).isUc2Char() == true) \
  call(A, Uc2Iterator(first, fb), Uc2Iterator(first, fe), B, C, D, E, F); \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);
*/


#define DISPATCH21(call, first, fb, fe, second, sb, se, A) \
if ((first).isCChar() == true) \
{ \
  if ((second).isCChar() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A); \
  else if ((second).isUc2Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A); \
  else if ((second).isUtf8Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUc2Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A); \
  else if ((second).isUc2Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A); \
  else if ((second).isUtf8Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUtf8Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A); \
  else if ((second).isUc2Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A); \
  else if ((second).isUtf8Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);


#define DISPATCH22(call, first, fb, fe, second, sb, se, A, B) \
if ((first).isCChar() == true) \
{ \
  if ((second).isCChar() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B); \
  else if ((second).isUc2Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B); \
  else if ((second).isUtf8Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUc2Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B); \
  else if ((second).isUc2Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B); \
   else if ((second).isUtf8Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUtf8Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B); \
  else if ((second).isUc2Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B); \
   else if ((second).isUtf8Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);



#define DISPATCH23(call, first, fb, fe, second, sb, se, A, B, C) \
if ((first).isCChar() == true) \
{ \
  if ((second).isCChar() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C); \
  else if ((second).isUc2Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C); \
  else if ((second).isUtf8Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUc2Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C); \
  else if ((second).isUc2Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C); \
  else if ((second).isUtf8Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUc2Char() == true) \
{ \
  if ((second).isUtf8Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C); \
  else if ((second).isUc2Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C); \
  else if ((second).isUtf8Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);


#define DISPATCH24(call, first, fb, fe, second, sb, se, A, B, C, D) \
if ((first).isCChar() == true) \
{ \
  if ((second).isCChar() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C, D); \
  else if ((second).isUc2Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C, D); \
  else if ((second).isUtf8Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C, D); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUc2Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C, D); \
  else if ((second).isUc2Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C, D); \
  else if ((second).isUtf8Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C, D); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUtf8Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C, D); \
  else if ((second).isUc2Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C, D); \
  else if ((second).isUtf8Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C, D); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);


#define DISPATCH25(call, first, fb, fe, second, sb, se, A, B, C, D, E) \
if ((first).isCChar() == true) \
{ \
  if ((second).isCChar() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C, D, E); \
  else if ((second).isUc2Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C, D, E); \
  else if ((second).isUtf8Char() == true) \
    call(AsciiIterator(first, fb), AsciiIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C, D, E); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUc2Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C, D, E); \
  else if ((second).isUc2Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C, D, E); \
  else if ((second).isUtf8Char() == true) \
    call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C, D, E); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUtf8Char() == true) \
{ \
  if ((second).isCChar() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), A, B, C, D, E); \
  else if ((second).isUc2Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), A, B, C, D, E); \
  else if ((second).isUtf8Char() == true) \
    call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), A, B, C, D, E); \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);


// call(a, it1, it1, it2, it2, it3, it3, b, c, d)
#define DISPATCH34(call, first, fb, fe, second, sb, se, third, tb, te, A, B, C, D) \
if ((first).isCChar() == true) \
{ \
  if ((second).isCChar() == true) \
  { \
     if ((third).isCChar() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
     else if ((third).isUc2Char() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
     else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else if ((second).isUc2Char() == true) \
  { \
    if ((third).isCChar() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
    else if ((third).isUc2Char() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
    else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else if ((second).isUtf8Char() == true) \
  { \
    if ((third).isCChar() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
    else if ((third).isUc2Char() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(AsciiIterator(first, fb), AsciiIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
    else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUc2Char() == true) \
{ \
  if ((second).isCChar() == true) \
  { \
    if ((third).isCChar() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
    else if ((third).isUc2Char() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
    else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else if ((second).isUc2Char() == true) \
  { \
    if ((third).isCChar() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
    else if ((third).isUc2Char() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
    else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else if ((second).isUtf8Char() == true) \
  { \
    if ((third).isCChar() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
    else if ((third).isUc2Char() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(Uc2Iterator(first, fb), Uc2Iterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
    else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else if ((first).isUtf8Char() == true) \
{ \
  if ((second).isCChar() == true) \
  { \
    if ((third).isCChar() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
    else if ((third).isUc2Char() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), AsciiIterator(second, sb), AsciiIterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
    else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else if ((second).isUc2Char() == true) \
  { \
    if ((third).isCChar() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
    else if ((third).isUc2Char() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Uc2Iterator(second, sb), Uc2Iterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
    else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else if ((second).isUtf8Char() == true) \
  { \
    if ((third).isCChar() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), AsciiIterator(third, tb), AsciiIterator(third, te), A, B, C, D); \
    else if ((third).isUc2Char() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), Uc2Iterator(third, tb), Uc2Iterator(third, te), A, B, C, D); \
    else if ((third).isUtf8Char() == true) \
      call(Utf8CharIterator(first, fb), Utf8CharIterator(first, fe), Utf8CharIterator(second, sb), Utf8CharIterator(second, se), Utf8CharIterator(third, tb), Utf8CharIterator(third, te), A, B, C, D); \
    else STRING_THROW_INCOMPATIBLE_CHARTYPE(third); \
  } \
  else STRING_THROW_INCOMPATIBLE_CHARTYPE(second); \
} \
else STRING_THROW_INCOMPATIBLE_CHARTYPE(first);

#endif //DOXYGENONLY
#endif //acdk_lang_StringInternals_h

