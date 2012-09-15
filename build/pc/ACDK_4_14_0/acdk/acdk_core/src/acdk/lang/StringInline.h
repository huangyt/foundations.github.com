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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringInline.h,v 1.34 2005/04/25 13:19:14 kommer Exp $

#ifndef acdk_lang_StringInline_h
#define acdk_lang_StringInline_h

#if defined(ACDK_OS_DARWIN) || defined(ACDK_OS_CYGWIN32)
#ifdef tolower 
# undef tolower
#endif
#ifdef toupper
# undef toupper
#endif
extern "C" int tolower(int c);
extern "C" int toupper(int c);

#endif // ACDK_OS_DARWIN



namespace acdk {
namespace lang {

inline
String::StringIterator::StringIterator(IN(RString) str, int offset) 
: _str(&str)
, _idx(offset) 
, _cit(0)
, _uccit(0)
{
  _initPtrIt();
}

inline 
void 
String::StringIterator::_initPtrIt()
{
  if (_str->characterClass() == CCAscii)
    _cit = (char*)_str->byte_begin();
  else if (_str->characterClass() == CCUcs2)
    _uccit = (uc2char*)_str->byte_begin();
}

inline
RString 
String::StringIterator::substr(StringIterator& other) const 
{ 
  return _str->substr(_idx, other._idx); 
}



inline
String::String(IN(RString) str, int start/* = 0*/, int length/* = -1*/) 
: Object()
, _stringFlags(UnspecSST)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _init(*str, start, length);
}

inline
String::String(const String& str, int start/* = 0*/, int length/* = -1*/)
: Object()
, _stringFlags(UnspecSST)
, _buffer(0)
, _bufferLength(0)
, _begin(0)
, _end(0)
, _hashCode(-1)
{
  _init(str, start, length);
}

inline
String::String(const StaticAsciiLiteral& strLit)
: Object()
, _stringFlags(CCAscii | ConstSST)
, _buffer(0)
, _bufferLength(0)
, _begin((byte*)strLit.text)
, _end((byte*)(strLit.text + strLit.length))
, _hashCode(strLit.hashCode)
{
}


inline
//foreign 
RString 
String::toString()
{
  return this;
}

  
inline
bool 
String::equals(IN(RString) s)
{ 
  return s != Nil && equals(*s);
}


inline bool 
String::hashCodeCompatible(CharacterClass first, CharacterClass second)
{
  return first == second || (CCAscii == first || second == CCAscii);
}

inline 
bool 
String::equals(const String& str)
{
  if (this == &str)
    return true;
  if (hashCodeCompatible(characterClass(), str.characterClass()) == true)
    if (hashCode() != const_cast<String&>(str).hashCode())
      return false;
  return equalsNoHash(str);
}

inline 
bool 
String::equalsWithHash(const char* cstr, int theHash)
{
  if (hashCode() != theHash)
    return false;
  return equals(cstr);
}

inline 
bool 
String::equalsWithHash(const char* cstr, int len, int theHash)
{
  if (hashCode() != theHash)
    return false;
  return equals(cstr, len);
}
  
inline
bool 
String::equals(const StaticAsciiLiteral& slit)
{
  return equalsWithHash(slit.text, slit.length, slit.hashCode);
}

inline
bool 
String::equals(IN(RObject) o)
{
  if (instanceof(o, String) == false)
    return false;
  const String& sr = *RString(o);
  return const_cast<String*>(this)->equals(sr);
}


inline
//virtual 
int
String::compareToIgnoreCase(IN(RString) o) const
{
  if (o == Nil)
    return 1;
  return compareToIgnoreCase(*o);
}


inline
bool 
String::equalsIgnoreCase(IN(RString) o) const
{
  if (o == Nil)
    return false;
  return equalsIgnoreCase(*o);
}

inline
int 
String::compareTo(IN(RObject) o) 
{
  return compareTo(RString(o));
}

inline  
int 
String::compareTo(IN(RString) o)
{
  if (o == Nil)
    return 1;
  return compareTo(*o);
}



inline
RString 
String::concat(IN(RString) o) const
{
  if (o == Nil)
    return this;
  return concat(*o);
}


inline
//static
RString 
String::copyValueOf(IN(RcharArray) data, int offset/* = 0*/, int count/* = -1*/)
{
  return new String(data, offset, count);
}

//static 
inline
RString 
String::valueOf(const String& s) { return &s; }

inline
bool 
String::startsWith(IN(RString) prefix, int toffset) const
{
  if (prefix == Nil)
    return false;
  return startsWith(*prefix, toffset);
}





inline
bool 
String::endsWith(IN(RString) suffix) const
{
  if (suffix == Nil || suffix->length() == 0 || length() == 0)
    return false;
  return endsWith(*suffix);
}


inline
bool 
String::regionMatches(int toffset, IN(RString) other, int ooffset, int len, bool ignoreCase/* = false*/) const
{
  return regionMatches(toffset, *other, ooffset, len, ignoreCase);
}



inline
RcharArray 
String::tocharArray() const
{
  return getChars();
}


inline
int 
String::indexOf(IN(RString) str, int fromIndex/* = 0*/) const
{
  return indexOf(*str, fromIndex);
}

inline
int 
String::lastIndexOf(IN(RString) str, int fromIndex/* = -1*/) const
{
  return lastIndexOf(*str, fromIndex);
}

inline
RString 
String::substring(int startidx, int endidx/* = -1*/) const
{
  return substr(startidx, endidx);
}

inline 
RString 
String::left(int len) const
{
  if (len > length())
    len = length();
  return substr(0, len);
}

inline 
RString 
String::right(int len) const
{
  if (len > length())
    len = length();
  return substr(len - length());
}

inline 
RString 
String::mid(int start, int len) const
{
  if (start > length())
    return String::emptyString();
  if (length() < start + len)
    len = length() - start;
  return substr(start, start + len);
}

inline
RString 
String::replace(IN(RString) find, IN(RString) repl) const
{
  return replace(*find, *repl);
}


inline
RString 
String::replace(int startidx, int endidx, IN(RString) str) const
{
  return replace(startidx, endidx, *str);
}



inline
int 
String::elementCount(IN(RString) str) const
{
  return elementCount(*str);
}


inline
RString 
String::valueOf(char* ptr) { return valueOf((const char*)ptr); }

inline RString String::valueOf(ucchar* ptr) { return valueOf((const ucchar*)ptr); }

inline RString String::operator+(const char* text) const { return concat(text); }
inline RString String::operator+(char* text) const { return concat((const char*)text); }
inline RString String::operator+(const ucchar* text) const { return concat(text); }
inline RString String::operator+(ucchar* text) const { return concat((const ucchar*)text); }


inline 
RString 
String::convert(int flags) const 
{ 
  return convert(flags, ::acdk::locale::ReportCodingError, ::acdk::locale::ReportCodingError); 
}

inline 
RString 
String::convert(int flags, acdk::locale::CodingErrorAction onMalformed,
                             acdk::locale::CodingErrorAction onUnmappable)  const
{ 
  if ((_stringFlags & (CharacterClassMask | CodePageMask)) == flags)
    return this;
  return _convert(flags, onMalformed, onUnmappable);
}

inline
RString 
String::convertToNative() const { return convert(CCOsNative); }

inline
RString 
String::narrow() const
{
  if (isCharacterClass(CCAscii))
    return this;
  return _narrow();
}

inline
RString 
String::decodeAscUnicode(const char* source, const char* end)
{
  if (strstr(source, "\\u") == 0)
    return RCS(source);
  return _decodeAscUnicode(source, end);
}

inline 
RString 
String::decodeAscUnicode(const char* source)
{
  return decodeAscUnicode(source, source + strlen(source));
}

#ifdef OLD_STRING_OP_PLUS
inline
//ACDK_CORE_PUBLIC 
RString 
RString::operator+(IN(RString) str) const
{
  return getIPtr()->concat(str);
}
#endif //OLD_STRING_OP_PLUS

//static 
inline
RString 
String::getEmptyString()
{
  return emptyString();
}
//static 
inline 
RString 
String::defaultString(IN(RString) str)
{
  return str == Nil ? getEmptyString() : str;
}

//static 
inline 
RString 
String::defaultString(IN(RString) str, IN(RString) defaultString)
{
  return str == Nil ? defaultString : str;
}

//static 
inline 
bool 
String::isEmpty(IN(RString) str)
{
  if (str == Nil)
    return true;
  return str->length() == 0;
}

//static 
inline 
bool 
String::isNotEmpty(IN(RString) str)
{
  if (str == Nil)
    return false;
  return str->length() > 0;
}  

//static 
inline 
bool 
String::isNotBlank(IN(RString) str)
{
  return isBlank(str) == false;
}

//static 
inline 
bool 
String::contains(IN(RString) str, uc2char c)
{
  if (str == Nil)
    return false;
  return str->indexOf(c) != -1;
}

//static 
inline 
bool 
String::contains(IN(RString) str, IN(RString) find)
{
  if (str == Nil)
    return false;
  return str->indexOf(find) != -1;
}

namespace dmi {

inline
bool 
NamedMetaInfo::equalsName(IN(RString) other) const
{
  if (getNameHashCode() != other->hashCode())
    return false;
  return other->equals(name);
}

inline
void 
NamedMetaInfo::_calcNameHashCode() const
  {
    const_cast<NamedMetaInfo*>(this)->nameHashCode = String::calcHashCode(name);
  }

inline 
bool 
ClazzMethodInfo::equalsAltName(IN(acdk::lang::RString) n) const
{
  if (getAltNameHashCode() != n->hashCode())
    return false;
  return n->equals(altlabel);
}

} // dmi
} // lang
} // acdk


template <typename T>
inline
::acdk::lang::RString 
BasicArray<T> ::toString()
  {
    ::acdk::lang::StringBuffer str("[");
    int i;
    for (i = 0; i < length() && i < 20; i++) 
    {
      if (i > 0)
        str.append(", ");
      str.append(acdk::lang::String::valueOf(get(i)));
    }
    if (i < length())
      str.append(", ...");
    str.append("]");
    return str.toString();
  }

#endif //acdk_lang_StringInline_h

