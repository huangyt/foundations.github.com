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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringBuffer.h,v 1.42 2005/03/07 13:49:04 kommer Exp $

#ifndef acdk_lang_StringBuffer_h
#define acdk_lang_StringBuffer_h

#if defined(__MWERKS__)
#include <sstream>
#endif // defined(__MWERKS__)

#include "IndexOutOfBoundsException.h"
#include <acdk/io/Serializable.h>


namespace acdk {
namespace lang {


ACDK_DECL_CLASS(StringBuffer);

/**
  The Stringbuffer should work in a quite similar way to the java StringBuffer.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.42 $
  @date $Date: 2005/03/07 13:49:04 $
  @see String
*/

class ACDK_CORE_PUBLIC StringBuffer
: extends ::acdk::lang::Object
, implements ::acdk::lang::Cloneable
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(StringBuffer)
private:
  RString _str;
  mutable bool _shared;
public:
  static RObject create_instance() { return new StringBuffer(); }
  StringBuffer();
  foreign StringBuffer(size_t cap);
  StringBuffer(IN(RString) str);
public:
  foreign virtual ~StringBuffer() { }
public:
  inline bool isCChar() { return _str->isCChar(); }
  inline bool isUc2Char() { return _str->isUc2Char(); }
  inline bool isUc4Char() { return _str->isUc4Char(); }
  foreign RString toString()
  {
    _shared = true;
    return _str;
  }
  /** return the length of the byte chars */
  int length() { return _str->length(); }
  /** return the length of the unicode byte length */
  //int uclength() { return _str->uclength(); }
  /**
    API: ACDK<br>
    resets the internal string, without freeing internal buffer.<br>
    Note: if internal buffer is shared with another String, internally
    _unshare will be called.
  */
  void reset();
  /**
    returns the char at byte char position
  */
  uc2char charAt(int idx);

  /**
    set char at index position
  */
  void setCharAt(int idx, char c);
  void setCharAt(int idx, uc2char c);

  /**
    set char at unicode position
  */
  //void ucsetCharAt(int idx, uc2char c);
  /**
    set char at unicode position
  */
  //void ucsetCharAt(int idx, uc2char c);
  /**
    replace current buffer with given argument.
    API: ACDK
    @param str the new String
    @return this
  */
  RStringBuffer set(IN(RString) str);
  /**
   * Kopiert die Zeichen ab der Position srcBegin bis zur Position srcEnd in
   * den Puffer dst, wobei srcEnd die Position hinter dem letzten zu
   * kopierenden Zeichen ist. dstBegin legt fest, ab welcher Position im
   * Puffer die Zeichen hineinkopiert werden. Wenn srcEnd kleiner gleich
   * srcBegin ist, bleibt die Methode wirkungslos.
   *
   * @param srcBegin
   * @param srcEnd
   * @param dst
   * @param dstBegin
   */
  void getChars(int srcBegin, int srcEnd, IN(RcharArray) dst, int dstBegin, IN(acdk::locale::REncoder) enc);

  /**
    API: JDK<br>
    reverse this buffer
    @return this
  */
  RStringBuffer reverse();
  /**
    API: ACDK
  */
  foreign RStringBuffer append(const char* cptr, int len = -1);
  foreign RStringBuffer append(const uc2char* cptr, int len = -1);
#if defined(__BORLANDC__)
  foreign RStringBuffer append(const wchar_t* cptr, int len = -1);
#endif
  RStringBuffer append(IN(RString) c)
  {
    return append(*c);
  }
  foreign RStringBuffer append(const String& str);
  foreign RStringBuffer append(String::iterator begin, String::iterator end);
  /**
    API: JDK<br>
    Status: Basic Tests  */
  RStringBuffer append(char c)
  {
    char cbuf[2]; cbuf[1] = 0;
    cbuf[0] = c;
    return append(cbuf, 1);
  }
  RStringBuffer append(uc2char c)
  {
    if (c < 0x80) // 128
      return append((char)c);
    uc2char cbuf[2]; cbuf[1] = 0;
    cbuf[0] = c;
    return append(cbuf, 1);
  }
  RStringBuffer append(bool b)
  {
    return append(String::valueOf(b));
  }
  RStringBuffer append(int b)
  {
    return append(String::valueOf(b));
  }
  RStringBuffer append(jlong b)
  {
    return append(String::valueOf(b));
  }
  RStringBuffer append(float b)
  {
    return append(String::valueOf(b));
  }
  RStringBuffer append(double b)
  {
    return append(String::valueOf(b));
  }
  RStringBuffer append(IN(RObject) b)
  {
    return append(String::valueOf(b));
  }
  int capacity()
  {
    return _str->_bufferLength;
  }
  /**
    ensure capacity of mincap chars
  */
  void ensureCapacity(int mincap);
  /**
    @param newLength character length
  */
  void setLength(int newLength) THROWS1(RIndexOutOfBoundsException);

  /**
    API: like JDK. renamed from delete to deleteRegion
    @param start byte char index
    @param end byte char index
  */
  RStringBuffer deleteRegion(int start, int end) THROWS1(RIndexOutOfBoundsException);
  /**
    API: like JDK. renamed from delete to deleteRegion
    @param start unicode char index
    @param end unicode char index
  */
  RStringBuffer deleteCharAt(int index) THROWS1(RIndexOutOfBoundsException);


  RStringBuffer replace(int start, int end, IN(RString) str) THROWS1(RIndexOutOfBoundsException);

  RString substring(int start, int end = -1) THROWS1(RIndexOutOfBoundsException);
  foreign RStringBuffer insert(int index, const String& str);
  foreign RStringBuffer insert(int index, const char* str);
  foreign RStringBuffer insert(int index, const uc2char* str);

  foreign RStringBuffer insert(int index, const char* str, int offset, int len);
  /**
    @param index unicode char index
    @param offset byte ofset into string
    @param len byte length of string
  */
  foreign RStringBuffer insert(int index, const uc2char* str, int offset, int len);

  RStringBuffer insert(int offset, IN(RString) str) THROWS1(RIndexOutOfBoundsException);

  RStringBuffer insert(int offset, char ch) THROWS1(RIndexOutOfBoundsException)
  {
    char cbuf[2]; cbuf[0] = ch; cbuf[1] = 0;
    return insert(offset, (const char*)cbuf, 0, 1);
  }
  RStringBuffer insert(int offset, uc2char ch) THROWS1(RIndexOutOfBoundsException)
  {
    if (ch < 0x80)
      return insert(offset, (char)ch);

    uc2char cbuf[2]; cbuf[0] = ch; cbuf[1] = 0;
    return insert(offset, (const uc2char*)cbuf, 0, 1);
  }
  RStringBuffer insert(int offset, IN(RObject) obj) THROWS1(RIndexOutOfBoundsException);
  RStringBuffer insert(int offset, int b)  THROWS1(RIndexOutOfBoundsException)
  {
    return insert(offset, String::valueOf(b));
  }
  RStringBuffer insert(int offset, jlong l)  THROWS1(RIndexOutOfBoundsException)
  {
    return insert(offset, String::valueOf(l));
  }
  RStringBuffer insert(int offset, double d)  THROWS1(RIndexOutOfBoundsException)
  {
    return insert(offset, String::valueOf(d));
  }

  RStringBuffer insert(int offset, float f)  THROWS1(RIndexOutOfBoundsException)
  {
    return insert(offset, String::valueOf(f));
  }
  RStringBuffer insert(int offset, bool b)  THROWS1(RIndexOutOfBoundsException)
  {
    return insert(offset, String::valueOf(b));
  }
  RStringBuffer append(int offset, int b)  THROWS1(RIndexOutOfBoundsException)
  {
    return insert(offset, String::valueOf(b));
  }

  /// mainly used via DMI
  RStringBuffer operator+=(IN(RString) other) { return append(other);  }
  /// mainly used via DMI
  RStringBuffer operator+=(IN(RObject) other) { return append(other);  }
  /// mainly used via DMI
  RStringBuffer operator+=(bool other) { return append(other);  }
  /// mainly used via DMI
  RStringBuffer operator+=(char other) { return append(other);  }
  RStringBuffer operator+=(uc2char other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator+=(byte other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator+=(short other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator+=(int other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator+=(jlong other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator+=(float other) { return append(other); }
    /// mainly used via DMI
  RStringBuffer operator+=(double other) { return append(other);  }
  
  /// mainly used via DMI
  RStringBuffer operator_lt_lt(IN(RString) other) { return append(other);  }
  /// mainly used via DMI
  RStringBuffer operator_lt_lt(IN(RObject) other) { return append(other);  }
  /// mainly used via DMI
  RStringBuffer operator_lt_lt(bool other) { return append(other);  }
  /// mainly used via DMI
  RStringBuffer operator_lt_lt(char other) { return append(other);  }
  RStringBuffer operator_lt_lt(uc2char other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator_lt_lt(byte other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator_lt_lt(short other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator_lt_lt(int other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator_lt_lt(jlong other) { return append(other);  }
    /// mainly used via DMI
  RStringBuffer operator_lt_lt(float other) { return append(other); }
    /// mainly used via DMI
  RStringBuffer operator_lt_lt(double other) { return append(other);  }

  /**
    API: extended
    Checks if o is StringBuffer or String and tests strcmp
  */
  foreign bool equals(IN(RObject) o);

 /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) StringBuffer(toString()); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  /**
    API: ACDK
    @return internal char buffer
  */
  foreign const char* c_str() { return _str->c_str(); }
private:
  /**
    @param mincap are bytes capacity
  */
  inline void _unShare(int mincap = 0)
  {
    if (_shared == false && (_str->_bufferLength >= mincap))
      return;
    _unShare2(mincap);
  }
  /**
    @param mincap are bytes
  */
  void _unShare2(int mincap);
};



inline
void
StringBuffer::getChars(int srcBegin, int srcEnd, IN(RcharArray) dst, int dstBegin, IN(acdk::locale::REncoder) enc)
{
  SYNCHRONIZETHIS();
  _str->getChars(srcBegin, srcEnd, dst, dstBegin, enc);
}


inline
//virtual
uc2char
StringBuffer::charAt(int idx)
{
  if (idx >= length() || idx < 0)
    ObjectBase::_throwIndexOutOfBoundsException(idx, length(), "StringBuffer::charAt()");
  return _str->charAt(idx);
}

template <class T>
inline
StringBuffer& operator<<(StringBuffer& sb, const RefHolder<T>& t)
{
  if (t == Nil)
    sb.append("Nil");
  else
    sb.append(t->toString());
  return sb;
}


inline
StringBuffer& operator<<(StringBuffer& p,  acdk::lang::Object* b)
{
  RObject obj = b;
  p << obj;
  return p;
}


inline StringBuffer& operator<<(StringBuffer& sb, bool t)
{
  sb.append(t);
  return sb;
}

inline StringBuffer& operator<<(StringBuffer& sb, char t)
{
  sb.append(t);
  return sb;
}

inline StringBuffer& operator<<(StringBuffer& sb, uc2char t)
{
  sb.append(t);
  return sb;
}

inline StringBuffer& operator<<(StringBuffer& sb, byte t)
{
  sb.append(t);
  return sb;
}

inline StringBuffer& operator<<(StringBuffer& sb, short t)
{
  sb.append(t);
  return sb;
}



inline StringBuffer& operator<<(StringBuffer& sb, int t)
{
  sb.append(t);
  return sb;
}

inline StringBuffer& operator<<(StringBuffer& sb, jlong t)
{
  sb.append(t);
  return sb;
}


inline StringBuffer& operator<<(StringBuffer& sb, float t)
{
  sb.append(t);
  return sb;
}

inline StringBuffer& operator<<(StringBuffer& sb, double t)
{
  sb.append(t);
  return sb;
}



inline StringBuffer& operator<<(RStringBuffer& rsb, bool t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}


inline StringBuffer& operator<<(RStringBuffer& rsb, char t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}

inline StringBuffer& operator<<(RStringBuffer& rsb, uc2char t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}


inline StringBuffer& operator<<(RStringBuffer& rsb, byte t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}



inline StringBuffer& operator<<(RStringBuffer& rsb, short t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}


inline StringBuffer& operator<<(RStringBuffer& rsb, int t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}



inline StringBuffer& operator<<(RStringBuffer& rsb, jlong t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}



inline StringBuffer& operator<<(RStringBuffer& rsb, float t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}


inline StringBuffer& operator<<(RStringBuffer& rsb, double t)
{
  StringBuffer& sb = *rsb;
  sb.append(t);
  return sb;
}

inline
StringBuffer& operator<<(StringBuffer& p, const char* b)
{
  p.append(b);
  return p;
}

inline
StringBuffer& operator<<(StringBuffer& p, char* b)
{
  p.append(b);
  return p;
}

inline
StringBuffer& operator<<(StringBuffer& p, const uc2char* b)
{
  p.append(b);
  return p;
}

inline
StringBuffer& operator<<(StringBuffer& p, uc2char* b)
{
  p.append(b);
  return p;
}


template <class T>
inline
StringBuffer& operator<<(RStringBuffer& rsb, T* b)
{
  StringBuffer& sb = *rsb;
  RObject obj = b;
  sb << obj;
  return sb;
}

template <class T>
inline
StringBuffer& operator<<(RStringBuffer& rsb, const RefHolder<T>& t)
{
  StringBuffer& sb = *rsb;
  if (t == Nil)
    const_cast<StringBuffer&>(sb).append("Nil");
  else
    const_cast<StringBuffer&>(sb).append(t->toString());
  return sb;
}

inline
StringBuffer& operator<<(RStringBuffer& rsb, const char* b)
{
  StringBuffer& sb = *rsb;
  sb.append(b);
  return sb;
}

inline
StringBuffer& operator<<(RStringBuffer& rsb, char* b)
{
  StringBuffer& sb = *rsb;
  sb.append(b);
  return sb;
}


inline
StringBuffer& operator<<(RStringBuffer& rsb, const uc2char* b)
{
  StringBuffer& sb = *rsb;
  sb.append(b);
  return sb;
}

inline
StringBuffer& operator<<(RStringBuffer& rsb, uc2char* b)
{
  StringBuffer& sb = *rsb;
  sb.append(b);
  return sb;
}

inline StringBuffer& operator<<(StringBuffer& rsb, const StringConcenator& sc)
{
  return rsb << (RString)sc;
}

inline StringBuffer& operator<<(RStringBuffer& rsb, const StringConcenator& sc)
{
  StringBuffer& sb = *rsb;
  return sb << (RString)sc;
}


/**
  can be used for inline stringified output, for example exection description
*/
#define SBSTR(text) (const_cast< ::acdk::lang::StringBuffer&>((const ::acdk::lang::StringBuffer&)::acdk::lang::StringBuffer(256))  << text).toString()


inline
RString::RString(IN(RStringBuffer) strbuffer)
{
  if (strbuffer != Nil)
    *this = strbuffer->toString();
}


} // lang
} // acdk

#include "StringInline.h"


#endif //acdk_lang_StringBuffer_h

