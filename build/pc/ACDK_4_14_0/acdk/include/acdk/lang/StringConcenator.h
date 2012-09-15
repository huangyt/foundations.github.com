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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringConcenator.h,v 1.19 2005/04/09 19:26:50 kommer Exp $

#ifndef acdk_lang_StringConcenator_h
#define acdk_lang_StringConcenator_h

#if !defined(DOXYGENONLY)

#include <acdk/lang/sys/core_static_vector.h>

#ifndef OLD_STRING_OP_PLUS
namespace acdk {
namespace lang {

/**
  Helper class for StringConcenator
  Holds String or const char*
  @internal
*/
foreign struct ACDK_CORE_PUBLIC StringOrCString
{

  enum 
  {
    StringClass,
    CString,
    CCUcs2,
  } type;
    
  union {
    //String* astr;
    char buffer[sizeof(RString)];
    const char* cstr;
    const ucchar* uccstr;
  } str;
  
  StringOrCString(const StringOrCString& other)
    : type(other.type)
  {
    if (type == StringClass)
      new (str.buffer) RString(other.rstring());
    else if (type == CCUcs2)
      str.uccstr = other.str.uccstr;
    else
      str.cstr = other.str.cstr;
  }
  StringOrCString()
    : type(CString)
  {
    str.cstr = 0;
  }
  StringOrCString(const RString& s)
  : type(StringClass)
  {
    new (str.buffer) RString(s);
    
  }
  ~StringOrCString()
  {
    if (type == StringClass)
      rstring().~RString();
  }
  inline RString& rstring() const
  {
    return *((RString*)str.buffer);
  }
  void reset()
  {
    if (type == StringClass)
    {
      rstring().~RString();
      type = CString;
    }
    str.cstr = "";
  }
  int size() const;
  StringOrCString& operator=(const char* s)
  {
    reset();
    type = CString;
    str.cstr = s;
    return *this;
  }
  StringOrCString& operator=(const RString& s)
  {
    reset();
    type = StringClass;
    new (str.buffer) RString(s);
    return *this;
  }

  //byte* fill(byte* buffer) const;
};


/** 
  The size of the internal buffer of the StringConcenator
  @internal
*/
foreign 
enum 
{
  MaxStringConcenators = 5
};


/**
  Helper class to implement operator+ for Strings
  @internal
*/
foreign class StringConcenator
{
public:
  typedef sys::core_static_vector<StringOrCString, MaxStringConcenators + 1> container;
  container _strings;
  
  typedef container::const_iterator const_iterator;
  typedef container::iterator iterator;

  StringConcenator()
  {
  }
  StringConcenator(const RString& s1, const RString& s2)
  {
    if (s1 != Nil)
      _strings.push_back() = s1;
    if (s2 != Nil)
      _strings.push_back() = s2;
  }
  StringConcenator(const RString& s1, const char* s2)
  {
    if (s1 != Nil)
      _strings.push_back() = s1;
    _strings.push_back() = s2;
  }
  StringConcenator(const char* s1, const RString& s2)
  {
    _strings.push_back() = s1;
    if (s2 != Nil)
      _strings.push_back() = s2;
  }
  StringConcenator(const char* s1, const char* s2)
  {
    _strings.push_back() = s1;
    _strings.push_back() = s2;
  }
  StringConcenator(const ucchar* s1, const RString& s2)
  {
    _strings.push_back() = s1;
    if (s2 != Nil)
      _strings.push_back() = s2;
  }
  StringConcenator(const ucchar* s1, const ucchar* s2)
  {
    _strings.push_back() = s1;
    _strings.push_back() = s2;
  }
  StringConcenator(const ucchar* s1, const char* s2)
  {
    _strings.push_back() = s1;
    _strings.push_back() = s2;
  }
  StringConcenator(const char* s1, const ucchar* s2)
  {
    _strings.push_back() = s1;
    _strings.push_back() = s2;
  }
  void push_back(const RString& s1)
  {
    if (s1 != Nil)
      _strings.push_back() = s1;
  }
  void push_back(const char* s1)
  {
    _strings.push_back() = s1;
  }
  
  operator RString() const { return merge(const_cast<StringConcenator&>(*this)); }
  ACDK_CORE_PUBLIC static RString merge(StringConcenator& This);
  inline int string_length(const RString& str) { return str->length(); }
  inline int string_length(const char* str) { return strlen(str); }
  
  
  template <typename T> RString toString(const T& lastElem)
  {
    push_back(lastElem);
    return *this;
  }
  template <typename T> RString toStringOrCString(const T& t)
  {
    return String::valueOf(t);
  }
  inline const char* toStringOrCString(const char* str)
  {
    return str;
  }
  inline const ucchar* toStringOrCString(const ucchar* str)
  {
    return str;
  }
  template <typename T> 
  StringConcenator& operator+(const T& s1)
  {
    if (MaxStringConcenators <= _strings._curpos) 
    {
      RString str = toString(toStringOrCString(s1));
      _reset();
      _strings.push_back() = str;
      return *this;
    }
    _strings.push_back() = toStringOrCString(s1);
    return *this;
  }
  StringConcenator& operator+(const RString& s1)
  {
    if (s1 == Nil)
      return *this;

    if (MaxStringConcenators <= _strings._curpos) 
    {
      RString str = toString(s1);
      _reset();
      _strings.push_back() = str;
      return *this;
    }
    _strings.push_back() = s1;
    return *this;
  }
  void _reset()
  {
    iterator it = _strings.begin();
    iterator end = _strings.end();
    for (; it != end; ++it)
    {
      it->reset();
    }
    _strings._curpos = 0;
  }
};

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, bool t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/

inline
StringConcenator
operator+(const RString& s1, byte t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, char t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, uc2char t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, short t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, int t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, jlong t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, float t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, double t) { return StringConcenator(s1, String::valueOf(t)); }

/** 
  global concatination for Strings 
  @internal
*/
template <typename T>
inline
StringConcenator
operator+(const RString& s1, const RefHolder<T>& t)
{
  return StringConcenator(s1, String::valueOf(&t));
}


/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, const RString& s2)
{
  return StringConcenator(s1, s2);
}

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, const char* s2)
{
  return StringConcenator(s1, s2);
}

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const RString& s1, const ucchar* s2)
{
  return StringConcenator(s1, s2);
}

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const char* s1, const RString& s2)
{
  return StringConcenator(s1, s2);
}

/** 
  global concatination for Strings 
  @internal
*/
inline
StringConcenator
operator+(const ucchar* s1, const RString& s2)
{
  return StringConcenator(s1, s2);
}


} // lang
} // acdk

#endif // #ifndef OLD_STRING_OP_PLUS
#endif //!defined(DOXYGENONLY)
#endif //acdk_lang_StringConcenator_h

