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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/String.h,v 1.73 2005/04/28 11:13:07 kommer Exp $


#ifndef acdk_lang_String_h
#define acdk_lang_String_h


#include <ctype.h>
#ifdef ACDK_STD_CPP_CONVERTERS
#include <string>
#endif //ACDK_STD_CPP_CONVERTERS

#ifndef acdk_lang_Object_h
#  include "Object.h"
#endif //acdk_lang_Object_h

#include "BasicArray.h"
#include "Comparable.h"
#include "Cloneable.h"
#include "StringUtf8Utils.h"
#include <acdk/locale/CodingErrorAction.h>
#include <acdk/lang/sys/core_alloca.h>

#if defined(ACDK_OS_DARWIN)
size_t wcslen(const wchar_t* ptr);
#endif

#if defined(UNICODE) && defined(ACDK_OS_WIN32)
# if defined(__BORLANDC__)
typedef wchar_t native_char;
# else
typedef uc2char native_char;
# endif
#else
/**
  defines the nativ used character type
*/
typedef char native_char;
#endif

namespace acdk {
namespace locale {
ACDK_DECL_CLASS(Encoder);
ACDK_DECL_CLASS(Decoder);
ACDK_DECL_CLASS(Encoding);
enum CodingErrorAction;

} // namespace locale
namespace util {
  ACDK_DECL_INTERFACE(Iterator);
} // namespace util
namespace lang{

ACDK_DECL_ENUM_FQ(acdk::locale, CodingErrorAction)

ACDK_DECL_CLASS(Object);

class String;
class RString;
class Comparable;
struct StaticAsciiLiteral;

ACDK_DECL_CLASS(StringBuffer);

/**
  defines how the String class store its internal buffer
  @ingroup acdkstring
*/
enum StringStorageType
{
  UnspecSST = 0x0000,
    /**
      normal, means will use normal free store
    */
  NormalSST = 0x0001,
    /**
      is String from another string.
      _buffer contains not the buffer, but
      the pointer to the underlying String
    */
  SubSST   = 0x0002,
    /**
      String has not allocated itself the buffer,
      just hold it.
    */
  ConstSST = 0x0004,
    /**
      String is only an entry in a StringTable
      currently not implemented
    */
  HashSST = 0x0008,

  StorageMaskSST = 0x000F
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, StringStorageType);


#if defined(UNICODE) && defined(ACDK_OS_WIN32)
// @internal
# define CCOS_NATIVE CCUcs2
#else
# define CCOS_NATIVE CCUtf8
#endif

/**
    specifies the encoding of the 
    character buffer
    @ingroup acdkstring
*/
enum CharacterClass
{
    /// contains 7bit ascii
    CCAscii = 0x0010,
    /// contains UCS-2 stream
    CCUcs2   = 0x0020,
    /// contains UCS-4 stream
    CCUcs4   = 0x0030,
    /// contains 8bit character stream, connected with code table in _isoCode
    CCAnsi  = 0x0040,
    /// does not necessarly support indexOf, charAt, replace, etc. operations
    CCUtf8  = 0x0050,
    /// does not necessarly support indexOf, charAt, replace, etc. operations
    CCUtf16  = 0x0060,
    /// type used by underlying os calls
    CCOsNative = CCOS_NATIVE,
    CharacterClassMask = 0x00F0
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, CharacterClass);

#undef CCOS_NATIVE

/**
    ISO code pages supported by string
    only used if CharacterClass is CCAnsi
    @ingroup acdkstring
  */
foreign enum CodePage
{

  Ascii_CP         = 0x0000,
  Latin_1_CP       = 0x0100,
  CodePageMask  = 0xFF00
};


/**
  flags for String::trim
   @ingroup acdkstring
*/
enum TrimFlags
{
  /**
    remove Character::isSpace
  */
  TrimSpace =    0x01,
  /**
    remove '\r' '\n'
  */
  TrimNewLines  = 0x02,
  /**
    Character::isControl
  */
  TrimControl   = 0x04,

  TrimWhiteSpace = TrimSpace | TrimNewLines | TrimControl,
  /** trim left side of string */
  TrimLeft  =     0x10,
  /** trim right side of string */
  TrimRight =     0x20,
  TrimBoth =      TrimLeft | TrimRight

};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, TrimFlags);


/**
  Similar to Java but extended
  Some extional function are inspired by the
    org.apache.commons.lang.StringUtils class
  @see For more details see gw_ref[acdk_hb_lang_string]
  @author Roger Rene Kommer
  @version $Revision: 1.73 $
  @date $Date: 2005/04/28 11:13:07 $
  @ingroup acdkstring

*/
class ACDK_CORE_PUBLIC String
: extends ::acdk::lang::Object
, implements ::acdk::lang::Comparable
, implements ::acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(String)
public :
  static RObject create_instance() { return new String(); }

  typedef int (*CharConvertFunc)(int c);
  /// standard iterator over single byte chars
  typedef byte* byte_iterator;
  typedef unsigned short StringFlags;
private:

  /**
    a bit combination of StorageType, CharacterClass and CodePage
  */
  StringFlags _stringFlags;



  /** used to store buffer */
  foreign byte* _buffer;

  /** length of byte buffer */
  int _bufferLength;

  /**
    first byte char
  */
  foreign byte_iterator _begin;
  /**
    byte char behind last char.
    if Normal, points to Terminator
  */
  foreign byte_iterator _end;
  /**
    backuped hasCode 
  */
  int _hashCode;
  /**
    initialization for sub string constructor
  */
  foreign void _init(const String& str, int start/* = 0*/, int end/* = -1*/);
  /**
    initialization with ASCII or Ansi stream
  */
  foreign void _init(const char* ptr, int length, int flags);
  /**
    initialization with UCS-2 stream
  */
  foreign void _init(const uc2char* ptr, int length, int flags);

  /**
    @param ptr to concat ucchar string
    @param length length of the ptr
    @param buffer original buffer to append
    @param end iterator set to end of new returned buffer
    @param targetoffset length of orignal buffer
    @return new created buffer contains concaned utf string
  */
  foreign char* _init(const uc2char* ptr, int length, const byte* buffer, byte*& end, int targetoffset);
  /**
    initialize string with given byte char stream using the decoder
  */
  foreign void _init(byte_iterator sit, byte_iterator eit, acdk::locale::Decoder& decoder);

public:
  /**
    iterator of uc2chars of this string.
    @note this implementation may be slower, depending of the CharacterClass of
    the string.
    @internal
  */
  foreign class StringIterator
  {
    String* _str;
    int _idx;
    /// in case it stores char* (ASCII) the _begin pointer
    char* _cit;
    /// in case it stores ucchar* (UCS2) the _begin pointer
    uc2char* _uccit;
  public:
    StringIterator(IN(RString) str, int offset);
    StringIterator(const String* str, int offset) 
    : _str(const_cast<String*>(str))
    , _idx(offset)
    , _cit(0)
    , _uccit(0)
    {
      _initPtrIt();
    }
    StringIterator(const String* str, int offset, char* cit, uc2char* uccit) 
    : _str(const_cast<String*>(str))
    , _idx(offset)
    , _cit(cit)
    , _uccit(uccit)
    {}
    inline int operator-(const StringIterator& other) const { return _idx - other._idx; }
    inline StringIterator operator+(int offset) const { return StringIterator(_str, _idx + offset, _cit, _uccit); }
    inline StringIterator operator-(int offset) const { return StringIterator(_str, _idx - offset, _cit, _uccit); }
    inline StringIterator& operator++() { ++_idx; return *this; }
    inline StringIterator operator++(int) { StringIterator temp(_str, _idx, _cit, _uccit); ++_idx; return temp; }
    inline StringIterator& operator--() { --_idx; return *this; }
    inline StringIterator operator--(int) { StringIterator temp(_str, _idx, _cit, _uccit); --_idx; return temp; }
    inline StringIterator& operator+=(int offset) { _idx += offset; return *this; }
    inline StringIterator& operator-=(int offset) { _idx -= offset; return *this; }
    inline bool operator==(const StringIterator& other) const { return _idx == other._idx; }
    inline bool operator!=(const StringIterator& other) const { return _idx != other._idx; }
    inline bool operator<(const StringIterator& other) const { return _idx < other._idx; }
    inline bool operator<=(const StringIterator& other) const { return _idx <= other._idx; }
    inline bool operator>(const StringIterator& other) const { return _idx > other._idx; }
    inline bool operator>=(const StringIterator& other) const { return _idx >= other._idx; }
    inline uc2char operator*() 
    { 
      return _cit != 0 ? 
        (uc2char)*(_cit + _idx) : (_uccit != 0 ? *(_uccit + _idx) : _str->charAt(_idx)); }
    /**
      Requirements this < other
    */
    RString substr(StringIterator& other) const;
    String* getString() const { return _str; }
    int getIndex() const { return _idx; }
  private:
    void _initPtrIt();
  };
  typedef StringIterator iterator;

  /**
    constructs an empty string
  */
  String();

  /**
    Creates a Substring from given str.
    @param str parent string
    @param start byte char position
    @param length of string (endpos = start + offset.
           if length == -1 until end of parent string
  */
  explicit String(IN(RString) str, int start = 0, int length = -1);

  /**
    Creates a Substring from given str.
    @param start byte char position
    @param end byte char position
  */
  foreign explicit String(const String& str, int start/* = 0*/, int length = -1);

  /**
    Creates a Substring from given str.
    @param start byte char position
    @param end byte char position
  */
  foreign explicit String(StringBuffer& buf, int start = 0, int end = -1);
  /**
    Creates a Substring from given str.
    @param start byte char position
    @param end byte char position
  */
  explicit String(IN(RStringBuffer) buf, int start = 0, int end = -1);
  /**
    Create instance of given C-str.
    @param ptr the null terminated C-str.
    @param flags Type to create.
               Valid types are Normal, Const, Hash
               Explicit encoding Ascii or Utf can also be provided
  */
  foreign explicit String(const char* ptr, int flags = ConstSST | CCAscii);
  /**
    initialize with const wide char character
  */
  foreign explicit String(const uc2char* ptr, int flags = ConstSST | CCUcs2);
  /**
     initialize with a 4 byte unicode character
     will be converted to a uc2char internally
  */
  foreign String(const uc4char* ptr);
  /**
     initialize with a wchar_t unicode character stream
     will be converted to a uc2char internally
  */
#if ACDK_UCLITERAL_WIDE!=ACDK_UCCHAR_SIZE || defined(__BORLANDC__)
  foreign explicit String(const wchar_t* ptr, int flags = ConstSST | CCOsNative);
  foreign explicit String(const wchar_t* ptr, int length, int flags);
#endif
  /**
    Create instance of given C-str.
    @param ptr the null terminated C-str.
    @param length length of C-string not including ending '\0'
    @param typ Type to create.
               Valid types are Normal, Const, Hash
               Explicit encoding Ascii or Utf can also be provided
  */
  foreign String(const char* ptr, int length, int flags);
   /**
    using wide character to initilize
  */
  foreign String(const uc2char* ptr, int length, int flags);

  /**
    @param ba contains ASCII or UTF8 stream
  */
  explicit String(IN(RbyteArray) ba, int offset = 0, int size = -1);
  String(IN(RbyteArray) ba, IN(acdk::locale::RDecoder) decoder, int offset = 0, int size = -1);
  foreign String(const byte* cptr, IN(acdk::locale::RDecoder) decoder, int offset = 0, int size = -1);

  /**
    initilize string with UTF8 or ASCII character stream
  */
  explicit String(IN(RcharArray) ba, int offset = 0, int size = -1);
  /**
    initialize with unicode character array
  */
  explicit String(IN(RuccharArray) ba, int offset = 0, int size = -1);
  /**
    internal constructor to create string with prepared stream
  */
  foreign String(byte* buffer, int bufferlength, byte_iterator begin, byte_iterator end, StringFlags flags)
  : Object()
  , _stringFlags(flags)
  , _buffer(buffer)
  , _bufferLength(bufferlength)
  , _begin(begin)
  , _end(end)
  , _hashCode(-1)
  {
  }
  foreign String(const StaticAsciiLiteral& strLit);

  /**
    Construct a new Substring
  */
  foreign String(String* parent, byte_iterator bit, byte_iterator eit);
  /**
    creates new string
  */
  foreign String(iterator begin, iterator end);
  /**
    Constructs a Normal string with a internal buffer of buffersize
    _buffer = _begin = _end.
    @param buffersize new size of buffer
    @param the type of contained string
  */
  foreign explicit String(int buffersize);

  foreign virtual ~String();

  inline  StringStorageType storageType() const throw() { return StringStorageType(_stringFlags & StorageMaskSST); }
  inline CharacterClass characterClass() const throw() { return CharacterClass(_stringFlags & CharacterClassMask); }
  foreign inline CodePage codePage() const throw() { return CodePage(_stringFlags & CodePageMask); }
  foreign inline StringFlags characterEncoding() const throw() { return (_stringFlags & CodePageMask) | (_stringFlags & CharacterClassMask); }
  foreign StringFlags stringFlags() const throw() { return _stringFlags; }
  inline bool isCharacterClass(CharacterClass cc) const throw() { return (_stringFlags & CharacterClassMask) == cc; }
  foreign static inline bool isCharacterClass(int flags, CharacterClass cc) throw() { return (flags & CharacterClassMask) == cc; }
  foreign inline static StringStorageType storageType(StringFlags flags) throw() { return StringStorageType(flags & StorageMaskSST); }
  foreign inline static CharacterClass characterClass(StringFlags flags) throw() { return CharacterClass(flags & CharacterClassMask); }
  foreign inline static CodePage codePage(StringFlags flags) throw() { return CodePage(flags & CodePageMask); }

  foreign inline byte_iterator buffer() const throw() { return _getSubstrBase()->_buffer; }
  foreign inline byte_iterator byte_begin() const throw() { return _begin; }
  foreign inline byte_iterator byte_end() const throw() { return _end; }
  /**
    returns the length of the stored bytes (not character) 
  */
  foreign inline int bufferLength() const throw() { return _bufferLength; }
  // STL like iterator
  foreign inline iterator begin() const  { return iterator(this, 0); }
  // STL like iterator
  foreign inline iterator end() const { return iterator(this, const_cast<String*>(this)->length()); }
  // @internal
  foreign inline void _set(int flags, byte* buffer, int bufs, byte_iterator b, byte_iterator e)
  {
    _stringFlags = flags;
    _buffer = buffer;
    _bufferLength = bufs;
    _begin = b;
    _end = e;
  }
  // @internal
  foreign inline void _setEnd(byte_iterator e) { _end = e; }


  foreign RString toString();


   /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc);
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  /**
    return false if object is not a string instance,
    or string does not have same content
  */
  foreign bool equals(IN(RObject) o);
  bool equals(IN(RString) str);
  /**
    compare with null terminated native string
  */
  foreign bool equals(const char* cstr);
  /**
    compare with native string on given length
  */
  foreign bool equals(const char* cstr, int len);
  /**
    if given hasCode is not equal to hashCode of this string
    return false, othewise use equals()
  */
  foreign bool equalsWithHash(const char* cstr, int hashCode);
  /**
    if given hasCode is not equal to hashCode of this string
    return false, othewise use equals()
  */
  foreign bool equalsWithHash(const char* cstr, int len, int hashCode);
  /**
    if given hasCode is not equal to hashCode of this string
    return false, othewise use equals()
  */
  foreign bool equals(const StaticAsciiLiteral& slit);
  /**
    compare with null terminated native string
  */
  foreign bool equals(const uc2char* cstr);
  /// @internal implementation detail
  foreign bool equals(const String& str);
  /// return true if 2 string character classes are hash-compatible
  foreign static bool hashCodeCompatible(CharacterClass first, CharacterClass second);
  /// traditional hashing
  foreign bool equalsNoHash(const String& str);
  /**
    compare 2 string ignore case.
  */
  bool equalsIgnoreCase(IN(RString) other) const;
  /**
    compare 2 string ignore case.
  */
  foreign bool equalsIgnoreCase(const char* other) const;
  /**
    compare 2 string ignore case.
  */
  foreign bool equalsIgnoreCase(const uc2char* other) const;
  /// @internal implementation
  foreign bool equalsIgnoreCase(const String& o) const;


  /**
    API: JDK
    /// reimplemented from Object
  */
  foreign int compareTo(IN(RObject) o);

  /**
    compare two string lexically.
    comparison will be done via unicode character number
    and may not correct for all locals
  */
  int compareTo(IN(RString) s);
  /**
    compare two string lexically.
    comparison will be done via unicode character number
    and may not correct for all locals
  */
  foreign int compareTo(const char* cstr);
  /**
    compare two string lexically.
    comparison will be done via unicode character number
    and may not correct for all locals
  */
  foreign int compareTo(const uc2char* cstr);
  /// @internal implementation
  foreign int compareTo(const String& other);

  /// @internal implementation
  foreign int compareToIgnoreCase(const String& other) const;
  /**
    compare two string lexically. 
    call UnicodeCharacter::compareToIgnoreCase for each character
    comparison will be done via unicode character number
    and may not correct for all locals
  */
  int compareToIgnoreCase(IN(RString) other) const;
  /**
    compare two string lexically. 
    call UnicodeCharacter::compareToIgnoreCase for each character
    comparison will be done via unicode character number
    and may not correct for all locals
  */
  foreign int compareToIgnoreCase(const char* other) const;
  /**
    compare two string lexically. 
    call UnicodeCharacter::compareToIgnoreCase for each character
    comparison will be done via unicode character number
    and may not correct for all locals
  */
  foreign int compareToIgnoreCase(const uc2char* other) const;
  /**
    return character at given offset
    @throw StringIndexOutOfBoundsException
  */
  uc2char charAt(int idx) const;
  /**
    return the hash code of this string
    the String does cache the hash code for performance reasons
  */
  foreign inline int hashCode()
  {
    if (_hashCode != -1)
      return _hashCode;
    _calcHashCode();
    return _hashCode;
  }

  /**
    return the count of contained characters
  */
  int length() const;
  /**
    API: STL-Like<br>
    @return the underlying character pointer
  */

  /**
    return the underlying character stream
    throws exception if not compatible stream
  */
  foreign const char* c_str() const;
  /**
    return the underlying character stream
    throws exception if not compatible stream
  */
  foreign const uc2char* uc2c_str() const;

#if defined(UNICODE) && defined(ACDK_OS_WIN32)
# if defined(__BORLANDC__)
  /**
    returns a native character pointer
    @note the string has first to normalized and converted
    to the proper encoding
  */
  foreign const wchar_t* native_c_str() const { return (wchar_t*)uc2c_str(); }
# else
  foreign const uc2char* native_c_str() const { return uc2c_str(); }
# endif
#else
  foreign const char* native_c_str() const { return c_str(); }
#endif
  /**
    Append param o to this string an return
    the concated string
  */
  RString concat(IN(RString) o) const;
  /**
    @see concat(IN(RString) o) const;
   */
  foreign RString concat(const String& str) const;
  /**
    @see concat(IN(RString) o) const;
   */
  foreign RString concat(const char* str) const;
  /**
    @see concat(IN(RString) o) const;
   */
  foreign RString concat(const uc2char* str) const;

  /**
    @param data utf or 7bit ASCII chars
  */
  static RString copyValueOf(IN(RcharArray) data, int offset = 0, int count = -1);

  /**
    return true if prefix starts at offset of this string
    @param prefix string
    @param toffset character offset
  */
  bool startsWith(IN(RString) prefix, int toffset = 0) const;

  /**
    @see startsWith(IN(RString) prefix, int toffset = 0) const;
   */
  foreign bool startsWith(const char* prefix, int toffset = 0) const;

  /**
    @see startsWith(IN(RString) prefix, int toffset = 0) const;
   */
  foreign bool startsWith(const uc2char* prefix, int toffset = 0) const;

  /**
    @see startsWith(IN(RString) prefix, int toffset = 0) const;
   */
  foreign bool startsWith(const String& prefix, int toffset = 0) const;

  /**
    return true if this string ends with suffix string
  */
  bool endsWith(IN(RString) suffix) const;
  /**
    @see endsWith(IN(RString) suffix) const;
  */
  foreign bool endsWith(const char* text) const;
  /**
    @see endsWith(IN(RString) suffix) const;
  */
  foreign bool endsWith(const uc2char* text) const;
  /**
    @see endsWith(IN(RString) suffix) const;
  */
  foreign bool endsWith(const String& suffix) const;

  /**
    @param toffset character offset of this string
    @param other check if other is part of this string
    @param len character length to compare. If len == -1 compare length of other
    @param ignoreCase if true ignore case sensitivity
  */
  bool regionMatches(int toffset, IN(RString) other, int otheroffset = 0, int len = -1, bool ignoreCase = false) const;

  /**
    @see regionMatches(int toffset, IN(RString) other, int otheroffset = 0, int len = -1, bool ignoreCase = false) const;
  */
  foreign bool regionMatches(int toffset, const char* other, int otheroffset = 0, int len = -1, bool ignoreCase = false) const;
  /**
    @see regionMatches(int toffset, IN(RString) other, int otheroffset = 0, int len = -1, bool ignoreCase = false) const;
  */
  foreign bool regionMatches(int toffset, const uc2char* other, int otheroffset = 0, int len = -1, bool ignoreCase = false) const;
  /**
    @see regionMatches(int toffset, IN(RString) other, int otheroffset = 0, int len = -1, bool ignoreCase = false) const;
  */
  foreign bool regionMatches(int toffset, const String& other, int otheroffset = 0, int len = -1, bool ignoreCase = false) const;

  /**
    Search an character in this string starting at fromIndex
  */
  int indexOf(char ch, int fromIndex = 0) const;
  /**
    Search an character in this string starting at fromIndex
  */
  int indexOf(uc2char ch, int fromIndex = 0) const;
  /**
    Search an character from back in this string starting at fromIndex
    @param fromIndex right position where to start.
           if fromIndex == -1 start searching from end of this string
    @return -1 if character cannot be not found
  */
  int lastIndexOf(char ch, int fromIndex = -1) const;
  /**
    @see lastIndexOf(char ch, int fromIndex = -1) const;
  */
  int lastIndexOf(uc2char ch, int fromIndex = -1) const;


  /**
    Find sub string in this string starting at fromIndex
    @param str substring
    @param fromIndex left position in this string where to start to search
    @return -1 if not found otherwise index position of left start of substring
  */
  int indexOf(IN(RString) str, int fromIndex = 0) const;
  /**
    @see indexOf(IN(RString) str, int fromIndex = 0) const;
  */
  foreign int indexOf(const char* str, int fromIndex = 0) const;
  /**
    @see indexOf(IN(RString) str, int fromIndex = 0) const;
  */
  foreign int indexOf(const uc2char* str, int fromIndex = 0) const;
  /**
    @see indexOf(IN(RString) str, int fromIndex = 0) const;
  */
  foreign int indexOf(const String& str, int fromIndex = 0) const;

  /**
    Find substring from back in this string.
    @param str substring to search
    @param fromIndex right position in this string where to start search
           if fromIndex == -1 search from end of this string
    @return -1 if substring cannot be found in this string.
            Otherwise index position of substring in this string
  */
  int lastIndexOf(IN(RString) str, int fromIndex = -1) const;
  /**
    @see lastIndexOf(IN(RString) str, int fromIndex = -1) const;
  */
  foreign int lastIndexOf(const char* str, int fromIndex = -1) const;
  /**
    @see lastIndexOf(IN(RString) str, int fromIndex = -1) const;
  */
  foreign int lastIndexOf(const uc2char* str, int fromIndex = -1) const;
  /**
    @see lastIndexOf(IN(RString) str, int fromIndex = -1) const;
  */
  foreign int lastIndexOf(const String& str, int fromIndex = -1) const;

  /**
    copy utf8 stream character into dst
    @param srcBegin byte offset
    @param srsEnd byte offset
  */
  void getChars(int srcBegin, int srcEnd, IN(RcharArray) dst, int dstBegin, IN(acdk::locale::REncoder) enc) const;
  /**
    copy ucchar into dst
    @param srcBegin unicode offset
    @param srcEnd unicode offset
  */
  void getUc2Chars(int srcBegin, int srcEnd, IN(RuccharArray) dst, int dstBegin) const;
  /**
    The corresponding getByte functions are superflous.
    my throw exception if underlying CharacterClass is not compatible
  */
  RcharArray getChars() const;

  /**
    return the bytes in current internal encoding
  */
  RbyteArray getBytes() const;
  /**
    return the bytes of this string using the given encoder
  */
  RbyteArray getBytes(IN(RString) enc) const;
  /**
    return the bytes of this string using the given encoder
  */
  RbyteArray getBytes(IN(acdk::locale::REncoder) enc) const;
  /**
    return the bytes of this string using the given encoder
    Better to use getBytes()
  */
  RcharArray getChars(IN(acdk::locale::REncoder) enc) const;
  /**
    return the bytes as unicode
  */
  RuccharArray getUcChars() const;
  /**
    Does the same as:
    RcharArray getChars();
  */
  RcharArray tocharArray() const;

  /**
    Just an alias to substring()
    @see substring(int startidx, int endidx = -1) const;
  */
  RString substr(int startidx, int endidx = -1) const;
  /**
    create a substring of this string.
    @param startidx character offset of this string the substring starts
    @param endidx character offset of this string the substring ends
           or -1 if string should be reach to end of this string
           length of new string is endidx - startidx
  */
  RString substring(int startidx, int endidx = -1) const;

  /**
    return a substring with len characters from left
    if len > length() return the complete string
    returns substr(0, len)
  */
  inline RString left(int len) const;
  /**
    return substring from right
    if len > length() return the complete string
    returns substr(len);
  */
  inline RString right(int len) const;

  /**
    return a substr(start, start + len)
    if start > length() return empty string
    if start + len > length() return substr(start)
  */
  inline RString mid(int start, int len) const;
  
  /**
    return the string before the first occourence of find
    if find is Nil return the this
    if find cannot be found return this 
  */
  RString substringBefore(IN(RString) find) const;
  /**
    return the string after the first found find string
    if find is Nil return empty string
    if find cannot be found return empty string
    if found index + length of find > as length() return empty string
  */
  RString substringAfter(IN(RString) find) const;
  /**
    see substringBefore but relation used lastIndexOf instead of indexOf
  */
  RString substringBeforeLast(IN(RString) find) const;
  /**
    see substringAfter but relation used lastIndexOf instead of indexOf
  */
  RString substringAfterLast(IN(RString) find) const;
  /**
    return a new string with lowercase character.
    May return this, if this string is always lowercase
  */
  RString toLowerCase() const;
  /**
    return true, if all character has lowercase
    or non-letter types
  */
  bool isLowerCase() const;

  /**
    return a new string with upper case character.
    May return this, if this string is always upper case
  */
  RString toUpperCase() const;
  /**
    return true, if all character has upper case
    or non-letter types
  */
  bool isUpperCase() const;
  /**
    Cut whitespaces from the end of  the string.
    May return this, if no white spaces are cut.
  */
  RString trim(int trimflags = TrimBoth | TrimWhiteSpace) const;

  /**
    put it into a weak hashmap.
    interned Strings are usefull, if many equal strings are
    created (f.e. in XML dom's) 
  */
  RString intern() const;
  /**
    replaces all appeariances of oldChar with newChar
  */
  RString replace(char oldChar, char newChar) const;
  /**
    unicode variant of character replacemant
  */
  RString replace(uc2char oldChar, uc2char newChar) const;

  /**
      replace every string find with repl string.
    If find string cannot be found this will be returned
    otherwise a new created string.
  */
  RString replace(IN(RString) find, IN(RString) repl) const;
  /**
    @see replace(IN(RString) find, IN(RString) repl) const;
  */
  foreign RString replace(const char* find, const char* repl) const;

  /**
    @see replace(IN(RString) find, IN(RString) repl) const;
  */
  foreign RString replace(const uc2char* find, const uc2char* repl) const;
  /**
    @see replace(IN(RString) find, IN(RString) repl) const;
  */
  foreign RString replace(const String& find, const String& repl) const;



  /**
    Cut text from startidx to endix and replace this text with str
    Different to the other replace function this is (naturally) not
    recursiv.
    uses byte offsets
  */
  RString replace(int startidx, int endidx, IN(RString) str) const;
  /**
    @internal
  */
  foreign RString replace(int startidx, int endidx, const String& str) const;

  /**
    render the given value to a string
  */
  static RString valueOf(bool z);

  /**
    render the given value to a string
  */
  static RString valueOf(int z);

  /**
    render the given value to a string
  */
  static RString valueOf(char z);

  /**
    render the given value to a string
  */
  static RString valueOf(uc2char z);
  //static RString valueOf(long z);

  /**
    render the given value to a string
  */
  static RString valueOf(jlong z);

  /**
    render the given value to a string
  */
  static RString valueOf(float z);

  /**
    render the given value to a string
  */
  static RString valueOf(double z);

  /**
    render the given value to a string
  */
  static RString valueOf(IN(RObject) obj);
  /**
    render the given value to a string
  */
  foreign static RString valueOf(char* ptr);
  /**
    render the given value to a string
  */
  foreign static RString valueOf(const char* ptr);
  /**
    render the given value to a string
  */
  foreign static RString valueOf(uc2char* ptr);
  /**
    render the given value to a string
  */
  foreign static RString valueOf(const uc2char* ptr);
  /**
    render the given value to a string
  */
  foreign static RString valueOf(void* ptr);
  /**
    render the given value to a string
  */
  foreign static RString valueOf(size_t s);
  /**
    render the given value to a string
  */
  foreign static RString valueOf(const String& s);
  /**
    returns how often the caracter can be found in String.
  */
  int elementCount(char c) const;
  /**
    @see elementCount(char c) const;
  */
  int elementCount(uc2char c) const;

  /**
    API: ACDK
    return how often the string occurs in the String.
    Overlapping strings will not be counted
  */
  int elementCount(IN(RString) str) const;
  /**
    @see elementCount(IN(RString) str) const;
  */
  foreign int elementCount(const String& str) const;
  /**
    @see elementCount(IN(RString) str) const;
  */
  foreign int elementCount(const char* str) const;
  /**
    @see elementCount(IN(RString) str) const;
  */
  foreign int elementCount(const uc2char* str) const;
  /**
    find the first of one of the chars
    @param chars list of character to find
    @return -1 if not found, otherwise the first char of chars
  */
  int getFirstIndexOf(IN(RString) chars) const;
  /**
    return the nth line of this string
    @param lineNo line number starting with 0
    @return Nil if no line was found or lineNo is < 0
  */
  RString peekLine(int lineNo) const;

  /**
    @internal
     public implementation, used internally.
     just wraps c-implementation
  */
  static RString _itoa(int i, int radix);
  /**
    @internal
     public implementation, used internally.
     just wraps c-implementation
  */
  static RString _jltoa(jlong i, int radix);

  /**
    returns a copy of the String undlying char* buffer.
    API: ACDK
    The copy
    is allocated via the given allocator. If no allocator
    is given, the current allocator of String will be used.
    may throw exception, if not compatible stream
  */

  foreign char* c_strdup(sys::Allocator* allocator = 0) const;

  friend class StringBuffer;


  /**
    convert current string to another CharacterClass

    return this, if string already is this CharacterClass
    @param flags bitwise combination of CharacterClass and CodePage
  */

  RString convert(int flags, acdk::locale::CodingErrorAction onMalformed,
                             acdk::locale::CodingErrorAction onUnmappable) const;
  /**
    convert the string on given flags.
    @param flags bitwise combination of CharacterClass and CodePage
  */
  inline RString convert(int flags) const;
  /**
    convert string to native underlying operation system
    calls.
  */
  inline RString convertToNative() const;
  /**
    Try to narrow the string to the smallest possible
    character set.
    If string cannot be narrowed, it just return this string
  */
  RString narrow() const;

  /**
    In case this string doesn't own the string buffer
    (but is substring or a const string, it return
    a new string which owns the buffer.
    Otherwise just return itself.
  */
  RString getNormalized() const;
  
  static OUT(RString) emptyString();
  /**
    return an empty String
  */
  static RString getEmptyString();
  
  /**
    return "" if String is Nil otherwise the String itself
  */
  static inline RString defaultString(IN(RString) str);
  /**
    return defaultString if str is nil otherwise str
  */
  static inline RString defaultString(IN(RString) str, IN(RString) defaultString);
  /** 
    return true if str is Nil or length == 0
  */
  static inline bool isEmpty(IN(RString) str);
  /**
    return true if str is not Nil and length > 0
  */
  static inline bool isNotEmpty(IN(RString) str);
  /**
    return true if str is Nil or empty or all characters of str are white spaces
  */
  static bool isBlank(IN(RString) str);
  /**
    return not isBlank(str)
  */
  static inline bool isNotBlank(IN(RString) str);
  /**
    return true if all characters are alph
    if Nil returns false
    if string is empty returns true
  */
  static bool isAlpha(IN(RString) str);
  /**
    return true if all characters of str are spaces or letters
    return false if str is nil
    return true if str length is 0
  */
  static bool isAlphaSpace(IN(RString) str);
  /**
    return true if all characters of str are letters or numbers
    return false if str is nil
    return true if str length is 0
  */
  static bool isAlphanumeric(IN(RString) str);
  /**
    return true if all characters of str are letters or numbers or spaces
    return false if str is nil
    return true if str length is 0
  */
  static bool isAlphanumericSpace(IN(RString) str);
  
  /**
    return true if all characters of str are numbers
    return false if str is nil
    return true if str length is 0
  */
  static bool isNumeric(IN(RString) str);
  /**
    return true if all characters of str are numbers or white spaces
    return false if str is nil
    return true if str length is 0
  */
  static bool isNumericSpace(IN(RString) str);

  /**
    return false if str is Nil or str->indexOf(c) == -1
  */
  static inline bool contains(IN(RString) str, uc2char c);
  /**
    return false if str is Nil or str->indexOf(find) == -1
  */
  static inline bool contains(IN(RString) str, IN(RString) find);
   /**
    return true if str or chars is Nil
    returns true if none of the characters in chars can be found in str
    otherwise return false
  */
  static bool containsNone(IN(RString) str, IN(RString) chars);

  /**
    return false if str or chars is Nil
    return true if in str only characters, which defined in chars
  */
  static bool containsOnly(IN(RString) str, IN(RString) chars);
  /**
    find the first index of one of the characters
    if str or chars is Nil return -1
    otherwise find the lower index of one of the characters in chars
  */
  static int indexOfAny(IN(RString) str, IN(RuccharArray) chars);
  /**
    find the first index of one of the characters in chars
    if str or chars is Nil return -1
    otherwise find the lower index of one of the characters in chars
  */
  static int indexOfAny(IN(RString) str, IN(RString) chars);
  
  /**
    find the first index of one of the String in strings
    if str or strings is Nil return -1
    otherwise find the lower index of one of the strings
  */
  static int indexOfAny(IN(RString) str, IN(RStringArray) strings);
  /**
    find the last index of chars in str
    return -1 if str or chars is Nil
    otherwise return largest index found of one out chars in str
    if non found return -1
  */
  static int lastIndexOfAny(IN(RString) str, IN(RString) chars);
  /**
    find the last index of chars in str
    return -1 if str or chars is Nil
    otherwise return largest index found of one out chars in str
    if non found return -1
  */
  static int lastIndexOfAny(IN(RString) str, IN(RuccharArray) chars);

  /**
    find the last index of strings in str
    return -1 if str or strings is Nil
    otherwise return largest index found of one out strings in str
    if non found return -1
  */
  static int lastIndexOfAny(IN(RString) str, IN(RStringArray) strings);

  /**
    concat each element from iterator with character
    if iterator it is Nil return Nil
    for each element returned by the iterator toString() will be called
    if the element is Nil add nothing to the joined string

    At beginning and end are no delimiter
  */
  static RString join(IN(acdk::util::RIterator) it);
  /**
    concat each element from iterator with character
    if iterator it is Nil return Nil
    for each element returned by the iterator toString() will be called
    if the element is Nil add nothing to the joined 
    At beginning and end are no delimiter
  */
  static RString join(IN(acdk::util::RIterator) it, uc2char delimiter);
  static RString join(IN(acdk::util::RIterator) it, IN(RString) delimiter);
  /**
    concats each element of the array to a string
    calls join(oa, String::emptyString())
    see join(IN(RObjectArray) oa, IN(RString) delimiter)
  */
  static RString join(IN(RObjectArray) oa);
  /**
    see join(IN(RObjectArray) oa, IN(RString) delimiter)
  */
  static RString join(IN(RObjectArray) oa, uc2char delimiter);
  /*
    concats each element of the array to a string
    if oa is Nil return Nil
    if a element of oa is Nil add nothing
  */
  static RString join(IN(RObjectArray) oa, IN(RString) delimiter);
  /**
    return a StringArray splitted.
    basically calls: StringTokenizer(this)->allToken()
  */
  RStringArray split() const;
  /**
    basically calls: StringTokenizer(this, delimiter)->allToken()
  */
  RStringArray split(uc2char delimiter) const;
  /**
    basically calls: StringTokenizer(this, delimiterChars)->allToken()
  */
  RStringArray split(IN(RString) delimiterChars) const;

  /**
    return all whitespace characters from string
    if str is Nil returns Nil
  */
  static RString deleteWhitespace(IN(RString) str);
  /**
    repeat this string n times
  */
  RString repeat(int ntimes) const;
  /**
    padd the this text on the right with paddchar (default is space ' ')
    if length of this string is less or equal size return this string
  */
  RString rightPad(int size, uc2char padchar = ' ') const;
  /**
    see rightPad
  */
  RString leftPad(int size, uc2char padchar = ' ') const;
  /**
    center this string in screen with size wide
  */
  RString center(int size, uc2char padchar = ' ') const;
  /**
    returns the index position where first differs to second
    if no difference was found return -1
    if first or second is Nil return -1
  */
  static int indexOfDifference(IN(RString) first, IN(RString) second);
  /**
    calculate a distance between 2 string corresponding
    to the Levenshtein Distance 
    first and second must not Nil
    The (slitly transformed) code is from (http://www.merriampark.com/ld.htm).
  */
  static int getDistance(IN(RString) first, IN(RString) second);

  /** 
    @internal mainly for using via DMI 
  */
  RString operator+(IN(RString) other) const;
  /** @internal mainly for using via DMI */
  RString operator+(IN(RObject) o) const;
  /** @internal mainly for using via DMI */
  RString operator+(bool v) const;
  /** @internal mainly for using via DMI */
  RString operator+(char v) const;
  /** @internal mainly for using via DMI */
  RString operator+(uc2char v) const;
  /** @internal mainly for using via DMI */
  RString operator+(short v) const;
  /** @internal mainly for using via DMI */
  RString operator+(int v) const;
  /** @internal mainly for using via DMI */
  RString operator+(jlong v) const;
  /** @internal mainly for using via DMI */
  RString operator+(float v) const;
  /** @internal mainly for using via DMI */
  RString operator+(double v) const;
  /** @internal mainly for using via DMI */
  foreign RString operator+(const char* text) const;
  /** @internal mainly for using via DMI */
  foreign RString operator+(char* text) const;
  /** @internal mainly for using via DMI */
  foreign RString operator+(const uc2char* text) const;
  /** @internal mainly for using via DMI */
  foreign RString operator+(uc2char* text) const;
  /**
    return true if this string holds the characters in 8 bit
  */
  inline bool isCChar() const { return isCharacterClass(CCAscii) || isCharacterClass(CCAnsi); }
  /**
    return true if this string holds the characters in 16 bit
  */
  inline bool isUc2Char() const { return isCharacterClass(CCUcs2); }
  /**
    return true if this string holds the characters in 32 bit
  */
  inline bool isUc4Char() const { return isCharacterClass(CCUcs4); }
  /**
    return true if this string holds the characters in UTF8 encoding (variable bits per character)
  */
  inline bool isUtf8Char() const { return isCharacterClass(CCUtf8); }

  /**
    return the maximum byte storage for a character
    may used for buffer allocation
  */
  inline int getMaxCharacterSize() const { return isCChar() ? 1 : isUc2Char() ? 2 : 4; }

  /**
    return size of capacity in chars of the underlying string
    this is not related to length(), may be greater or lesser
  */
  int getCharCapacity() const;
  /**
    Convert string from escaped const char* string, which contains \uabcd escapes
  */
  foreign static RString fomUtfEscapedLiteral(const char* text);
  /**
    Convert string from escaped const uc2char* string, which contains \uabcd escapes
  */
  foreign static RString fomUtfEscapedLiteral(const uc2char* text);

  /// @internal set byte end iterator
  foreign void setEnd(byte_iterator it) { _end = it; }
  /**
    return the real holding substr String instance
    @internal
  */
  foreign String* _getSubstrBase() throw()
  {
    if (_stringFlags & SubSST)
      return reinterpret_cast<String*>(_buffer)->_getSubstrBase();
    return this;
  }
  /// @internal
  foreign const String* _getSubstrBase() const throw()
  {
    if (_stringFlags & SubSST)
      return reinterpret_cast<const String*>(_buffer)->_getSubstrBase();
    return this;
  }
  /**
    Asuming in the character are embedded "\\uABCD" token
  */
  RString decodeAscUnicode();
  /**
    encode the current string ascii with "\\uABCD" 
    The resulting string has always the characterClass CCAscii
  */
  RString encodeAscUnicode();
  /**
     Convert a ascii stream with utf escape codes
     use this function only with literal string
  */
  foreign static RString decodeAscUnicode(const char* source, const char* end);
  /**
     Convert a ascii stream with utf escape codes
     use this function only with literal string
  */
  foreign static inline RString decodeAscUnicode(const char* source);
  /// @internal
  foreign static inline uc2char decodeAscUnicodeCharacter(const char* source)
  {
    if (*source != '\\' || *(source + 1) != 'u')
      return *source;
    return _decodeAscUnicodeCharacter(source);
  }
  /// calculate hascode from 0 terminated string
  foreign static int calcHashCode(const char* t);
  /// calculate hascode from string with len
  foreign static int calcHashCode(const char* t, int len);
  /// @internal
  foreign static RString _decodeAscUnicode(const char* source, const char* end);
  /// @internal
  foreign static uc2char _decodeAscUnicodeCharacter(const char* source);
  /// @internal
  foreign static void _throwIncompatibleString(const String& s);
private:
  /// @internal
  foreign void normalize() const
  {
    if (_stringFlags & SubSST)
      const_cast<String*>(this)->_normalize();
  }
  /// @internal
  foreign void _normalize();
  /**
    @see convert(int flags);
  */
  foreign RString _convert(int flags, acdk::locale::CodingErrorAction onMalformed,
                             acdk::locale::CodingErrorAction onUnmappable) const;
  /**
    @see naroww()
  */
  foreign RString _narrow() const;
  /**
    Used as optimized version in case target and source has same length
    @internal
  */
  foreign void _throwIndexOutOfBound(const char* text, int idx, int max);
  /// @internal
  foreign void _throwIndexOutOfBound(const uc2char* text, int idx, int max);
  /// @internal
  foreign void _calcHashCode();
  
};



typedef RefHolder<String> RStringBase;

#ifdef _MSC_VER
// typedef-Name 'RStringBase' wird als Synonym fuer den Klassennamen 'ObjectWith1Interface<class acdk::lang::String,class acdk::lang::Comparable>' verwendet
# pragma warning(disable: 4097)
#endif
/**
  specialization of RefClass1Interface<String, RObject, RComparable> to enable String related operators
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.73 $
  @date $Date: 2005/04/28 11:13:07 $
  @ingroup acdksmartptr
  @ingroup acdkstring
*/

foreign
class ACDK_CORE_PUBLIC RString
: public RStringBase
{
public:
  /** default constructor */
  RString(NilRef nil = Nil)  : RStringBase(nil) {  }
  /** copy constructor */
  RString(const RString& other) : RStringBase(other) { }
  //RString(const RefHolder<String>& other) : RStringBase(other) { }
  /** create constructor */
  
  RString(const String* impl) : RStringBase(const_cast<String*>(impl))  {  }
  explicit RString(const String& s) : RStringBase(const_cast<String*>(&s))  {  }
  /** cast constructor */
  template <class OT>
  explicit
  RString(const RefHolder<OT>& other)
  : RStringBase(other)
  {
  }
  /*
  template <class OT>
	explicit
  RString(OT* other)
  : RStringBase(other)
  {
  }
  */
  /*
  template <class OT>
  explicit
  RString(const InterfaceHolder<OT>& o)
  : RStringBase(o)
  {
  }
  */
  inline static String* _stringByCharPtr(char* ptr)
  {
    if (ptr != 0 && *ptr == 0)
      return String::emptyString().iptr();
    return new String(ptr);
  }
  inline static String* _stringByCharPtr(const char* ptr)
  {
    if (ptr != 0 && *ptr == 0)
      return String::emptyString().iptr();
    return new String(ptr);
  }
  inline static String* _stringByCharPtr(const uc2char* ptr)
  {
    if (ptr != 0 && *ptr == 0)
      return String::emptyString().iptr();
    return new String(ptr);
  }

  RString(char* ptr)
    : RStringBase(_stringByCharPtr(ptr))
  {
  }
  RString(const char* ptr)
  : RStringBase(_stringByCharPtr(ptr))
  {
  }
  RString(uc2char* ptr)
  : RStringBase(_stringByCharPtr((const uc2char*)ptr))
  {
  }
  RString(const uc2char* ptr)
  : RStringBase(_stringByCharPtr(ptr))
  {
  }
  RString(const StaticAsciiLiteral& strLit)
  : RStringBase(new String(strLit))
  {
  }
#ifdef ACDK_STD_CPP_CONVERTERS
  RString(const std::string& str)
  : RStringBase(new String(str.c_str()))
  {
  }
#endif
  RString(IN(RStringBuffer) strbuffer);

  explicit RString(IN(::acdk::lang::dmi::ScriptVar) sv)
  : RStringBase()
  {
    *this = (RString) sv.getObjectVar();
  }
#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /**
      because there is no implizite upcast functionality
  */
  operator ::acdk::lang::RObject () { return ::acdk::lang::RObject(*this); }
#endif //ACDK_CORE_NEED_ROBJECT_CONVERTER
  RString& operator=(NilRef nil)
  {
      RStringBase::operator=(nil);
      return *this;
  }
  RString& operator=(const char* ptr)
  {
    *this = RString(new String(ptr));
    return *this;
  }
  RString& operator=(const uc2char* ptr)
  {
    *this = RString(new String(ptr));
    return *this;
  }
//#if _MSC_VER >= 1300
  bool operator==(const RString& other) const
  {
	  return iptr() == other.iptr();
  }
  bool operator!=(const RString& other) const
  {
	  return iptr() != other.iptr();
  }
  bool operator==(NilRef other) const
  {
	  return iptr() == 0;
  }
  bool operator!=(NilRef other) const
  {
	  return iptr() != 0;
  }
  String* operator->() const
  {
    ACDK_ASSERT_NULL_PTR(iptr());
	  return iptr();
  }
  RString* _ref_this() { return this; }
  
  
//#endif //_MSC_VER >= 1300
  
};


typedef ::ObjectArrayImpl<RString> StringArray;
typedef ::RObjectArrayImpl<RString> RStringArray;

typedef ::ObjectArrayImpl< ::RObjectArrayImpl<RString> > StringArrayArray;
typedef ::RObjectArrayImpl< ::RObjectArrayImpl<RString> > RStringArrayArray;

/**
  creates a const string
  @param ptr is a constant (normally literal) ASCII or UTF byte character stream (const char*)
  @ingroup acdkstring
*/
inline
RString constr(const char* ptr)
{
  return new String(ptr, ConstSST | CCAscii);
}
/**
  creates a const string
  @param ptr is a constant (normally literal) UCS-2 charcter character stream (const uc2char*)
  @ingroup acdkstring
*/
inline
RString constr(const uc2char* ptr)
{
  return new String(ptr, ConstSST | CCUcs2);
}

/**
  ptr is a constant (normally literal) ASCII or UTF byte character stream (const char*)
  String will not copy this into own buffer, but points to given ptr
  @ingroup acdkstring
*/
#define RCS(ptr) ::acdk::lang::constr(ptr)

/**
  ptr is a valid ASCII or UTF8 byte character stream (const char*)
  String create own buffer
  @ingroup acdkstring
*/
inline RString stkstr(const char* ptr)
{
  return new String(ptr, NormalSST | CCAscii);
}
/**
  ptr is a valid UCS-2 character stream (const uc2char*)
  String create own buffer
  @ingroup acdkstring
*/
inline RString stkstr(const uc2char* ptr)
{
  return new String(ptr, NormalSST | CCUcs2);
}
/**
  ptr is a valid UCS-4 character stream (const uc4char*)
  String create own buffer
  @ingroup acdkstring
*/
inline RString stkstr(const uc4char* ptr)
{
  return new String(ptr, NormalSST | CCUcs4);
}

#if defined(__BORLANDC__)
inline RString stkstr(const wchar_t* ptr)
{
  return new String((const uc2char*)ptr, NormalSST | CCUcs2);
}
#endif

/**
  create string of given valid ASCII or UTF8 byte character stream
  stream.
  String create own buffer
  @ingroup acdkstring
*/
#define SCS(ptr) ::acdk::lang::stkstr(ptr)

/**
  to wrap a literal on stack with a string
  Use only if sure that the String (or derived substrings)
  will only lived in the current stack scope and no reference
  of the returned string will be stored outside scope
  @ingroup acdkstring
*/
#define ACDK_STACK_STR(arg) ::acdk::lang::RString(::acdk::lang::String(arg))



#if defined(ACDK_OS_MINGW)
/** 
  defines a cast wrapper to underlying OS api 
  @ingroup acdkstring
*/
#define ACDK_API_CHAR(val) wchar_t(val)
/** 
  defines a wrapper to underlying OS api 
  @ingroup acdkstring
*/
#define ACDK_API_CHARPTR(val) (wchar_t*)(val)
/** 
  defines a wrapper to underlying OS api 
  @ingroup acdkstring
*/
#define ACDK_API_CONSTCHARPTR(val) (const wchar_t*)(val)
#else
#define ACDK_API_CHAR(val) wchar_t(val)
# if defined(ACDK_OS_WIN32)
#  define ACDK_API_CHARPTR(val) reinterpret_cast<LPWSTR>(val)
#  define ACDK_API_CONSTCHARPTR(val) reinterpret_cast<LPCWSTR>(val)
# else
#  define ACDK_API_CHARPTR(val) (val)
#  define ACDK_API_CONSTCHARPTR(val) (val)
# endif
#endif



/**
  wraps a literal string with embededded unicode escapes
  @ingroup acdkstring
*/
#define _UC(text) ::acdk::lang::String::decodeAscUnicodeCharacter(text)
/**
  wraps a literal string with embededded unicode escapes an return it as character
  @ingroup acdkstring
*/
#define _US(text) ::acdk::lang::String::decodeAscUnicode(text)




/**
  This template compiles a ascii string literal hash code at up to 20 characters at compile time.
  this enables using it in case branches.

  This code was found in the boost mailing list, posted by Jens Nilsson

  @code
    RString text = "testing";

  switch (text->hashCode())
  {
  case StringHash<'t','e','s','t', 'i', 'n', 'g' >::hash:
    sys::coreout << "OK";
    break;
  default:
    break;
  }
  @endcode
  @ingroup acdkstring
*/

template<const char C0, const char C1='\0', const char C2='\0', const char C3='\0',
            const char C4='\0', const char C5='\0', const char C6='\0',
            const char C7='\0', const char C8='\0', const char C9='\0',
            const char C10='\0', const char C11='\0', const char C12='\0',
            const char C13='\0', const char C14='\0', const char C15='\0',
            const char C16='\0', const char C17='\0', const char C18='\0',
            const char C19='\0', const char C20='\0'>
struct StringHash
{
  template<const char c, unsigned long h>
  struct HashCalc
  {
    enum { value = c == 0 ? h : h * 31 + c };
  };
  enum { hash = HashCalc<C20, HashCalc<C19, HashCalc<C18, HashCalc<C17,
                      HashCalc<C16, HashCalc<C15, HashCalc<C14,  HashCalc<C13,
                      HashCalc<C12, HashCalc<C11, HashCalc<C10, HashCalc<C9,
                      HashCalc<C8, HashCalc<C7, HashCalc<C6, HashCalc<C5,
                      HashCalc<C4, HashCalc<C3, HashCalc<C2, HashCalc<C1,
                      HashCalc<C0, 0>::value>::value>::value>::value>::value>
                      ::value>::value>::value>::value>::value>::value>::value>::value>::value>::value>::value>::value>::value>::value>::value>::value
  };
};

/**
  this is a little helper class for fast comparing
  String with string literals.
  The class itself is not doing anything with heap, so 
  the common using is as static variable inside a function
  @code
    bool fastEqual(IN(RString) str)
    {
      // length and hashCode has only calculated at first call of fastCall
      static StaticAsciiLiteral myLit("myLiteral"); 
      return myLit == str;
      // or if str != Nil
      return str->equals(myLit);
    }
  @endcode
  @ingroup acdkstring
*/
foreign
struct ACDK_CORE_PUBLIC StaticAsciiLiteral
{
  int hashCode;
  int length;
  const char* text;
  StaticAsciiLiteral(const char* ptr);
  bool operator==(IN(RString) str) const
  {
    return str != Nil && str->equalsWithHash(text, length, hashCode) == true;
  }
  bool operator!=(IN(RString) str) const
  {
    return str == Nil || str->equalsWithHash(text, length, hashCode) == false;
  }
};

/**
  use this if you use a ascii literal in your source code.
  it declares a static varaible lit_strlit.
  @param lit is the string literal, but without ""
  @ingroup acdkstring
*/
#define ASCLITERAL(strlit) static StaticAsciiLiteral lit_##strlit(#strlit)
/**
  same as ASCLITERAL but with extern binding instead of static
  @ingroup acdkstring
*/
#define EXTERN_ASCLITERAL(strlit) extern acdk::lang::StaticAsciiLiteral lit_##strlit

/**
  same as ASCLITERAL but with no binding instead of static
  @ingroup acdkstring
*/
#define GLOBAL_ASCLITERAL(strlit) acdk::lang::StaticAsciiLiteral lit_##strlit(#strlit)

/**
  same as EXTERN_ASCLITERAL but with DLL export tag
  @ingroup acdkstring
*/
#define EXTERN_EXPORT_ASCLITERAL(Export, strlit) extern Export acdk::lang::StaticAsciiLiteral lit_##strlit
/**
  same as GLOBAL_ASCLITERAL but with DLL export tag
  @ingroup acdkstring
*/
#define EXPORT_ASCLITERAL(Export, strlit) Export acdk::lang::StaticAsciiLiteral lit_##strlit(#strlit)

} // namespace lang
} // namespace acdk

#include "StringConcenator.h"


#endif //acdk_lang_String_h

