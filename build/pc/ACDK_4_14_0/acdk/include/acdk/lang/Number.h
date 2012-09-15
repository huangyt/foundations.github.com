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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Number.h,v 1.20 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Number_h
#define acdk_lang_Number_h

#include <acdk.h>
#include <acdk/io/Serializable.h>
#include <acdk/util/Locale.h>

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Number);


/**
  defines if number will be stored
  as Big or LittleEndian 
*/
enum Endian
{
  BigEndian = 0,
  LittleEndian = 1
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, Endian);

/**
  This class provides some extended number parsing routines
  which also supports the extended unicode code character set
  and localization.
  API: Java + ACDK<br>

  @author Roger Rene Kommer
  @version $Revision: 1.20 $
  @date $Date: 2005/04/09 19:26:48 $
*/  
class ACDK_CORE_PUBLIC Number
: extends Object
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Number)
public:
  Number() {}
  virtual byte byteValue() = 0;
  virtual short shortValue() = 0;
  virtual int intValue() = 0;
  virtual jlong longValue() = 0;
  virtual float floatValue() = 0;
  virtual double doubleValue() = 0;
  
  virtual int hashCode() { return intValue(); }

  static jlong decodeIntegerNumber(IN(RString) str, INOUT(bool) tryOnly, INOUT(char) typeChar, INOUT(int) ignoreaTrailing, bool ignoreLeadingWs = false) THROWS1(RNumberFormatException);
  /**
    @param tryOnly if true, method doesn't throws RNumberFormatException
           In this case method set tryOnly = false and return error information in 
           return value:
           0 -> general number format error
           -1 -> over/underflow
    @param typeChar 
      'b' or 'B' -> byte
      's' or 'S' -> short
      'i' or 'I' -> integer
      'l' or 'L' -> long
      'f' or 'F' -> float
      'd' or 'D' -> double
      if typeChar == 0 no asumption of type is given. If the postfix value is set in String, it will be set by this method
  */
  static jlong parseIntegerNumber(IN(RString) str, int radix, INOUT(bool) tryOnly, INOUT(char) typeChar, 
                                  INOUT(int) ignoreTrailing, bool ignoreLeadingSpaces = false) THROWS1(RNumberFormatException);
  /**
    working method
    @param str the string to parse
    @param tryOnly if parsing and the float cannot be parsed it does not throw an exception
           otherwise this param will be set to false if parsing failed
    @param typeChar hint which type was parsed
    @param ignoreLeadingSpaces ignore leading spaces
    @param ignoreTrailing if this is 0 it the method only succeed if the complete string was parsed
           otherwise it return the index position into the string where parsing was stopped
    @param locale if locale is Nil it uses the C / en_US locale otherwise the 
           given locale to parse the float number
    see also decodeIntegerNumber
  */
  static double parseFloatNumber(IN(RString) str, INOUT(bool) tryOnly, INOUT(char) typeChar, 
                                 INOUT(int) ignoreTrailing, bool ignoreLeadingSpaces = false, 
                                 IN(acdk::util::RLocale) locale = Nil) THROWS1(RNumberFormatException);
  /**
    return one of the typeChar which fits this number
  */
  static char getSmallestTypeChar(jlong number);
  /**
    return one of the float type char, in which this number fits
  */
  static char getSmallestTypeChar(double number);
  /**
    creates a number instance of given value. If typeChar is not given (= 0)
    it returns a class, which the value fits in
  */
  static RNumber getNumber(jlong value, char typeChar = 0) THROWS1(RNumberFormatException);
  /**
    creates a number instance of given value. If typeChar is not given (= 0)
    it returns a class, which the value fits in
  */
  static RNumber getNumber(double value, char typeChar = 0) THROWS1(RNumberFormatException);
  /**
    Parses a number from a unicode string
    if the string is a decimal string (radix = 10) all unicode character are allowed, which are digits (not limited to 0 - 9)

    @param str the string to parse
    @param tryOnly if tryOnly is true this method does'nt throw a RNumberFormatException, but only returns Nil
           if the string cannot be decoded to a number
    @param ignoreLeadingWs ignore leading white spaces
    @param locale if locale is Nil it uses the C / en_US locale otherwise the 
           given locale to parse the float number
  */
  static RNumber decodeToNumber(IN(RString) str, bool tryOnly = false, bool ignoreLeadingWs = false, 
                                IN(acdk::util::RLocale) locale = Nil) THROWS1(RNumberFormatException);
  /**
    parses a string to given integer number
  */
  static RNumber parseToIntegerNumber(IN(RString) str, int radix, bool tryOnly = false, bool ignoreLeadingSpaces = false) THROWS1(RNumberFormatException);
  /**
    @param locale may be Nil
    @param fraction if locale is Nil it returns '.'
    @param exponent if locle is Nil it returns 'e'
  */
  static void getFractionAndExponentSignFromLocale(IN(acdk::util::RLocale) locale, OUT(ucchar) fraction, OUT(ucchar) exponent);
  foreign virtual dmi::ScriptVar toScriptVar();
  /** group to convert plattform pendig bit order to or from BigEndian
      which will be used in Serialization and network
  */
  template <class T> template_static T toBigEndian(T t)
  {
#if defined(ACDK_BIGENDIAN)
    return t;
#else
    return swapBits(t);
#endif
  }
  template <class T> template_static T fromBigEndian(T t)
  {
#if defined(ACDK_BIGENDIAN)
    return t;
#else
    return swapBits(t);
#endif
  }
  template <class T> template_static T fromLittleEndian(T t)
  {
#if !defined(ACDK_BIGENDIAN)
    return t;
#else
    return swapBits(t);
#endif
  }
  template <class T> template_static T toLittleEndian(T t)
  {
#if !defined(ACDK_BIGENDIAN)
    return t;
#else
    return swapBits(t);
#endif
  }
  foreign static void swap2(char* ptr);
  foreign static void swap4(char* ptr);
  foreign static void swap8(char* ptr);
  template <class T> template_static T swapBits(T t)
  {
    if (sizeof(T) == 1)
      return t;
    if (sizeof(T) == 2)
      swap2((char*)&t);
    else if (sizeof(T) == 4)
      swap4((char*)&t);
    else if (sizeof(T) == 8)
      swap8((char*)&t);
    return t;
  }
  
  template <class T> template_static
  int getBitCountOf(T )
  {
    return sizeof(T) * 8;
  }
  template <class T> template_static
  int  bitCount(T val)
  {
    int count;
    int bitcount = getBitCountOf(val);
    for (count = 0; count != bitcount; val >>= 1)
      if (val & 1 )
        ++count;
    return count;
  }
  template <class T> template_static
  T rotateLeft(T val, int count)
  {
    int bitcount = getBitCountOf(val);
    if (count >= bitcount)
		  count %= 64;
    return (val << count) |  (val >> (bitcount - count));
  }
  template <class T> template_static
  T rotateRight(T val, int count)
  {
    int bitcount = getBitCountOf(val);
  	if (count >= bitcount)
		  count %= bitcount;
    return (val >> count) | (val << (64 - count));
  }
};

//static 
inline
void 
Number::swap2(char* ptr)
{
  char t1 = ptr[0];
  ptr[0] = ptr[1];
  ptr[1] = t1;
}

//static 
inline
void 
Number::swap4(char* ptr)
{
  char t0 = ptr[0];
  char t1 = ptr[1];
  ptr[0] = ptr[3];
  ptr[1] = ptr[2];
  ptr[3] = t0;
  ptr[2] = t1;
}

//static 
inline
void 
Number::swap8(char* ptr)
{
  char t0 = ptr[0];
  char t1 = ptr[1];
  char t2 = ptr[2];
  char t3 = ptr[3];
  ptr[0] = ptr[7];
  ptr[1] = ptr[6];
  ptr[2] = ptr[5];
  ptr[3] = ptr[4];
  ptr[7] = t0;
  ptr[6] = t1;
  ptr[5] = t2;
  ptr[4] = t3;
}




} // lang
} // acdk


#endif //acdk_lang_Number_h

