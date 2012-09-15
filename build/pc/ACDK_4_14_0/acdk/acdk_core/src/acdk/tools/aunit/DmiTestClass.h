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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/DmiTestClass.h,v 1.36 2005/04/13 12:59:58 kommer Exp $

#ifndef acdk_tools_testunit_DmiTestClass_h
#define acdk_tools_testunit_DmiTestClass_h


#include <acdk.h>
#include "Config.h"
#include "SayHelloInterface.h"
#include <acdk/lang/Integer.h>
#include <acdk/lang/StringBuffer.h>
#include <acdk/lang/ByteBuffer.h>
#include <acdk/lang/dmi/Marshaler.h>
#include <acdk/util/Set.h>

namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_CLASS(DmiTestClass);


enum DmiTestEnumeration
{
  EnumValue1 = 0,
  EnumValue2 = 1,
  EnumValue3 = 41,
  EnumValue4 = 42
};

ACDK_DEF_LIB_ENUM(ACDK_TOOLS_AUNIT_PUBLIC, DmiTestEnumeration);


/**
  This class may be used to test script code.

  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.36 $
  @date $Date: 2005/04/13 12:59:58 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("Key1", "Val1"))
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("Key2", "Val2"))
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClassInitAttribute(: initFunction = "_initializeClass"))
class ACDK_TOOLS_AUNIT_PUBLIC DmiTestClass
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(DmiTestClass)
private:
  int privInt;
  RString privString;
  RInteger privInteger;
  static int privStaticInt;
  static RString privStaticString;
  static RInteger privStaticInteger;

  /** private constructor */
  DmiTestClass(int i)
  : pubInt(i)
  {
  }
  virtual void privateMethod()
  {
  }
  static void privateStaticMethod()
  {
  }
  static void _initializeClass()
  {
    foreignStaticBoolean = true;
  }
protected:

public:
  foreign static bool foreignStaticBoolean;
  bool pubBool;
  char pubChar;
  byte pubByte;
  short pubShort;
  int pubInt;
  jlong pubLong;
  float pubFloat;
  double pubDouble;
  RObject pubObject;
  RString pubString;
  RInteger pubInteger;
  acdk::util::RSet pubSet;

  static bool pubStaticBool;
  static char pubStaticChar;
  static byte pubStaticByte;
  static short pubStaticShort;
  static int pubStaticInt;
  static jlong pubStaticLong;
  static float pubStaticFloat;
  static double pubStaticDouble;
  static RObject pubStaticObject;
  static RString pubStaticString;
  static RInteger pubStaticInteger;


  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("DefaultConstructor", 1))
  DmiTestClass();
  DmiTestClass(IN(RString) str, int i = 42)
  : pubInt(i)
  , pubString(str)
  {
  }
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("altGetPubStaticInt"))
  static int getPubStaticInt() { return pubStaticInt; }
  static int setGetPubStaticInt(int i, IN(RString) msg)
  {
    int ret = pubStaticInt;
    pubStaticString = msg;
    pubStaticInt = i;
    return ret;
  }
  ACDK_METHODATTRIBUTE(acdk.tools.mc.StringTagAttribute("Key3", "Val3"))
  virtual RString toString()
  {
    return pubString;
  }
  bool dynamicMethodz(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return bv;
  }
  char dynamicMethodc(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return cvar;
  }
  byte dynamicMethodb(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return bvar;
  }
  short dynamicMethods(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return svar;
  }
  int dynamicMethodi(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return ivar;
  }
  jlong dynamicMethodl(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return jlvar;
  }
  float dynamicMethodf(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return fvar;
  }
  double dynamicMethodd(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return dvar;
  }
  RObject dynamicMethodO(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return obj;
  }

  static bool staticMethodz(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return bv;
  }
  static char staticMethodc(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return cvar;
  }
  static byte staticMethodb(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return bvar;
  }
  static short staticMethods(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return svar;
  }
  static int staticMethodi(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return ivar;
  }
  static jlong staticMethodl(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return jlvar;
  }
  static float staticMethodf(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return fvar;
  }
  static double staticMethodd(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return dvar;
  }
  static RObject staticMethodO(bool bv, char cvar, byte bvar, short svar, int ivar, jlong jlvar, float fvar, double dvar, IN(RObject) obj)
  {
    return obj;
  }

  jlong setgetl(jlong l) { return l; }
  float setgetf(float f) { return f; }
  double setgetd(double d) { return d; }

  void inOutMethod(IN(int) ini, IN(RString) instr, OUT(int) outi, OUT(RString) outstr)
  {
    outi = ini + 1;
    outstr = instr + " returned";
  }
  void inOutMethodA(INOUT(int) count, INOUT(RString) val)
  {
    ++count;
    val = val + " returned";
  }
  /**
    @param toset integer value
    @param toret returns new Integer with toset value
  */
  void outMethod(int toset, OUT(RInteger) toret)
  {
    toret = new Integer(toset);
  }
  /**
    @param iargg == 42
    @param sarg == "sarg"
    @param sbarg == "sarg"
  */
  bool namedArgsMethod(IN(int) iarg, IN(RString) sarg, IN(RStringBuffer) sbarg)
  {
    return iarg == 42 &&
           sarg->equals("sarg") &&
           sbarg->toString()->equals("sbarg");
  }

  void byValIn(BYVALIN(RStringBuffer) sb)
  {
    sb->append(" appended");
  }
  void byValOut(BYVALOUT(RInteger) integer)
  {
    integer = pubInteger;
  }
  void byValInOut(BYVALINOUT(RStringBuffer) sb)
  {
    RStringBuffer tsb;
    tsb = sb;
    tsb->append(" appended");
    sb = tsb;
  }

  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("methodWithoutArgs", 0))
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("methodWithOneArgs", 1))
  int methodWithDefaultArgs(int i = 3)
  {
    return i;
  }
  /// to test overloading of basic arrays
  int methodWithBasicArray(IN(RintArray) iarray)
  {
    return iarray->length();
  }
  /// to test overloading of basic arrays
  int methodWithBasicArray(IN(RcharArray) carray)
  {
    return carray->length();
  }
  /**
    @param exectiontype: 0 -> Throwable
                         1 -> NumberFormatException
                         2 -> UnsupportedOperationException
    @throw RNumberFormatException if exectiontype == 1
    @throw RUnsupportedOperationException if exectiontype == 2
  */
  void throwExceptionMethod(int exectiontype) throw(RNumberFormatException, RUnsupportedOperationException)
  //void throwExceptionMethod(int exectiontype) THROWS2(RNumberFormatException, RUnsupportedOperationException)
  {
    if (exectiontype == 2)
      THROW1(UnsupportedOperationException, "This Operation is not supported");
    if (exectiontype == 1)
      THROW1(NumberFormatException, "Not A Number");
    THROW1(Throwable, "other exception");
  }
  /**
    Method test arrays of strings
    @return concated strings
  */
  RString appendAll(IN(RStringArray) sa)
  {
    StringBuffer sb;
    for (int i = 0; i < sa->length(); ++i)
    {
      sb << sa[i];
    }
    return sb.toString();
  }
  /**
    Method test array of basic types
    return summary of elements
  */
  int makeSum(IN(RintArray) ia)
  {
    int erg = 0;
    for (int i = 0; i < ia->length(); ++i)
    {
      erg += ia[i];
    }
    return erg;
  }
  /**
    Method test array of Object types
    return summary of elements
  */
  int makeSum(IN(RIntegerArray) ia)
  {
    int erg = 0;
    for (int i = 0; i < ia->length(); ++i)
    {
      erg += ia[i]->intValue();
    }
    return erg;
  }
  int makeNumSum(IN(RNumberArray) na)
  {
    int erg = 0;
    for (int i = 0; i < na->length(); ++i)
    {
      erg += na[i]->intValue();
    }
    return erg;
  }
  /**
    test for casting to InterfaceArrays
    @return true if array is sorted
  */
  bool sorted(IN(RComparableArray) ca)
  {
    for (int i = 0; i < ca->length() - 1; ++i)
      if (ca[i]->compareTo((RObject)ca[i + 1]) >= 0)
        return false;
    return true;
  }

  /**
    Test for operator handling
  */
  int operator+(int otherint)
  {
    return privInt + otherint;
  }
  /**
    group of function to test correct polymorphFunc overloading
  */
  RString polymorphFunc(IN(RInteger) integer) { return "Integer"; }
  /**
    group of function to test correct polymorphFunc overloading
  */
  RString polymorphFunc(IN(RNumber) number) { return "Number"; }
  /**
    group of function to test correct polymorphFunc overloading
  */
  RString polymorphFunc(IN(RComparable) comparable) { return "Comparable"; }
  /**
    group of function to test correct polymorphFunc overloading
  */
  RString polymorphFunc(IN(RString) str) { return "String"; }
  virtual int virtualMethod() { return 1; }
  /**
    test enumeration mappings
  */
  DmiTestEnumeration getEnumeration(DmiTestEnumeration en)
  {
    if (en == EnumValue1)
      return EnumValue3;
    if (en == EnumValue2)
      return EnumValue4;
    return en;
  }
  virtual RString sayHelloViaInterface(IN(RSayHelloInterface) helloi, IN(RString) sayto)
  {
    return helloi->sayHello(sayto);
  }
  static RString methodWithDefaults(IN(RObject) obj, IN(RString) message,  IN(RString) defaultDir = "", IN(RString) defaultFile = "", IN(RString)  wildCard = "*.*", int style = 0, RStringBuffer pos = new StringBuffer(""))
  {
    StringBuffer sb;
    sb << "methodWithDefaults with message: " << message
       << "; defaultDir: " << defaultDir
       << "; defaultFile: " << defaultFile
       << "; wildCard: " << wildCard
       << "; style: " << style
       << "; pos: " << pos->toString();
    return sb.toString();
  }
};


} //namespace aunit
} // namespace tools
} // namespace acdk

inline
int operator+(IN(acdk::tools::aunit::RDmiTestClass) thisobj, int otherint)
{
  return *thisobj + otherint;
}

#endif //acdk_tools_testunit_DmiTestClass_h
