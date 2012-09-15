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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/dmi/acdk_lang_dmi_StdDispatch_Test.cpp,v 1.56 2005/04/18 14:30:56 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/tools/aunit/DmiTestClass.h>

#include <vector>
namespace tests {
namespace acdk {
namespace lang {
namespace dmi {

BEGIN_DECLARE_TEST( StdDispatch_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( New )
  DECLARE_TEST( Invoke )
  DECLARE_TEST( InvokeStatic )
  DECLARE_TEST( PeekPoke )
  DECLARE_TEST( PeekPokeStatic )
  DECLARE_TEST( memberAccessCast )
  DECLARE_TEST( inOutParam )
  DECLARE_TEST( nameArgs )
  DECLARE_TEST( nameArgs2 )
  DECLARE_TEST( byval )
  DECLARE_TEST( basicArrays )
  DECLARE_TEST( objectArrays )
  DECLARE_TEST( arrayPolymorphic )
  DECLARE_TEST( operatorFunctions )
  DECLARE_TEST( sampleCode )
  DECLARE_TEST( hashInvoke )
  DECLARE_TEST( enumerations )
  DECLARE_TEST( dmiProxy )
END_DECLARE_TEST( StdDispatch_Test  )

BEGIN_DEFINE_TEST( StdDispatch_Test )
  ADD_TEST( StdDispatch_Test, standard ) 
  ADD_TEST( StdDispatch_Test, New ) 
  ADD_TEST( StdDispatch_Test, Invoke ) 
  ADD_TEST( StdDispatch_Test, InvokeStatic ) 
  ADD_TEST( StdDispatch_Test, PeekPoke ) 
  ADD_TEST( StdDispatch_Test, PeekPokeStatic ) 
  ADD_TEST( StdDispatch_Test, memberAccessCast ) 
  ADD_TEST( StdDispatch_Test, inOutParam ) 
  ADD_TEST( StdDispatch_Test, nameArgs ) 
  ADD_TEST( StdDispatch_Test, nameArgs2 ) 
  ADD_TEST( StdDispatch_Test, byval ) 
  ADD_TEST( StdDispatch_Test, basicArrays ) 
  ADD_TEST( StdDispatch_Test, objectArrays ) 
  ADD_TEST( StdDispatch_Test, arrayPolymorphic ) 
  ADD_TEST( StdDispatch_Test, operatorFunctions ) 
  ADD_TEST( StdDispatch_Test, enumerations ) 
  ADD_TEST( StdDispatch_Test, sampleCode ) 
  ADD_TEST( StdDispatch_Test, hashInvoke ) 
  ADD_TEST( StdDispatch_Test, dmiProxy ) 
  
END_DEFINE_TEST( StdDispatch_Test )


using ::acdk::lang::reflect::Modifier;
using namespace ::acdk::lang::dmi;
using namespace ::acdk::lang;

void
StdDispatch_Test::standard()
{
  {

    RObject sb = (RObject)StdDispatch::New("acdk/lang/StringBuffer", (const char*)"Hello ");

    sb->invoke("append", (const char*)"ACDK");

    RObject oerg = (RObject)sb->invoke("toString");
    RString erg = (RString)sb->invoke("toString");
    //int i = sb->invoke("toString");
    testAssert(erg->equals((const char*)"Hello ACDK") == true);
  }
  {
  RStringBuffer sb = (RStringBuffer)StdDispatch::New("acdk/lang/StringBuffer", (const char*)"Hello ");
  sb->append((const char*)"ACDK");
  RString erg = (RString)sb->invoke("toString");
  testAssert(erg->equals("Hello ACDK") == true);
  }
}

void
StdDispatch_Test::New()
{
  const char* c = "Hello ";
  ScriptVar s = StdDispatch::New("acdk/lang/StringBuffer", c);
  
  try {
    RObject o = (RObject)StdDispatch::New("acdk/tools/aunit/DmiTestClass", 42);
    testAssert("expect exception" == 0);
  } catch (RDmiException ex) {
    System::out->println("Expected Exception: \n" + ex->getMessage());
  }
  RObject o1 = (RObject)StdDispatch::New("acdk/tools/aunit/DmiTestClass", (const char*)"Test");
  testAssert(o1 != Nil);
  RObject o2 = (RObject)StdDispatch::New("acdk/tools/aunit/DmiTestClass", (const char*)"Test", 44);
  testAssert(o2 != Nil);
}

void
StdDispatch_Test::Invoke()
{
  RObject o = (RObject)StdDispatch::New("acdk/tools/aunit/DmiTestClass", (const char*)"Test");
  {
    //jlong l = o->invokel("setgetl", "l", jlong(123456789012));
    float fa(3.145);
    float f = o->invoke("setgetf", fa);
  }
  testAssert(bool(o->invoke("dynamicMethodz", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(123456789012)), double(3.4), double(1.234567), o)) == true);
  testAssert(char(o->invoke("dynamicMethodc", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(123456789012)), double(3.4), double(1.234567), o)) == 42);
  testAssert(byte(o->invoke("dynamicMethodb", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == 233);
  testAssert(short(o->invoke("dynamicMethods", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == 6000);
  testAssert(int(o->invoke("dynamicMethodi", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == 12345);
  testAssert(jlong(o->invoke("dynamicMethodl", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(12345678)), double(3.4), double(1.234567), o)) == 12345678);
  float f = o->invoke("dynamicMethodf", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o);
  //silly, does not work testAssert(f == 3.4);
  testAssert(double(o->invoke("dynamicMethodd", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == 1.234567);
  testAssert(RObject(o->invoke("dynamicMethodO", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == o);
  try {
    o->invoke("privateMethod");
    testAssert("expect exception" == 0);
  } catch (RDmiException ex) {
      System::out->println("Expected Exception: \n" + ex->getMessage());
    // ok
  }
}

void
StdDispatch_Test::InvokeStatic()
{
  RObject o = (RObject)StdDispatch::New("acdk/tools/aunit/DmiTestClass", (const char*)"Test");
  testAssert(bool(StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethodz", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(123456789012)), double(3.4), double(1.234567), o)) == true);
  testAssert(char(StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethodc", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(123456789012)), double(3.4), double(1.234567), o)) == 42);
  testAssert(byte(StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethodb", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == 233);
  testAssert(short(StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethods", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == 6000);
  testAssert(int(StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethodi", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == 12345);
  testAssert(jlong(StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethodl", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == JLONG_CONSTANT(1234567890));
  float f = StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethodf", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o);
  //silly, does not work testAssert(f == 3.4);
  testAssert(double(StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethodd", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == 1.234567);
  testAssert(RObject(StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "staticMethodO", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(1234567890)), double(3.4), double(1.234567), o)) == o);
  try {
    StdDispatch::invoke_static("acdk/tools/aunit/DmiTestClass", "privateStaticMethod", "");
    testAssert("expect exception" == 0);
  } catch (RDmiException ex) {
    System::out->println("Expected Exception: \n" + ex->getMessage());
    
  }
  testAssert(bool(StdDispatch::invoke_static("acdk::tools::aunit::DmiTestClass", "staticMethodz", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(123456789012)), double(3.4), double(1.234567), &o)) == true);
  testAssert(bool(StdDispatch::invoke_static("acdk.tools.aunit.DmiTestClass", "staticMethodz", true, char(42), byte(233), short(6000), int(12345), jlong(JLONG_CONSTANT(123456789012)), double(3.4), double(1.234567), &o)) == true);
  
}

void
StdDispatch_Test::PeekPoke()
{
  RObject o = (RObject)StdDispatch::New("acdk/tools/aunit/DmiTestClass");
  o->poke("pubBool", true);
  testAssert(bool(o->peek("pubBool")) == true);
  o->poke("pubBool", false);
  
  o->poke("pubChar", char(42));
  testAssert(char(o->peek("pubChar")) == 42);
  o->poke("pubChar", char(42));
  o->poke("pubByte", byte(13));
  testAssert(byte(o->peek("pubByte")) == 13);
  o->poke("pubByte", 0);
  
  o->poke("pubShort", short(600));
  testAssert(short(o->peek("pubShort")) == 600);
  o->poke("pubShort", 0);

  o->poke("pubInt", short(12345));
  testAssert(int(o->peek("pubInt")) == 12345);
  o->poke("pubInt", 0);
  
  o->poke("pubLong", jlong(123457890));
  testAssert(jlong(o->peek("pubLong")) == 123457890);
  o->poke("pubLong", jlong(0));
  
  o->poke("pubFloat", float(2.1));
  float tf = o->peek("pubFloat");
  //vc6 bug?! testAssert(peekf("pubFloat") == 2.1);
  o->poke("pubFloat", float(0));

  o->poke("pubDouble", double(1.2345));
  testAssert(double(o->peek("pubDouble")) == 1.2345);
  o->poke("pubDouble", double(0));

  o->poke("pubObject", o);
  testAssert(RObject(o->peek("pubObject")) == o);
  o->poke("pubObject", Nil);

  try {
    o->poke("privInt", 2);
    testAssert("expect exception" == 0);
  } catch (RDmiException ex) {
    System::out->println("Expected Exception: \n" + ex->getMessage());
  }
}

void
StdDispatch_Test::PeekPokeStatic()
{
//#if defined(__BORLANDC__)  && 
//  System::out->println("Access to static Member is buggy with Borland C++"); //###
//#else //defined(__BORLANDC__) 
  const char* cn = "acdk/tools/aunit/DmiTestClass";
  RObject o = (RObject)StdDispatch::New("acdk/tools/aunit/DmiTestClass");

  StdDispatch::poke_static(cn, "pubStaticBool", true);
  testAssert(bool(StdDispatch::peek_static(cn, "pubStaticBool")) == true);
  StdDispatch::poke_static(cn, "pubStaticBool", false);

  StdDispatch::poke_static(cn, "pubStaticChar", char(42));
  testAssert(char(StdDispatch::peek_static(cn, "pubStaticChar")) == 42);
  StdDispatch::poke_static(cn, "pubStaticChar", char(0));

  StdDispatch::poke_static(cn, "pubStaticByte", byte(212));
  testAssert(byte(StdDispatch::peek_static(cn, "pubStaticByte")) == 212);
  StdDispatch::poke_static(cn, "pubStaticByte", byte(0));

  StdDispatch::poke_static(cn, "pubStaticShort", short(1234));
  testAssert(short(StdDispatch::peek_static(cn, "pubStaticShort")) == 1234);
  StdDispatch::poke_static(cn, "pubStaticShort", short(0));

  StdDispatch::poke_static(cn, "pubStaticInt", int(123456));
  testAssert(int(StdDispatch::peek_static(cn, "pubStaticInt")) == 123456);
  StdDispatch::poke_static(cn, "pubStaticInt", int(0));
  
  StdDispatch::poke_static(cn, "pubStaticLong", jlong(123456789));
  testAssert(jlong(StdDispatch::peek_static(cn, "pubStaticLong")) == 123456789);
  StdDispatch::poke_static(cn, "pubStaticLong", jlong(0));

  StdDispatch::poke_static(cn, "pubStaticFloat", float(2.5));
  float f = StdDispatch::peek_static(cn, "pubStaticFloat");
  StdDispatch::poke_static(cn, "pubStaticFloat", float(0));

  StdDispatch::poke_static(cn, "pubStaticDouble", double(1.3456));
  testAssert(double(StdDispatch::peek_static(cn, "pubStaticDouble")) == 1.3456);
  StdDispatch::poke_static(cn, "pubStaticDouble", double(0));

  StdDispatch::poke_static(cn, "pubStaticObject", o);
  testAssert(RObject(StdDispatch::peek_static(cn, "pubStaticObject")) == o);
  StdDispatch::poke_static(cn, "pubStaticObject", Nil);
//#endif //defined(__BORLANDC__)   
}


void
StdDispatch_Test::memberAccessCast()
{
  /*
  RObject o = StdDispatch::NewO("acdk/tools/aunit/DmiTestClass", "");
  
  ScriptVar sv = o->getMember("pubDouble", AcdkDmiClient::getDmiClient(), 
                              MiPublic, ClazzInfo::getDoubleClazz());
                            */
}

template <typename T> 
inline
RObject* outOfThis(RefHolder<T>& t)
{
  return reinterpret_cast<RObject*>(t._ref_this());
}



void StdDispatch_Test::inOutParam()
{
  /* ### implement StdDispatch_Test::inOutParam tests
  RObject o = Newt<RObject>("acdk/tools/aunit/DmiTestClass");
  
  {
    ::acdk::tools::aunit::RDmiTestClass dtc = (::acdk::tools::aunit::RDmiTestClass)o;
    int reti = 42;
    RString str = "test";
    RString retstr;
    dtc->inOutMethod(4, str, reti, retstr);
    testAssert(retstr != Nil);
    testAssert(retstr->equals("test returned") == true);
    testAssert(reti == 5);
  }
  {
    int reti  = 42;
    RString str = "test";
    RString retstr;
    acdk::lang::dmi::invokev(o, "inOutMethod", 4, str, outOf(reti), outOf(retstr));
    testAssert(retstr != Nil);
    testAssert(retstr->equals("test returned") == true);
    testAssert(reti == 5);
  }
  */
}

void
StdDispatch_Test::nameArgs()
{
  const char* cn = "acdk/tools/aunit/DmiTestClass";
  RObject o = (RObject)StdDispatch::New("acdk/tools/aunit/DmiTestClass");

  
  
  int iarg = 42;
  RString sarg = new String("sarg");
  RStringBuffer sbarg = new StringBuffer("sbarg");

  {
    ScriptVarArray args;
    args.push_back(ScriptVar(iarg));
    args.push_back(ScriptVar(&sarg));
    args.push_back(ScriptVar(&sbarg));
    ScriptVar erg = o->invokeMethod( "namedArgsMethod"
                                    , args
                                    , ::acdk::lang::dmi::AcdkDmiClient::getDmiClient()
                                    , Nil
                                    );
    testAssert(erg.getBoolVar() == true);
  }
  {
    StringArray namedArgs(2);
    namedArgs[0] = new String("sarg");
    namedArgs[1] = new String("sbarg");
    ScriptVarArray args;
    args.push_back(ScriptVar(iarg));
    args.push_back(ScriptVar(&sarg));
    args.push_back(ScriptVar(&sbarg));
    ScriptVar erg = o->invokeMethod( "namedArgsMethod"
                                    , args
                                    , ::acdk::lang::dmi::AcdkDmiClient::getDmiClient()
                                    , &namedArgs
                                    );
    testAssert(erg.getBoolVar() == true);
  }

  // order of call is different
  {
    StringArray namedArgs(2);
    namedArgs[0] = new String("sbarg");
    namedArgs[1] = new String("sarg");
    ScriptVarArray args;
    args.push_back(ScriptVar(iarg));
    args.push_back(ScriptVar(&sbarg));
    args.push_back(ScriptVar(&sarg));
    ScriptVar erg = o->invokeMethod( "namedArgsMethod"
                                    , args
                                    , ::acdk::lang::dmi::AcdkDmiClient::getDmiClient()
                                    , &namedArgs
                                    );
    testAssert(erg.getBoolVar() == true);
  }
}

void invokena(const NamedArgs& args)
{
  //sys::coreout << args << sys::eofl;
}

void
StdDispatch_Test::nameArgs2()
{
  invokena((NamedArg("arg1", 42), NamedArg("arg2", &RCS("Blub"))));
  invokena(NamedArg("arg1", 42));
  invokena((NamedArg("arg1", 42), 
            NamedArg("arg2", &RCS("Blub")),
            NamedArg("arg1", 42),
            NamedArg("arg2", &RCS("Blub"))));
}


void 
StdDispatch_Test::byval()
{
  /* 
    this does not work further, because changed 
    with introducing acdkx_rdmi
  ::acdk::tools::aunit::DmiTestClass dmitest;
  RStringBuffer sb = new StringBuffer("Text");
  dmitest.byValIn(TSendMarshaler<RStringBuffer>(sb));
  testAssert(sb->toString()->equals("Text") == true);
  RInteger integer = new Integer(-1);
  dmitest.byValOut(TReceiveMarshaler<RInteger>(integer));
  testAssert(integer != dmitest.pubInteger);
  testAssert(integer->equals(dmitest.pubInteger) == true);

  RStringBuffer oldsb = sb;
  dmitest.byValInOut(sb);
  dmitest.byValInOut(TSendReceiveMarshaler<RStringBuffer>(sb));
  // Standard Marshaler doesn't work this way: 
  //testAssert(oldsb != sb);
  //testAssert(oldsb->toString()->equals("Test") == true);
  testAssert(sb->toString()->equals("Text appended") == true);


  sb->set("Text");
  dmitest.invoke("byValIn", &sb);
  testAssert(sb->toString()->equals("Text") == true);

  integer = new Integer(-1);
  dmitest.invoke("byValOut", outOf(integer));
  testAssert(integer != dmitest.pubInteger);
  testAssert(integer->equals(dmitest.pubInteger) == true);
  */
  

}

void
StdDispatch_Test::basicArrays()
{
  intArray ia(2);
  ia[0] = 41;
  ia[1] = 42;
  testAssert(int(ia.invoke("length")) == 2);
  testAssert(int(ia.invoke("get", 1)) == 42);
  ia.invoke("set", 1, inOf(43));
  testAssert(int(ia.invoke("get", 1)) == 43);
  ia.invoke("append", inOf(44));
  testAssert(int(ia.invoke("length")) == 3);
  testAssert(int(ia.invoke("get", 2)) == 44);
  ScriptVar sv = ia.invoke("getref", 2);
  sv = 45;
  testAssert(int(ia.invoke("get", 2)) == 45);
}


bool sorted(IN(RComparableArray) ca)
{
  for (int i = 0; i < ca->length() - 1; ++i)
    if (ca[i]->compareTo((RObject)ca[i + 1]) >= 0)
      return false;
  return true;
}


void
StdDispatch_Test::objectArrays()
{
  
  {
  StringArray sa(2);
  sa[0] = new String("ACDK");
  sa[1] = new String("DMI");
  testAssert(int(sa.invoke("length")) == 2);
  testAssert(sa.invoke("get", 1).getStringVar()->equals("DMI")  == true);
  sa.invoke("append", inOf(RString(new String("appended"))));
  testAssert(int(sa.invoke("length")) == 3);
  testAssert(sa.invoke("get", 2).getStringVar()->equals("appended")  == true);
  ScriptVar sv = sa.invoke("getref", 2);
  sv = new String("modified");
  testAssert(sa.invoke("get", 2).getStringVar()->equals("modified")  == true);
  }
  {
    RIntegerArray ia = new IntegerArray(3);
    ia[0] = new Integer(40);
    ia[1] = new Integer(41);
    ia[2] = new Integer(42);
    ::acdk::tools::aunit::DmiTestClass dmitest;
    int sum  = dmitest.makeSum(ia);
    testAssert(int(dmitest.invoke("makeSum", inOf(ia))) == sum);
    sum  = dmitest.makeNumSum((RNumberArray)ia);
    testAssert(int(dmitest.invoke("makeNumSum", inOf((RNumberArray)ia))) == sum);
    testAssert(int(dmitest.invoke("makeNumSum", inOf(ia))) == sum);
    testAssert(dmitest.sorted((RComparableArray)ia) == true);
    testAssert(bool(dmitest.invoke("sorted", inOf(ia))) == true);
    testAssert(bool(dmitest.invoke("sorted", inOf((RComparableArray)ia))) == true);
  }
  {
    RObject obj = (RObject)StdDispatch::New("acdk.lang.IntegerArray", inOf(1));
    obj->invoke("set", inOf(0), inOf(new Integer(42)));
    RInteger tint = (RInteger)obj->invoke("get", inOf(0));
    testAssert(tint->intValue() == 42);
  }
  {
    RObject obj = (RObject)StdDispatch::New("[acdk.lang.Integer", inOf(1));
    obj->invoke("set", inOf(0), inOf(RObject(new Integer(42))));
    RInteger tint = (RInteger)obj->invoke("get", inOf(0));
    testAssert(tint->intValue() == 42);
  }
}

void
StdDispatch_Test::arrayPolymorphic()
{
  ::acdk::tools::aunit::DmiTestClass dmitest;
  intArray ia(1);
  ia[0] = 42;
  charArray ca(2);
  ca[0] = 4;
  ca[1] = 2;
  testAssert(int(dmitest.invoke("methodWithBasicArray", &ia)) == 1);
  testAssert(int(dmitest.invoke("methodWithBasicArray", &ca)) == 2);
}

void
StdDispatch_Test::operatorFunctions()
{
  {
    ::acdk::tools::aunit::DmiTestClass dmitest;
    int erg = dmitest + 42;
    testAssert(erg == 45);
  }
  {
    ::acdk::tools::aunit::RDmiTestClass dmitest = new ::acdk::tools::aunit::DmiTestClass();
    int erg = dmitest + 42;
    testAssert(erg == 45);
  }
  {
    ::acdk::tools::aunit::RDmiTestClass dmitest = new ::acdk::tools::aunit::DmiTestClass();
    int erg = int(dmitest->invoke("operator_pl", inOf(42)));
    testAssert(erg == 45);
    erg = int(dmitest->invoke("operator+", inOf(42)));
    testAssert(erg == 45);
  }
}


void
StdDispatch_Test::hashInvoke()
{
  // dynamic call
  {
    StringBuffer sb("Hallo");
    ClassArray ca(1);
    ca[0] = Integer::getTYPE();
    ::acdk::lang::reflect::RMethod meth = sb.getClass()->getMethod("append", &ca);
    testAssert(meth != Nil);
    const ClazzMethodInfo* mi = (const ClazzMethodInfo*)meth->getMetaInfo();
    int mh = mi->getMethodSignatureHashValue();
    ScriptVar ret;
    ScriptVarArray args(1);
    args[0] = inOf(42);
    const ClazzInfo* ci = sb.getClazzInfo();
    sb.standardDispatch("", ret, args, AcdkDmiClient::getDmiClient(), Nil, 
      MiPublic | MiIvViaHash, ci, (const ClazzMethodInfo*)mh);
    testAssert(sb.toString()->equals("Hallo42") == true);
  }
  
  // static call
  {
    // call static RString Short::toString(short s, int radix = 10);
    ClassArray ca(2);
    ca[0] = Short::getTYPE();
    ca[1] = Integer::getTYPE();
    
    ::acdk::lang::reflect::RMethod meth = Short::GetClass()->getMethod("toString", &ca);
    testAssert(meth != Nil);
    const ClazzMethodInfo* mi = (const ClazzMethodInfo*)meth->getMetaInfo();
    int mh = mi->getMethodSignatureHashValue();
    ScriptVar ret;
    ScriptVarArray args(2);
    args[0] = inOf(short(42));
    args[1] = inOf(int(16));
    const ClazzInfo* ci = Short::clazzInfo();
    StandardDispatch("", ret, args, AcdkDmiClient::getDmiClient(), Nil, 
      MiPublic | MiIvViaHash, ci, (const ClazzMethodInfo*)mh);
    RString erg = (RString)(RObject)ret;
    testAssert(erg->equals("2a") == true);
  }
}

void
StdDispatch_Test::enumerations()
{
  ::acdk::tools::aunit::DmiTestClass dtcl;
  testAssert(int(dtcl.invoke("getEnumeration", ::acdk::tools::aunit::EnumValue2)) == ::acdk::tools::aunit::EnumValue4);
  // will not work further testAssert(int(dtcl.invoke("getEnumeration", inOf("acdk::tools::aunit::EnumValue2"))) == ::acdk::tools::aunit::EnumValue4);

}

void 
StdDispatch_Test::sampleCode()
{
  
  {
    // first the sample code in normal ACDK C++ using 
    // the known classes directly

    RString initvalue = "Hello";

    ::acdk::lang::RStringBuffer sb = new ::acdk::lang::StringBuffer(initvalue);
    sb->append(" ACDK");

    ::acdk::io::RPrintWriter out = System::out;
    RString str = sb->toString();
    RString erg = "String should contain 'Hello ACDK': " + str;
    out->println(erg);
  }
  {
    // the same code using the Dynamic Method Invocation
    RString initvalue = "Hello";

    // create a class acdk::lang::StringBuffer and call the constructur
    // with one argument.
    // the new instance will be assigned to sb
    RObject sb = (RObject)StdDispatch::New("acdk/lang/StringBuffer", &initvalue);

    // calling a non static method 'append' with one argument
    sb->invoke("append", (const char*)" ACDK");

    // from the class acdk::lang::System get the static member
    // 'out'
    RObject out = (RObject)StdDispatch::peek_static("acdk/lang/System", "out");
    
    // call from sb the method 'toString'
    RString str = (RString)sb->invoke("toString");
    RString erg = "String should contain 'Hello ACDK': " + str;

    // call from out (which is a PrintWriter) the the method println
    out->invoke("println", &erg);
  }
}

void
StdDispatch_Test::dmiProxy()
{
  RString clsname = "acdk.lang.Integer_DmiProxy";
  //RClass cls = Class::forName(clsname);
  RObject integer = (RObject)StdDispatch::New("acdk.lang.Integer_DmiProxy", inOf(42));
  const acdk::lang::dmi::ClazzInfo* ci = Object::clazzInfo();
  RString s = integer->toString();
  testAssert(s->equals("42") == true);
  s = (RString)integer->invoke("toString");
  testAssert(s->equals("42") == true);
}

} // namespace lang 
} // namespace dmi 
} //namespace acdk 
} //namespace tests 



