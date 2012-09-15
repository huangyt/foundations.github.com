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
// $Header: /cvsroot/acdk/acdk/acdk_java/tests/acdk/java/serialization/acdk_java_serialization_Basics_Test.cpp,v 1.14 2005/03/12 11:51:43 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/FileWriter.h>
#include <acdk/java/serialization/JavaObjectReader.h>
#include <acdk/java/serialization/JavaObjectWriter.h>
#include <acdk/util/ArrayList.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace java {
namespace serialization {

using namespace ::acdk::lang;



  
BEGIN_DECLARE_TEST( Basic_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( util )
END_DECLARE_TEST( Basic_Test  )

BEGIN_DEFINE_TEST( Basic_Test )
  ADD_TEST( Basic_Test, standard ) 
  ADD_TEST( Basic_Test, util ) 
END_DEFINE_TEST( Basic_Test )

RString getDumpString(IN(RString) s)
{
  StringBuffer sb;
  sb << "[" << s->length() << "] ";
  for (int i = 0; i < s->length(); ++i)
    sb << s->charAt(i) << "=" << (short)s->charAt(i) << ", ";
  return sb.toString();
}
void readTest(IN(RString) fname, IN(RObject) compare)
{
  RObject obj;
  RString acdkhome = System::getProperties()->getProperty("ACDKHOME");
  RString rfname = acdkhome + "/acdk_java/tests/acdk/java/serialization/" + fname;
  {
    
    
    ::acdk::io::FileReader fin(rfname);
    ::acdk::java::serialization::JavaObjectReader jin(&fin);
    obj = jin.readObject();
    System::out->println(getDumpString(compare->toString()) + "\n\n" + getDumpString(obj->toString()));
    System::out->println(compare->toString() + "=" + obj->toString());
    testAssert(compare->equals(obj) == true);
    ::acdk::io::FileWriter fout(rfname + ".out.dat");
    ::acdk::java::serialization::JavaObjectWriter jout(&fout);
    jout.writeObject(obj);
  }
  {
    ::acdk::io::FileReader fin(rfname + ".out.dat");
    ::acdk::java::serialization::JavaObjectReader jin(&fin);
    RObject obj2 = jin.readObject();
    testAssert(obj->equals(obj2) == true);
  }
}


void readTestArray(IN(RString) fname, IN(RObject) compare)
{
  ::acdk::io::FileReader fin(fname);
  ::acdk::java::serialization::JavaObjectReader jin(&fin);
  RObject obj = jin.readObject();
  RObjectArray c = (RObjectArray)compare;
  RObjectArray oc = (RObjectArray)obj;
  testAssert(c->length() == oc->length());
  for (int i = 0; i < c->length(); ++i)
  {
    testAssert(c[i]->equals(oc[i]) == true);
  }

}


void
Basic_Test::standard()
{
  readTest("ReadInteger.dat", new Integer(42));
  {
    
    RdoubleArray da = new doubleArray(10);
    for (int i = 0; i < da->length(); ++i)
    {
      da[i] = 0.12 * i;
    }
    readTest("doubleArray.dat", &da);
  }
  //return;
  {
    // test with multiple references
    RInteger naint = new Integer(42);
    RIntegerArray da = new IntegerArray(10);
    for (int i = 0; i < da->length(); ++i)
    {
      if ((i % 2) == 0)
        da[i] = naint;
      else
        da[i] = new Integer(i);
    }
    RString clsname = da->getClass()->getName();
    readTest("IntegerArray.dat", &da);
  }
  {
    // test with multiple references
    RString naint = _US("Bla: \\u00e4\\u00fc\\u00c4\\u00d6\\u00df\\u00e9\\u00ed");
    RStringArray da = new StringArray(10);
    for (int i = 0; i < da->length(); ++i)
    {
      if ((i % 2) == 0)
        da[i] = naint;
      else
        da[i] = Integer::toString(i);
    }
    readTest("StringArray.dat", &da);
  }
  {
    RuccharArray ca = new uccharArray(8);
    ca[0] = 'A';
    ca[1] = 'C';
    ca[2] = 'D';
    ca[3] = 'K';
    ca[4] = _UC("\\u00e4"); // ae
    ca[5] = _UC("\\u00d6"); // 'Oe'
    ca[6] = _UC("\\u00df"); // 'ss'
    ca[7] = _UC("\\u00e9"); // 'eaccent'
    readTest("charArray.dat", &ca);
  }
  {
    // "Test for StringBuffer with Umlauts: ae ue Ae Oe ss eaccent iaccent"
    RStringBuffer sb = new StringBuffer(_US("Test for StringBuffer with Umlauts: \\u00e4\\u00fc\\u00c4\\u00d6\\u00df\\u00e9\\u00ed"));
    readTest("StringBuffer.dat", &sb);
  }
  {
    RThrowable obj = new Throwable("This is the Reason");
    readTest("Throwable.dat", &obj);
  } 
}

void
Basic_Test::util()
{
  {
    ::acdk::util::RArrayList array = new ::acdk::util::ArrayList();

    array->add(new Short((short)42));
    array->add(new String("Bla"));
    //array->add(&array); // equals doesn't handle 
    readTest("ArrayList.dat", &array);
  } 
}

} // namespace serialization
} // namespace java
} //namespace acdk 
} //namespace tests

