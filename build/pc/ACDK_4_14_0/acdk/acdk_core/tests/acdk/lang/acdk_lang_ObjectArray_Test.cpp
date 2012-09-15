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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_ObjectArray_Test.cpp,v 1.16 2005/03/23 22:26:52 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/ClassCastException.h>
#include <string>
#include <vector>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( ObjectArray_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( testClone )
  DECLARE_TEST( iteratoren )
  DECLARE_TEST( interfaces )
  DECLARE_TEST( find )
  DECLARE_TEST( className )
END_DECLARE_TEST( ObjectArray_Test  )

BEGIN_DEFINE_TEST( ObjectArray_Test )
  ADD_TEST( ObjectArray_Test, standard ) 
  ADD_TEST( ObjectArray_Test, testClone ) 
  ADD_TEST( ObjectArray_Test, iteratoren ) 
  ADD_TEST( ObjectArray_Test, interfaces ) 
  ADD_TEST( ObjectArray_Test, find ) 
  ADD_TEST( ObjectArray_Test, className ) 
  
END_DEFINE_TEST( ObjectArray_Test )



void ObjectArray_Test::standard()
{
  intArray ia(0);
  ia.insert(0, 2);
  ia.insert(0, 0);
  ia.insert(1, 1);
  ia.insert(3, 3);
  
  ia.remove(3);
  ia.remove(1);
  ia.remove(0);
  ia.remove(0);

}

void 
ObjectArray_Test::testClone()
{
  RString d = "asdf";
  d->length();
  std::vector<std::string> svec;
  svec.push_back("dfsa");
  ::acdk::lang::sys::core_vector<char> cvec;
  cvec.push_back('a');
  RStringBufferArray ba = new StringBufferArray(3);
  ba[0] = new StringBuffer("Hallo");
  ba[2] = new StringBuffer("ACDK");
  RStringBufferArray ba2 = (RStringBufferArray)ba->clone();
  testAssert(ba->equals(&ba2) == true);
}

void
ObjectArray_Test::iteratoren()
{
  RStringBufferArray ba = new StringBufferArray(3);
  ba[0] = new StringBuffer("Hallo");
  ba[2] = new StringBuffer("ACDK");
  StringBufferArray::array_iterator it = ba->begin();
  StringBufferArray::array_iterator end = ba->end();
  while (it < end)
  {
    if (*it != Nil)
      System::out->println((*it)->toString());
    else 
      System::out->println("IsNil");
    ++it;
  }
}

void
ObjectArray_Test::interfaces()
{
  RIntegerArray ia = new IntegerArray(2);
  ia[0] = new Integer(42);
  ia[1] = new Integer(43);
  RInteger i43 = new Integer(43);
  RInteger i42 = new Integer(42);
  int ci = ia[1]->compareTo(i43);
  RComparableArray ca = (RComparableArray)ia;
  ca[0] = &i42;
  if (ia[0] != i42)
    System::out->println("Casting to interface arrays will copy storage");
  RObjectArray oa = (RObjectArray)ia;
  RComparable cmp = ca[1];
  ci = cmp->compareTo(&i43);
  ci = cmp->compareTo(&i42);
  //testAssert(ca[1]->compareTo(new Integer(43)) == 0);
}

void
ObjectArray_Test::find()
{
  RStringArray sa = new StringArray(3);
  sa[0] = "A";
  sa[2] = "B";
  testAssert(sa->find("A") == 0);
  testAssert(sa->find("B") == 2);
  testAssert(sa->find("c") == -1);
  testAssert(sa->find(Nil) == 1);

}

void
ObjectArray_Test::className()
{
  RStringArray sa = new StringArray(0);
  RClass cls = sa->getClass();
  RString clsName = cls->getName();
  RString tstr = cls->toString();
}

} // namespace lang 
} //namespace acdk  
} //namespace tests 


