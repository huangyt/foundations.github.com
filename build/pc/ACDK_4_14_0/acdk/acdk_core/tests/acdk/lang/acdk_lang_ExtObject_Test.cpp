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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_ExtObject_Test.cpp,v 1.7 2005/02/05 10:45:08 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/ExtObject.h>

struct MyStruct
{
  int _ivar;

  MyStruct(int i) : _ivar(i) {}
  int getI() { return _ivar; }
};


ACDK_DECL_EXT_CLASS_VAL(MyStruct);

int acdk_hashCode(INP(RMyStructObject) s) { return s->_ivar; }
::acdk::lang::RString acdk_toString(INP(RMyStructObject) s) { return acdk::lang::String::valueOf(s->_ivar); }


/*
class MyStruct;
typedef ::ExtObjectVal<MyStruct> MyStructObject;
typedef ::RefHolder<MyStructObject> RMyStructObject;
typedef ::ObjectArrayImpl<RMyStructObject> MyStructObjectArray;
typedef ::RObjectArrayImpl<RMyStructObject> RMyStructObjectArray;
*/
ACDK_DECL_EXT_CLASS_PTR(MyStruct);

namespace tests {
namespace acdk {
namespace lang {

BEGIN_DECLARE_TEST( ExtObject_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( arrays )
END_DECLARE_TEST( ExtObject_Test  )

BEGIN_DEFINE_TEST( ExtObject_Test )
  ADD_TEST( ExtObject_Test, standard )
  ADD_TEST( ExtObject_Test, arrays )
END_DEFINE_TEST( ExtObject_Test )

using namespace acdk::lang;



void
ExtObject_Test::standard()
{
  RMyStructObject iobj = new MyStructObject(42);
  int i = iobj->getI();
  testAssert(i == 42);

  RString s = iobj->toString();
  testAssert(s->equals("42") == true);

  /*
  RMyStructObjectPtr ioptr = new MyStructObjectPtr(new MyStruct(42));
  i = ioptr->getI();
  testAssert(i == 42);
  s = RObject(ioptr)->toString();
  // here the reason why not use ACDK_DECL_EXT_CLASS_PTR
  testAssert(s->equals("42") == false);
  */
}


void
ExtObject_Test::arrays()
{
  RMyStructObjectArray sa= new MyStructObjectArray(1);
  sa[0] = new MyStructObject(41);
  testAssert(sa->length() == 1);
  RMyStructObject t = new MyStructObject(42);
  sa->append(t);
  testAssert(sa[0]->getI() == 41);
  testAssert(sa[1]->getI() == 42);
  //sa->append(new MyStructObject(42));

}

} // namespace lang
} //namespace acdk
} //namespace tests



