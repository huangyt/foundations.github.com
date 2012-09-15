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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_ObjectBase_Test.cpp,v 1.16 2005/04/13 13:00:01 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/tools/aunit/DmiTestClass.h>

#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/ByteBuffer.h>
#include <acdk/lang/ClassCastException.h>
#include <acdk/lang/sys/core_tick.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( ObjectBase_Test )
  DECLARE_TEST( badCast )
  DECLARE_TEST( perfIn )
  DECLARE_TEST( inOutParams )
  DECLARE_TEST( serializedClone )
END_DECLARE_TEST( ObjectBase_Test  )

BEGIN_DEFINE_TEST( ObjectBase_Test )
  ADD_TEST( ObjectBase_Test, badCast ) 
  ADD_TEST( ObjectBase_Test, perfIn ) 
  ADD_TEST( ObjectBase_Test, inOutParams ) 
  ADD_TEST( ObjectBase_Test, serializedClone ) 
  
END_DEFINE_TEST( ObjectBase_Test )


void ObjectBase_Test::badCast()
{
#ifndef ACDK_NO_BADCAST_CHECKING
  RObject obj = new String("asdf");
  try {
    RInteger integer = new Integer(1);
    integer = (RInteger)obj;

  } catch (RClassCastException ex) {
    System::out->println("Expected ClassCast: " + ex->getMessage());
  }
#endif
}

void fooIn(IN(RObject) obj, IN(RString) val, int reccount)
{
  if (reccount >= 0)
    fooIn(obj, val, --reccount);
}


void foo(RObject obj, RString val, int reccount)
{
  if (reccount >= 0)
    foo(obj, val, --reccount);
}



using ::acdk::lang::sys::core_tick;
using ::acdk::lang::sys::tick_t;
void
ObjectBase_Test::perfIn()
{
  int loops = 100000;
  RObject obj = new Object();
    RString str = "asdf";
    
  {
    tick_t start = core_tick::now();
    for (int i = 0; i < loops; ++i)
    {
      foo(obj, str, 50);
    }
    tick_t end = core_tick::now();
    sys::coreout << "foo(RObject obj, RString val): " << int(end - start) << sys::eofl;
  }
  {
    tick_t start = core_tick::now();
    for (int i = 0; i < loops; ++i)
    {
      fooIn(obj, str, 50);
    }
    tick_t end = core_tick::now();
    sys::coreout << "IN(RObject) obj, IN(RString) val, int reccount): " << int(end - start) << sys::eofl;
  }
}

void inOutTest(IN(int) iint, OUT(int) oint, IN(RString) istr, OUT(RString) oval)
{
  oint = iint + 1;
  oval = new String(istr + " returned");

}



void
ObjectBase_Test::inOutParams()
{
  RString str;
  int outv;
  inOutTest(1, outv, "val", str);
  testAssert(outv == 2);
  testAssert(str->equals("val returned") == true);
  if (Nil == str)
    ;

}

using namespace ::acdk::util;
using namespace ::acdk::tools::aunit;


void
ObjectBase_Test::serializedClone()
{
  /*
  RThrowable ex = new Throwable();
  RStackFrameArray tsa = ex->getStackFrames();

  RStackFrameArray tsa2 = (RStackFrameArray)tsa->serialized_clone(true);
  for (int i = 0; i < tsa2->length(); ++i)
  {
    RStackFrame sf = tsa2[i];
    //RStackFrame sf2 = RStackFrame((StackFrame*)(void*)dynamic_cast<StackFrame*>(sf.impl()), sf.impl());
    sf->hasFileAndLine();
  }
  */
  /*
  RProperties orgProps = System::getProperties();
  RProperties clonedProps = (RProperties)orgProps->serialized_clone(true);
  int psize = clonedProps->size();
  */
  RDmiTestClass tdmit = new DmiTestClass();
  tdmit->pubSet = new HashSet(10);
  tdmit->pubSet->add(new Integer(2));
  RDmiTestClass tdmitc = (RDmiTestClass)tdmit->serialized_clone(true);
  int i = tdmitc->pubSet->size();
}

} // namespace lang 
} //namespace acdk 
} //namespace tests 


