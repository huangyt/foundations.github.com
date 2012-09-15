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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/acdk_util_WeakHashMap_Test.cpp,v 1.9 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/util/WeakHashMap.h>


#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace util {

using namespace ::acdk::lang;
using namespace ::acdk::util;


  
BEGIN_DECLARE_TEST( WeakHashMap_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( WeakHashMap_Test  )

BEGIN_DEFINE_TEST( WeakHashMap_Test )
  ADD_TEST( WeakHashMap_Test, standard ) 
  
END_DEFINE_TEST( WeakHashMap_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);
USING_CLASS(::acdk::lang::, String);
USING_CLASS(::acdk::util::, WeakHashMap);


//static
void
WeakHashMap_Test::standard()
{
#if defined(ACDK_HAS_BOEHMGC)
  System::out->println("WeakHashMap doesn't work with the Boehm Gargabe Collector");
#else
  WeakHashMap whm;

  {
    RString a = new String("a");
    RString A = new String("A");
    RString b = new String("b");
    RString B = new String("B");
    
    whm.put(&a, &A);
    whm.put(&b, &B);
    testAssert(whm.get(&a)->equals(&A) == true);
    testAssert(whm.get(&b)->equals(&B) == true);
    a = Nil;
    A = Nil;
    b = Nil;
    B = Nil;
  }

  testAssert(whm.get(new String("a")) == Nil);
  {
     RObject obj1 = new String("a");
     RObject obj2 = new Object();
     whm.put(obj1, Nil);
     whm.put(obj2, Nil);
  }
#endif //defined(ACDK_HAS_BOEHMGC)
}


} // namespace util
} //namespace acdk 
} //namespace tests

