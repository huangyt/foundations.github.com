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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Throwable_Test.cpp,v 1.14 2005/04/28 15:00:36 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/ArithmeticException.h>
#include <acdk/lang/StackOverflowError.h>
#include <acdk/lang/sys/core_memtrace.h>
#include <acdk/io/StringWriter.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( Throwable_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( derived )
  DECLARE_TEST( throwInstance )
  DECLARE_TEST( throwNullPointer )
  DECLARE_TEST( throwThreadNullPointer )
  DECLARE_TEST( throwThreadUnhandledNullPointer )
  DECLARE_TEST( divideByZeroTest )
  DECLARE_TEST( stackOverflowException )
  DECLARE_TEST( backtrace )
END_DECLARE_TEST( Throwable_Test  )

BEGIN_DEFINE_TEST( Throwable_Test )
  ADD_TEST( Throwable_Test, standard ) 
  ADD_TEST( Throwable_Test, derived ) 
  ADD_TEST( Throwable_Test, throwInstance ) 
  ADD_TEST( Throwable_Test, throwNullPointer ) 
  ADD_TEST( Throwable_Test, throwThreadNullPointer ) 
  ADD_TEST( Throwable_Test, throwThreadUnhandledNullPointer ) 
  ADD_TEST( Throwable_Test, divideByZeroTest ) 
  ADD_TEST( Throwable_Test, stackOverflowException ) 
  ADD_TEST( Throwable_Test, backtrace ) 
  
END_DEFINE_TEST( Throwable_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);

void Throwable_Test::standard()
{
   
  RString msg = "Ooops";
  try {
    THROW1(Throwable, msg);
    testAssert(false);
   } catch (RThrowable ex) {
      testAssert(ex->getMessage()->equals(msg) == true);
      ex->printStackTrace();
   }
}

void Throwable_Test::derived()
{
  bool catched = false;
  try {
    RString msg = "Ooops";
    THROW1(Exception, msg);
    
  } catch (RThrowable ex) {
    catched = true;
    ex->printStackTrace();
  } 
  testAssert(catched == true);
}

ACDK_DECL_THROWABLE(MyThrowable, NoSuchMethodException);

class MyThrowable
: extends NoSuchMethodException
{
public:
  MyThrowable() {}
  MyThrowable(IN(RString) msg) : NoSuchMethodException(msg) {}
};

void
Throwable_Test::throwInstance()
{
  try {
    try {
      THROW0(NoSuchMethodException);
    } catch (RException ex) {
      THROW_INSTANCE(ex); // this should throw RNoSuchMethodException internally
    } 
  } catch (RNoSuchMethodException ex) {
    
  } catch (RException ex) {
    testAssertComment(false, "structured throwing via THROW_INSTANCE failed");
  }
  try {
    try {
      THROW0(MyThrowable);
    } catch (RMyThrowable ex) {
      THROW_INSTANCE(ex); // this should throw RNoSuchMethodException internally
    } 
  } catch (RMyThrowable ex) {
    
  } catch (RException ex) {
    testAssertComment(false, "structured throwing via THROW_INSTANCE failed");
  }
}



void
Throwable_Test::throwNullPointer()
{
  try {
    RObject o = Nil;
    o->toString();
  } catch (RNullPointerException ex) {
    ex->printStackTrace();

  }
}

class ThrowNullPointerThread
: extends ::acdk::lang::Thread
{
public:
  ThrowNullPointerThread(){}
  virtual void callMalad()
  {
    RObject o = Nil;
    o->toString();
  }
  void run()
  {
    try {
      callMalad();
    } catch (RNullPointerException ex) {
      ex->printStackTrace();
   }
  }
};


void
Throwable_Test::throwThreadNullPointer()
{
  RThread t = new ThrowNullPointerThread();
  t->start();
  t->join();
}

class ThrowUnhandledNullPointerThread
: extends ::acdk::lang::Thread
{
public:
  ThrowUnhandledNullPointerThread(){}
  virtual void callMalad()
  {
    RObject o = Nil;
    o->toString();
  }
  void run()
  {
    callMalad();
  }
};

void
Throwable_Test::throwThreadUnhandledNullPointer()
{
  /** tear down application
  RThread t = new ThrowUnhandledNullPointerThread();
  t->start();
  t->join();
  */
}

void
Throwable_Test::divideByZeroTest()
{
#if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
  try {
    int c = 0;
    float ret = 32 / c;
    testAssertComment(false, "should caught ArithmeticException");
  } catch (RArithmeticException ex) {
    testAssertComment(true, "divide by zero triggered ArithmeticException");
  }
#else
  System::out->println("this testcase is not supported on this platform");
#endif
}

bool runRecursive()
{
  char buffer[1024 * 100];
  buffer[sizeof(buffer) - 1] = 0;
  int i = 42;
  if (i)
    return runRecursive();
  else
    return false;
}

void
Throwable_Test::stackOverflowException()
{
  #if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
  try {
    runRecursive();

    testAssertComment(false, "should caught StackOverflowError");
  } catch (RStackOverflowError ex) {
    testAssertComment(true, "stack overflow triggered StackOverflowError");
    ex->printStackTrace(System::out);
    System::out->println(ex->getMessage());
    System::out->flush();
  }
#else
  System::out->println("this testcase is not supported on this platform");
#endif
}

void
Throwable_Test::backtrace()
{
  //System::in->readLine();
  ::acdk::lang::sys::core_memtrace trace;
  {
    ::acdk::io::StringWriter sout;
    ::acdk::io::PrintWriter pout(&sout);
    System::printStackTrace(&pout);
  }
  trace.reportUnfreed();
}

} // namespace lang 
} //namespace acdk 
} //namespace tests 


