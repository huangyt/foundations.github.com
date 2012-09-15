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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/selftests/TestInterface.h,v 1.18 2005/04/25 13:20:47 kommer Exp $

#ifndef acdkx_orb_selftests_TestInterface_h
#define acdkx_orb_selftests_TestInterface_h

#if !defined(DOXYGENONLY)

#include <acdkx/orb/orb.h>
#include <acdkx/orb/std_orb.h>
#include <acdk/lang/NumberFormatException.h>


ACDK_DECL_INTERFACE(Hello);

enum AnEnum
{
  First = 0,
  Second
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, AnEnum);

/** 
  class to test with OB orb 
*/

ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbDispatchAttribute())
class ACDKX_ORB_PUBLIC Hello 
: implements ::org::omg::CORBA::portable::InvokeHandler
{
  ACDK_WITH_METAINFO(Hello)
public:
  ACDK_CORBA_INTERFACE(Hello)
  
public:

  virtual void hello() = 0;
  virtual AnEnum foo(IN(AnEnum) i1, OUT(AnEnum) i2)
  {
    i2 = i1;
    return i1;
  }
};



ACDK_DECL_INTERFACE(tty);
/** 
  class to test with mico orb 
*/


ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbDispatchAttribute())
class ACDKX_ORB_PUBLIC tty
: implements ::org::omg::CORBA::portable::InvokeHandler
{
  ACDK_WITH_METAINFO(tty)
public:
  ACDK_CORBA_INTERFACE(tty)
public:
  virtual void print(IN(RString) msg) = 0;
};



namespace acdkx {
namespace orb {
namespace selftests {



  
ACDK_DECL_THROWABLE(TestException, Throwable);

class ACDKX_ORB_PUBLIC TestException 
: extends ::acdk::lang::Throwable 
{ 
  ACDK_WITH_METAINFO(TestException)  
public: 
  TestException() : Throwable() {} 
  TestException(IN(RString) msg) : Throwable(msg) {} 
};


ACDK_DECL_INTERFACE(TestInterface);

ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbDispatchAttribute())
class ACDKX_ORB_PUBLIC TestInterface 
: implements ::org::omg::CORBA::portable::InvokeHandler
{
  ACDK_WITH_METAINFO(TestInterface)
public:
  ACDK_CORBA_INTERFACE(TestInterface)
public:
  enum State 
  {
    HOLDING, 
    ACTIVE, 
    DISCARDING, 
    INACTIVE
  };
  
  virtual void activate() THROWS1(RTestException) = 0;
  virtual void hold_requests(OUT(bool) wait_for_completion) THROWS1(RTestException) = 0;
  virtual void discard_requests(OUT(bool) wait_for_completion) THROWS1(RTestException) = 0;
  virtual void deactivate(OUT(bool) etherealize_objects, OUT(bool) wait_for_completion) THROWS1(RTestException) = 0;
  
  /** 
    @returns the value previoulsly set with invalue or inoutvalue
  */
  virtual int retvalfoo() = 0;

  /**
    @param outval returns the value previoulsly set with invalue or inoutvalue
  */
  virtual void outvalue(OUT(int) outval) = 0;
  /**
    @param set internal int value
  */
  virtual void invalue(IN(int) inval) = 0;
  /**
    @param inoutval set internal int value, returns previous value
  */
  virtual void inoutvalue(INOUT(int) inoutval) = 0;
  /**
    @param ostr returned istr + " returned"
    @param istr see ostr
    @return the string " returned"
  */
  virtual RString stringTest(IN(RString) istr, OUT(RString) ostr) = 0;
  /**
    returns Nil
  */
  virtual acdk::lang::RObject getManager() = 0;
  /**
    Sleeps 3 seconds
  */
  virtual oneway void doSomething() = 0;
  /**
    should throw exception:
  */
  virtual void throwException(int extype) THROWS2(RTestException, RNumberFormatException) = 0;
};

} // namespace selftests 
} // namespace orb 
} // namespace acdkx 



#endif //!defined(DOXYGENONLY)
#endif //acdkx_orb_selftests_TestInterface_h
