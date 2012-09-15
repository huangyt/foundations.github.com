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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/selftests/TestInterfaceImpl.h,v 1.6 2005/02/05 10:45:40 kommer Exp $

#ifndef acdkx_orb_selftests_TestInterfaceImpl_h
#define acdkx_orb_selftests_TestInterfaceImpl_h


#include <acdkx/orb/orb.h>
#include <acdkx/orb/std_orb.h>
#include "TestInterface.h"

namespace acdkx {
namespace orb {
namespace selftests {

ACDK_DECL_CLASS(TestInterfaceImpl);

class ACDKX_ORB_PUBLIC TestInterfaceImpl
: extends ::acdkx::orb::ServerDelegate
, implements ::acdkx::orb::selftests::TestInterface 
{
  //DECL_ACDK_DEFAULT_METACLASS(TestInterface)
  ACDK_WITH_METAINFO(TestInterfaceImpl)
  ACDK_ORB_IMPL_INTERFACE(TestInterface)
public:
  int iValue;
  TestInterfaceImpl()
  : ServerDelegate()
  , iValue(0)
  {
  }
  virtual void activate() THROWS1(RTestException) 
  {
  }
  virtual void hold_requests(OUT(bool) wait_for_completion) THROWS1(RTestException) 
  {
  }
  virtual void discard_requests(OUT(bool) wait_for_completion) THROWS1(RTestException)
  {
  }
  virtual void deactivate(OUT(bool) etherealize_objects, OUT(bool) wait_for_completion) THROWS1(RTestException)
  {
  }
  
  virtual int retvalfoo() 
  {
    return iValue;
  }
  virtual void outvalue(OUT(int) inval)
  {
    inval = iValue;
  }
  virtual void invalue(IN(int) inval)
  {
    iValue = inval;
  }
  virtual void inoutvalue(INOUT(int) inoutval)
  {
    int t = inoutval;
    inoutval = iValue;
    iValue = t;
  }
  virtual RString stringTest(IN(RString) istr, OUT(RString) ostr)
  {
    ostr = istr + " returned";
    return " returned";
  }
  virtual RObject getManager()
  {
    return new ::acdk::lang::Integer(iValue);
  }
  virtual oneway void doSomething()
  {
    Thread::sleep(3000);
  }
  virtual void throwException(int extype) THROWS2(RTestException, RNumberFormatException)
  {
    if (extype == 1)
      THROW1(TestException, "This is only an exception for testing");
    if (extype == 2)
      THROW1(NumberFormatException, "A NumberFormatException for testing");
    THROW1(Throwable, "another exception for testing");
  }
};


} // namespace selftests 
} // namespace orb 
} // namespace acdkx 




#endif //acdkx_orb_selftests_TestInterface_h
