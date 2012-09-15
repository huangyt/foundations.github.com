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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/PortableServer/POAManager.h,v 1.11 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_PortableServer_POAManager_h
#define org_omg_PortableServer_POAManager_h


#include <acdkx/orb/orb.h>

#include <acdkx/orb/std_orb.h>


/*
#include <acdkx/arb/arb.h>
#include <acdkx/arb/AObjectImpl.h>
*/

namespace org {
namespace omg {
namespace PortableServer {





ACDK_DECL_THROWABLE(AdapterInactive, Throwable);

class ACDKX_ORB_PUBLIC AdapterInactive 
: extends ::acdk::lang::Throwable 
{ 
  ACDK_WITH_METAINFO(AdapterInactive)  
public: 
  AdapterInactive() : Throwable() {} 
  AdapterInactive(IN(RString) msg) : Throwable(msg) {} 
};


ACDK_DECL_INTERFACE(POAManager);

ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbDispatchAttribute())
class ACDKX_ORB_PUBLIC POAManager 
: implements ::org::omg::CORBA::portable::InvokeHandler
{
  ACDK_WITH_METAINFO(POAManager)
public:
  ACDK_CORBA_INTERFACE(POAManager)
public:
  enum State 
  {
    HOLDING, 
    ACTIVE, 
    DISCARDING, 
    INACTIVE
  };
  virtual void activate() THROWS1(RAdapterInactive) = 0;
  virtual void hold_requests(OUT(bool) wait_for_completion) THROWS1(RAdapterInactive) = 0;
  virtual void discard_requests(OUT(bool) wait_for_completion) THROWS1(RAdapterInactive) = 0;
  virtual void deactivate(OUT(bool) etherealize_objects, OUT(bool) wait_for_completion) THROWS1(RAdapterInactive) = 0;
  
  virtual int retvalfoo() = 0;
  virtual void outvalue(OUT(int) inval) = 0;
  virtual void invalue(IN(int) inval) = 0;
  virtual void inoutvalue(INOUT(int) inval) = 0;
  virtual RString stringTest(IN(RString) ostr, OUT(RString) istr) = 0;
  virtual ::org::omg::CORBA::RObject getManager() = 0;
  virtual oneway void doSomething() = 0;
  //virtual State get_state() = 0;
};

} // namespace PortableServer
} // namespace omg
} // namespace org

#endif //org_omg_PortableServer_POAManager_h
