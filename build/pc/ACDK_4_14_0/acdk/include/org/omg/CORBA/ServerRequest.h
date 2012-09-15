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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/ServerRequest.h,v 1.6 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_ServerRequest_h
#define org_omg_CORBA_ServerRequest_h

#include "CORBA.h"
#include "Context.h"
#include "OrbExceptions.h"

namespace org {
namespace omg {
namespace CORBA {

ACDK_DECL_INTERFACE(ServerRequest);

class ACDKX_ORB_PUBLIC ServerRequest 
: extends ::acdk::lang::Object
{
public:
  virtual RString op_name() 
  {
    return operation();
  }
  virtual RString operation() 
  {
    THROW0(NO_IMPLEMENT);
    return Nil;
  }
  
  virtual ::org::omg::CORBA::RContext ctx() = 0;
  
  /*
  //@deprecated use arguments()
  virtual void params(org.omg.CORBA.NVList params) { arguments(params); }
  virtual void arguments(org.omg.CORBA.NVList nv) {
  throw new org.omg.CORBA.NO_IMPLEMENT();
  }
  */
  /*
  /// @deprecated use set_result()
  virtual void result(org.omg.CORBA.Any result) {
  set_result(result);
  }
  virtual void set_result(org.omg.CORBA.Any result) {
  throw new org.omg.CORBA.NO_IMPLEMENT();    
  }
  */
  /*
  /// @deprecated use set_exception()
  virtual void except(org.omg.CORBA.Any except) { set_exception(except); }
  virtual void set_exception(org.omg.CORBA.Any except) {
  throw new org.omg.CORBA.NO_IMPLEMENT();
  }
  */
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_ServerRequest_h
