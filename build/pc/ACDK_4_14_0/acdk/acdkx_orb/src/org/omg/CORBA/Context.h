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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/Context.h,v 1.6 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_Context_h
#define org_omg_CORBA_Context_h

#include "CORBA.h"


namespace org {
namespace omg {
namespace CORBA {

ACDK_DECL_CLASS(Context);

class ACDKX_ORB_PUBLIC Context
: extends ::acdk::lang::Object
{
public:
  virtual RString context_name() = 0;
  virtual RContext parent() = 0;
	virtual RContext create_child(IN(RString) child_ctx_name) = 0;
  /*
	virtual void set_one_value(RString propname, org.omg.CORBA.Any propvalue) = 0;
	virtual void set_values(::org::omg::CORBA::RNVList values) = 0;
	virtual void delete_values(RString propname) = 0;
	virtual ::org::omg::CORBA::RNVList get_values(RString start_scope, int op_flags, RString pattern) = 0;	
  */
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_Context_h
