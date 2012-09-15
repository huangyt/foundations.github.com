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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AContextImpl.h,v 1.11 2005/02/05 10:45:39 kommer Exp $

#ifndef acdkx_orb_AContextImpl_h
#define acdkx_orb_AContextImpl_h

#include <acdk.h>
#include <acdk/util/ArrayList.h>

#include <org/omg/CORBA/Context.h>


namespace acdkx {
namespace orb {

USING_CLASS(::org::omg::CORBA::, Context);

ACDK_DECL_CLASS(AContextImpl);
/**
  corresponds to CORBA spec
  Not really needed
*/
class ACDKX_ORB_PUBLIC AContextImpl
: extends ::org::omg::CORBA::Context
{

private:
  RString _name;
  ::acdk::util::RArrayList _childs;
	RContext _parent;
public:
  AContextImpl(IN(RString) name, IN(RContext) p)
  : Context(),
    _name(name),
    _childs(new ::acdk::util::ArrayList()),
    _parent(p)
  {
  }
  virtual RString context_name() { return _name; }
  virtual RContext parent() { return _parent; }
	virtual RContext create_child(IN(RString) child_ctx_name)
  {
    RAContextImpl c = new AContextImpl(child_ctx_name, this);
    _childs->add((::acdk::lang::RObject)c);
    return &c;
  }

  /*
	virtual void set_one_value(RString propname, org.omg.CORBA.Any propvalue) = 0;
	virtual void set_values(::org::omg::CORBA::RNVList values) = 0;
	virtual void delete_values(RString propname) = 0;
	virtual ::org::omg::CORBA::RNVList get_values(RString start_scope, int op_flags, RString pattern) = 0;	
  */
};

} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_AContextImpl_h
