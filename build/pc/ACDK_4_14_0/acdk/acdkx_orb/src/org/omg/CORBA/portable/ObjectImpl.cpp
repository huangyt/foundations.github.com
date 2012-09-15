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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/portable/ObjectImpl.cpp,v 1.7 2005/02/05 10:45:41 kommer Exp $



#include "ObjectImpl.h"
#include "ApplicationException.h"
#include "RemarshalException.h"
#include <acdkx/orb/ServerDelegate.h>


namespace org {
namespace omg {
namespace CORBA {
namespace portable {


ObjectImpl::ObjectImpl()
{
}

/*
ObjectImpl::ObjectImpl(RString objkey)
: _orb(ORB::init()),
  _delegate(new acdkx::orb::ServerDelegate(_orb, objkey))
{
}


ObjectImpl::ObjectImpl(RORB theOrb, RString objkey)
: _orb(theOrb),
  _delegate(new acdkx::orb::ServerDelegate(_orb, objkey))
{
}
*/

//virtual 
ObjectImpl::~ObjectImpl()
{
}
  
bool 
ObjectImpl::is_local()
{
  return _get_delegate()->is_local(this);
}
  
RDelegate 
ObjectImpl::_get_delegate() 
{ 
  
  if (_delegate == Nil) {
    THROW1(Exception, "_delegate is nil");
    //_delegate = new acdkx::orb::ServerDelegate(ORB::init(), new acdkx::orb::ObjectKey(this));
  }
  return _delegate;
}


ACDK_BCC_RTHROWABLE_DEFINITION(RemarshalException)

} // namespace portable
} // namespace CORBA
} // namespace omg
} // namespace org 



