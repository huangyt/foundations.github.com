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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiProxy.cpp,v 1.8 2005/02/05 10:44:58 kommer Exp $

#include <acdk.h>
#include "DmiProxy.h"
#include <acdk/lang/sys/LocalGcHeap.h>

namespace acdk {
namespace lang {
namespace dmi {

DmiProxyBase::DmiProxyBase(IN(RObjectArray) proxies, IN(RObject) dmiTarget, int flags, const ::acdk::lang::dmi::ClazzInfo* ci)
: _dmiTarget(new acdk::lang::ref::WeakReference(dmiTarget))
, _dmiClient(AcdkDmiClient::getDmiClient())
{
/*
  if (ci == 0)
    return;
  int i;
  for (i = 0; i < ci->getInterfacesCount(); ++i)
  {
    RClass cls = Class::getSingeltonClass(ci->interfaces[i]->type);
    cls->getDmiProxies(_parentProxies, dmiTarget, flags);
  }
  for (i = 0; i < _parentProxies->length(); ++i)
  {
    proxies->append(_parentProxies[i]);
  }
  */
}
void 
DmiProxyBase::_initThis(Object* This)
{
  This->_setObjectRefFlag(true, ObjectBase::ObjectHasLocalGc);
}

Object* 
DmiProxyBase::_cast( const ::acdk::lang::dmi::ClazzInfo* ci)
{
  if (dynamic_cast<Object*>(this)->getClazzInfo()->assignableFrom(ci) == true)
    return dynamic_cast<Object*>(this);
#if defined(ACDK_USE_WEAK_PROXY_BASE)
  /* ### TODO
  
  for (int i = 0; i < _parentProxies->length(); ++i)
  {
    if (_parentProxies[i]->getClazzInfo()->assignableFrom(ci) == true)
      return _parentProxies[i];
  }
  */
  if (_dmiTarget != Nil)
  {
    RObject tobj = _dmiTarget->get();
    acdk::lang::ref::RWeakReference tmp = _dmiTarget;
    
    _dmiTarget = Nil;
    Object* ret = tobj->_cast(ci);
    _dmiTarget = tmp;
    return ret;
  }
#else
  if (_dmiTarget != Nil)
  {
    RObject tobj = _dmiTarget;
    _dmiTarget = Nil;
    Object* ret = tobj->_cast(ci);
    _dmiTarget = tobj;
    return ret;
  }
#endif
  return 0;
}
  
bool 
DmiProxyBase::_gc_releaseRef(const Object* This) const
{
  if (This->_releaseRefCount() == true) 
    return true;
  int refC = This->refCount();
  if (refC == 1 && _dmiTarget != Nil)
    return sys::LocalGcHeap::gcObject((Object*)This);
  return false;
}

} // dmi
} // lang
} // acdk

