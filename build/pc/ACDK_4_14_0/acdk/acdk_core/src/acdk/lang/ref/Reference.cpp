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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/Reference.cpp,v 1.19 2005/03/07 13:53:35 kommer Exp $


#include <acdk.h>
#include "ref.h"
#include "NotifyObjectEvent.h"

namespace acdk {
namespace lang {
namespace ref {

using namespace acdk::lang;

Reference::Reference(IN(RObject) ref, IN(RReferenceQueue) queue/* = Nil*/)
: Object(),
  _ref(ref.impl()),
  _releaseReference(true),
  _queue(queue),
  _isOnQueue(false),
  _isDequeued(false),
  _next(Nil),
  _inDestructor(false)
{
  _ref->addRef(); // hold reference, weak reference may release reference
  NotifyObjectEvent::add(_ref, this);
  NotifyObjectEvent::add(this, this);
}

//virtual 
Reference::~Reference()
{
  _inDestructor = true;
  NotifyObjectEvent::removeObject(this);
  NotifyObjectEvent::removeListener(this);
  if (_ref != 0)
  {
    if (_releaseReference == true)
    {
      Object* tref = _ref;
      _ref = 0;
      tref->releaseRef();
    } else
      _ref = 0;
  }
}

//virtual 
RObject 
Reference::get()
{
  return _ref;
}

//virtual 
void 
Reference::clear()
{
  SYNCTHIS();
  if (_isOnQueue == true && _ref != 0)
  {
    if (_releaseReference == true)
    {
      Object* tref = _ref;
      _ref = 0;
      tref->releaseRef();
    }
  }
  _ref = 0;
  _queue = Nil;
}

//virtual 
bool 
Reference::isEnqueued()
{
  SYNCTHIS();
  return _isOnQueue;
}

//virtual
bool 
Reference::enqueue() 
{
  SYNCTHIS();
  if (_isOnQueue == true)
    return false;
  if (_queue == Nil) 
    return false;
  if (_ref != 0 && _ref->refCount() == 0)
    _ref->addRef();
  _isOnQueue = true;
  _queue->enqueue(this);
  _queue = Nil;
  return true;
}

//virtual 
void 
Reference::notifyBeforeConstruction(Object* obj)
{
  
}

//virtual 
bool 
Reference::notifyBeforeDestruction(Object* obj)
{
  SYNCTHIS();
  
  if (_inDestructor == true)
    return true;
  if (obj == this)
  {
    //sys::coreout << "bd: " << (void*)this << " " << (void*) _ref << " " << _isOnQueue << sys::eofl;
    if (_isOnQueue == true)
      return false;
    return enqueue() == false;
  } else {
    if (_isOnQueue == true || enqueue() == true)
      return false;
    return true;
  }
}

//virtual 
void 
Reference::notifyWhileDestruction(Object* obj)
{ 
  if (obj != _ref)
    return;
  _ref = 0;
}

PhantomReference::PhantomReference(IN(RObject) ref, IN(RReferenceQueue) queue)
: Reference(ref, queue)
{
  if (ref != Nil)
  {
    _releaseReference = false;
    //ref->_setObjectRefFlag(true, IsWeakRef);
    ref->releaseRef();
  }
}
SoftReference::SoftReference(IN(RObject) ref, IN(RReferenceQueue) queue)
  : Reference(ref, queue)
  {
  if (ref != Nil)
  {
    //ref->_setObjectRefFlag(true, IsWeakRef);
    //ref->releaseRef();
  }
}

SoftReference::~SoftReference()
{
  //SYNCTHIS();
}

WeakReference::WeakReference(IN(RObject) ref, IN(RReferenceQueue) queue)
: Reference(ref, queue)
{
  
  if (ref != Nil/* && _queue == Nil*/)
  {
    _releaseReference = false;
    ref->releaseRef();
  }
   //sys::coreout << "constr: " << (void*)this << " " << (void*) _ref << " " << _isOnQueue << sys::eofl;
}

WeakReference::~WeakReference()
{
  //sys::coreout << "DESTR: " << (void*)this << " " << (void*) _ref << " " << _isOnQueue << sys::eofl;  
  /* never release it
  if (_isOnQueue == false && _isDequeued == true)
  {
    _releaseReference = true;
  }*/
}

//virtual 
bool 
SoftReference::notifyBeforeDestruction(Object* obj)
{
  if (_inDestructor == true)
    return true;
  SYNCTHIS();
  if (obj == this) {
    return enqueue() == false;
  } else {
  
#ifndef ACDK_USE_EXT_REFERER 
  obj->addRef();
#endif
  obj->setSoftReference(true);
  
  if (_isOnQueue == true || enqueue() == true)
    return false;
  return true;
  }
}

//virtual 
RObject 
WeakReference::get()
{
  SYNCTHIS();
  if (_isOnQueue == true)
    return Nil;
  return _ref;
}

//virtual 
bool 
WeakReference::notifyBeforeDestruction(Object* obj)
{
  if (_inDestructor == true)
    return true;

  SYNCTHIS();
  //
  if (obj == this) 
  {
    bool ret = true;
    
    if (_isOnQueue == true && _isDequeued == false)
      ret = false;
    else if (_isOnQueue == true && _isDequeued == true)
      ret = true;
    else
      ret =  enqueue() == false;
    return ret;
  }

  if (_ref != obj)
  {
    sys::coreout << "*** not ref != obj: " << (void*)this << " " << (void*) _ref << " " << (void*) obj << " " << _isOnQueue << " " << _isDequeued << sys::eofl;
    return true;
  }
  
  if (enqueue() == true) 
  {
    _ref = 0;
    return true;
  }
  _ref = 0;
  return true;
}

//virtual 
bool 
PhantomReference::notifyBeforeDestruction(Object* obj)
{
  if (_inDestructor == true)
    return true;
  SYNCTHIS();
  if (obj == this) 
  {
    return enqueue() == false;
  }
  
  enqueue();
  return true;
}


} // ref
} // lang
} // acdk



