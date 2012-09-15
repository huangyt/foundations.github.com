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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/SharedOwning.cpp,v 1.10 2005/04/21 10:21:07 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/sys/core_system.h>
#include <acdk/lang/sys/ObjectHeap.h>
#include "SharedOwning.h"
#include <set>

namespace acdk {
namespace lang {
namespace ref {

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)
#define DOUT(msg) do { sys::coreout << msg << sys::eofl; } while(false)
#else
#define DOUT(msg) do { } while(false)
#endif
/*
  void registerSharedObject(Object* obj, RObject* pointsTo, bool isMaster)
  {
    SharedOwned& sharedOwning = getElement(obj);
    if (sharedOwning._obj == 0)
      NotifyObjectEvent::add(obj, this);
    _sharedElements.push_back(SharedOwned(obj, pointsTo, isMaster));
    if (isMaster == false && pointsTo != 0)
      (*pointsTo)->releaseRef();
  }*/
  
SharedOwning::~SharedOwning()
{
  NotifyObjectEvent::removeListener(this);
}

void 
SharedOwning::registerSharedObject(Object* obj, RObject* refedBy, bool isMaster)
{
  DOUT("SharedOwning:registerSharedObject: " << (void*)obj << "; refedBy: " << (refedBy != 0 ? (void*)refedBy->impl() : 0));
  SharedOwned& sharedOwning = getElement(obj);
  if (sharedOwning._obj == 0)
    NotifyObjectEvent::add(obj, this);
 _sharedElements.push_back(SharedOwned(obj, refedBy, isMaster));
 if (isMaster == true && refedBy != 0 && refedBy->impl() != 0)
  refedBy->impl()->releaseRef();
}


void 
SharedOwning::registerSharedObjectRefs(RObject* masterRef, RObject* slaveRef)
{
  DOUT("SharedOwning:registerSharedObject: master: " << (void*)masterRef->impl() << "; slave: " << (void*)slaveRef->impl());
  registerSharedObject(masterRef->impl(), masterRef, true);
  registerSharedObject(slaveRef->impl(), slaveRef, false);
}

void 
SharedOwning::unregisterSharedObjectRefs(RObject* masterRef, RObject* slaveRef, bool onDelete)
{
  unregisterSharedObject(masterRef->impl(), slaveRef, onDelete);
  unregisterSharedObject(slaveRef->impl(), masterRef, onDelete);
}

void 
SharedOwning::unregisterAll()
{
  //NotifyObjectEvent::removeListener(this);
  //_sharedElements.resize(0);
  
  for (SharedOwnedVec::iterator it = _sharedElements.begin();
      it < _sharedElements.end();
      ++it)
  {
    if (it->_obj != 0)
    {
      if (it->_isMaster == true)
      {
        
        it->_obj->addRef();
        DOUT("SharedOwn reset: " << (void*)it->_obj << "; rc=" << it->_obj->refCount());
      }
    }
  }
  _sharedElements.resize(0);
}

void 
SharedOwning::setMaster(SharedOwned& shown, bool isMaster) 
{
  if (isMaster == true)
  {
    DOUT("SharedOwning: " << (void*)shown._obj << " now Master");
    shown._isMaster = true;
    shown._obj->releaseRef();
  }
  else
  {
    DOUT("SharedOwning: " << (void*)shown._obj  << " now Slave");
    shown._isMaster = false;
    shown._obj->addRef();
  }
}



bool 
SharedOwning::isNowMaster(SharedOwned& toDel, SharedOwned& el, SharedOwned& oldMaster) 
{
  if (el._obj == 0)
    return false;
  if (el._obj->refCount() > 1)
  {
    setMaster(el, true);
    if (oldMaster._obj != 0)
      setMaster(oldMaster, false);
    else if (toDel._isMaster == true)
    {
      setMaster(toDel, false);
      return true;
    }
    return true;
  }
  /* not correctly working
  else if (toDel._obj != el._obj)
  {
    static std::set<void*> _currentlyInspected;
    if (_currentlyInspected.find((void*)el._obj) != _currentlyInspected.end())
      return false;

    _currentlyInspected.insert((void*)el._obj);
    bool bret = acdk::lang::sys::ObjectHeap::notifyBeforeObjectDestruction(el._obj);
    _currentlyInspected.erase((void*)el._obj);
    if (bret == true)
      return false;
    setMaster(el, true);
    if (oldMaster._obj != 0)
      setMaster(oldMaster, false);
    else if (toDel._isMaster == true)
    {
      setMaster(toDel, false);
      return true;
    }
    return true;
  }*/
  return false;
}


SharedOwned& 
SharedOwning::getMaster() 
{
  for (SharedOwnedVec::iterator it = _sharedElements.begin();
      it < _sharedElements.end();
      ++it)
  {
    if (it->_isMaster == true)
      return *it;
  }
  return _getEmtpySharedOwned();
}

SharedOwned& 
SharedOwning::getElement(Object* obj) 
{
  for (SharedOwnedVec::iterator it = _sharedElements.begin();
      it < _sharedElements.end();
      ++it)
  {
    if (it->_obj == obj)
      return *it;
  }
  return _getEmtpySharedOwned();
}

bool 
SharedOwning::canRelease(Object* obj) 
{
  dump("canRelease=?");
  SharedOwned& oldMaster = getMaster();
  SharedOwned& sharedOwning = getElement(obj);
  
  for (SharedOwnedVec::iterator it = _sharedElements.begin();
       it < _sharedElements.end();
        ++it)
  {
    if (isNowMaster(sharedOwning, *it, oldMaster) == true)
    {
      dump("canRelease=false");
      return false;
    }
  }
  if (sharedOwning._isMaster == false && oldMaster._obj != 0)
  {
    oldMaster._obj->addRef();
  }
  dump("canRelease=true");
  removeElement(obj);
  return true;
}

void 
SharedOwning::removeElement(Object* obj)
{
  int rc  = obj->refCount();
fromBegin:
  for (SharedOwnedVec::iterator it = _sharedElements.begin();
      it < _sharedElements.end();
      ++it)
  {
    if (obj == it->_obj)
    {
      
      if (it->_refedBy != 0)
      {
        //(*it->_refedBy) = Nil;
        it->_refedBy->_reset_ptr();
        rc  = obj->refCount();
      }
      _sharedElements.erase(it);
      goto fromBegin;
      return;
    }
  }
}

void 
SharedOwning::unregisterSharedObject(Object* obj, RObject* pointsTo, bool onDelete)
{
  removeElement(obj);
}


//static 
SharedOwned& 
SharedOwning::_getEmtpySharedOwned() 
{
  static SharedOwned emptyMaster;
  return emptyMaster;
}

void 
SharedOwning::dump(const char* msg)
{
#if defined(LOCAL_DEBUG)
  if (acdk::lang::sys::core_system::inMain() == false)
    return;
  StringBuffer sb;
  sb << msg << " ";
  for (SharedOwnedVec::iterator it = _sharedElements.begin();
       it < _sharedElements.end();
        ++it)
  {
          sb << Integer::toHexString(int((void*)it->_obj)) << ": " << it->_isMaster << ": " << (it->_obj == 0 ? -1 : it->_obj->refCount()) << /* "; " << (it->_obj == 0 ? RString("Nil") : it->_obj->toString()) << */ "|";
  }
  DOUT(sb.toString()->c_str());
#endif //defined(LOCAL_DEBUG)
}

} // ref
} // lang
} // acdk


