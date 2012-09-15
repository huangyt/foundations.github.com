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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ObjectDebug.cpp,v 1.15 2005/03/23 18:04:32 kommer Exp $


#if 0

#include <acdk.h>

#include "ObjectDebug.h"


namespace acdk {
namespace lang {


int DebugObjectPool::debugLevel = 8;
DebugObjectPool _debugObjectPool;

void 
DebugObjectPool::pushFrame(const char* name)
{
  frame = new DebugStackFrame(frame, name);
}

void 
DebugObjectPool::popFrame()
{
  if (frame) {
    DebugStackFrame* tframe = frame->upper;
    delete frame;
    frame = tframe;
  }
}




void 
_debugPrint(const char* msg)
{
#ifdef ACDK_DEBUG
  sys::coreout << msg << sys::eofl;
#endif //ACDK_DEBUG
}



void 
DebugStackFrame::addObject(const Object* obj, size_t size)
{
#ifdef ACDK_USE_GC
  if (obj == 0 /*|| obj->isWeakRef()*/)
    return;
  ++_created;
  _BytesAllocated += size;
  stack.push_back(obj);
#endif //ACDK_USE_GC
}

bool 
DebugStackFrame::hasObject(const Object* obj, bool recursiv) const
{
  std::vector<const Object*>::const_reverse_iterator it = stack.rbegin();
  std::vector<const Object*>::const_reverse_iterator end = stack.rend();
  while (it != end) {
    if (*it == obj) {
      return true;
    }
    ++it;
  }
  if (upper != 0 && recursiv == true)
    return upper->hasObject(obj);
  return false;
}

bool 
DebugStackFrame::removeObject(const Object* obj)
{
#ifdef ACDK_USE_GC
  if (obj == 0 /*|| obj->isWeakRef()*/)
    return true;
  std::vector<const Object*>::iterator it = stack.begin();
  std::vector<const Object*>::iterator end = stack.end();
  while (it != end) {
    if (*it == obj) {
      stack.erase(it);
      ++_destroyed;
      return true;
    }
    ++it;
  }
  if (upper)
    return upper->removeObject(obj);

  _debugObjectPool.print(SBSTR("Try to remove Object out of: " << name.c_str())->c_str(), 0);
#endif //ACDK_USE_GC
  return false;
}

bool
DebugStackFrame::checkRecursivReference(const RObject* robj)
{
  if (robj->impl() == 0)
    return false;

  Object*obj = robj->impl()/*->getObjectPtr()*/;
  if (obj->isMarked()) {
    if (DebugObjectPool::debugLevel > 8) 
      sys::coreout << "Found recursive Reference" << sys::eofl;
    return true;
  }
  ControlPushPopOnStack<ControlObjectSetMarket> _mark(obj);
  //dmi::SysFields f = obj->getInternalFields(::acdk::lang::dmi::MiNonStatic);// only collectables
  FieldReferences f;
  obj->getCollectableFields(f);

  FieldReferences::const_iterator it = f.begin();
  FieldReferences::const_iterator end = f.end();
  for (; it < end; ++it) 
  {
    if (checkRecursivReference(*it) == true)
      return true;
  }
  return false;
}

bool
DebugStackFrame::checkRecursivReferences() const
{
  std::vector<const Object*>::const_reverse_iterator it = stack.rbegin();
  std::vector<const Object*>::const_reverse_iterator end = stack.rend();
  for (; it != end; ++it) 
  {
    const Object* obj = (*it);
    if (obj->isMarked() == true) {
      if (DebugObjectPool::debugLevel > 8) 
        sys::coreout << "Found recursive Reference" << sys::eofl;
      return true;
    }
    ControlPushPopOnStack<ControlObjectSetMarket> _mark(obj);

    //dmi::SysFields f = const_cast<Object*>(obj)->getInternalFields();
    FieldReferences f;
    const_cast<Object*>(obj)->getCollectableFields(f);
    FieldReferences::const_iterator fit = f.begin();
    FieldReferences::const_iterator fend = f.end();
    for (;fit != fend; ++fit) 
    {
      if (checkRecursivReference(*fit) == true)
        return true;
    }
  }
  return false;
}

struct GC_ObjectRelatives
{
  bool marked;
  std::vector<const Object*> deps;
};


typedef std::map<const Object*, GC_ObjectRelatives> GC_ObjectTable;


bool
findNonRecDepends(const Object* findobject, const Object* obj, GC_ObjectTable& objectTable)
{
  GC_ObjectRelatives& rel = objectTable[obj];
  if (rel.marked == true)
    return false;
  if (rel.deps.size() == 0)
    return true;
  
  rel.marked = true;
  std::vector<const Object*>::iterator it = rel.deps.begin();
  std::vector<const Object*>::iterator end = rel.deps.end();
  //bool foundrecref = false;
  while (it != end) {
    if ((*it) != findobject) {
      if (objectTable.find(*it) == objectTable.end()) {
        rel.marked = false;
        return true;
      }
      if (findNonRecDepends(findobject, *it, objectTable) == true) {
        rel.marked = false;
        return true;
      } 
      
    } 
    ++it;
  }
  rel.marked = false;
  return false;
}


bool
DebugStackFrame::_setInFieldToNil(const Object* obj, bool recursiv)
{
  bool foundrefinfield = false;
frombeginning:
  ObjectStack::iterator oit = stack.begin();
  ObjectStack::iterator oend = stack.end();
  
  for (; oit < oend; ++oit) 
  {
    const Object* fieldobj = (*oit);
    FieldReferences f;
    const_cast<Object*>(fieldobj)->getCollectableFields(f);
    FieldReferences::iterator fit = f.begin();
    FieldReferences::iterator fend = f.end();
    for (; fit != fend; ++fit) 
    {
      const RObject* o = *fit;;
      if (o->impl() == obj) 
      {
        *const_cast<RObject*>(o) = Nil;
        foundrefinfield = true;
        goto frombeginning;
      }
    }
  }
  if (recursiv == true && upper != 0)
    return upper->_setInFieldToNil(obj, recursiv) || foundrefinfield;
  return foundrefinfield;
}


bool 
DebugStackFrame::gc(bool recursiv)
{
  /** building table, whith key object, and a list of dependensies.
    A is hold by B
    B is hold by C and A
  */
  bool bret = false;
  GC_ObjectTable objectTable;
  ObjectStack::reverse_iterator oit = stack.rbegin();
  ObjectStack::reverse_iterator oend = stack.rend();
  for (; oit < oend; ++oit) 
  {
    FieldReferences f;
    const_cast<Object*>(*oit)->getCollectableFields(f);
    FieldReferences::const_iterator fit = f.begin();
    FieldReferences::const_iterator fend = f.end();
    for (; fit < fend; ++fit) 
    {
      Object* fobj = (*fit)->impl();
      objectTable[fobj].deps.push_back(*oit);
    }
  }
beginsearch:
  GC_ObjectTable::iterator tit = objectTable.begin();
  GC_ObjectTable::iterator tend = objectTable.end();
  for (; tit != tend; ++tit) 
  {
    if ((*tit).first == 0) 
      continue;
    
    if (findNonRecDepends((*tit).first, (*tit).first, objectTable) == true) 
    {
      objectTable.erase(tit);
      goto beginsearch;
    }
  }
  //beginsweap:
  tit = objectTable.begin();
  tend = objectTable.end();
  for (; tit != tend; ++tit) 
  {
    const Object* optr = (*tit).first;
    if (optr == 0) 
      continue;
    if (hasObject(optr, recursiv) == false) 
      continue;
    if (_setInFieldToNil((*tit).first, recursiv) == true)
      bret = true;
  }
  return bret;

/*

  
restart_gc:
  ObjectStack::reverse_iterator it = stack.rbegin();
  ObjectStack::reverse_iterator end = stack.rend();
  while (it != end) {
    if (gc(*it) == true) {
      bret = true;
      goto restart_gc;
    }
    ++it;
  }
  return bret;
*/
}

/*
struct GcObject
{
  const Object* obj;
  int recRefCount;
  GcObject(Object* o) : obj(o), recRefCount(0) { }
  GcObject(const GcObject& other) : obj(other.obj), recRefCount(other.recRefCount) { }
};
*/





//static
bool
DebugStackFrame::gc(const Object* obj, GcObjectStack& stack)
{
  stack[obj] = 0;
  ControlPushPopOnStack<ControlObjectSetMarket> _mark(obj);
  FieldReferences f;
  const_cast<Object*>(obj)->getCollectableFields(f);
  FieldReferences::const_iterator it = f.begin();
  FieldReferences::const_iterator end = f.end();
  bool retval = false;
  for (; it < end;  ++it) 
  {
    const RObject* o = *it;
    if (o != 0 && o->impl() != 0) 
    {
      if (o->impl()->/*getObjectPtr()->*/isMarked() == true) 
      {
        ++stack[o->impl()/*->getObjectPtr()*/];
        retval = true;
        continue;
      } 
      else 
      {
        retval = gc(o->impl()/*->getObjectPtr()*/, stack) || retval;
      }
    }
  }
  return retval;
}

/* not needed
//static 
void
DebugStackFrame::setRefRecToNil(const Object* obj, const Object* refobj)
{
  dmi::SysFields f = obj->getInternalFields();
  dmi::SysFields::const_iterator it = f.begin();
  dmi::SysFields::const_iterator end = f.end();
  bool retval;
  while (it != end) {
    if ((*it).type == dmi::SysField::FT_Object) {
      const RObject* o = (*it).oval;
      if (o != 0 && o->getObjectPtr() != 0 && o) {
        if (o->getObjectPtr() != refobj)
          setRefRecToNil(o->getObjectPtr(), refobj);
        if (o->getObjectPtr() == refobj) {
          //const_cast<RObject*>(o)->setWeakRef();
          //const_cast<RObject*>(o)->releaseRef();
        }
      }
    }
    ++it;
  }
}
*/


void
DebugStackFrame::_setInFieldToNil(const Object* obj, GcObjectStack& gcstack)
{
  GcObjectStack::iterator oit = gcstack.begin();
  GcObjectStack::iterator oend = gcstack.end();
  for (; oit != oend; ++oit) 
  {
    const Object* obj = (*oit).first;
    FieldReferences f;
    const_cast<Object*>(obj)->getCollectableFields(f);
    FieldReferences::iterator fit = f.begin();
    FieldReferences::iterator fend = f.end();
    for (; fit < fend; ++fit) 
    {
      const RObject* o = *fit;
      if (o->impl() == obj) 
      {
        *const_cast<RObject*>(o) = Nil;
        return;
      }
    }
  }
}

bool 
DebugStackFrame::gc(const Object* obj)
{
#ifndef ACDK_USE_EXT_REFERER
#ifdef ACDK_USE_GC
  if (obj == 0)
    return false;
  GcObjectStack _gcstack;
  if (gc(obj, _gcstack) == false)
    return false;
  GcObjectStack::iterator it = _gcstack.begin();
  GcObjectStack::iterator end = _gcstack.end();
  for (; it != end; ++it) 
  {
    if ((*it).first != 0) 
    {
      const Object* obj = (*it).first;
      int rrefc = (*it).second;
      int orc = obj->refCount(); 
      if (orc <= rrefc) 
      {// here only internal recursive reference
        if (DebugObjectPool::debugLevel > 8) 
          sys::coreout << "GC Object" << sys::eofl;
        //_debugObjectPool.removeObject(obj); //delete obj will do that
        // avoid, that releaseRef causes delete this;
        //_gcstack.erase(it); is not needed, because _gcstack will be destroyed in return
        //const_cast<Object*>(obj)->setWeakRef();
        _setInFieldToNil(obj, _gcstack);
        //delete obj;
        return true;
      }
    }
  }
#endif //ACDK_USE_GC
#endif //ACDK_USE_EXT_REFERER
  return false;
}  

void
DebugObjectPool::printAllObjects(sys::core_output& os)
{
  
  if (frame == 0)
    return;
  DebugStackFrame* sictop = frame;
  pushFrame("DebugObjectPool::printAllObjects");
  try {
    DebugStackFrame* cf = sictop;
    while (cf != 0) {
      cf->printObjects(os);
      cf = cf->upper;
    }
  } catch (...) {
  }
  popFrame();
}

void
DebugObjectPool::printObjects(sys::core_output& os)
{
   
  if (frame == 0)
    return;
  DebugStackFrame* sictop = frame;
  pushFrame("DebugObjectPool::printObjects");
  try {
    sictop->printObjects(os);
  } catch(...) {
  }
  popFrame();
}

void
DebugStackFrame::printObjects(sys::core_output& os) const
{
#ifdef ACDK_USE_GC

  os << "StackFrame: " << name.c_str() << ". created: " << _created << ", destroyed: " << _destroyed << sys::eofl;
  ObjectStack::const_iterator it = stack.begin();
  ObjectStack::const_iterator end = stack.end();
  os << "Allocated Objects: " << sys::eofl;
  const Object* obj = 0;
  int oldDebugLevel = DebugObjectPool::debugLevel;
  DebugObjectPool::debugLevel = 8;
  for (; it < end; ++it) 
  {
    obj = *it;
    //if (obj->isWeakRef() == false)
    os << " Object: " <<  const_cast<Object*>(obj)->getClass()->getName()->c_str() << "; " << obj->refCount() << " references: " << const_cast<Object*>(obj)->toString()->c_str() << sys::eofl;
  }
  os << "End ofAllocated Objects\n" << sys::eofl;
  DebugObjectPool::debugLevel = oldDebugLevel;
  
#endif //ACDK_USE_GC
}

int 
DebugObjectPool::objectCount() const
{
  if (frame == 0)
    return 0;
  return frame->objectCount();
}

int 
DebugObjectPool::objectAllCount() const
{
  const DebugStackFrame* cf = frame;
  int erg = 0;
  while (cf != 0) 
  {
    erg += cf->objectCount();
    cf = cf->upper;
  }
  return erg;
}
  

void 
DebugObjectPool::addObject(const Object* obj, size_t size)
{
  if (_inDebug == true)
    return;
  _inDebug = true;

  ensureFrame();
  if (debugLevel > 8) 
    sys::coreout << "Add Object" << sys::eofl; // no more information, because Object is not initialized here
  frame->addObject(obj, size);
  _inDebug = false;
}

void 
DebugObjectPool::removeObject(const Object* obj)
{
  if (_inDebug == true)
    return;
  _inDebug = true;
  if (obj == 0 || obj->isWeakRef() == true) {
    _inDebug = false;
    return;
  }
  ensureFrame();
  if (debugLevel > 8)
    sys::coreout << "Remove Object:" << const_cast<Object*>(obj)->getName()->c_str() << sys::eofl;
  if (frame->removeObject(obj) == false)
    sys::coreout << "Object: " << const_cast<Object*>(obj)->toString()->c_str() << " not found in DebugObjectPool!" << sys::eofl;
  _inDebug = false;
}

bool 
DebugObjectPool::hasObject(const Object* obj) const
{
  ensureFrame();
  return frame->hasObject(obj);
}

void 
DebugObjectPool::print(const char* msg, int level) // = 0
{
  if (level <= debugLevel)
    sys::coreout << msg << sys::eofl;
}


bool
DebugObjectPool::checkRecursivAllReferences() const
{
  const DebugStackFrame* cf = frame;
  while (cf != 0) {
    if (cf->checkRecursivReferences() == true)
      return true;
    cf = cf->upper;
  }
  return false;
}

bool
DebugObjectPool::checkRecursivReferences() const
{
  ensureFrame();
  return frame->checkRecursivReferences();
}


bool 
DebugObjectPool::gc()
{
  ensureFrame();
  return frame->gc();
}
  
bool 
DebugObjectPool::gcAll()
{

  return false;
}

bool
DebugObjectPool::gc(const Object* obj)
{
  ensureFrame();
  frame->gc(obj);
  return false;
}

} // lang
} // acdk


#endif // 0
