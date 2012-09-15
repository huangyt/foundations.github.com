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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/RC_GC_Heap.cpp,v 1.17 2005/03/07 14:02:12 kommer Exp $


#include <acdk.h>
#include "sys.h"
#include "RC_GC_Heap.h"
#include "AllocatorInternals.h"

#include "../ref/NotifyObjectEvent.h"

#  include <typeinfo>

#ifdef _MSC_VER
# pragma warning(disable : 4512)
#endif 

#include <vector>
#include <map>

#include "../Exception.h"

namespace acdk {
namespace lang {
namespace sys {


//using namespace std;
using namespace acdk::lang;
/*
struct GC_ObjectRelatives
{
  bool marked;
  std::vector<const Object*> deps;
};
typedef std::map<const Object*, GC_ObjectRelatives> GC_ObjectTable;
*/
/*
//virtual 
void
RC_GC_Heap::add(const Object* obj, size_t size)
{
  _allocated += size;
  ++_objectCount;
  _stack[obj] = size;
}
*/

//GC_ObjectTable* __currentObjectTable = 0;
/*
//virtual 
bool 
RC_GC_Heap::removeObject(const Object* obj, bool recursiv)
{
  
  ObjectStack::iterator it = _stack.find(obj);
  bool found = false;
  if (it != _stack.end()) {
    --_objectCount;
    _stack.erase(it);
    found = false;
  }
  if (__currentObjectTable != 0) {
    GC_ObjectTable::iterator fit = __currentObjectTable->find(obj);
    if (fit != __currentObjectTable->end())
      __currentObjectTable->erase(fit);
  }
    
  return found;
}

//virtual 
bool 
RC_GC_Heap::hasObject(const Object* obj, bool recursiv) const
{
  return _stack.find(obj) != _stack.end();
}

//virtual 
void 
RC_GC_Heap::printObjects(sys::core_output& os, bool recursiv) const
{
  ObjectStack::const_iterator it = _stack.begin();
  ObjectStack::const_iterator end = _stack.end();
  while (it != end) {
    if ((*it).first != 0) {
      const Object* o = (*it).first;
      os << typeid(*o).name()  << ": " << o->refCount() << sys::eofl; //": " << o->toString()->c_str() << std::end;
    }
    ++it;
  }
  if (recursiv == true && up() != 0)
    up()->printObjects(os, recursiv);
}

//virtual 
int 
RC_GC_Heap::objectCount(bool recursiv) const
{
  return _objectCount;
}

//virtual 
size_t 
RC_GC_Heap::totalAllocated(bool recursiv) const
{
  return _allocated;
}

//virtual 
bool 
RC_GC_Heap::checkCyclicReferences(bool recursiv) const //### not implemented?
{
  return false;
}



*/
/*
void 
RC_GC_Heap::removeAllStaticReferences()
{
  ObjectStack::const_iterator it = _stack.begin();
  ObjectStack::const_iterator end = _stack.end();
  while (it != end) {
    const Object* obj = (*it).first;
    if (_valid(obj) == true) {
      if ((*it).first != 0 && (*it).first->isStaticRef() == true) {
        if (obj->refCount() == 0)
          sys::coreout << "Refcount Object " << obj << " is already 0" << sys::eofl;
        const_cast<Object*>(obj)->finalize();
        obj->releaseRef();
        removeObject((*it).first);
        removeAllStaticReferences();
        return;
      }
    }
    ++it;
  }
}
*/
/*
  dead code
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
*/

/*
dead code

bool
RC_GC_Heap::_setInFieldToNil(const Object* obj, bool recursiv)
{
  bool foundrefinfield = false;
frombeginning:
  ObjectStack::iterator oit = stack().begin();
  ObjectStack::iterator oend = stack().end();
  
  while (oit != oend) 
  {
    if (!((*oit).second.flags & ObjectMem)) 
    {
      ++oit;
      continue;
    }
    Object* fieldobj = reinterpret_cast<Object*>((*oit).first);
    if (_valid(fieldobj) == true) 
    {
      FieldReferences frefs;
      const_cast<Object*>(fieldobj)->getCollectableFields(frefs);
      FieldReferences::const_iterator fit = frefs.begin();
      FieldReferences::const_iterator fend = frefs.end();
    for (; fit < fend; ++fit)
    {
      const RObject* o = *fit;
      if (o->impl() == obj) 
      {
        bool restart = false;
        if (o->impl() != 0 && o->impl()->refCount() == 1)
          restart = true;
         *const_cast<RObject*>(o) = Nil;//
         foundrefinfield = true;
         if (restart)
          goto frombeginning;
      }
    }
      
    }
    ++oit;
  }
  if (recursiv == true && up() != 0)
    return up()->_setInFieldToNil(obj, recursiv) || foundrefinfield;
  return foundrefinfield;
}

*/


// in pagedheap.cpp
bool doGc(Allocator* allocator, AllocatorObjectsIterator* iterator, bool recursiv);

//virtual 
bool 
RC_GC_Heap::gc(bool recursiv)
{
  TracedRawAllocatorIterator trai(_allocator);
  return doGc(_allocator, &trai, recursiv);
/*
  {
BegincleanUp:
  TracedRawAllocator::ObjectsSet::iterator oit =  _allocator->_heap.begin();
  TracedRawAllocator::ObjectsSet::iterator oend =  _allocator->_heap.end();
  //ObjectStack::iterator oit = _stack.begin();
  //ObjectStack::iterator oend = _stack.end();
    while (oit != oend) 
    {  
      if (!((*oit).second.flags & ObjectMem)) {
       ++oit;
        continue;
      }
      Object* o = reinterpret_cast<Object*>((*oit).first);
      if (_valid(o) == false) {
        _allocator->_heap.erase(oit);
        goto BegincleanUp;
      }
      ++oit;
    }
  }
  bool bret = false;
  GC_ObjectTable objectTable;
  __currentObjectTable = &objectTable;
  ObjectStack::reverse_iterator oit = stack().rbegin();
  ObjectStack::reverse_iterator oend = stack().rend();
  while (oit != oend) 
  {
    if (!((*oit).second.flags & ObjectMem)) {
       ++oit;
       continue;
    }
    Object* o = reinterpret_cast<Object*>((*oit).first);
    if (_valid(o) == false) {
      //(*oit).first = 0;
      ++oit;
      continue;
    }
    //dmi::SysFields f;
    FieldReferences frefs;
    o->getCollectableFields(frefs);
    FieldReferences::const_iterator fit = frefs.begin();
    FieldReferences::const_iterator fend = frefs.end();
    for (; fit < fend; ++fit)
    {
      Object* fobj = &(*(*fit));
      if (fobj != 0)
        objectTable[fobj].deps.push_back(o);
    }
   
    ++oit;
  }
beginsearch:
  GC_ObjectTable::iterator tit = objectTable.begin();
  GC_ObjectTable::iterator tend = objectTable.end();
  while (tit != tend) {
    if ((*tit).first == 0) {
      ++tit;
      continue;
    }
    if (findNonRecDepends((*tit).first, (*tit).first, objectTable) == true) {
      objectTable.erase(tit);
      goto beginsearch;
    }
    ++tit;
  }
beginsweap:
  tit = objectTable.begin();
  tend = objectTable.end();
  while (tit != tend) {
    const Object* optr = (*tit).first;
    if (_valid(optr) == false) {
      ++tit;
      continue;
    }
    
    if (_setInFieldToNil((*tit).first, recursiv) == true) {
      bret = true;
      goto beginsweap;
    }
    ++tit;
  }
  __currentObjectTable = 0;
  return bret;
  */
}


//virtual 
void 
RC_GC_Heap::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  //don't because race condition: HEAPLOCKGUARD();
  allocator()->listObjects(listener, flags);
}


} // sys
} // lang
} // acdk

