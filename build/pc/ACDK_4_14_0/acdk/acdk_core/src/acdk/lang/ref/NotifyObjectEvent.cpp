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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/NotifyObjectEvent.cpp,v 1.13 2005/04/09 19:26:55 kommer Exp $


#include <acdk.h>
#include <map>
#include <vector>
#include "NotifyObjectEvent.h"
#include <acdk/lang/sys/core_fastmutex.h>
#include <acdk/lang/sys/core_vector.h>
#include <acdk/lang/sys/core_hashmap.h>


#if defined(ACDK_LOCAL_DEBUG)
#define DOUT(msg) do { sys::coreout << msg << sys::eofl; } while(false)
#else
/// @internal
#define DOUT(msg)
#endif
/// @internal
int hash(acdk::lang::Object* k, int maxsize)
{
  return int(k) % maxsize;
}


namespace acdk {
namespace lang {
namespace ref {
/// @internal
typedef std::vector<NotifyObjectEventListener*> NotifyListenerList ;
/// @internal
typedef NotifyListenerList::iterator NotifyListenerListIterator ;
/// @internal
typedef std::map<Object*, NotifyListenerList> NotifierMap;
/// @internal
typedef NotifierMap::iterator NotifierMapIterator;


namespace {
/// @internal
static NotifierMap __elements;
/// @internal
NotifierMap& elements()
{
  //crash at exit in vc6: static NotifierMap __elements;
  return __elements;
}

/// @internal
sys::core_fastmutex __elementMutex;

} // anon namespace

//static 
void 
NotifyObjectEvent::add(Object* obj, NotifyObjectEventListener* el)
{
  sys::core_lock_guard<sys::core_fastmutex> _lock(__elementMutex);
  DOUT("Add listener: " << (void*)el << " to element " << (void*)obj);
  NotifierMapIterator it = elements().find(obj);
  if (it != elements().end()) {
    (*it).second.push_back(el);
  } else {
    NotifyListenerList vec;
    vec.push_back(el);
    elements().insert(NotifierMap::value_type(obj, vec));
    obj->setExternalRecorded(true);
  }
}


//static 
void 
NotifyObjectEvent::removeListener(NotifyObjectEventListener* el)
{
  sys::core_lock_guard<sys::core_fastmutex> _lock(__elementMutex);
//restart_first:
  DOUT("remove listener: " << (void*)el);
  NotifierMapIterator it = elements().begin();
  NotifierMapIterator end = elements().end();
  while (it != end) 
  {
restart:
    NotifyListenerListIterator vit = (*it).second.begin();
    NotifyListenerListIterator vend = (*it).second.end();
    while (vit != vend) {
      if ((*vit) == el) {
        (*it).second.erase(vit);
        /*
        if ((*it).second.begin() == (*it).second.end())
        {
          elements().erase(it);
          goto restart_first;
        }*/
        goto restart;
      }
      ++vit;
    }
    ++it;
  }
}
 


//static 
void 
NotifyObjectEvent::remove(Object* obj, NotifyObjectEventListener* el)
{
  sys::core_lock_guard<sys::core_fastmutex> _lock(__elementMutex);
  DOUT("remove object listener: " << (void*)el << " to element " << (void*)obj);
  NotifierMapIterator it = elements().find(obj);
  NotifierMapIterator  end = elements().end();
  if (it == end)
  {
    DOUT("cannot find any listener: " << (void*)el << " to element " << (void*)obj);
    return;
  }
  bool found = false;
restart:
  NotifyListenerList& l = (*it).second;
  NotifyListenerListIterator vit = l.begin();
  NotifyListenerListIterator vend = l.end();
  for (; vit != vend; ++vit) 
  {
    if ((*vit) == el) {
      (*it).second.erase(vit);
      DOUT("removed listener: " << (void*)el << " to element " << (void*)obj);
      found = true;
      goto restart;
    }
  }
  if (found == false)
  {
     DOUT("cannot find listener: " << (void*)el << " to element " << (void*)obj);
  }
  //elements().erase(it);
}

//static 
void 
NotifyObjectEvent::removeObject(Object* obj)
{
  sys::core_lock_guard<sys::core_fastmutex> _lock(__elementMutex);
  NotifierMapIterator it = elements().find(obj);
  if (it == elements().end())
    return;
  elements().erase(it);
}

//static 
bool 
NotifyObjectEvent::notifyBeforeDestruction(Object* obj)
{
  sys::core_lock_guard<sys::core_fastmutex> _lock(__elementMutex);
  NotifierMapIterator it = elements().find(obj);
  NotifierMapIterator  end = elements().end();
  if (it == end) 
    return true;
  NotifyListenerList& l = (*it).second;
  NotifyListenerListIterator vit = l.begin();
  NotifyListenerListIterator vend = l.end();
  while (vit != vend) {
    if ((*vit)) {
      if ((*vit)->notifyBeforeDestruction(obj) == false)
        return false;
    }
    ++vit;
  }
  return true;
}


//static 
void 
NotifyObjectEvent::notifyWhileDestruction(Object* obj)
{
  {
    sys::core_lock_guard<sys::core_fastmutex> _lock(__elementMutex);// ### not needed (other too)
    NotifierMapIterator it = elements().find(obj);
    NotifierMapIterator end = elements().end();
    if (it == end) 
      return;

    NotifyListenerListIterator vit = (*it).second.begin();
    NotifyListenerListIterator vend = (*it).second.end();
    while (vit != vend) 
    {
      if ((*vit))
        (*vit)->notifyWhileDestruction(obj);
      ++vit;
    }
  }
  removeObject(obj);
}

} // namespace ref
} // namespace lang 
} // namespace acdk 

