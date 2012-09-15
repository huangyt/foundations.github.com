// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*-
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
//
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
//
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/ObjectHeap.cpp,v 1.52 2005/05/02 23:12:27 kommer Exp $


#include <acdk.h>
#include "sys.h"
#include "ObjectHeap.h"

#include "RC_Heap.h"
#include "RC_GC_Heap.h"
#include "Garbage_Heap.h"
#include "PagedHeap.h"
#include "BoehmGCAllocator.h"
#include "BitmapPagedAllocator.h"

#include "../ref/ref.h"

#if defined(__BORLANDC__)
# include <malloc.h>
#endif //!defined(__BORLANDC__)

#include <stdio.h>

#include <acdk/lang/System.h>
#include <acdk/lang/Exception.h>
#include <acdk/lang/sys/core_mutex.h>
#include <acdk/lang/sys/core_fastmutex.h>

#include <acdk/lang/ThreadLocalImpl.h>
#include <acdk/lang/ThreadImpl.h>
#include "core_specific.h"
#include "core_syslocal.h"

namespace acdk {
namespace lang {
namespace dmi {
void ClazzInfo_deinit();
}
namespace sys {

using namespace acdk::lang;

#define DOUT(msg)
  //#define DOUT(msg) sys::coreout << msg << sys::eofl


core_vector<SysObject*>&
sysObjects()
{
  static core_vector<SysObject*> _sysObjects;
  return _sysObjects;
}

void cleanup_system_objects()
{
}

//static
jlong&
ObjectHeap::threadMaxMemoryUsage()
{
  static sys::specific<jlong> _threadMaxMemory;
  if (core_system::inMain() == false)
    return maxMemoryUsage();
  if (_threadMaxMemory.isSet() == false)
    _threadMaxMemory = 1024 * 1024 * 1024;
  return _threadMaxMemory.get();
}

//static
jlong&
ObjectHeap::maxMemoryUsage()
{
  static jlong MaxMemoryUsage = 1024 * 1024 * 1024; // 1 GB
  return MaxMemoryUsage;

  jlong& thr = threadMaxMemoryUsage();
  return MaxMemoryUsage < thr ? MaxMemoryUsage : thr;
  return threadMaxMemoryUsage();
}

//static
jlong&
ObjectHeap::curMemUsage()
{
  static jlong CurMemUsage = 0;
  return CurMemUsage;
}

//static
jlong&
ObjectHeap::curThreadMemUsage()
{
  static sys::specific<jlong> _threadCurMemory;
  if (core_system::inMain() == false)
    return curMemUsage();
  if (_threadCurMemory.isSet() == false)
    _threadCurMemory = 0;
  return _threadCurMemory.get();
}


SysObject::SysObject()
: _refCount(0)
{
  DOUT("SysObject::SysObject(" << (void*) this << ")");
  if (System::isInMain() == false)
    return;
  sysObjects().push_back(this);
}

SysObject::~SysObject()
{
  DOUT("SysObject::~SysObject(" << (void*) this << ")");
  if (System::isInMain() == false)
    return;
  for (int i = 0; i < sysObjects().size(); i++)
  {
    if (sysObjects()[i] == this) {
      sysObjects()[i] = 0;
      break;
    }
  }
}

void
SysObject::releaseRef() const
{
  /*
  DOUT("SysObject::releaseRef(" << (void*) this << ", " <<  _refCount.read() << ")");
  if (_refCount.decr_test_zero() == true) {
    delete const_cast<SysObject*>(this);
  }*/
}

void
SysObject::addRef() const
{
  //DOUT("SysObject::increment(" << (void*) this << ", " <<  _refCount.read() << ")");
  //_refCount.increment();
}

void 
HeapLockMutex::lock()
{
  if (acdk::lang::System::isInMain() == true)
    static_mutex::lock();
}

void 
HeapLockMutex::unlock()
{
  if (acdk::lang::System::isInMain() == true)
    static_mutex::unlock();
}


#ifdef ACDK_MT

HeapLockMutex _heapAccessLock;

#endif // ACDK_MT

/** thread-neutral heap */

RHeapFrame& _getTopFrame()
{
  static RHeapFrame __topFrame;// = new RC_GC_Heap();
  return __topFrame;
}

RHeapFrame _getStaticObjectsFrame()
{
  static RHeapFrame __staticObjectsFrame;
  return __staticObjectsFrame;
}
DEFINE_STATIC_SPECIFIC(RHeapFrame*, _topHeapStaticSpecific);

static_specific<RHeapFrame>& thrTopHeap()
{
  static static_specific<RHeapFrame> _th(core_system::acdk_core_static_bound, &_topHeapStaticSpecific);
  return _th;
}
/*
ThreadLocalImpl& thrTopHeap()
{
  static ThreadLocalImpl _thrTopHeap;
  return _thrTopHeap;
}
*/

core_vector<RHeapFrame>& allHeaps()
{
  static core_vector<RHeapFrame> _allHeaps;
  return _allHeaps;
}

//static
void
SysObject::_throwNullPointerException()
{
  ::acdk::lang::ObjectBase::_throwNullPointerException("SysObject");
}


HeapFrame::HeapFrame(RHeapFrame top/* = 0*/, int  flags/* = HeapIsThread*/, const char* name/* = ""*/)
: _top(top),
  _name(name),
  _flags(flags),
  _threadId()
{
  DOUT("HeapFrame::HeapFrame(" << (void*) this << ")");
  if (_flags & HeapIsThread)
    _threadId = (int)ThreadID::getCurrentThreadID().threadID();
  //allHeaps().push_back(this);
}

//virtual
HeapFrame::~HeapFrame()
{
  //sys::coreout << "HeapFrame::~HeapFrame() [" << _name.c_str() << "] id=[" << _threadId << "]" << sys::eofl;
}

//virtual
bool
HeapFrame::_setInFieldToNil(const Object* obj, bool recursiv)
{
  if (recursiv == true && up() != 0)
    return up()->_setInFieldToNil(obj, recursiv);
  return false;
}





void _setTopFrame(RHeapFrame h)
{
  if (allHeaps().hasElement(h) == false)
    allHeaps().push_back(h);

  if (h->flags() & HeapIsGlobal || ::acdk::lang::System::isInMain() == false)
  {
    _getTopFrame()  = h;
  }
  else if (h->flags() & HeapIsThread)
  {
    RHeapFrame& oldheap  = thrTopHeap().get();
    oldheap = h;
    /* old
    RHeapFrame oldheap = (RHeapFrame)(HeapFrame*)thrTopHeap().get();
    if (oldheap != Nil)
      oldheap->releaseRef();
    thrTopHeap().set((HeapFrame*)h);
    */
    if (h != Nil)
      h->addRef();
  } else
    _getStaticObjectsFrame() = h;

}

void setTopFrame(RHeapFrame h)
{
  DOUT("setTopFrame(" << (void*) h.impl() << ")");
  if (System::isInMain() == false)
  {
    DOUT("isInMain is false(" << (void*) h.impl() << ")");
    _getStaticObjectsFrame() = h;
    _getTopFrame() = h;
    return;
  }
  HEAPLOCKGUARD();
  _setTopFrame(h);
}



void removeFrame(RHeapFrame h)
{
  allHeaps().deleteElement(h);
  core_vector< RHeapFrame>::iterator it = allHeaps().begin();
  core_vector< RHeapFrame>::iterator end = allHeaps().end();
  while (it != end)
  {
    RHeapFrame f = *it;
    if (f == h)
      sys::coreout << "Oops removeFrame" << __FILE__ << ":" << __LINE__ << sys::eofl;
    ++it;
  }
  h->dispose();
}

RHeapFrame topFrame()
{
  // ### old code. current should work for all platforms.
  //#if (defined(ACDK_OS_WIN32) && defined(ACDK_STATIC_LIB)) || defined(ACDK_OS_UNIX)
  if (::acdk::lang::System::isInMain() == false)
    return _getTopFrame();
  //#endif

  RHeapFrame h = (RHeapFrame)(HeapFrame*)thrTopHeap().get();
  //sys::coreout << "topframe is " << (void*)(HeapFrame*)h << sys::eofl;
  if (h == 0)
    return _getTopFrame();

  return h;
}


RHeapFrame staticObjectsFrame()
{
   if (_getStaticObjectsFrame() != 0)
     return _getStaticObjectsFrame();
  _getStaticObjectsFrame() = new acdk::lang::sys::RC_GC_Heap(topFrame(), HeapIsStatic, "Statics");
  return _getStaticObjectsFrame();
}



namespace {


class LastOnHeapAllocated;
DEFINE_STATIC_SPECIFIC(LastOnHeapAllocated*, _staticSecificLastOnHeap);

enum {
  MAX_NEW_STACK = 32
};
class LastOnHeapAllocated
{
  Object* _allocatedStack[MAX_NEW_STACK];
  int _top;
public:
  LastOnHeapAllocated()
  {
    _top = -1;
    memset(_allocatedStack, 0, sizeof(_allocatedStack));
  }
  /**
     this version is used if not already initialized
  */
  static LastOnHeapAllocated& _static()
  {
    static LastOnHeapAllocated _loh;
    return _loh;
  }
  ~LastOnHeapAllocated()
  {
  }
  inline bool _fetch(Object* obj)
  {
    if (_top == -1)
       return false;
    if (_allocatedStack[_top] == obj) {
      --_top;
      return true;
    }
    return false;
  }
  inline void _put(Object* obj)
  {
#if defined(ACDK_DEBUG)
    if (_top >= MAX_NEW_STACK)
      THROW1(Error, "Max object allocation recursion reached");
#endif
    _allocatedStack[++_top] = obj;
  }
  //static sys::specific<LastOnHeapAllocated> _lastObjectOnHeap;

  static void put(Object* obj);
  static bool fetch(Object* obj);
};

static_specific<LastOnHeapAllocated>&
getSLOH()
{
  static static_specific<LastOnHeapAllocated> _lastObjectOnHeap(core_system::acdk_core_static_bound, &_staticSecificLastOnHeap);
  return _lastObjectOnHeap;
}

//static 
void 
LastOnHeapAllocated::put(Object* obj)
  {
    getSLOH().get()._put(obj);
  }
//static 
bool 
LastOnHeapAllocated::fetch(Object* obj)
  {
    return getSLOH().get()._fetch(obj);
  }


//DEFINE_STATIC_SPECIFIC(LastOnHeapAllocated, _lastOnHeapThreadStatic);
//sys::specific<LastOnHeapAllocated> LastOnHeapAllocated::_lastObjectOnHeap;

void ensureFrame2();

inline
void
ensureFrame()
{
  if (topFrame() == Nil)
    ensureFrame2();
}


void
ensureFrame2()
{
  switch (System::defaultHeapType)
  {
  case ObjectHeap::PA_Heap :
    setTopFrame(new PagedHeap());
    break;
  case ObjectHeap::RC_Heap:
    setTopFrame(new RC_Heap());
    break;
  case ObjectHeap::RC_GC_Heap:
    setTopFrame(new RC_GC_Heap());
    break;
  case ObjectHeap::GC_Heap:
    if (System::isInMain() == true)
      setTopFrame(new BoehmGCHeapFrame());
    else
      setTopFrame(new RC_GC_Heap());
    break;
  default:
    sys::coreout << "unknown heap type" << sys::eofl;
    setTopFrame(new PagedHeap());
  }
}

} // anon namespace





static ThreadLocalImpl _stackBase;
void
ObjectHeap::saveStackBase(unsigned int sp)
{
  _stackBase.set((void*)sp);
}

unsigned int
ObjectHeap::stackBase()
{
  return (unsigned int)_stackBase.get();
}


// #### make them inlines

//static
RHeapFrame
ObjectHeap::globalHeap()
{
  return _getTopFrame();

}

//static
RHeapFrame
ObjectHeap::staticHeap()
{
  return _getStaticObjectsFrame();
}


RHeapFrame
ObjectHeap::getThreadLocalHeap()
{
  static RHeapFrame __thrlocalheap = new acdk::lang::sys::RC_GC_Heap(topFrame(), HeapIsStatic, "ThreadLocals");
  return __thrlocalheap;
}


//static
Allocator*
ObjectHeap::getThreadLocalAllocator()
{

  return getThreadLocalHeap()->allocator();
}

// stores thread specific base  stack address
//ThreadLocalImpl __baseStackAddress;



//static
void
ObjectHeap::newHeapObject(Object* obj) // ### inline this have to be reviewed!
{
  LastOnHeapAllocated::put(obj);
}

//static
bool
ObjectHeap::onHeapAllocated(Object* obj)
{
  return LastOnHeapAllocated::fetch(const_cast<Object*>(obj));


  /* no longer supported, because not all cases are regarded
    RObject getObject()
    {
      static String s("asdf");
      return &s; // oops mean that it is allocated on heap!
    }
#if !defined(ACDK_OS_WIN32) && !defined(ACDK_OS_LINUX) && !defined(ACDK_OS_SOLARIS)
  HEAPLOCKGUARD(); // is this ever needed?
#endif
  //unsigned int basicEsp = (unsigned int)__baseStackAddress.get();
  unsigned int basicEsp = System::getStackBase();
  if (basicEsp == 0) {
    //__baseStackAddress.set((void*)_getSP());
    return false;
  }
  unsigned int curesp = _getSP();
  unsigned int objadr = (unsigned int)obj;
  if (objadr > curesp && objadr <= basicEsp)
    return false;
  return true;
  */
}


//static
bool
ObjectHeap::notifyBeforeObjectDestruction(Object* obj)
{
  HEAPLOCKGUARD();
  return ::acdk::lang::ref::NotifyObjectEvent::notifyBeforeDestruction(obj);
}

//static
void
ObjectHeap::notifyWhileObjectDestruction(Object* obj)
{
  HEAPLOCKGUARD();
  ::acdk::lang::ref::NotifyObjectEvent::notifyWhileDestruction(obj);
}

//static
Object*
ObjectHeap::_newStaticObject(Object* obj)
{
  //_removeObject(obj);
  //staticObjectsFrame()->add(obj, 0);
  return obj;
}


typedef core_vector<RObject*> ReferencePointerVector;


ReferencePointerVector&
getStaticReferences()
{
  static ReferencePointerVector _staticObjectReferences;
  return _staticObjectReferences;
}


//static
void
ObjectHeap::registerStaticReference(RefHolder<Object>* reference)
{
  HEAPLOCKGUARD();
  getStaticReferences().push_back(reference);
/* ### @todo dont do anything
  HEAPLOCKGUARD();

  _newStaticObject(const_cast<Object*>(reference->impl()));
  if (reference->impl() == 0)
    return;
  reference->impl()->setStaticRef(true); // use impl not iptr, because it may be a forced cased from an Interface
  //deprevated: reference->impl()->setInterThreadReference(true); // use impl not iptr, because it may be a forced cased from an Interface
  _staticObjectReferences.push_back(reference);
  */
}

//static 
bool 
ObjectHeap::isStaticReferenceObject(Object* obj)
{
  HEAPLOCKGUARD();
  ReferencePointerVector& refvec = getStaticReferences();
  ReferencePointerVector::iterator it = refvec.begin();
  ReferencePointerVector::iterator end = refvec.end();
  for (; it != end; ++it)
  {
    if ((*it)->impl() == obj)
      return true;
  }
  return false;
}

//static
void
ObjectHeap::registerStaticReference(Object* reference)
{
  /** ### @todo dont do anything
  HEAPLOCKGUARD();
  if (reference == 0)
    return;
  _newStaticObject(reference);
  reference->setStaticRef(true);
  */
}


//static
void
ObjectHeap::clearAllStaticReferences()
{
  //don't do this here, HEAPLOCKGUARD();
  return;
  ReferencePointerVector& refvec = getStaticReferences();
  ReferencePointerVector::iterator it = refvec.begin();
  ReferencePointerVector::iterator end = refvec.end();
  while (it != end) {
    if ((*it) != 0 && _valid((*it)->impl()))  {
      RefHolder<Object>* tobj = (*it);
      if (*tobj != Nil)
      {

          //sys::coreout << "clear static reference: " << (void*) tobj->iptr() << sys::eofl; //": " << (*tobj)->toString()->c_str() << sys::eofl;
        *tobj = Nil;
        //sys::coreout << "cleared static reference: "  << sys::eofl;
      }
    }
    ++it;
  }
  
  ::acdk::lang::dmi::ClazzInfo_deinit();
  // crashes with new Object _staticObjectsFrame->removeAllStaticReferences();
}



//static
Allocator*
ObjectHeap::allocator()
{
  ensureFrame();
  HeapFrame* tf = topFrame();
  if (tf == 0)
  {
    int *tptr = 0;
    *tptr = 42;
    tf = topFrame();
  }
  return tf->allocator();
}

//static
Allocator*
getThreadLocalAllocator();

//static
int
ObjectHeap::objectCount(bool total)
{
  HEAPLOCKGUARD();
  //### implement me
  return 0;
}

//static
size_t
ObjectHeap::totalAllocated(bool total)
{
  HEAPLOCKGUARD();
  //### implement me
  return 0;
}


//static
bool
ObjectHeap::gc(bool total)
{
  HEAPLOCKGUARD();
  HeapFrame *top = topFrame();
  if (top  == 0)
    return false;
  bool erg = false;
  {
    //UNLOCKHEAPGUARD();
    erg = top->gc(false);
  }
  if (total == false)
    return erg;
  core_vector<RHeapFrame>::iterator it = allHeaps().begin();
  core_vector<RHeapFrame>::iterator end = allHeaps().begin();
  while (it != end) 
  {
    if (*it != top)
    {
      //UNLOCKHEAPGUARD();
      erg |= (*it)->gc(false);
    }
    ++it;
  }
  return erg;
}


//static
void
ObjectHeap::pushFrame(RHeapFrame heap)
{
  HEAPLOCKGUARD();
  _setTopFrame(heap);
}

//static
void
ObjectHeap::pushFrame(HeapType heaptype, HeapInfo info, const char* name)
{
  HEAPLOCKGUARD();
  switch(heaptype)
  {
  case PA_Heap:
    _setTopFrame(new acdk::lang::sys::PagedHeap(4096, topFrame(), info, name));
    break;
  case RC_Heap :
    _setTopFrame(new acdk::lang::sys::RC_Heap(topFrame(), info, name));
    break;
  case RC_GC_Heap:

    _setTopFrame(new acdk::lang::sys::RC_GC_Heap(topFrame(), info, name));
    break;
  case Garbage_Heap :
    _setTopFrame(new acdk::lang::sys::Garbage_Heap(topFrame(), info, name));
    break;
  case GC_Heap :
    if (System::isInMain() == true)
      _setTopFrame(new acdk::lang::sys::BoehmGCHeapFrame(topFrame(), info, name));
    else
      _setTopFrame(new acdk::lang::sys::RC_GC_Heap(topFrame(), info, name));
    break;
  case Private_Heap :
    // no break

  default:
    THROW1(Exception, "ObjectHeap::pushFrame() given HeapType not supported");
    break;
  }
}


/* removes the Heap connected with this thread */
void
ObjectHeap::removeThreadHeap()
{
  RHeapFrame& oldtop = thrTopHeap().get();
  if (oldtop == Nil)
    return;
  removeFrame(oldtop);
  oldtop = Nil;
  /* old
  RHeapFrame oldtop = (RHeapFrame)(HeapFrame*)thrTopHeap().get();
  if (oldtop == Nil)
    return;
  removeFrame(oldtop);
  thrTopHeap().set(0);
  */
  //## will gpf' oldtop->releaseRef();
}

//static
void
ObjectHeap::popFrame(HeapInfo info)
{
  HEAPLOCKGUARD();
  RHeapFrame oldtop;
  if (info == HeapIsGlobal || ::acdk::lang::System::isInMain() == false)
    oldtop = _getTopFrame();
  else if (info == HeapIsThread)
    oldtop = (RHeapFrame)(HeapFrame*)thrTopHeap().get();
  else
    THROW1(Exception, RString("Unsupported HeapFrame type in ObjectHeap::popFrame(HeapInfo info):") + info);
  if (oldtop == 0) {
    THROW1(Exception, RString("Unsupported HeapFrame type in ObjectHeap::popFrame(HeapInfo info):") + info);
    //sys::coreout << "acdk::lang::sys::ObjectHeap::popFrame() called without an active Frame" << sys::eofl;
    return;
  }

  RHeapFrame newTop  = oldtop->up();
  //if (newTop != Nil)
  //{
    oldtop->setUpFrame(Nil);
    if (newTop != Nil)
      _setTopFrame(newTop);
    removeFrame(oldtop);
  //}
}

//static
RHeapFrame
ObjectHeap::topHeapFrame()
{
  HEAPLOCKGUARD();
  return topFrame();
}


//static
void
ObjectHeap::listHeaps(::acdk::lang::ref::NotifyObjectEventListener* listener, bool allthreads)
{
  HEAPLOCKGUARD();
  core_vector<RHeapFrame>::iterator it = allHeaps().begin();
  core_vector<RHeapFrame>::iterator end = allHeaps().end();
  while (it != end) {
    RHeapFrame tf = *it;
    if (listener->listHeaps(tf) == false)
      break;
    ++it;
  }
}
//static 
void 
ObjectHeap::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  if (flags & ListObjectsRecursive)
  {
    int sf = flags & ~ListObjectsRecursive;
    topHeapFrame()->listObjects(listener, sf);
    core_vector<RHeapFrame>::iterator it = allHeaps().begin();
    core_vector<RHeapFrame>::iterator end = allHeaps().end();
    while (it != end) 
    {
      RHeapFrame tf = *it;
      tf->listObjects(listener, sf);
      ++it;
    }
  }
  else
  {
    topHeapFrame()->listObjects(listener, flags);
  }
}

//static 
void 
ObjectHeap::listedAllocated(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags, AllocatedType type)
{
  // ### @todo not implementable
  /*
  if (flags & ListObjectsRecursive)
  {
    int sf = flags & ~ListObjectsRecursive;
    core_vector<RHeapFrame>::iterator it = allHeaps().begin();
    core_vector<RHeapFrame>::iterator end = allHeaps().end();
    while (it != end) 
    {
      RHeapFrame tf = *it;
      if (tf->listedAllocated(listener, sf, type) == false)
        break;
      ++it;
    }
  }
  else
  {
    topHeapFrame()->listedAllocated(listener, flags, type);
  }*/
}

//static
void
ObjectHeap::lockHeap()
{
  _heapAccessLock.lock();
}

//static
void
ObjectHeap::unlockHeap()
{
  _heapAccessLock.unlock();
}

} // sys
} // lang
} // acdk
