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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/ObjectHeap.h,v 1.19 2005/04/28 15:00:05 kommer Exp $

#ifndef acdk_lang_sys_ObjectHeap_h
#define acdk_lang_sys_ObjectHeap_h

#include "../../Config.h"

#include "SysRefHolder.h"
#include "HeapFrame.h"

template <class T> class RefHolder;
/*
namespace acdk {
namespace lang {

class Object;
class InterfaceBase;

namespace ref {

class NotifyObjectEventListener;

} //namespace ref 
} //namespace lang 
} // namespace acdk
*/

namespace acdk {
namespace lang {
namespace sys {

using namespace acdk::lang;

ACDK_DECL_SYS_CLASS(HeapFrame);


/** 
  Manages the different Heaps, including Reference Counting, gargabe Collecting etc.
  There is no Instance of this heap.
  
*/

class Allocator;

class ACDK_CORE_PUBLIC ObjectHeap 
{
private:
  /// private constructor. This class is static
  ObjectHeap() { }
public:

/** HeapType declares the Type of heap, with different memory management strategies
  */
enum HeapType 
  {
    /** Objects are reference counted, but no bookkeeping of References will be made. 
        Cyclic references will not be resolved and freed at gc().
        gc() does nothing.      */
    RC_Heap = 0x0001,
    /** Objects are reference counted and collected in the Heap.
        gc() will try to free cyclic references */
    RC_GC_Heap = 0x0002,

    /** Use a seperate PageAllocator with mark/sweep algo */
    PA_Heap = 0x0004,
    
    /** using Boehms Conservative Garbage Collector */
    GC_Heap = 0x0008,

    /** Objects are allocated on the heap, no reference counting, 
        no bookkeeping of references, just allocate and forget.
        garantee of memory leeks.
    */
    Garbage_Heap = 0x0010,
    /*
      Objects are allocated on a seperated heap, 
      on #popFrame() all Objects will be freed. Please take care of
      dangling pointers! 
    */
    Private_Heap = 0x0020,
  };

  /** returns the current allocator */
  static Allocator* allocator();
  

  /** called if RefCount is zero.
    @return true if Object should be destroyed
  */
  static bool notifyBeforeObjectDestruction(Object* obj);
  /** will be called after finalize but bufore ~Object */
  static void notifyWhileObjectDestruction(Object* obj);
  static void listHeaps(::acdk::lang::ref::NotifyObjectEventListener* listener, bool allthreads);
  /*
    @param flags combination of ListObjectsFlags
  */
  static void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
  static void listedAllocated(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags, AllocatedType type);
  
  /** 
    returns the count of allocated Object in current or all Heaps 
    If the current Heap has no tracking, returns 0.
  */
  static int objectCount(bool total = false);
  /** 
  returns the allocated Memory in current or all Heaps.
  If the current Heap has no tracking, returns 0.
  */
  static size_t totalAllocated(bool total = false);
  /** checks if Heap has recursiv References */
  //static bool checkCyclicReferences(bool recursiv = false);

  /** 
    try to free Object References, which has no external References.
    @return returns true, if an Reference island was found
  */
  static bool gc(bool total = false);
  /** 
    try to free the given Object, if it has no external References.
    @return returns true, if an Reference island was found
  */
  //static bool gc(const Object* obj);
  /**
    register this object as new dynamically allocated object 
    */
  static void newHeapObject(Object* obj);
  /** 
    looks if given Object is in the current heap
  */
  static bool onHeapAllocated(Object* obj);
  /** 
    creates a new HeapFrame
    @param heaptype the type of the heap
    RC_Heap
        Objects are reference counted, but no bookkeeping of References will be made. 
        Cyclic references will not be resolved and freed at gc().
        gc() does nothing.      
    
    RC_GC_Heap
        Objects are reference counted and collected in the Heap.
        gc() will try to free cyclic references 
        
    Garbage_Heap
        Objects are allocated on the heap, no reference counting, 
        maybe no bookkeeping of references.
        on #popFrame() all Objects will be freed. Please take care of
        dangling pointers! 
    @param name the name of the heap. Used for debugging purpose or as identifier to delete them.
  
  */
  static void pushFrame(HeapType heaptype = RC_GC_Heap, HeapInfo info = HeapIsThread, const char* name = "");
  /**
    destroys the the top (last pushed) Heap 
  */
  static void popFrame(HeapInfo info = HeapIsThread);
  
  /**
    Make the given HeapFrame as top active HeapFrame
    */
  static void pushFrame(RHeapFrame heap);
  /** 
    destroys the frame, take care, this can have side effects
    @param the name of the heapframe.
    @return true if frame is deleted
  */
  static bool deleteFrame(const char* name);
  /**
    returns the last pushed frame
  */
  static RHeapFrame topHeapFrame();
  /** 
    The which holds globals (not bound to Thread */
  static RHeapFrame globalHeap();
  /** 
    The which hold the statics
    @deprecated
  */
  static RHeapFrame staticHeap();
  /** 
    The which hold the statics
    @deprecated
  */
  static RHeapFrame getStaticFrame() { return staticHeap(); }
  /**
    return the allocator configured to allocate static objects
  */
  Allocator* staticAllocator() { return staticHeap()->allocator(); }
  
  /** used to allocate ThreadLocal */
  static RHeapFrame getThreadLocalHeap();
  /**
    return the allocator specific to this thread
  */
  static Allocator* getThreadLocalAllocator();
  /**
    Registers the object as an static instance. 
    Normally you dont need to call this function by hand, but use 
    registerStaticReference().
    @param object the Object, which will be stored in a static/global reference
  */
  
  static Object* newStaticObject(Object* object);
  /**
    register the an reference to Object as an static/global reference.
    This reference can be reset clearAllStaticReferences().
    @param reference the Reference to the Object. 
  */
  static void registerStaticReference(RObject* reference);
  /**
    register the an reference to Object as an static/global reference.
    Will call releaseRef() at at end of program
  */
  static void registerStaticReference(Object* reference);
  /**
    clear all via registerStaticReference registered reference (set to Nil).
    Calling this method is maybe dangerous, if other parts of the applications
    rely on these static variables
  */
  static void clearAllStaticReferences();
  /**
    return true if the given object instance is held by a static reference
  */
  static bool isStaticReferenceObject(Object* obj);

  /** locks the ObjectHeap interface */
  static void lockHeap();
  static void unlockHeap();

  /** removes the heaps connected with the current Thread */
  static void removeThreadHeap();

  /** return the adress of the stack base pointer */
  static unsigned int stackBase();
  static void saveStackBase(unsigned int sp); 

    /**
    return the overal limit of memory usage
  */
  static jlong getMaxMemoryUsage() { return maxMemoryUsage(); }
  /**
    set the overall limit of memory usage
  */
  static void setMaxMemoryUsage(jlong maxmem) { maxMemoryUsage() = maxmem; }
  /**
    return the limit of memory usage inside this thread
  */
  static jlong getThreadMaxMemoryUsage() { return threadMaxMemoryUsage(); }
  static void setThreadMaxMemoryUsage(jlong maxmem) { threadMaxMemoryUsage() = maxmem; }
  static jlong& threadMaxMemoryUsage();
  static jlong& maxMemoryUsage();
  static jlong& curMemUsage();
  static jlong& curThreadMemUsage();
  static jlong getMaxAllocatedMem() { return threadMaxMemoryUsage() < maxMemoryUsage() ? threadMaxMemoryUsage() : maxMemoryUsage(); }

protected:
  /// unsynchronized version
  //static void _addObject(const Object* obj, size_t size);
  /// unsynchronized version
  static bool _onHeapAllocated(const Object* obj);
  /// unsynchronized version
  //static bool _removeObject(const Object* obj);
  /// unsynchronized version
  static Object* _newStaticObject(Object* obj);
};

foreign class LockObjectHeap
{
public:
  LockObjectHeap()
  {
    ObjectHeap::lockHeap();
    
  }
  ~LockObjectHeap()
  {
    ObjectHeap::unlockHeap();
  }
};

/**
  different to static_mutex it only locks/unlock if System::isInMain() is true
*/
struct HeapLockMutex
: public static_mutex
{
  void lock();
  void unlock();
};

#ifdef ACDK_MT
  extern HeapLockMutex _heapAccessLock; 
# define HEAPLOCKGUARD() TLockGuard<HeapLockMutex>  lockthis(_heapAccessLock)
# define UNLOCKHEAPGUARD() core_unlock_guard<HeapLockGuard> unlockthis(_heapAccessLock)

#else 

# define HEAPLOCKGUARD() 
# define UNLOCKHEAPGUARD()

#endif //ACDK_MT


} // sys
} // lang
} // acdk



#endif //acdk_lang_sys_ObjectHeap_h


