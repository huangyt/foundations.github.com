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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/sys/acdk_lang_sys_HeapTest.cpp,v 1.40 2005/04/28 15:00:36 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/System.h>
#include <acdk/lang/sys/ObjectHeap.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/ref/NotifyObjectEvent.h>
#include <acdk/lang/OutOfMemoryError.h>
#include <acdk/lang/sys/core_tick.h>
#include <acdk/util/ArrayList.h>
#include <acdk/util/LinkedList.h>
#include <acdk/lang/Integer.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/lang/sys/core_memtrace.h>

namespace tests {
namespace acdk {
namespace lang {
namespace sys {

using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;
using namespace ::acdk::util;

  
BEGIN_DECLARE_TEST( Heap_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( gc )
  DECLARE_TEST( lowMemTest )
  DECLARE_TEST( lowMemWithGcTest )
  DECLARE_TEST( lowMemWithGcThreadedTest )
  DECLARE_TEST( getRoots )
  DECLARE_TEST( internal )
  DECLARE_TEST( refHolderMtTest )
  DECLARE_TEST( memTrace )
END_DECLARE_TEST( Heap_Test  )

BEGIN_DEFINE_TEST( Heap_Test )
  ADD_TEST( Heap_Test, standard ) 
  ADD_TEST( Heap_Test, gc ) 
  ADD_TEST( Heap_Test, lowMemTest ) 
  ADD_TEST( Heap_Test, lowMemWithGcTest ) 
  ADD_TEST( Heap_Test, lowMemWithGcThreadedTest ) 
  ADD_TEST( Heap_Test, getRoots ) 
  ADD_TEST( Heap_Test, internal ) 
  ADD_TEST( Heap_Test, refHolderMtTest ) 
  ADD_TEST( Heap_Test, memTrace ) 
END_DEFINE_TEST( Heap_Test )





class CheckHeapListener
: extends ::acdk::lang::ref::AbstractHeapListener
{
public:
  int listedCount;
  CheckHeapListener()
    : listedCount(0)
  {
  }
  virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) 
  { 
    //sys::coreout << "listedAllocate(" << (void*)theheap.impl() << ", " << obj << ", " << (int)type << ")" << sys::eofl;
    if (type == ObjectMem) 
    {
      Object* tobj = reinterpret_cast<Object*>(obj);
      if (tobj->isStaticRef() == true || instanceof(RObject(tobj), Class) == true)
        return true;
      //sys::coreout << reinterpret_cast<Object*>(obj)->getClass()->toString()->c_str() << sys::eofl;
    } else {
      //sys::coreout << sys::eofl;
    }
    ++listedCount;
    return true;
  }
  bool checkObjectCount(IN(RHeapFrame) hf, int count)
  {
    listedCount = 0;
    hf->listObjects(this, false);
    return listedCount == count;
  }

};

void prepareTest()
{
  RObject o = new Object();
  o->getClass();
  RString str = new String();
  str->getClass();
  RObjectArray oa = new ObjectArray(1);
  oa->getClass();
  RHashMap hm = new HashMap();
  hm->getClass();
  hm->put(new String("asdf", NormalSST | CCAscii), &oa);
}

void 
testStandard(ObjectHeap::HeapType ht)
{

  prepareTest();
  CheckHeapListener chl;
  

  ObjectHeap::pushFrame(ht);

  RHeapFrame th = ObjectHeap::topHeapFrame();

  testAssert(chl.checkObjectCount(th, 0));
  
  {
    RObject obj = new Object();
    testAssert(chl.checkObjectCount(th, 1));
    RString str = new String("asdf", NormalSST | CCAscii);
    testAssert(chl.checkObjectCount(th, 3));
  }
  testAssert(chl.checkObjectCount(th, 0));
  ObjectHeap::popFrame();

}

void 
testGc(ObjectHeap::HeapType ht)
{

  prepareTest();
  CheckHeapListener chl;
  ObjectHeap::pushFrame(ht);
  RHeapFrame th = ObjectHeap::topHeapFrame();

  {
    RObjectArray oa = new ObjectArray(1);
    oa[0] = &oa;
    //sys::coreout << "oa" << (void*)&oa << sys::eofl;
    testAssert(chl.checkObjectCount(th, 2));
    testAssert(chl.checkObjectCount(th, 2));
  }
  testAssert(chl.checkObjectCount(th, 2));
  th->gc();
  testAssert(chl.checkObjectCount(th, 0));


  {
    RObjectArray oa = new ObjectArray(4);
    oa[1] = &oa;
    oa[2] = &oa;
    //sys::coreout << "oa" << (void*)&oa << sys::eofl;
    testAssert(chl.checkObjectCount(th, 2));
    testAssert(chl.checkObjectCount(th, 2));
  }
  testAssert(chl.checkObjectCount(th, 2));
  th->gc();
  testAssert(chl.checkObjectCount(th, 0));

  {
    RObjectArray oa = new ObjectArray(2);
    RObjectArray oa2 = new ObjectArray(2);
    oa[1] = &oa2;
    oa2[1] = &oa;
    testAssert(chl.checkObjectCount(th, 4));
  }
  testAssert(chl.checkObjectCount(th, 4));
  th->gc();
  testAssert(chl.checkObjectCount(th, 0));

  {
    RHashMap hm1 = new HashMap();
    RHashMap hm2 = new  HashMap();
    hm1->put(new String("first Key", NormalSST | CCAscii), &hm2);
    hm2->put(new String("first Key", NormalSST | CCAscii), &hm1);
  }
  th->gc();
  testAssert(chl.checkObjectCount(th, 0));
  {
    RLinkedList rList = new LinkedList();
    for (int i = 0; i < 10; ++i)
      rList->add(new Integer(i));
    rList = Nil;
    th->gc();
    testAssert(chl.checkObjectCount(th, 0));
  }
  ObjectHeap::popFrame();
}


void 
Heap_Test::standard()
{
  NoSuccessTestLogging = true;
  prepareTest();
  // doesn not work, because not tracing testStandard(ObjectHeap::RC_Heap);
  testStandard(ObjectHeap::RC_GC_Heap);
  testStandard(ObjectHeap::PA_Heap);
  NoSuccessTestLogging = false;
}

void 
Heap_Test::gc() 
{
  NoSuccessTestLogging = true;
#if defined(__BORLANDC__)
  System::out->println("Borland not supported for GC at the moment");
#else
  testGc(ObjectHeap::RC_GC_Heap);
  testGc(ObjectHeap::PA_Heap);
#endif
  NoSuccessTestLogging = false;
}

void
Heap_Test::lowMemTest()
{
  System::gc();
  ObjectHeap::pushFrame(ObjectHeap::PA_Heap);
  {
    RHeapFrame th = ObjectHeap::topHeapFrame();
    const AllocatorInfo& alocinf1 = th->allocator()->getAllocatorInfo();
    RIntegerArray rList = new IntegerArray(0);
    for (int i = 0; i < 20; ++i)
      rList->append(new Integer(i));
    const AllocatorInfo& alocinf2 = th->allocator()->getAllocatorInfo();
    rList = Nil;
  }
  jlong mmu = System::getMaxMemoryUsage();
  jlong cmu = ObjectHeap::curMemUsage();
  System::setMaxMemoryUsage(cmu + 1024 * 1024 * 2);
  
  //RLinkedList rList = new LinkedList();//RArrayList rList = new ArrayList();
  RIntegerArray rList = new IntegerArray(0);
  int i = 0;
  bool OutOfMemoryErrorThrown = false;
  try
  {
    while(true)
    {
      //rList->add(new Integer(i++));
      rList->append(new Integer(i++));
    }
  }
  catch(ROutOfMemoryError ex)
  { 
    RHeapFrame th = ObjectHeap::topHeapFrame();
    const AllocatorInfo& alocinf = th->allocator()->getAllocatorInfo();
    rList = Nil;
    System::out->println("Expected Exception: " + ex->getMessage());
    OutOfMemoryErrorThrown = true;
  }
  System::setMaxMemoryUsage(mmu);
  //System::gc();
  RHeapFrame th = ObjectHeap::topHeapFrame();
  const AllocatorInfo& alocinf = th->allocator()->getAllocatorInfo();
  testAssert(OutOfMemoryErrorThrown == true);

  ObjectHeap::popFrame();
}

ACDK_DECL_CLASS(GcableObject);

class GcableObject
: extends ::acdk::lang::Object
{
public:
  byte buffer[4097];
  RGcableObject _other;
  GcableObject()
  {
    _setObjectRefFlag(true, ObjectBase::ObjectHasLocalGc);

  }
  GcableObject(IN(RGcableObject) other)
  {
    _setObjectRefFlag(true, ObjectBase::ObjectHasLocalGc);
    _other = other;
    other->_other = this;;
  }
  void getCollectableFields(FieldReferences& fields)
  {
    fields.push_back((RObject*)_other._ref_this());
  }
};

void
Heap_Test::lowMemWithGcTest()
{
  System::gc();
  jlong mmu = System::getMaxMemoryUsage();
  jlong cmu = ObjectHeap::curMemUsage();
  
  System::setMaxMemoryUsage(cmu + 1024 * 1024 * 2);
  for (int i = 0; i < 1000; ++i)
  {
    RGcableObject lobj = new GcableObject();
    lobj->_other = lobj;
  }
  System::setMaxMemoryUsage(mmu);
  System::gc();
}

bool gc_test_failed = false;

ACDK_DECL_CLASS(WorkThread);
class WorkThread
: extends ::acdk::lang::Thread
{
  int _loops;
  int _maxThreadedMem;
public:
  WorkThread(int loops, int maxThreadedMem)
    : _loops(loops)
    , _maxThreadedMem(maxThreadedMem)
  {
  }
  void run()
  {
    try {
    jlong mmu = System::getThreadMaxMemoryUsage();
    jlong cmu = ObjectHeap::curThreadMemUsage();
    System::setThreadMaxMemoryUsage(cmu + _maxThreadedMem);
    for (int i = 0; i < _loops; ++i)
    {
      RGcableObject lobj = new GcableObject();
      lobj->_other = lobj;
      if ((_loops % 100) == 0)
        Thread::sleep(1);
    }
    } catch (ROutOfMemoryError ex) {
      System::out->println("GC doesn't gc anything: " + ex->getMessage());
      gc_test_failed = true;
    }
    System::gc();
  }
};

ACDK_DECL_CLASS(NoiseThread);
class NoiseThread
: extends ::acdk::lang::Thread
{
  int _loops;
public:
  NoiseThread(int loops)
    : _loops(loops)
  {
  }
  void makeNoise()
  {
    int lloops = 10;
    int i;
    RStringArray sa = new StringArray(0);
    for (i = 0; i < lloops; ++i)
    {
      sa->append(String::valueOf(i));
    }
    RStringBuffer sb = new StringBuffer();
    for (i = 0; i < sa->length(); ++i)
    {
      sb->append(" ");
      sb->append(sa[i]);
    }
  }
  void run()
  {
    for (int i = 0; i < _loops; ++i)
    {
      makeNoise();
    }
  }
};

void
Heap_Test::lowMemWithGcThreadedTest()
{
  try {
  ::acdk::util::logging::RLogger acdk_cfgscript_logger = new ::acdk::util::logging::Logger("acdk.lang.gc");
  acdk_cfgscript_logger->addConsumer(new ::acdk::util::logging::ConsoleConsumer(new ::acdk::util::logging::SimpleFormatter()));
  ::acdk::util::logging::LogManager::Threshold = ::acdk::util::logging::LogManager::MinLevel = ::acdk::util::logging::SysDebug;

  /*
  {
    RWorkThread thread = new WorkThread(1000, 1024 * 1024 * 2);
    thread->start();
    thread->join();
  }*/
#if defined(ACDK_OS_SOLARIS) && !defined(__sparc__) // solaris on intel
  int threadCount = 2;
#else
  int threadCount = 18;
#endif
  int loops = 1000;
  System::out->println(SBSTR("Start " << threadCount << " Thread which does automatically gc on low memory situation"));
  RWorkThreadArray thr = new WorkThreadArray(threadCount);
  //RNoiseThread noise = new NoiseThread(loops);
  int i;
  for (i = 0; i < threadCount; ++i)
    thr[i] = new WorkThread(loops, 1024 * 1024 * 2);
  for (i = 0; i < threadCount; ++i)
    thr[i]->start();
  //noise->start();
  for (i = 0; i < threadCount; ++i)
    thr[i]->join();
  //noise->join();
  System::gc();
  testAssert(gc_test_failed == false);
  } catch (RThrowable ex) {
    ex->printStackTrace();
    System::out->println("Caught: " + ex->getClass()->getName() + "; :" + ex->getMessage());
  }
}

void
Heap_Test::getRoots()
{
  RInteger rint1 = new Integer(12345678); // is root object
  RInteger rint2 = new Integer(8765432);
  RObjectArray n = new ObjectArray(1);
  n[0] = &rint2; // rint2 is not root, because member of n
  RObjectArray roots = System::getRootObjects();
  testAssert(roots->find(&rint1) != -1);
  testAssert(roots->find(&rint2) == -1);
  for (int i = 0; i < roots->length(); ++i)
  {

    RObject o = roots[i];
    if (instanceof(o, ObjectArray) == true)
    {
      RObjectArray oa = (RObjectArray)o;
      
      ::acdk::lang::System::out->println(SBSTR("Root Object: (" << Integer::toHexString((int)(void*)o.impl()) << "): " << o->getClass()->getName() << ": " << oa->length()));
      for (int i = 0; i < oa->length(); ++i)
      {
        o = oa[i];
        if (o != Nil)
          ::acdk::lang::System::out->println(SBSTR("  RootArray Object: (" << Integer::toHexString((int)(void*)o.impl()) << "): " << o->getClass()->getName() << ": " << o->toString()));
      }
    }
    else
    {
      ::acdk::lang::System::out->println(SBSTR("Root Object: (" << Integer::toHexString((int)(void*)o.impl()) << "): " << o->getClass()->getName() << ": " << o->toString()));
    }
  }
}

void buildRec(IN(RObjectArray) arr, int loops, int rec)
{
  for (int i = 0; i < loops; ++i)
  {
    RObjectArray ca = new ObjectArray(0);
    if (rec > 0)
      buildRec(ca, loops, --rec);
    arr->append(&ca);
  }
}

void
Heap_Test::internal()
{
  int maxl = 100;
  /*
  for (int j = 0; j < maxl; ++j)
  {
    RintArray iarr = new intArray(1000);
    for (int i = 0; i < maxl; ++i)
    {
      iarr->append(i);
      testAssert(iarr->length() > 0);
    }
  }
  {
    RObjectArray b = new ObjectArray(0);
    buildRec(b, 70, 7);
  }*/
  RObjectArray oa = new ObjectArray(0);
  RObject d = new String("asdf");
  for (int i = 0; i < 6001; ++i)
  {
    oa->append(d);
  }
  
  oa->append(d);
  RString s = new String("asdf");
}

ACDK_DECL_CLASS(RhThreadClass);

class RhThreadClass 
  : extends Thread
{
  RObject refObject;
  int _loops;
public:
  RhThreadClass(IN(RObject) ro, int loops)
    : refObject(ro)
    , _loops(loops)
  {
  }
  void run()
  {
    for (int i = 0; i < _loops; ++i)
    {
      RObject ro1 = refObject;
      for (int j = 0; j < 100; j++)
      {
        RObject ro2 = ro1;
      }
    }
  }
};

void
Heap_Test::refHolderMtTest()
{
  int threadCount = 5;
  int loops = 1000;
  RRhThreadClassArray thr = new RhThreadClassArray(threadCount);
  //RNoiseThread noise = new NoiseThread(loops);
  int i;
  RObject obj = new StringBuffer("test");
  for (i = 0; i < threadCount; ++i)
    thr[i] = new RhThreadClass(obj, loops);
  for (i = 0; i < threadCount; ++i)
    thr[i]->start();
  //noise->start();
  for (i = 0; i < threadCount; ++i)
    thr[i]->join();
}

void
Heap_Test::memTrace()
{
  core_memtrace mt(false);
  RObject obj = new StringBuffer();
  void* ptr = obj.impl();
  int allocated = mt.getAllocatedCount();
  mt.reportUnfreed();
  obj = Nil;
  mt.reportPointer(ptr);
  allocated = mt.getAllocatedCount();
}

} // namespace sys
} // namespace lang
} // namespace acdk
} // namespace tests
