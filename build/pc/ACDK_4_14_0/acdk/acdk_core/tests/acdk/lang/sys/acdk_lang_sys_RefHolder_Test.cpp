// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/sys/acdk_lang_sys_RefHolder_Test.cpp,v 1.24 2005/03/07 17:52:10 kommer Exp $
//

#include <acdk.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/System.h>
#include <acdk/lang/sys/core_atomicop.h>
#include <acdk/lang/sys/core_fastmutex.h>
#include <acdk/lang/sys/core_guard.h>
#include <acdk/io/BinaryDataWriter.h>
#include <acdk/io/RandomAccessFile.h>
#include <acdk/tools/aunit/core_test.h>


namespace tests {
namespace acdk {
namespace lang {
namespace sys {

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::lang::sys;

void testNil()
{
  {
    RObject obj;
    testAssert(obj == Nil);
    testAssert((obj != Nil) == false);
  }
  {
    RObject obj = new Object();
    testAssert(obj != Nil);
    testAssert((obj == Nil) == false);
  }
  {
    RObject obj = new Object();
    obj = Nil;
    testAssert(obj == Nil);
  }
}

void testPointerIntegrity()
{
  Object* obj = new Object();
  obj->hashCode();
  delete obj;
  String* str = new String("hallo");
  //str->append(" welt");
  str->hashCode();
  delete str;
  StringBuffer* sb = new StringBuffer("x");
  sb->addRef();
  sb->append(" welt");
  RString rstr = sb->toString();
  delete sb;
}

void testEqual()
{
  RObject obj1 = Nil;
  RObject obj2 = Nil;
  testAssert(obj1 == obj2);
  
  obj1 = new Object();
  testAssert(obj1 != obj2);

  obj2 = obj1;
  testAssert(obj1 == obj2);
}

ACDK_DECL_CLASS(SelfAssignTest);

class SelfAssignTest
: public ::acdk::lang::Object
{
public:
  RSelfAssignTest next;
};

void testSelfAssignment()
{
  RObject obj1 = new Object;
  obj1 = obj1;
  testAssert(obj1 != Nil);

  RSelfAssignTest h = new SelfAssignTest();
  h->next = new SelfAssignTest();
  h = h->next;
  testAssert(h != Nil);
}

void voidRObjectCall(RObject obj)
{
  testAssert(obj != Nil);
}

void voidObjectPtrCall(Object* obj)
{
  testAssert(obj != 0);
}

void voidObjectRefCall(Object& obj)
{
  obj.hashCode();
}

void testBasicCall()
{
  RObject obj = new Object();
  
  voidRObjectCall(obj);
  testAssert(obj != Nil);
  
  voidObjectPtrCall(&obj);
  testAssert(obj != Nil);
  
  voidObjectRefCall(*obj);
}

#define DC(targetclass) (targetclass)

void testObjectUpCast()
{
  RObject obj = new Integer(1);
  obj = new Integer(2);
  RInteger integer = new Integer(3);
  obj = (RObject)integer;
  
  RBinaryDataWriter bin = new BinaryDataWriter(Nil);
  RAbstractFilterWriter afw = (RAbstractFilterWriter)bin;
  afw = DC(RAbstractFilterWriter)bin;
  RAbstractWriter aw = DC(RAbstractWriter)bin;
  aw = DC(RAbstractWriter)bin;
  testAssert(aw != Nil);
  aw = DC(RAbstractWriter)afw;
  testAssert(aw != Nil);
  testAssert( RObject(aw) != Nil);
  
}

void testInterfaceSideCast()
{

   RRandomAccessFile f = new RandomAccessFile("dummy", "rw");
   RDataWriter dw = (RDataWriter)f;
   testAssert(dw != Nil);
   dw = (RDataWriter)f;
   testAssert(dw != Nil);
   RDataReader dr = (RDataReader)f;
   testAssert(dr != Nil);
   dr = (RDataReader)f;
   testAssert(dr != Nil);

   RDataReader dr2 = (RDataReader)dw;
   testAssert(dr2 != Nil);
   dr2 = (RDataReader)dw;
   testAssert(dr2 != Nil);

   RRandomAccessFile f2 = (RRandomAccessFile)dr;
   testAssert(f2 != Nil);
   f2 = (RRandomAccessFile)dr;
   testAssert(f2 != Nil);
}


void testObjectArray()
{
   RObjectArray oa = new ObjectArray(2);
   oa[0] = new Object();
   oa[1] = new Object();
   testAssert(oa[0] != Nil);
   testAssert(oa[1] != Nil);
   oa[1]->hashCode();
   RObject tobj = oa[0];
   testAssert(tobj != Nil);
   oa[1] = tobj;
   testAssert(oa[1] != Nil);
}

void testIntegerArray()
{
   RIntegerArray ia = new IntegerArray(2);
   ia[0] = new Integer(0);
   ia[1] = new Integer(1);
   RInteger i0 = new Integer(0);
   RInteger i1 = new Integer(1);
   testAssert(ia[0]->equals(i0));
   testAssert(ia[1]->equals(i1));
   RObject obj = (RObject)ia[1];
   testAssert(obj != Nil);
   obj->hashCode();

   RObjectArray oa = (RObjectArray)ia;
   testAssert(oa != Nil);
   testAssert(oa[0] != Nil);
   testAssert(oa[1] != Nil);
   RInteger ti = (RInteger)oa[1];
   testAssert(ti != Nil);
}


void testInterfaceArray()
{
   RComparableArray ca = new ComparableArray(2);
   ca[0] = new Integer(42);
   ca[1] = new String("asdf");
}

void testStackObjects()
{
   String str("asdf");
   voidRObjectCall(&str);
   RObject obj = &str;

}

void test_atomic_time()
{
#if defined(ACDK_OS_WIN32)
  int maxCount = 1000000;
  DWORD start = GetTickCount();
  {
    int val = 0;
    for (int i = 0; i < maxCount; i++) 
    {
      ++val;
      if (--val == 0){
        
      }
    }
  }
  DWORD diff1 = GetTickCount() - start;
  start = GetTickCount();
  {
    core_atomicop val(0);
    for (int i = 0; i < maxCount; i++) 
    {
      val.increment();
      if (val.decr_test_zero() == true) {
      }
    }
  }
  DWORD diff2 = GetTickCount() - start;

  start = GetTickCount();

  {
    core_fastmutex mutex;
    int val = 0;
    start = GetTickCount();
    for (int i = 0; i < maxCount; i++) 
    {
      core_lock_guard<core_fastmutex> guard(mutex);
      ++val;
      if (--val == 0){
        
      }
    }
  }
  DWORD diff3 = GetTickCount() - start;
  //GetTickCount();
#endif // defined(ACDK_OS_WIN32)
}


class Main
{
public:
  static void testRefHolder()
  {
  test_atomic_time();
  testNil();
  testPointerIntegrity();
  testEqual();
  testSelfAssignment();
  testBasicCall();
  testObjectUpCast();
  testInterfaceSideCast();
  testObjectArray();
  testIntegerArray();
  testInterfaceArray();
  testStackObjects();

  }

  static int testMain(RStringArray args)
  {
    //RRandomAccessFile f = new RandomAccessFile("dummy", "rw");
    return 0;
  }
};

} // namespace sys
} //namespace lang 
} //namespace acdk 
} // namespace tests 


int
main(int argc, char* argv[], char** envptr)
{
  int ret = acdk::lang::System::main(::tests::acdk::lang::sys::Main::testMain, argc, argv, envptr);
  sys::coreout << "Tests OK" << sys::eofl;
  return ret;
}
