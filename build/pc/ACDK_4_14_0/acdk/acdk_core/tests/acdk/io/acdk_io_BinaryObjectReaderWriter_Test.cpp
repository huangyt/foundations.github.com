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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_BinaryObjectReaderWriter_Test.cpp,v 1.17 2005/03/14 17:59:12 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>

#include <acdk/io/MemReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/BinaryObjectWriter.h>
#include <acdk/io/BinaryObjectReader.h>
#include <acdk/util/Arrays.h>
#include <acdk/util/HashMap.h>
#include <acdk/lang/dmi/DmiObject.h>

#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( BinaryObjectReaderWriter_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( recursive )
  DECLARE_TEST( cyclic )
  DECLARE_TEST( container )
  DECLARE_TEST( scriptVar )
  DECLARE_TEST( throwable )
END_DECLARE_TEST( BinaryObjectReaderWriter_Test  )

BEGIN_DEFINE_TEST( BinaryObjectReaderWriter_Test )
  ADD_TEST( BinaryObjectReaderWriter_Test, standard ) 
  ADD_TEST( BinaryObjectReaderWriter_Test, recursive ) 
  ADD_TEST( BinaryObjectReaderWriter_Test, cyclic ) 
  ADD_TEST( BinaryObjectReaderWriter_Test, container ) 
  ADD_TEST( BinaryObjectReaderWriter_Test, scriptVar ) 
  ADD_TEST( BinaryObjectReaderWriter_Test, throwable )
END_DEFINE_TEST( BinaryObjectReaderWriter_Test )


  using namespace ::acdk::lang;
  using namespace ::acdk::io;
  using namespace ::acdk::util;


RObject serializeClone(RObject obj, int flags)
{
  MemWriter mout;
  BinaryObjectWriter boout(&mout, flags);
  boout.writeObject(obj);
  MemReader min(mout.getBuffer());
  BinaryObjectReader boin(&min, flags);
  return boin.readObject();
}

bool readWriteObject(RObject obj)
{
  sys::coreout << "start readwrite" << sys::eofl;
  RObject robj = serializeClone(obj, SerializeDefaultFlags | SerializeAll);
  if (robj->equals(obj) == false)
    return false;
  robj = serializeClone(obj, SerializeDefaultFlags | SerializeJoinedStrings | SerializeAll);
  if (robj->equals(obj) == false)
    return false;
  return true;
}


void
BinaryObjectReaderWriter_Test::standard()
{
  testAssert(readWriteObject(new String("Hallo")) == true);
  testAssert(readWriteObject(new StringBuffer("Hallo")) == true);
  testAssert(readWriteObject(new Integer(42)) == true);

}

void 
BinaryObjectReaderWriter_Test::recursive()
{
  {
    RObjectArray oa = new ObjectArray(2);
    oa[0] = new String("Hallo");
    oa[1] = new String("Hallo");
    testAssert(readWriteObject(&oa) == true);
  }
  {
    RStringBufferArray oa = new StringBufferArray(2);
    oa[0] = new StringBuffer("Hallo");
    oa[1] = new StringBuffer("Hallo");
    testAssert(readWriteObject(&oa) == true);
  }
  {
    RStringArrayArray oa = new StringArrayArray(2);
    oa[0] = new StringArray(1);
    oa[0][0] = new String("Hallo");
    oa[1] = new StringArray(0);
    testAssert(readWriteObject(&oa) == true);
  }

  {
    RObjectArrayImpl<RStringArrayArray> oa = new ObjectArrayImpl<RStringArrayArray>(1);
    oa[0] = new StringArrayArray(2);
    oa[0][0] = new StringArray(1);
    oa[0][0][0] = new String("Hallo");
    oa[0][1] = new StringArray(0);
    testAssert(readWriteObject(&oa) == true);
  }
}

void 
BinaryObjectReaderWriter_Test::cyclic()
{
  RObjectArray oa = new ObjectArray(1);
  oa[0] = &oa;
  RObject robj = serializeClone(&oa, SerializeDefaultFlags  | SerializeAll);
  testAssert(robj != Nil);
  testAssert(instanceof(robj, ObjectArray) == true);
  RObjectArray ra = (RObjectArray)robj;
  testAssert(ra->length() == 1);
  RObject oae = ra[0];
    
  testAssert(ra[0] == ra);

}

void
BinaryObjectReaderWriter_Test::container()
{
  {
    RObjectArray oa = new ObjectArray(4);
    oa[0] = new String("A");
    oa[1] = Nil;
    oa[2] = new String("A");
    oa[3] = new String("B");
    testAssert(readWriteObject(&oa) == true);
  }
  {
    ::acdk::util::HashMap hm;
    hm.put(new String("A"), new Integer(1));
    hm.put(new String("B"), Nil);
    hm.put(new String("C"), new Integer(3));
    hm.put(new String("D"), Nil);
    testAssert(readWriteObject(&hm) == true);
  }
  {
    ::acdk::util::HashMap hm;
    hm.put(new String("A"), new String("A"));
    hm.put(new String("B"), new String("B"));
    hm.put(new String("C"), new String("A" ));
    testAssert(readWriteObject(&hm) == true);
  }

}

USING_CLASS(::acdk::lang::dmi::, DmiObject);

void
BinaryObjectReaderWriter_Test::scriptVar()
{
#if defined(ACDK_OS_BSD)
  System::out->println("Testcase BinaryObjectReaderWriter_Test::scriptVar deactivated for FreeBSD");
#else
  {
    RDmiObject obj = new DmiObject(42);
    
    testAssert(readWriteObject(&obj) == true);
  }

  {
    RObject tobj = new String("ACDK DMI String Object");
    DmiObject obj(tobj);
    testAssert(readWriteObject(&obj) == true);
  }
 
  {
    ::acdk::util::HashMap hm;
    for (int i = 0; i < 500;)
    {
      hm.put(new String("A no" + Integer::toString(++i)), new DmiObject(RObject(new String("ACDK DMI String Object 1"))));
      hm.put(new String("A no" + Integer::toString(++i)), new DmiObject(RObject(new String("ACDK DMI String Object 2"))));
    }
    testAssert(readWriteObject(&hm) == true);
  }
#endif //!defined(ACDK_OS_BSD)
}

void
BinaryObjectReaderWriter_Test::throwable()
{
  RException obj = new Exception("asdf");
  testAssert(readWriteObject(&obj) == true);
}

} // namespace io
} // namespace acdk
} // namespace tests


