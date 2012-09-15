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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_BinaryDataReaderWriter_Test.cpp,v 1.19 2005/02/05 10:45:08 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>

#include <acdk/io/MemReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/BinaryDataWriter.h>
#include <acdk/io/BinaryDataReader.h>
#include <acdk/util/Arrays.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( BinaryDataReaderWriter_Test )
  DECLARE_TEST( standard )
 
END_DECLARE_TEST( BinaryDataReaderWriter_Test  )

BEGIN_DEFINE_TEST( BinaryDataReaderWriter_Test )
  ADD_TEST( BinaryDataReaderWriter_Test, standard ) 
  
END_DEFINE_TEST( BinaryDataReaderWriter_Test )


  using namespace ::acdk::lang;
  using namespace ::acdk::io;
  using namespace ::acdk::util;

void
BinaryDataReaderWriter_Test::standard()
{
  ::acdk::lang::Endian en[2];
  en[0] = ::acdk::lang::BigEndian;
  en[1] = ::acdk::lang::LittleEndian;
  for (int i = 0; i < 2; ++i)
  {
    MemWriter mout;
    
    BinaryDataWriter bdout(&mout, en[i]);
    bdout.writeBoolean(false);
    bdout.writeBoolean(true);
    bdout.writeChar('a');
    bdout.writeChar('&');
    bdout.writeShort(Short::MIN_VALUE);
    bdout.writeShort(Short::MAX_VALUE);
    
    bdout.writeInt(Integer::MIN_VALUE);
    bdout.writeInt(Integer::MAX_VALUE);
    
    bdout.writeLong(Long::MIN_VALUE);
    bdout.writeLong(Long::MAX_VALUE);
    bdout.writeFloat(Float::MIN_VALUE);
    bdout.writeFloat(Float::MAX_VALUE);
    bdout.writeDouble(Double::MIN_VALUE);
    bdout.writeDouble(Double::MAX_VALUE);
    
    byteArray ba(3);
    ba[0] = (byte)-123;
    ba[1] = +123;
    ba[2] = 4;
    bdout.write(&ba);
    
    String str("ACDK basics for distributed computing");
    bdout.writeString(&str);
    
    bdout.writeOpaque(&ba);
        
    
    RbyteArray rba = mout.getBuffer();
    RcharArray rca = (RcharArray)rba;

    //System::out->println("written:" + rca->toString());
    
    MemReader min(mout.getBuffer());
    BinaryDataReader bdin(&min, en[i]);
    testAssert(bdin.readBoolean() == false);
    testAssert(bdin.readBoolean() == true);
    testAssert(bdin.readChar() == 'a');
    testAssert(bdin.readChar() == '&');
    testAssert(bdin.readShort() == Short::MIN_VALUE);
    testAssert(bdin.readShort() == Short::MAX_VALUE);
    
    testAssert(bdin.readInt() == Integer::MIN_VALUE);
    testAssert(bdin.readInt() == Integer::MAX_VALUE);
    
    testAssert(bdin.readLong() == Long::MIN_VALUE);
    testAssert(bdin.readLong() == Long::MAX_VALUE);
    testAssert(bdin.readFloat() == Float::MIN_VALUE);
    testAssert(bdin.readFloat() == Float::MAX_VALUE);
    testAssert(bdin.readDouble() == Double::MIN_VALUE);
    testAssert(bdin.readDouble() == Double::MAX_VALUE);
    byteArray baerg(3);
    bdin.read(&baerg, 0, 3);
    testAssert(Arrays::equals(RbyteArray(&ba), RbyteArray(&baerg)) == true);
    
    RString strerg = bdin.readString();
    testAssert(str.equals(strerg) == true);
    RbyteArray b2 = bdin.readOpaque();
    System::out->println(ba.toString() + " = " + b2->toString());
    testAssert(ba[0] == b2[0]);
    testAssert(ba[1] == b2[1]);
    testAssert(ba[2] == b2[2]);
  }
}


} // namespace io
} // namespace acdk
} // namespace tests

