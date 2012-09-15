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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/acdk_net_TransReaderWriter_Test.cpp,v 1.14 2005/02/05 10:45:30 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/Double.h>
#include <acdk/io/MemReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/util/Arrays.h>
#include <acdk/net/TransRateReader.h>
#include <acdk/net/TransRateWriter.h>

namespace tests {
namespace acdk {
namespace net {
  

int getNextServerPort();
int getServerPort();


BEGIN_DECLARE_TEST( TransReaderWriter_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( TransReaderWriter_Test  )

BEGIN_DEFINE_TEST( TransReaderWriter_Test )
  ADD_TEST( TransReaderWriter_Test, standard ) 
END_DEFINE_TEST( TransReaderWriter_Test )

using namespace ::acdk::lang;
using namespace ::acdk::net;


class TransListenerImpl
: extends ::acdk::lang::Object
, implements ::acdk::net::TransListener
{
public:
  
  virtual bool listen(IN(byte) block, int block_size, int millisecs)
  {
    double secs = millisecs / 1000;
    if (secs == 0.0)
      secs = 1.0;
    System::out->println(Double::toString(block_size / secs));
    return true;
  }
};

void transReader(IN(RbyteArray) array, int limit)
{
  ::acdk::io::MemReader mem(array);

  TransRateReader trr(&mem, limit);
  trr._listener = new TransListenerImpl();
  byteArray target(array->length());
  trr.read(&target);
  testAssert(array->equals(RbyteArray(&target)) == true);
}


void transWriter(IN(RbyteArray) array, int limit)
{
  ::acdk::io::MemWriter mem;

  TransRateWriter trr(&mem, limit);
  trr._listener = new TransListenerImpl();
  trr.write(array);
  testAssert(array->equals(mem.getBuffer()) == true);
}


void TransReaderWriter_Test::standard()
{
  StringBuffer buff;
  for (int i = 0; i < 150; ++i)
  {
    buff.append("asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf");
  }
  tick_t start = core_tick::now();
  transReader(buff.toString()->getBytes(), 0);
  int unbufferedtime = core_tick::millisecsSince(start);

  start = core_tick::now();
  transReader(buff.toString()->getBytes(), 5000);
  int buffered5000 = core_tick::millisecsSince(start);
  //testAssert(unbufferedtime < buffered5000);
  start = core_tick::now();
  //transReader(buff.toString()->getBytes(), 1000);
  int buffered1000 = core_tick::millisecsSince(start);
  //testAssert(buffered5000 < buffered1000);
  
  transWriter(buff.toString()->getBytes(), 5000);
  {
    transWriter(RString("1")->getBytes(), 0);
    transWriter(RString("1")->getBytes(), 1000);
    transReader(RString("1")->getBytes(), 0);
    transReader(RString("1")->getBytes(), 1000);
  }
}


} // namespace net
} // namespace acdk
} // namespace tests



