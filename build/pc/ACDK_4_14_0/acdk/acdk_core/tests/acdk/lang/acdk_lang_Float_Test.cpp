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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Float_Test.cpp,v 1.9 2005/02/05 10:45:08 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Float.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( Float_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( toAndFromString )
  
END_DECLARE_TEST( Float_Test  )

BEGIN_DEFINE_TEST( Float_Test )
  ADD_TEST( Float_Test, standard ) 
  ADD_TEST( Float_Test, toAndFromString ) 
  
END_DEFINE_TEST( Float_Test )

using namespace acdk::lang;

class MyFloatTest
{
private:
  float _value;
public:
  MyFloatTest(float val)
  : _value(val)
  {
  }
  float floatValue() { return _value; }
};

void floatTest()
{
  float dv = 3.145;
  MyFloatTest mft(dv);
  mft.floatValue();
  Float d(dv);
  d.floatValue();
}
class FloadThread
: extends ::acdk::lang::Thread
{
public:
  virtual void run()
  {
    floatTest();
  }
};
void
Float_Test::standard()
{
  floatTest();
  RThread th = new FloadThread();
  th->start();
  floatTest();
  th->join();
  floatTest();
  /*try {
    throw "blabla";
  } catch (...) {
  } */
  floatTest();
}

void
Float_Test::toAndFromString()
{
  float dv = 3.145;
  float dr = Float::valueOf(Float(dv).toString())->floatValue();
  testAssert(dv == dr);
  dv = 0.0;
  dr = Float::valueOf(Float(dv).toString())->floatValue();
  testAssert(dv == dr);
  dv = 24.0;
  dr = Float::valueOf(Float(dv).toString())->floatValue();
  testAssert(dv == dr);
  {
    RString str = "123.0000";
    float widths = Float::valueOf(str)->floatValue();
    testAssert(widths == 123.0);
    System::out->println(widths);
  }

}





} // namespace lang
} //namespace acdk
} //namespace tests



