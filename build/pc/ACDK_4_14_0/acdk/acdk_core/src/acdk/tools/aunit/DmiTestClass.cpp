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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/DmiTestClass.cpp,v 1.8 2005/03/07 17:10:01 kommer Exp $



#include "DmiTestClass.h"
#include "core_test.h"

#ifndef __GNUG__
core_test_exception::core_test_exception(const std::string& msg)
  : std::runtime_error(msg)
  {
  }
#endif

namespace acdk {
namespace tools {
namespace aunit {


//static 
int DmiTestClass::privStaticInt = 1;
RString DmiTestClass::privStaticString = new String("privStaticString");
RInteger DmiTestClass::privStaticInteger = new Integer(1);
bool DmiTestClass::foreignStaticBoolean = false;


bool DmiTestClass::pubStaticBool = false;
char DmiTestClass::pubStaticChar = 0;
byte DmiTestClass::pubStaticByte = 0;
short DmiTestClass::pubStaticShort = 0;
int DmiTestClass::pubStaticInt = 0;
jlong DmiTestClass::pubStaticLong = 0;
float DmiTestClass::pubStaticFloat = 0;
double DmiTestClass::pubStaticDouble = 0;
RObject DmiTestClass::pubStaticObject;


RString DmiTestClass::pubStaticString = new String("pubStaticString");
RInteger DmiTestClass::pubStaticInteger  = new Integer(2);
  
DmiTestClass::DmiTestClass()
: privInt(3)
, privString(new String("privString"))
, privInteger(new Integer(3))
, pubBool(false)
, pubChar(0)
, pubByte(0)
, pubShort(0)
, pubInt(0)
, pubLong(0)
, pubFloat(0)
, pubDouble(0)
, pubString(new  String("pubString"))
, pubInteger(new Integer(3))
{
}
  



} //namespace aunit
} // namespace tools
} // namespace acdk 

