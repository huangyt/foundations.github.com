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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdk/IdlMappingTest.h,v 1.9 2005/02/05 10:45:41 kommer Exp $


#ifndef tests_acdkx_orb_IdlMappingTest_h
#define tests_acdkx_orb_IdlMappingTest_h

#include <acdkx/orb/std_orb.h>

namespace tests {
namespace acdkx {
namespace orb {

ACDK_DECL_CLASS(AdressInfo);

ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
class AdressInfo
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(AdressInfo)
public:
  AdressInfo() : Object() { }
  RString name;
  RString street;
  int streetnumber;
  RString toString()
  {
    return "name=[" + name + "]; street=[" + street + RString("]; streetnumber=[") + streetnumber + "];";
  }
};


ACDK_DECL_INTERFACE(AdressBook);

/** 
  class to test with other orb 
*/
ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbDispatchAttribute())
class AdressBook 
: implements ::org::omg::CORBA::portable::InvokeHandler
{
  ACDK_WITH_METAINFO(AdressBook)
public:
  ACDK_CORBA_INTERFACE(AdressBook)
public:
  virtual void ping() = 0;
  virtual void testArray(IN(RintArray) longs, OUT(RStringArray) strings) = 0;
  virtual RAdressInfo getAddressInfoA(RString name) = 0;
  virtual void getAddressInfoB(RString name, OUT(RAdressInfo) adressinfo) = 0;
  virtual void setAddressInfo(RString name, IN(RAdressInfo) adressinfo) = 0;
  virtual RAdressInfoArray getAllAdressInfos() = 0;
  virtual void setOtherAdressBook(IN(RAdressBook) other) = 0;
  virtual void getOtherAdressBook(OUT(RAdressBook) other) = 0;
};

} // namespace orb
} // acdkx
} // tests

#endif //tests_acdkx_orb_IdlMappingTest_h
