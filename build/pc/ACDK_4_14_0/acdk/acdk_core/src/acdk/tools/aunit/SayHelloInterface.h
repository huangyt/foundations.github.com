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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/SayHelloInterface.h,v 1.11 2005/02/07 11:14:08 kommer Exp $

#ifndef acdk_tools_testunit_SayHelloInterface_h
#define acdk_tools_testunit_SayHelloInterface_h


#include <acdk.h>
#include "Config.h"
#include <acdk/lang/Integer.h>
#include <acdk/lang/StringBuffer.h>


namespace acdk {
namespace tools {
namespace aunit {


ACDK_DECL_INTERFACE(SayHelloInterface);
/**
  Sample interface to demonstrate and Test 
  ACDK DMI features
*/
// will use now standard proxy ACDK_CLASSATTRIBUTE(acdk.tools.mc.DmiProxyAttribute)
class ACDK_TOOLS_AUNIT_PUBLIC SayHelloInterface
  ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(SayHelloInterface)
public:
  /** 
    Should print Hello message console 
    @return the printed message
  */
  virtual RString sayHello(IN(RString) sayto) = 0;
};


ACDK_DECL_CLASS(SayHelloInterfaceImpl);
class ACDK_TOOLS_AUNIT_PUBLIC SayHelloInterfaceImpl
: extends acdk::lang::Object
, implements SayHelloInterface
{
  ACDK_WITH_METAINFO(SayHelloInterfaceImpl)
public:
  SayHelloInterfaceImpl() {}
  virtual RString sayHello(IN(RString) sayto)
  {
    return "Hello to " + sayto;
  }

};

} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_SayHelloInterface_h
