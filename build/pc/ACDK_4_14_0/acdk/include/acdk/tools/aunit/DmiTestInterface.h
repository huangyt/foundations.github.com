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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/DmiTestInterface.h,v 1.11 2005/03/01 10:31:20 kommer Exp $

#ifndef acdk_tools_testunit_DmiTestInterface_h
#define acdk_tools_testunit_DmiTestInterface_h


#include <acdk.h>
#include "Config.h"
#include <acdk/lang/Integer.h>
#include <acdk/lang/StringBuffer.h>


namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_INTERFACE(DmiTestInterface);


/** 
  This Interface may be used to test script code.

  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/03/01 10:31:20 $
*/
// not used ACDK_CLASSATTRIBUTE(acdk.tools.mc.DmiProxyAttribute)
class ACDK_TOOLS_AUNIT_PUBLIC DmiTestInterface
  ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DmiTestInterface)
  
public:
  virtual void foo() THROWS2(RIndexOutOfBoundsException, RClassNotFoundException) = 0;
  virtual ::acdk::lang::RStringBuffer bar(IN(::acdk::lang::RStringBuffer) sb, OUT(RString) str) = 0;
};



} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_DmiTestInterface_h
