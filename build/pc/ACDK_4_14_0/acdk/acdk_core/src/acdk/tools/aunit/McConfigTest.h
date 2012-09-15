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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/McConfigTest.h,v 1.9 2005/02/07 11:14:07 kommer Exp $

#ifndef acdk_tools_testunit_McConfigTest_h
#define acdk_tools_testunit_McConfigTest_h


#include <acdk.h>
#include "Config.h"
#include <acdk/lang/Integer.h>
#include <acdk/lang/StringBuffer.h>


namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_CLASS(McConfigTest);

/**
  Dummy class to Test MetaCompiler with Attributes
*/

// don't generate Metainfo for methods or fields by default
ACDK_CLASSATTRIBUTE(McConfig(acdk.tools.mc.McConfNoFields | acdk.tools.mc.McConfNoMethods))
class ACDK_TOOLS_AUNIT_PUBLIC McConfigTest
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(McConfigTest)
public:
  int firstMember; // not metainfo will be available for firstMember
  
  // for the next field MetaInfo should be generated
  ACDK_FIELDATTRIBUTE(McConfig(acdk.tools.mc.McConfWithFields)) // metainfo will be available
  int secondMember;

  void firstMethod() {} // not metainfo will be available for firstMethod

  // for the next method MetaInfo should be generated
  ACDK_METHODATTRIBUTE(McConfig(acdk.tools.mc.McConfWithMethods))
  void secondMethod(){} // metainfo will be available
};

} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_McConfigTest_h
