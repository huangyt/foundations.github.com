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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/tools/mc/acdk_tools_mc_Attributes_Test.cpp,v 1.5 2005/02/05 10:45:09 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/tools/aunit/DmiTestClass.h>
#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace tools {
namespace mc {



BEGIN_DECLARE_TEST( Attributes_Test )
  DECLARE_TEST( classInitAttribute )
END_DECLARE_TEST( Attributes_Test  )

BEGIN_DEFINE_TEST( Attributes_Test )
  ADD_TEST( Attributes_Test, classInitAttribute ) 
END_DEFINE_TEST( Attributes_Test )



void 
Attributes_Test::classInitAttribute()
{
  
  ::acdk::tools::aunit::DmiTestClass::GetClass()->objectClazzInfo()->loadFullClazzInfo(); //### in Class:resolve(...)
  testAssertComment(::acdk::tools::aunit::DmiTestClass::foreignStaticBoolean == true,
            "registerd static clazz initializer doesn't work");
}

} // namespace mc
} // namespace tools
} //namespace acdk 
} //namespace tests

