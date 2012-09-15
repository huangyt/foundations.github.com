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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/dmi/acdk_lang_dmi_ScriptVar_Test.cpp,v 1.6 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Integer.h>

namespace tests {
namespace acdk {
namespace lang {
namespace dmi {

BEGIN_DECLARE_TEST( ScriptVar_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( ScriptVar_Test  )

BEGIN_DEFINE_TEST( ScriptVar_Test )
  ADD_TEST( ScriptVar_Test, standard ) 
  
END_DEFINE_TEST( ScriptVar_Test )

using namespace ::acdk::lang::dmi;

void
ScriptVar_Test::standard()
{

  ScriptVar v1((int)12);
  ScriptVar v2(1.2);
  testAssert(v1.addition(v2).getDoubleVar() == 13.2);
  testAssert(v1.subtraction(v2).getDoubleVar() == 10.8);
  testAssert(v1.multiply(ScriptVar(2)).getLongVar() == 24);
  testAssert(v1.divide(ScriptVar(2)).getLongVar() == 6);
}



} // namespace lang 
} // namespace dmi 
} //namespace acdk 
} //namespace tests 




