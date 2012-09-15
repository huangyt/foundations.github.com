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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/reflect/acdk_lang_reflect_Enumeration_Test.cpp,v 1.5 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/reflect/Enumeration.h>
#include <acdk/lang/System.h>
#include <acdk/util/Properties.h>
#include <acdk/util/ResourceBundle.h>
#include <acdk/tools/aunit/DmiTestClass.h>
#include <acdk/lang/dmi/DmiObject.h>

namespace tests {
namespace acdk {
namespace lang {
namespace reflect {

BEGIN_DECLARE_TEST( Enumeration_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( Enumeration_Test  )

BEGIN_DEFINE_TEST( Enumeration_Test )
  ADD_TEST( Enumeration_Test, standard ) 
  
END_DEFINE_TEST( Enumeration_Test )

using namespace ::acdk::lang::reflect;

USING_CLASS(::acdk::tools::aunit::, DmiTestClass);

void
Enumeration_Test::standard()
{
  REnumeration ei = Enumeration::getEnumeration("VarType", "acdk/lang/dmi");
  testAssert(ei != Nil);
  ei = Enumeration::getEnumeration("VarType", Nil);
  testAssert(ei != Nil);
  System::out->println("Enumeration VarType has following values:");
  REnumerationValueArray  va = ei->getValues();
  for (int i = 0; i < va->length(); ++i)
  {
    System::out->println("  " + va[i]->getName() + " = " + va[i]->getValue());
  }
  REnumerationValue ev = Enumeration::getEnumerationValue("LongVT", "acdk/lang/dmi");
  testAssert(ev != Nil && ev->getValue() == ::acdk::lang::dmi::LongVT);
}



} // namespace reflect
} // namespace lang 
} //namespace acdk 
} //namespace tests 




