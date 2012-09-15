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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/reflect/acdk_lang_reflect_Unit_Test.cpp,v 1.4 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/reflect/Unit.h>
#include <acdk/lang/System.h>
#include <acdk/util/Properties.h>
#include <acdk/util/ResourceBundle.h>
#include <acdk/lang/Process.h>

namespace tests {
namespace acdk {
namespace lang {
namespace reflect {

BEGIN_DECLARE_TEST( Unit_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( listAll )
END_DECLARE_TEST( Unit_Test  )

BEGIN_DEFINE_TEST( Unit_Test )
  ADD_TEST( Unit_Test, standard ) 
  ADD_TEST( Unit_Test, listAll ) 
END_DEFINE_TEST( Unit_Test )

using namespace ::acdk::lang::reflect;

void
Unit_Test::standard()
{
  RClass sbclass = StringBuffer::GetClass();
  RUnit unit = sbclass->getUnit();
  RUnit unit2 = Unit::getUnit("acdk/lang");
  testAssert(unit->equals(&unit2) == true);
}

void
Unit_Test::listAll()
{
  RUnitArray ua = Unit::getUnits();
  for (int i = 0; i < ua->length(); ++i)
  {
    System::out->println("Unit: " + ua[i]->getName());
    RClassArray ca = ua[i]->getClasses();
    for (int j = 0; j < ca->length(); ++j)
    {
      System::out->println("  Class: " + ca[j]->getName());
    }
  }
}

} // namespace reflect
} // namespace lang 
} //namespace acdk 
} //namespace tests 




