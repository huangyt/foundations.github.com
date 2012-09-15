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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/dmi/acdk_lang_dmi_DmiObject_Test.cpp,v 1.5 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/dmi/DmiObject.h>

namespace tests {
namespace acdk {
namespace lang {
namespace dmi {

BEGIN_DECLARE_TEST( DmiObject_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( DmiObject_Test  )

BEGIN_DEFINE_TEST( DmiObject_Test )
  ADD_TEST( DmiObject_Test, standard ) 
  
END_DEFINE_TEST( DmiObject_Test )

using namespace ::acdk::lang::dmi;

void
DmiObject_Test::standard()
{
  DmiObject dmi2(2);
  int i = 3;
  DmiObject dmi3(outOf(i));
  
}



} // namespace lang 
} // namespace dmi 
} //namespace acdk 
} //namespace tests 




