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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Double_Test.cpp,v 1.8 2005/02/05 10:45:08 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Double.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( Double_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( toAndFromString )
  DECLARE_TEST( limits )
END_DECLARE_TEST( Double_Test  )

BEGIN_DEFINE_TEST( Double_Test )
  ADD_TEST( Double_Test, standard ) 
  ADD_TEST( Double_Test, toAndFromString ) 
  ADD_TEST( Double_Test, limits ) 
END_DEFINE_TEST( Double_Test )

using namespace acdk::lang;


void 
Double_Test::standard()
{
  double dv = 3.145;
  Double d(dv);
  testAssert(d.doubleValue() == dv);

}

void 
Double_Test::toAndFromString()
{
  double dv = 3.145;
  double dr = Double::valueOf(Double(dv).toString())->doubleValue();
  testAssert(dv == dr);
  dv = 0.0;
  dr = Double::valueOf(Double(dv).toString())->doubleValue();
  testAssert(dv == dr);
  dv = 24.0;
  dr = Double::valueOf(Double(dv).toString())->doubleValue();
  testAssert(dv == dr);
}



void 
Double_Test::limits()
{

  /* don't work on all machines, because NaN does not work ==
   testAssert(Double(Double::NaN).doubleValue() == Double::NaN);
  */
#if !defined(__BORLANDC__) // has problem with NaN
  testAssert(Double::isNaN(Double(Double::NaN).doubleValue()) == true);
#endif //!defined(__BORLANDC__)  
}


} // namespace lang 
} //namespace acdk 
} //namespace tests 



