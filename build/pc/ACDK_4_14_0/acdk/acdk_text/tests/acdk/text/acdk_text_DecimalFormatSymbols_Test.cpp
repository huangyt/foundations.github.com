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
// $Header: /cvsroot/acdk/acdk/acdk_text/tests/acdk/text/acdk_text_DecimalFormatSymbols_Test.cpp,v 1.8 2005/02/05 10:45:33 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>
#include <acdk/text/DecimalFormatSymbols.h>
#include <acdk/text/DecimalFormat.h>
#include <acdk/text/FieldPosition.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace text {

using namespace ::acdk::lang;
using namespace ::acdk::text;


  
BEGIN_DECLARE_TEST( DecimalFormatSymbols_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( DecimalFormatSymbols_Test  )

BEGIN_DEFINE_TEST( DecimalFormatSymbols_Test )
  ADD_TEST( DecimalFormatSymbols_Test, standard ) 

END_DEFINE_TEST( DecimalFormatSymbols_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);




//static
void
DecimalFormatSymbols_Test::standard()
{
  
  DecimalFormatSymbols dfdefault;
  dfdefault.getCurrencySymbol();
  dfdefault.getPatternSeparator();
}



} // namespace text
} //namespace acdk 
} //namespace tests

