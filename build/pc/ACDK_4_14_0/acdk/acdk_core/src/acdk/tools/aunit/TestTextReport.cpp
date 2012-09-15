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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestTextReport.cpp,v 1.3 2005/02/05 10:45:03 kommer Exp $


#include "TestTextReport.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace tools {
namespace aunit {

TestTextReport::TestTextReport(int options, INP(acdk::io::RPrintWriter) out)
: _options(options)
, _out(out)
{
  if (_out == Nil)
    _out = acdk::lang::System::out;
}

void 
TestTextReport::print(INP(RTestResultEntryArray) tests) 
{

}


} //namespace aunit
} // namespace tools
} // namespace acdk 

