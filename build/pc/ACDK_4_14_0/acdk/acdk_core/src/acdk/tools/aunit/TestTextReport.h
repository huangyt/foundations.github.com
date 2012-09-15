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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestTextReport.h,v 1.5 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_TestTextReport_h
#define acdk_tools_testunit_TestTextReport_h


#include "TestReport.h"

namespace acdk {
namespace tools {
namespace aunit {




ACDK_DECL_CLASS(TestTextReport);

/**
  Writes an text Report after running the tests
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestTextReport
: extends acdk::lang::Object
, implements TestReport
{
  ACDK_WITH_METAINFO(TestTextReport)
protected:
  int _options;
  acdk::io::RPrintWriter _out;
public:
  /**
    @param options combinations of TestReportOptions
  */
  TestTextReport(int options, INP(acdk::io::RPrintWriter) out = Nil);
  virtual void print(INP(RTestResultEntryArray) tests);
};



} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_TestTextReport_h
