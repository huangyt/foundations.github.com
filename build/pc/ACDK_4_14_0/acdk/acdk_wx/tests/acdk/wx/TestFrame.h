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
// $Header: /cvsroot/acdk/acdk/acdk_wx/tests/acdk/wx/TestFrame.h,v 1.4 2005/02/05 10:45:36 kommer Exp $
#ifndef tests_acdk_wx_TestFrame_h
#define tests_acdk_wx_TestFrame_h

#include <acdk/tools/aunit/TestUnit.h>
#include <acdk/wx/Frame.h>
#include <acdk/wx/Timer.h>


namespace tests {
namespace acdk {
namespace wx {



using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::wx;


ACDK_DECL_CLASS(TestFrame);
class TestFrame
: extends Frame
{
  int _timerId;
  RTimer _testEndTimer;
public:
  TestFrame()
  {
    
    ACDK_SAFE_CONSTRUCTOR();
    if (::acdk::tools::aunit::TestCase::TestInBatchMode == false)
      return;
    _timerId = getFreeId();
    _testEndTimer = new Timer(this, _timerId);
    connect(TimerEvent::EvtTimer, _timerId, (ObjectEventFunction)&TestFrame::onTestTimeEnd);
    _testEndTimer->start(4000, true);
  }
  void onTestTimeEnd(IN(REvent) event)
  {
    _testEndTimer->stop();
    close(true);
  }
};


} // namespace wx
} //namespace acdk 
} //namespace tests 

#endif //tests_acdk_wx_TestFrame_h

