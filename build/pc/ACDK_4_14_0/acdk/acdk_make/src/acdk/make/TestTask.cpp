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


#include "TestTask.h"
#include "TaskManager.h"

namespace acdk {
namespace make {

TestTask::TestTask(IN(RString) name, IN(RStringArray) testtasks)
: AbstractTask(name, name, "Test Task")
//, _tests(testtasks)
{
  //if (_tests == Nil)
  //  _tests = new StringArray(0);
  ACDK_SAFE_CONSTRUCTOR();
  TaskManager::registerTask(this);
  for (int i = 0; testtasks != Nil && i < testtasks->length(); ++i)
  {
    addTest(testtasks[i]);
  }
}

bool 
TestTask::execute(IN(RString) exec, IN(RProps) props)
{
  return AbstractTask::execute("test", props);
  /*
  bool ret = true;
  for (int i = 0; i < _tests->length(); ++i)
  {
    RTask t = TaskManager::getTask(_tests[i]);
    if (t == Nil)
    {
      ACDK_NLOG("acdk.make", Error, "Test task cannot be found: " + _tests[i]);
      return false;
    }
    ret &= t->doExecute("test", props);
  }
  return ret;
  */
}

} // namespace make
} // namespace acdk



