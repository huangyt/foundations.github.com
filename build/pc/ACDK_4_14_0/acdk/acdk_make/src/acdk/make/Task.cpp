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

#include "TaskManager.h"
#include <acdk/cfgscript/ChDir.h>

namespace acdk {
namespace make {


//foreign virtual 
bool 
Task::doExecute(IN(RString) exec, IN(RProps) props, int flags)
{
  RTaskInfo ti = getTaskInfo();
  
  if ((flags & TaskExecuteForce) == false && ti->getTaskStatus() == TaskOkBuild)
    return true;

  if (ti->logLevel() != ::acdk::util::logging::None)
  {
    ACDK_INLOG("acdk.make", ti->logLevel(), exec + ": executing " + ti->getExecuteLogString());
  }
  
  acdk::cfgscript::ChDir cd(ti->getWorkingDir());

  bool bret = execute(exec, props);
  
  ti->setTaskBuildStatus(bret);

  if (ti->logLevel() != ::acdk::util::logging::None && bret == false)
  {
    ACDK_NLOG("acdk.make", Error, "Failed: " + exec);
  }
  return bret;
}

} // namespace make
} // namespace acdk 
