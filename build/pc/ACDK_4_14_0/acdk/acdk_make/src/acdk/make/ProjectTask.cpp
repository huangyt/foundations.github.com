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


#include "ProjectTask.h"
#include "TaskManager.h"


namespace acdk {
namespace make {


ProjectTask::ProjectTask(IN(RString) name, IN(RStringArray) subtasksnames)
: AbstractTask(name, name, "Collection of other tasks")
{
  ACDK_SAFE_CONSTRUCTOR();
  TaskManager::registerTask(this);
  if (subtasksnames == Nil)
    return;
  for (int i = 0; i < subtasksnames->length(); ++i)
  {
    RTask t = TaskManager::getTask(subtasksnames[i]);
    if (t == Nil)
    {
      ACDK_NLOG("acdk.make", Error, "ProjectTask: Task cannot be found: " + subtasksnames[i]);
      return;
    }
    addDependingTask(t);
  }
}



} // namespace make
} // namespace acdk

