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


namespace acdk {
namespace make {

//static 
RTaskManager 
TaskManager::getTaskManager()
{
  static RTaskManager tm = new TaskManager();
  return tm;
}

void 
TaskManager::_registerTask(IN(RTask) task)
{
  _registerTask(task->getTaskInfo()->getTaskName(), task);
}

void 
TaskManager::_registerTask(IN(RString) name, IN(RTask) task)
{
  _tasks->put(&name, (RObject)task);
  RString defaulttarget = "default";
  if (_tasks->get(&defaulttarget) == Nil)
  {
    _tasks->put(&defaulttarget, (RObject)task);
  }
}

RStringArray  
TaskManager::_getTasks()
{
  
  RStringArray erg = new StringArray(0);
  acdk::util::RIterator it = _tasks->keySet()->iterator();
  while (it->hasNext() == true)
    erg->append((RString)it->next());
  return erg;
}


} // namespace make
} // namespace acdk 
  

