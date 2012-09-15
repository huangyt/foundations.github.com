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

#ifndef acdk_make_TaskManager_h
#define acdk_make_TaskManager_h


#include "Task.h"
#include <acdk/util/HashMap.h>

namespace acdk {
namespace make {



ACDK_DECL_CLASS(TaskManager);


class ACDK_ACDK_MAKE_PUBLIC TaskManager
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(TaskManager)
private:
  ::acdk::util::RHashMap _tasks;
  bool _noFailOnBreak;  
public:

  TaskManager()
  : _tasks(new acdk::util::HashMap())
  , _noFailOnBreak(false)
  {
  }
  void _registerTask(IN(RTask) task);
  void _registerTask(IN(RString) name, IN(RTask) task);
  RTask _getTask(IN(RString) name) { return (RTask)_tasks->get(&name); }
  RStringArray _getTasks();
  static RTaskManager getTaskManager();

  static void registerTask(IN(RTask) task) { getTaskManager()->_registerTask(task); }
  static void registerTask(IN(RString) name, IN(RTask) task) { getTaskManager()->_registerTask(name, task); }
  static RTask getTask(IN(RString) name) { return getTaskManager()->_getTask(name); }
  static RStringArray getTasks() {  return getTaskManager()->_getTasks(); }
  static void noFailOnBreak(bool nofail) { getTaskManager()->_noFailOnBreak = nofail; }
  static bool noFailOnBreak() { return getTaskManager()->_noFailOnBreak; }

};




} // namespace make
} // namespace acdk 
  
#endif //acdk_make_Task_h
