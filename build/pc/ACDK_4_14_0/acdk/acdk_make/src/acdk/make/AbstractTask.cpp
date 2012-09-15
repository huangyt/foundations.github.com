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


#include "AbstractTask.h"
#include "TaskManager.h"

namespace acdk {
namespace make {

using namespace acdk::cfgscript;

AbstractTask::AbstractTask()
: _name("")
, _description("")
, _childs(new TaskArray(0))
, _taskProps(new Props())
{
  _workingDir = acdk::io::File::getCWD();
}

AbstractTask::AbstractTask(IN(RString) name, IN(RString) target, IN(RString) descr)
: _name(name)
, _description(descr)
, _targetName(target)
, _childs(new TaskArray(0))
, _taskProps(new Props())
{
  _workingDir = acdk::io::File::getCWD();
  
}

AbstractTask::AbstractTask(IN(RString) name, IN(RString) descr, IN(RTaskArray) childs)
: _name(name)
, _description(descr)
, _targetName(name)
, _childs(childs)
, _taskProps(new Props())
{
  _workingDir = acdk::io::File::getCWD();
}

void 
AbstractTask::addSubTask(IN(RString) name)
{
  RTask t = TaskManager::getTask(name);
  if (t == Nil)
  {
    ACDK_NLOG("acdk.make", Error, "AbstractTask::addSubTask: Taskname unknown: " + name);
    return;
  }
  addDependingTask(t);
}

RTaskInfo 
AbstractTask::getTaskInfo() 
{
  if (_taskInfo == Nil)
    _taskInfo = new TaskInfo(_name, _targetName, _description);
  _taskInfo->_workingDir = _workingDir;
  return _taskInfo;
}

void 
AbstractTask::registerTask()
{
  TaskManager::registerTask(this);
}

void 
AbstractTask::registerTask(IN(RString) alt_name)
{
  TaskManager::registerTask(alt_name, this);
}

//virtual 
bool 
AbstractTask::execute(IN(RString) exec, IN(RProps) props)
{
  for (int i = 0; i < _childs->length(); ++i)
  {
    RTask ct = _childs[i];
    RProps tempprops = new Props(ct->getTaskInfo()->getTargetName(), PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, props);
    if (ct->doExecute(exec, tempprops) == false && TaskManager::noFailOnBreak() == false)
      return false;
  }
  return true;
}

} // namespace make
} // namespace acdk



