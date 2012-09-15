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

#ifndef acdk_make_AbstractTask_h
#define acdk_make_AbstractTask_h

#include "Task.h"



namespace acdk {
namespace make {


ACDK_DECL_CLASS(AbstractTask);

/**
  Abstract base class for a Task
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC AbstractTask
: extends ::acdk::lang::Object
, implements Task
{
  ACDK_WITH_METAINFO(AbstractTask)
protected:
  RString _name;  
  RString _description;
  RString _targetName;
  RString _workingDir;
  /**
    Theses tasks has to be executed before
    this taks
  */
  RTaskArray _childs;
  RTaskInfo _taskInfo;
  RProps _taskProps;
public:
  AbstractTask();
  AbstractTask(IN(RString) name, IN(RString) target, IN(RString) descr);
  AbstractTask(IN(RString) name, IN(RString) descr, IN(RTaskArray) childs);

  RString getName() { return _name; }
  void setName(IN(RString) name) { _name = name; }

  virtual RTaskInfo getTaskInfo();

  virtual void addDependingTask(IN(RTask) task) 
  {
    _childs->append(task);
  }
  /**
    Wrapper to addDependingTask using registered tasks
  */
  void addSubTask(IN(RString) name);
  void addSubTask(IN(RTask) subtask) { addDependingTask(subtask); }

  /**
    register this task under given _name
  */
  void registerTask();
  /**
    Register task with an alternative name
  */
  void registerTask(IN(RString) alt_name);
  RTaskArray getChilds() { return _childs; }
  RProps getTaskProps() { return _taskProps; }
protected:
  virtual bool execute(IN(RString) exec, IN(RProps) props);
};



} // namespace make
} // namespace acdk


#endif //acdk_make_AbstractTask_h
