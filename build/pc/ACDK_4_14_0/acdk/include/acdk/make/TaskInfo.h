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

#ifndef acdk_make_TaskInfo_h
#define acdk_make_TaskInfo_h

#include "Config.h"

#include <acdk.h>

#include <acdk/cfgscript/Props.h>

namespace acdk {
namespace make {

USING_CLASS(::acdk::cfgscript::, Props);

/**
  Status of the Task
*/
enum TaskStatus
{
  /**
    Task not executed yet
  */
  TaskNotBuild,
  /**
    Task is Build.
    It is not needed to call again
  */
  TaskOkBuild,
  /**
    Task was executed with a failure
  */
  TaskFailBuild
};
ACDK_DEF_LIB_ENUM(ACDK_ACDK_MAKE_PUBLIC, TaskStatus);

ACDK_DECL_CLASS(TaskInfo);

/**
  Information about a task
*/
class ACDK_ACDK_MAKE_PUBLIC TaskInfo
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(TaskInfo)
public:
  RString _taskName;
  RString _targetName;
  RString _description;
  RString _logString;
  RString _workingDir;
  int _logLevel;
  TaskStatus _taskStatus;
  TaskInfo(IN(RString) taskname, IN(RString) targetname = Nil,
           IN(RString) desc = Nil, IN(RString) logstring = Nil,
           int loglevel = acdk::util::logging::None)
  : _taskName(taskname)
  , _targetName(targetname)
  , _description(desc)
  , _logString(logstring)
  , _logLevel(loglevel)
  , _taskStatus(TaskNotBuild)
  {
  }
  virtual int logLevel() { return _logLevel; }
  virtual RString getTaskName() { return _taskName; }
  virtual RString getDescription() { return _description; }
  virtual RString getTargetName() { return _targetName; }
  virtual RString getExecuteLogString() { return _logString; }
  virtual RString getWorkingDir() { return _workingDir; }
  virtual TaskStatus getTaskStatus() { return _taskStatus; }
  virtual void setTaskBuildStatus(bool erg)
  {
    if (erg == true)
      _taskStatus = TaskOkBuild;
    else
      _taskStatus = TaskFailBuild;
  }
};




} // namespace make
} // namespace acdk 
  
#endif //acdk_make_TaskInfo_h
