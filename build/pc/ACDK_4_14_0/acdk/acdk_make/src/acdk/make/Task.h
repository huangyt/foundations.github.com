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

#ifndef acdk_make_Task_h
#define acdk_make_Task_h

#include "Config.h"

#include <acdk.h>

#include <acdk/lang/Boolean.h>
#include <acdk/util/Properties.h>
#include <acdk/util/HashMap.h>
#include <acdk/io/File.h>
#include <acdk/io/FileStatus.h>

#include <acdk/cfgscript/Props.h>
#include "TaskInfo.h"

namespace acdk {
namespace make {

USING_CLASS(::acdk::util::, Properties);

enum TaskExecuteFlags
{
  /**
    Task, which executes successfully
    will skip next execution. 
    TaskExecuteForce forces execution.
  */
  TaskExecuteForce        = 0x0001,

  TaskExecuteDefault      = 0x0000
};
ACDK_DEF_LIB_ENUM(ACDK_ACDK_MAKE_PUBLIC, TaskExecuteFlags);

ACDK_DECL_INTERFACE(Task);

/**
  Base class of all acdkmake Tasks.
*/
class ACDK_ACDK_MAKE_PUBLIC Task
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Task)
public:
  /** 
    For logging and working dir
    @return valid TaskInfo
  */
  virtual RTaskInfo getTaskInfo() = 0;

  /** 
    This task depends on other task
    @param task which should executed before this task
  */
  virtual void addDependingTask(IN(RTask) task) = 0;
  /**
    Public interface to execute a task. 
    Implementation Task should implement execute.
    @param exec string identifies what to do
    @param props current properties
  */
  foreign virtual bool doExecute(IN(RString) exec, IN(RProps) props, int flags = TaskExecuteDefault);

protected:
  /**
    Public interface to execute a task. 
    Implementation Task should implement execute.
    @param exec string identifies what to do
    @param props current properties
  */
  virtual bool execute(IN(RString) exec, IN(RProps) props) = 0;
};




} // namespace make
} // namespace acdk 
  
#endif //acdk_make_Task_h
