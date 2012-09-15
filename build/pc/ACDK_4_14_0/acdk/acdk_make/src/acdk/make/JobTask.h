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

#ifndef acdk_make_JobTask_h
#define acdk_make_JobTask_h

#include "AbstractTask.h"
#include "ThreadPool.h"


namespace acdk {
namespace make {

enum JobResult;

ACDK_DECL_CLASS(JobTask);
ACDK_DECL_CLASS(JobExecuterTask);

/**
  run a task as a job 
  @see acdk::make::JobExecuterTask
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC JobTask
: extends AbstractTask
, implements Job
{
  ACDK_WITH_METAINFO(JobTask)
private:
  RTask _task;
  RString _cmd;
  RProps _props;
  bool _erg;
  bool _active;
  /// instance of JobExecuterTask
  RObject _executer;
public:
  JobTask(IN(RObject) executer, IN(RTask) task, IN(RString) cmd, IN(RProps) props)
  : AbstractTask()
  , _task(task)
  , _cmd(cmd)
  , _props(props)
  , _erg(true)
  , _active(false)
  , _executer(executer)
  {
  }
  ~JobTask();
  virtual bool execute(IN(RString) exec, IN(RProps) props)
  {
    return _erg = _task->doExecute(_cmd, _props);
  }
  void setExecutor(IN(RObject) executer)
  {
    _executer = executer;
  }
  virtual void run();
  bool isActive()
  {
    SYNCTHIS();
    return _active;
  }
  virtual JobResult getResult();
};




} // namespace make
} // namespace acdk


#endif //acdk_make_JobTask_h
