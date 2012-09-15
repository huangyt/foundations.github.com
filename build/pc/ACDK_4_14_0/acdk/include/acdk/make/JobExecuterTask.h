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

#ifndef acdk_make_JobExecuterTask_h
#define acdk_make_JobExecuterTask_h

#include "JobTask.h"
#include "ThreadPool.h"


namespace acdk {
namespace make {




ACDK_DECL_CLASS(JobExecuterTask);

/**
  Runs a collection of task in threads
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC JobExecuterTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(JobExecuterTask)
private:
  RThreadPool _pool;
  int _threadNum;
  bool _breakOnFail;
public:
  JobExecuterTask(int threadNum, bool breakOnFail = true);
  void addTask(IN(RJobTask) jt)
  {
    jt->setExecutor(this);
    _pool->enqeue(&jt);
  }
  void addTask(IN(RTask) task, IN(RString) cmd, IN(RProps) props)
  {
     _pool->enqeue(new JobTask(this, task, cmd, props));
  }
  virtual bool execute(IN(RString) exec, IN(RProps) props);
  bool breakOnFail() { return _breakOnFail; }
};




} // namespace make
} // namespace acdk


#endif //acdk_make_JobExecuterTask_h
