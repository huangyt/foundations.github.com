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

#ifndef acdk_make_DirExistsTask_h
#define acdk_make_DirExistsTask_h

#include "Task.h"



namespace acdk {
namespace make {


ACDK_DECL_CLASS(DirExistsTask);
/**
  Create a given directory.
  execute fails if directory cannot be created.
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC DirExistsTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(DirExistsTask)
public:
  ::acdk::io::RFile _dir;
  DirExistsTask(IN(RString) dir)
  : AbstractTask()
  , _dir(new ::acdk::io::File(dir))
  {
  }
  DirExistsTask(IN(::acdk::io::RFile) dir)
  : AbstractTask()
  , _dir(dir)
  {
  }
  virtual bool execute(IN(RString) exec, IN(RProps) props)
  {
    if (_dir->exists() == true)
      return true;
    return _dir->mkdirs();
  }
  RTaskInfo getTaskInfo() 
  {
    RTaskInfo ti = AbstractTask::getTaskInfo();
    ti->_targetName = _dir->getName(); 
    return ti;
  }
};



} // namespace make
} // namespace acdk


#endif //acdk_make_DirExistsTask_h


