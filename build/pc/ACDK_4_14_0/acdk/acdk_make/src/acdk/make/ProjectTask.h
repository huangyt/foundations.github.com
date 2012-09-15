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

#ifndef acdk_make_ProjectTask_h
#define acdk_make_ProjectTask_h

#include "AbstractTask.h"
#include "TaskManager.h"


namespace acdk {
namespace make {


ACDK_DECL_CLASS(ProjectTask);

/**
  A ProjectTask is collection of sub tasks
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC ProjectTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(ProjectTask)
public:
  ProjectTask(IN(RString) name, IN(RStringArray) subtasksnames = Nil);
  virtual bool execute(IN(RString) exec, IN(RProps) props)
  {
    return AbstractTask::execute(exec, props);
  }
};


} // namespace make
} // namespace acdk


#endif //acdk_make_ProjectTask_h
