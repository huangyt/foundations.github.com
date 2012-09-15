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


#include "AcdkProjectTask.h"
#include "FileSet.h"
#include "FileCopyTask.h"

namespace acdk {
namespace make {

using namespace acdk::cfgscript;

//virtual 
bool 
AcdkProjectTask::execute(IN(RString) exec, IN(RProps) props)
{
  bool berg = ProjectTask::execute(exec, props);
  if (exec->equals("install") == false)
    return berg;
  RString dirsep = props->getStringVal("DIRSEP", PropsParentRead | PropsWarnRead);
  RString target = props->getAcdkHome() + dirsep + "cfg";
  FileSet fs("cfg/**/*.*");
  if (FileCopyTask(&fs, target, "cfg", 
                     FileCopyOnlyNewer | FileCopyWithAttributes).execute(Nil, props) == false)
    return false;
  return berg;
}

} // namespace make
} // namespace acdk



