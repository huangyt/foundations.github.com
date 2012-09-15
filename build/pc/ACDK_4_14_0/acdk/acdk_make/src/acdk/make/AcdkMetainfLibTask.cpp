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


#include "AcdkMetainfLibTask.h"

namespace acdk {
namespace make {

AcdkMetainfLibTask::AcdkMetainfLibTask(IN(RString) name, IN(RAcdkLibTask) basedLibTask)
: AcdkLibTask(name, "ACDK metainfo library")
, _basedLibTask(basedLibTask)
{
  if (basedLibTask->_modules == Nil)
  {
    // ## may warn
    return;
  }
  int i;
  for (i = 0; i < basedLibTask->_modules->length(); ++i)
  {
    RString s = _basedLibTask->_modules[i];
    int idx = s->lastIndexOf('/');
    if (idx != -1)
    {
      RString sf = s->substr(idx + 1);
      s = s + "/" + sf + "_metainf";
      addSource(s);
    }
  }
  addAcdkLib(basedLibTask->getName());
  addAcdkLibs(basedLibTask->getAcdkLibs());
}
  
 



} // namespace make
} // namespace acdk



