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


#include "TaskCfgFile.h"
#include "TaskManager.h"

namespace acdk {
namespace make {




void 
TaskCfgFile::read()
{
  ::acdk::io::FileReader fin(_filename);
  load(&fin);
}

RObject 
TaskCfgFile::put(IN(RObject) k, IN(RObject) v)
{
  RString key = (RString)k;
  RString value = (RString)v;
  int pidx = key->indexOf("->");
  if (pidx != -1)
  {
    RString tname = key->substr(0, pidx);
    RString method = key->substr(pidx + 2);
    RObject obj = (RObject)TaskManager::getTask(tname);
    if (obj == Nil)
    {
      obj = get(&tname);
      if (obj == Nil)
      {
        ACDK_NLOG("acdk.make", Error, "TaskCfgFile: No task or variable known: " + tname);
        return obj;
      }
    }
    obj->invoke(method, v);
  } 
  else
  {
    RObject o = (RObject)New(key, v);
    Properties::put(v, o);
  }
  return Nil;
}

} // namespace make
} // namespace acdk 
  

