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


#include "FileDependTask.h"
#include "DirExistsTask.h"

namespace acdk {
namespace make {

//virtual 
bool 
FileDependTask::execute(IN(RString) exec, IN(RProps) props)
{
  ::acdk::io::File f(_target);
  DirExistsTask fd(f.getParentFile());
  if (fd.doExecute(exec, props) == false)
  {
    ACDK_NLOG("acdk.make", Trace, "FileDependTask: Dir doesn't exists: " + f.getParent());
    return false;
  }
  ::acdk::io::FileStatus ts(_target);
  if (ts.exists() == false)
  {
    ACDK_NLOG("acdk.make", Trace, "FileDependTask: Target doesn't exists: " + _target);
    return false;
  }
  if (_source != Nil)
  {
    ::acdk::io::FileStatus ss(_source);
    bool ret = ts.lastModified() - ss.lastModified() >= 0;
    if (ret == false)
      ACDK_NLOG("acdk.make", Trace, "FileDependTask: source is newer. Source: " + _source + " Target: " + _target);
    return ret;
  } 
  else if (_sources != Nil) 
  {
    for (int i = 0; i < _sources->length(); ++i)
    {
      ::acdk::io::FileStatus ss(_sources[i]);
      bool ret = ts.lastModified() - ss.lastModified() < 0;
      if (ret == true)
      {
        ACDK_NLOG("acdk.make", Trace, "FileDependTask: source is newer. Source: " + _sources[i] + " Target: " + _target);
        return false;
      }
    }
    return true;
  }
  return true;
}
  

} // namespace make
} // namespace acdk

