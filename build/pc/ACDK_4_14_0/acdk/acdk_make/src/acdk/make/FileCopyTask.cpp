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


#include "FileCopyTask.h"
#include "FileDependTask.h"
#include "DirExistsTask.h"
#include <acdk/io/File.h>

namespace acdk {
namespace make {

USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::io::, FileStatus);

//virtual 
bool 
FileCopyTask::executeFop(IN(RString) exec, IN(RString) source, IN(RString) target, IN(RProps) props)
{
  File sf(source);
  if (sf.exists() == false)
  {
    ACDK_NLOG("acdk.make", Error, "FileCopyTask::execute: Source does not exists: " + source);  
    return false;
  }

  if (sf.isFile() == true)
  {
    File tf(target);
    RFile nf;
    if (tf.exists() == true)
    {
      if (tf.isDirectory() == true)
        nf = new File(&tf, sf.getName());
      else {
        nf = &tf;
      }
    } else
        nf = &tf;
    if (nf->exists() == true)
    {
      if (_flags & FileCopyOnlyNewer)
      {
        FileDependTask fdep(sf.getCanonicalPath(), nf->getCanonicalPath());
        if (fdep.execute(Nil, props) == true)
          return true;
      }
      nf->deleteFile();
    } 
    else if (DirExistsTask(nf->getParentFile()).execute(Nil, props) == false)
    {
      ACDK_NLOG("acdk.make", Error, "FileCopyTask: Cannot create directory: " + nf->getParentFile()->getCanonicalPath());
      return false;
    }
    ACDK_NLOG("acdk.make", Info, "cp " + sf.getCanonicalPath() + " " + nf->getCanonicalPath());
    sf.getReader()->trans(nf->getWriter());
    if (_flags & FileCopyWithAttributes)
    {
      FileStatus sfs(sf.getCanonicalPath());
      FileStatus tfs(nf->getCanonicalPath());
      tfs.lastAccessed(sfs.lastAccessed());
      tfs.lastModified(sfs.lastModified());
      tfs.created(sfs.created());
    }
    return true;
  }
  ACDK_NLOG("acdk.make", Error, "FileCopyTask::execute: copy directories currently not supported: " + source);
  return false;
}
  



} // namespace make
} // namespace acdk



