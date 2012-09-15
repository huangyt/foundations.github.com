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

#ifndef acdk_make_FileCopyTask_h
#define acdk_make_FileCopyTask_h

#include "FileOpTask.h"



namespace acdk {
namespace make {




ACDK_DECL_CLASS(FileCopyTask);

/**
  Copy file or directory
  
    Source: /dir/sdir/file 
    Target: /dir/tdir
    Copied: /dir/tdir/file

    Source: /dir/sdir
    Target: /dir/tdir
    Copied: /dir/tdir/sdir

    @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC FileCopyTask
: extends FileOpTask
{
  ACDK_WITH_METAINFO(FileCopyTask)
public:
    
  FileCopyTask(IN(RString) source, IN(RString) target, IN(RString) sourcestartdir = Nil, int flags = 0)
  : FileOpTask(source, target, sourcestartdir, "copy file(s)", flags)
  {
  }
  FileCopyTask(IN(RFileSet) source, IN(RString) target, IN(RString) sourcestartdir = Nil, int flags = 0)
  : FileOpTask(source, target, sourcestartdir, "copy file(s)", flags)
  {
  }
  virtual bool executeFop(IN(RString) exec, IN(RString) source, IN(RString) target, IN(RProps) props);
};




} // namespace make
} // namespace acdk


#endif //acdk_make_FileCopyTask_h
