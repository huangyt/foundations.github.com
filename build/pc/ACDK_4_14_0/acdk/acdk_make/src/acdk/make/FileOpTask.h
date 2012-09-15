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

#ifndef acdk_make_FileOpTask_h
#define acdk_make_FileOpTask_h

#include "AbstractTask.h"

#include "FileSet.h"

namespace acdk {
namespace make {

enum FileCopyTaskFlags
{
  FileCopyOnlyNewer       = 0x0001,
  FileCopyWithAttributes  = 0x0002
};
ACDK_DEF_LIB_ENUM(ACDK_ACDK_MAKE_PUBLIC, FileCopyTaskFlags);

ACDK_DECL_CLASS(FileOpTask);

/**
  Abstract base class for file operations.
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC FileOpTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(FileOpTask)
public:
  RFileSet _source;
  RString _sourceBaseDir;
  RString _target;
  int _flags;
    
  FileOpTask(IN(RString) source, IN(RString) target = Nil, IN(RString) soursebasedir = Nil, 
            IN(RString) desc = "file op", int flags = 0)
  : AbstractTask(source, source, desc)
  , _source(new FileSet(source))
  , _sourceBaseDir(soursebasedir)
  , _target(target)
  , _flags(flags)
  {
  }
  FileOpTask(IN(RFileSet) source, IN(RString) target = Nil, IN(RString) soursebasedir = Nil, 
            IN(RString) desc = "file op", int flags = 0)
  : AbstractTask("<fileset>", "<fileset>", desc)
  , _source(source)
  , _sourceBaseDir(soursebasedir)
  , _target(target)
  , _flags(flags)
  {
  }
  virtual bool execute(IN(RString) exec, IN(RProps) props);
  
  virtual bool executeFop(IN(RString) exec, IN(RString) source, IN(RString) target, IN(RProps) props) = 0;
private:
  //bool _execute(IN(RString) exec, IN(RString) source, IN(RString) target, IN(RProps) props);
};




} // namespace make
} // namespace acdk


#endif //acdk_make_FileOpTask_h
