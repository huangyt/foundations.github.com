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

#ifndef acdk_make_CppSourceDependTask_h
#define acdk_make_CppSourceDependTask_h

#include "AbstractTask.h"



namespace acdk {
namespace make {

struct DepCacheStack;

ACDK_DECL_CLASS(CppSourceDependTask);

/**
  Checks include deps.
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC CppSourceDependTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(CppSourceDependTask)
public:
  RString _source;
  RString _target;
  RStringArray _includeDirs;
  /**
    Parse also found include files.
    If false only the directly included files are checked.
  */
  bool _checkRecursive;
  /**
    Check only files which are included
    #enclude "OtherFile.h"
    or
    #enclude "../src/OtherFile.h"
    ignore all 
    #enclude <OtherFile.h>
  */
  bool _checkOnlyDirectIncludes;
  static bool noSourceDeps;
  static bool onlyDirectIncludes;
  static bool recursiveDeps;
  static bool useCache;
  CppSourceDependTask(IN(RString) parseSource, IN(RString) target, bool checkRecursive = recursiveDeps, bool onlyDirectIncludes = onlyDirectIncludes)
  : AbstractTask(target, target, "")
  , _source(parseSource)
  , _target(target)
  , _includeDirs(new StringArray(0))
  , _checkRecursive(checkRecursive)
  , _checkOnlyDirectIncludes(onlyDirectIncludes)
  
  {
  }
  void addIncludeDir(IN(RString) str);
  /**
    returns true if target exists and is not older thant source
  */
  virtual bool execute(IN(RString) exec, IN(RProps) props);
protected:
  foreign bool parseFile(IN(RString) file, jlong ts, IN(RStringArray) parsedFiles, IN(RString) cwd, DepCacheStack& dcs);
  
};




} // namespace make
} // namespace acdk


#endif //acdk_make_CppSourceDependTask_h
