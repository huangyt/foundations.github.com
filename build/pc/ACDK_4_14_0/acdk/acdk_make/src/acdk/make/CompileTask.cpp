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
#include "CompileTask.h"
#include "CppSourceDependTask.h"
#include "ShellExecuteTask.h"
#include <acdk/lang/System.h>
#include <acdk/io/CharArrayWriter.h>

namespace acdk {
namespace make {

using namespace acdk::cfgscript;

//virtual 
bool 
CompileTask::execute(IN(RString) exec, IN(RProps) props)
{
  RString compilemask = props->getStringVal("CCC_COMPILE_MASK", PropsParentRead);
  RString sourcefile = props->getStringVal("SOURCEFILE", PropsParentRead);
  RString objfile = props->eval("${OBJDIR}${DIRSEP}${OBJFILE}", PropsEvalRecursive | PropsEvalQuoteFileNameArgs | PropsParentRead);
  FileDependTask fdep(sourcefile, objfile);
  if (fdep.doExecute("", props) == true)
  {
    if (CppSourceDependTask::noSourceDeps == true)
      return true;
    if (CppSourceDependTask::onlyDirectIncludes == true)
    {
      if (CppSourceDependTask(sourcefile, objfile).execute("", props) == true)
        return true;
    }
    RStringArray includes = props->getStringArrayVal("CCC_INCLUDE_LIST", PropsParentRead);
    CppSourceDependTask csp(sourcefile, objfile);
    for (int i = 0; i < includes->length(); ++i)
    {
      csp.addIncludeDir(includes[i]);
    }
    if (csp.execute("", props) == true)
      return true;
    
  }
  RString cmdline = props->eval(compilemask, PropsEvalRecursive | PropsParentRead);
  ACDK_NLOG("acdk.make", Info, cmdline);

#ifdef ACDK_OS_WIN32
# define SHELLFLAGS 0
#else
#define SHELLFLAGS 0 //UseShell
#endif
  ShellExecuteTask shelex("compile", "Compile source", cmdline, SHELLFLAGS);

  return shelex.doExecute("compile", props);
}

} // namespace make
} // namespace acdk 


