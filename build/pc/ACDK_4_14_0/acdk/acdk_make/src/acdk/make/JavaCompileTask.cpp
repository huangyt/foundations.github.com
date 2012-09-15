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


#include "JavaCompileTask.h"
#include "ShellExecuteTask.h"
#include <acdk/lang/System.h>


namespace acdk {
namespace make {

using namespace acdk::cfgscript;

void 
JavaCompileTask::help()
{
  System::out->println(
      "help       print this help\n"
      "compile    compile java source\n"
      "build      compile\n"
      );

}

bool 
JavaCompileTask::execute(IN(RString) exec, IN(RProps) props)
{
  if (exec->equals("compile") == true)
  {
    help();
    return true;
  }
  else if (exec->equals("compile") == true || exec->equals("build") == true)
  {
    Props cprops("", PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, props);
    cprops.setStringVal("SOURCE", _source);
    RString targetdir = _targetDir;
    if (targetdir == Nil)
    {
      targetdir = props->eval("${ACDKHOME}${DIRSEP}bin", PropsEvalRecursive | PropsParentRead);
    }
    cprops.setStringVal("JAVA_OUTDIR", targetdir, PropsNoFlags);
    RString cmdline = cprops.eval("${JAVA_COMPILE_MASK}", PropsEvalRecursive | PropsParentRead);
    ShellExecuteTask shelex("compile", "Java", cmdline, 0);
    if (shelex.doExecute("exec", props) == false)
    {
      ACDK_NLOG("acdk.make", Error, "Compile java failed");
      return false;
    }
    return true;
  } 
  else if (exec->equals("test") == true)
  {
    return true;
  } else {
    ACDK_NLOG("acdk.make", Error, "Unknown command: " + exec);
    help();
    return false;
  }
}


} // namespace make
} // namespace acdk



