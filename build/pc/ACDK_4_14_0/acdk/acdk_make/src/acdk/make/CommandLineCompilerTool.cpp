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

#include "CommandLineCompilerTool.h"
#include "LookupFileTask.h"
#include "FileDependTask.h"
#include "ShellExecuteTask.h"

namespace acdk {
namespace make {

using namespace acdk::cfgscript;


bool 
SimpleFileDepencyChecker::rebuild(IN(RString) sourceFile, IN(RString) targetFile, IN(RProps) env)
{
  FileDependTask t(sourceFile, targetFile);
  if (t.doExecute("", env) == true)
    return false;
  return true;
}

bool 
CommandLineCompilerTool::configure(IN(RProps) env)
{
  if (_configured == true)
    return _toolFound;
  _configured = true;
  RLookupFileTask l = new LookupFileTask(_toolProps->getStringVal("TOOL_BASENAME"), Executable);
  if (l->execute("", env) == false)
    return false;
    
    /*foreach (String t in l.foundPathes())
    {
      out.println("found tool: " + t);  
    }
    */
  if (_configurator != Nil)
  {
    if (_configurator->configure(l->foundPath(), env) == false)
      return false;
  }
  ACDK_NLOG("acdk.make", Info, "Tool " + getToolClass() + "/" + getToolTribe() + " found: " + l->foundPath());
  _toolProps->setStringVal("TOOL_FQNAME", l->foundPath());
  _toolFound = true;
  return _toolFound;
}

bool  
CommandLineCompilerTool::execute(IN(RString) exec, IN(RProps) props)
{
  RProps p = new Props();
  p->addParentProps(props);
  p->addParentProps(_toolProps);

  if (_depChecker != Nil)
  {
    RString target = props->getEvaluatedStringVal("TARGETFILE");
    if (p->hasValue("SOURCEFILE") == true)
    {
      RString source = props->getStringVal("SOURCEFILE");
      if (_depChecker->rebuild(source, target, props) == false)
      {
        ACDK_NLOG("acdk.make", Info, "Target is up to date: " + target);
        return true;
      }
    }
    else if (p->hasValue("SOURCEFILE_LIST") == true)
    {
      RStringArray sa = p->getStringArrayVal("SOURCEFILE_LIST");
      bool rebuild = false;
      for (int i = 0; i < sa->length(); ++i)
      {
        RString tf = sa[i];
        if (_depChecker->rebuild(tf, target, props) == true)
        {
          rebuild = true;
          break;
        }
      }
      if (rebuild == false)
      {
        ACDK_NLOG("acdk.make", Trace, "Target is up to date: " + target);
        return true;
      }
    }
  }
  RString toolmask = p->getStringVal("TOOL_MASK");
  RString cmdline = p->eval(toolmask, PropsEvalRecursive | PropsParentRead);
  ACDK_NLOG("acdk.make", Info, cmdline);
#ifdef ACDK_OS_WIN32
# define SHELLFLAGS 0
#else
# define SHELLFLAGS 0 //UseShell
#endif
  ShellExecuteTask shelex("compile", "Compile source", cmdline, SHELLFLAGS);
  // TODO set filter 
  return shelex.doExecute("compile", props);
  
}

} // namespace make
} // namespace acdk 
  
