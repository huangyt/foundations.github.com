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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/cfgscript/acdkcfgscript/Main.cpp,v 1.31 2005/05/02 23:12:05 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/IOException.h>

#include "Main.h"

#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/lang/sys/core_memtrace.h>

namespace acdk {
namespace cfgscript {

namespace acdkcfgscript {
using namespace acdk::util::logging;


void 
Main::help()
{
  System::out->println(SBSTR(
    "CfgScript Interpreter. Copyright 2003-2005 by Roger Rene Kommer, artefaktur\n"
    "Syntax:\n"
    "  acdkcfgscript <acdk-options> <cfgscript-options> file.csf <script arguments>\n"
    "  acdkcfgscript <acdk-options> <cfgscript-options> -e <scriptcode>\n"
    "cfgscript-options:\n"
    << Script::getCfgScriptCommandLineHelp()
    << "\nacdk-options:\n"
    << System::getSystemCmdLineOps()));
}



int 
Main::interActive(IN(RProps) props)
{
  RScript script = new Script("<console>");
  Script::breakToDebug();
  int ret = script->eval(";", props);
  return ret;
}

int 
Main::start(RStringArray sargs)
{
  

  RLogger acdk_make_logger = new Logger("acdk.make");
  acdk_make_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
  acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));

  RStringArray args = sargs;
  args = Script::parseCfgScriptOptions(args, Nil);
  bool isSet = false;
  true || false && (isSet = true);

  if (args->length() < 2) 
  {
    help();
    return 1;
  }
  bool interactive = false;
  RString fname;
  RString evalString;
  int scriptArgsBegin = -1;
  for (int i = 1; i < args->length(); ++i)
  {
    RString opt = args[i];
    
    if (opt->equalsIgnoreCase("-i") == true)
    {
      interactive = true;
    } 
    else  if (opt->equalsIgnoreCase("-e") == true)
    {
      ++i;
      StringBuffer sb;
      for (; i < args->length(); ++i)
        sb << " " << args[i];
      evalString = sb.toString();
      break;
    }
    else if (opt->startsWith("-") == true)
    {
      System::out->println("Unknown option: " + opt);
      help();
      return 1;
    }
    else
    {
      if (fname == Nil)
        fname = opt;
      else if (scriptArgsBegin == -1)
        scriptArgsBegin = i;
    }
  }

  int ret = 0;
  RProps props = new Props(PropsParentRead, Nil, false);
  if (scriptArgsBegin != -1)
  {
    RStringArray scriptArgs = new StringArray(args->length() - scriptArgsBegin);
    for (int i = scriptArgsBegin; i < args->length(); ++i)
      scriptArgs[i - scriptArgsBegin] = args[i];
    props->setObjectVal("scriptarguments", &scriptArgs);
  }
  else
    props->setObjectVal("scriptarguments", new StringArray(0));
  props->setSingleThreaded(true);
  RScript script;
  if (fname != Nil || evalString != Nil)
  {
    try {
      if (fname != Nil)
      {
        script = new Script(fname);
        ret = script->readEval(props);
      }
      else if (evalString != Nil)
      {
        script = new Script("");
        ret = script->eval(evalString, props);
      }
    } 
    catch (acdk::io::RThrowable ex)
    {
      if (script == Nil)
        throw;
      acdk::lang::System::err->println("unhandled exception " + ex->getClass()->getName() + ":\n" + ex->getMessage() + "\nIn:");
    
      acdk::lang::System::err->println(Script::getScriptBackTrace());
      acdk::lang::System::err->println("C++:");
      ex->printStackTrace(acdk::lang::System::err);
      return 1;
    }
  }
  if (interactive == true)
    return interActive(props);
  return ret;
  //return 0;
}


} 
}
}

struct MemChecker
{
  acdk::lang::sys::core_memtrace trace;
  MemChecker(bool memLeakOnly = true)
    : trace(memLeakOnly)
  {
  }
  ~MemChecker() 
  {
    trace.reportUnfreed(::acdk::lang::sys::MTRFUnfreed | ::acdk::lang::sys::MTRFMergeSameBt | 
                        ::acdk::lang::sys::MTRFRootsOnly | ::acdk::lang::sys::MTRFNoStatics);
    //trace.reportUnfreed(::acdk::lang::sys::MTRFMergeSameBt);
  }
};
//MemChecker _memchecker(false);

int
main(int argc, char* argv[], char** envptr)
{
  int erg = acdk::lang::System::main(::acdk::cfgscript::acdkcfgscript::Main::start, argc, argv, envptr);
  return erg;  

}
