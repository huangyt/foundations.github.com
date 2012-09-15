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


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/util/StringTokenizer.h>
#include <acdk/make/Task.h>
#include <acdk/make/PlattformSelectTask.h>
#include <acdk/make/AcdkLibTask.h>
#include <acdk/make/TaskManager.h>
#include <acdk/cfgscript/Script.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#if defined(AMAKE_WITH_LISP)
#include <acdk/lisp/LispEnvironment.h>
#include <acdk/lisp/LispAtom.h>
#endif //defined(AMAKE_WITH_LISP)

#include <acdk/make/MakeProps.h>
#include <acdk/make/CppSourceDependTask.h>
#include <acdk/make/AcdkUnitConfigTask.h>

namespace acdk {
namespace make {

using namespace ::acdk::util::logging;
using namespace ::acdk::cfgscript;

USING_CLASS(::acdk::io::, File);

/**
  (setf acdk_make_prj (new-acdk-project "acdk_make"))

  (setf acdk_make_lib (new-acdk-lib "acdk_make"))
  (acdk_make_lib 'addSource "src/acdk/make")
  (acdk_make_lib 'addAcdkLib "acdk_core")

  (acdk_make_prj 'addTask acdk_make_lib)

  (setf acdkmake_exe (new-acdk-exe "acdkmake"))
  (setf acdkmake_exe (new-acdk-exe "acdkmake"))
  (acdk_make_lib 'addSource "src/acdk/make/main")
  (acdk_make_lib 'addAcdkLib "acdk_core")
  (acdk_make_lib 'addAcdkLib "acdk_make")

  (acdkmake_exe 'addTask acdk_make_lib)

*/

class Main
{
public:
  static void help()
  {
    System::out->println(
      "acdkmake [options] [[target_options target_command]* tn]*\n"
      "  Copyright (C) 2005\n"
      "  $Revision: 1.48 $\n\n"
      "-f filename            Read make file instead of build.csf\n"
      "-sv key=value          Set string value\n"
      "-as key=value          Add string Array Value\n"
      "-to target_option      Target Option like DEBUG, STATIC, RELEASE\n"
      "-tn target_name        Target Name\n"
      "-tc target_command     Target command\n"
      "-dn                    No source dependencies\n"
      "-dd                    Only direct includes (default)\n"
      "-df                    Full dependencies\n"
      "-j number_of_jobs      number of paralall Jobs\n"
      "-k                     don't abort if failed\n"
      "-dump-tools            print available tools\n"
      "-dump-tasks            print task definitions\n"
      "-dump-targets          print available target names\n"
      "-dump-target target_name print target specs\n"
      "-dump-sel-target       print selected target specs\n"
      "-D defkey[=Value]      Passes as #define to compile tasks\n"
      "-I include             Passes as Include path to compile tasks\n"
      "-L libinclude          Passes as library path to compile/link tasks\n"
      "-PATH execpath         Passes as addionally execute path to ShellExecuteTasks\n"
      "-autoload directory    Loads/executes all script files in specific directory\n"
      "-help                  Print this help\n"

    );
    System::out->println("CfgScript options:");
    System::out->println(Script::getCfgScriptCommandLineHelp());
    System::out->println(System::getSystemCmdLineOps());
  }
  static bool isCompileType(IN(RProps) basicprops, RString tag)
  {
    if (tag->equals("RELEASE") || tag->equals("DEBUG") ||
        tag->equals("SHARED") || tag->equals("STATIC") ||
        tag->equals("BOEHMGC"))
        return true;
    return false;
  }
  static int executeTask(IN(RProps) basicprops, IN(RProps) target)
  {
    if (target->hasValue("tn", PropsNoFlags) == false)
      target->setStringVal("tn", "default", PropsNoFlags);
    RString targetname = target->getStringVal("tn", PropsNoFlags);
    RTask task = TaskManager::getTask(targetname);
    if (task == Nil)
    {
       ACDK_NLOG("acdk.make", Error, "Make task cannot be found: " + targetname);
       return 1;
    }

    if (target->containsInStringArrayVal("to", "DEBUG", PropsNoWarnRead) == false &&
        target->containsInStringArrayVal("to", "RELEASE", PropsNoWarnRead) == false)
        target->appendStringArrayVal("to", "DEBUG", PropsNoWarnRead);
    {
      RStringArray to = target->getStringArrayVal("to", PropsNoWarnRead);
      for (int i = 0; i < to->length(); ++i)
      {
        basicprops->appendStringArrayVal("COMPILE_TYPE", to[i], PropsNoStringDups | PropsNoWarnRead);
      }
    }


    int erg = 0;
    {
      RStringArray tc = target->getStringArrayVal("tc", PropsNoWarnRead);
      if (tc->length() == 0)
        tc->append("build");

      for (int i = 0; i < tc->length(); ++i)
      {
        RString taskcommand = tc[i];
        RProps tbprops = new Props("TaskProps", PropsParentRead, basicprops);

        bool berg = task->doExecute(taskcommand, tbprops, TaskExecuteForce);
        if (berg == false)
          erg |= 2;
      }
    }
    return erg;
  }
  static int autoloadFile(IN(RString) csffile, IN(RProps) basicProps)
  {
    ACDK_NLOG("acdk.make", Trace, "Autoload: " + csffile);
    RScript script = new Script(csffile);
    try {
      if (script->readEval(basicProps) != 0)
        return 2;    
      return 0;
    } catch (RThrowable ex) {
      System::out->println("Unandled exeption at:");
      if (script != Nil)
        System::out->println(acdk::cfgscript::Script::getScriptBackTrace());
      ex->printStackTrace();
      System::out->println("\n:" + ex->getMessage());
    }
    return 3;
  }
  static int autoload(IN(RString) directory, IN(RProps) basicProps)
  {
    RFile f = new File(directory);
    if (f->exists() == false)
    {
      ACDK_NLOG("acdk.make", Error, "Autoload: Cannot open file or directory: " + f->getCanonicalPath());
      return 2;
    }
    if (f->isFile() == true)
    {
      return autoloadFile(f->getCanonicalPath(), basicProps);
    }
    if (f->isDirectory() == true)
    {
      RFileArray files = f->listFiles(new acdk::io::GlobFilenameFilter("*.csf"));
      for (int i = 0; i < files->length(); ++i)
      {
        int erg = autoloadFile(files[i]->getCanonicalPath(), basicProps);
        if (erg != 0)
          return erg;
      }
    }
    return 0;
  }
  static void loadStdOpts(IN(RStringArray) args)
  {
    File f("amake.opts");
    if (f.exists() == false)
      return;
    RString s = acdk::io::ByteToCharReader(f.getReader()).readString();
    acdk::util::StringTokenizer stk(s);
    RStringArray sa = stk.allToken();
    for (int i = 0; i < sa->length(); ++i)
    {
      RString s = sa[i];
      args->append(s);
    }
  }
  static int acdkmain(RStringArray cmdlargs)
  {
    RStringArray args = cmdlargs;
    loadStdOpts(args);
    RScript script;
    try {

    if (LogManager::TresholdSet == false)
      LogManager::Threshold = Info;

      //System::out->println("ACDK_TOOLS_HOME: " + System::getAcdkToolsHome());
    RLogger acdk_make_logger = new Logger("acdk.make");
    acdk_make_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
    RLogger acdk_cfgscript_logger = new Logger("acdk.cfgscript");
    acdk_cfgscript_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
    //acdk::util::logging::LogManager::Threshold = acdk::util::logging::LogManager::MinLevel = acdk::util::logging::Info;
    //args = acdk::util::logging::LogManager::parseCommandLine(args);

    ACDK_NLOG("acdk.make", Info, "acdkmake (c) 2005 by artefaktur");


    RString buildfilename;
    RProps basicprops = new MakeProps("Root", Nil, false);
    RStringArray unspecs = new StringArray(0);
    args = Script::parseCfgScriptOptions(args, basicprops);
    RProps targets = new Props("Targets", PropsParentRead, Nil, true);
    RString lastTarget = "default";
    RAcdkUnitConfigTask unitConfig = new AcdkUnitConfigTask();
    {
    ScopedCfgVar globals(basicprops, "globals", new DmiObject((RObject)&basicprops), PropsNoFlags);
    basicprops->setObjectVal("AcdkUnitConfigTask", &unitConfig, PropsNoFlags);
    bool dumpdef = false;
    bool dumptarget = false;
    bool dumptools = false;
#define NEXT_OPT() \
  do { \
    RString sicopt = args[i]; \
    ++i; \
    if (i >= args->length()) { \
      help(); \
      System::out->println("\n" + sicopt + " need argument"); \
      return 10; \
    } \
    curArg = args[i]; \
  } while(false)
    RString curArg = "";
    for (int i = 1; i < args->length(); ++i)
    {
      RString s = args[i];
      if (s->equals("-sv") == true || s->equals("-as") == true)
      {
        NEXT_OPT();
        RString kv = curArg;
        int ipos = 0;
        if ((ipos = kv->indexOf('=')) == -1)
        {
          System::out->println("Parsing command line: expect Key=Value not [" + kv + "]");
          return 10;
        }
        if (s->equals("-sv") == true)
          basicprops->setStringVal(kv->substr(0, ipos), kv->substr(ipos + 1), PropsNoFlags);
        else
          basicprops->appendStringVal(kv->substr(0, ipos), kv->substr(ipos + 1), " ", PropsNoFlags);
      }
      else if (s->equals("-f") == true)
      {
        NEXT_OPT();
        buildfilename = curArg;
      }
      else if (s->equals("-to") == true)
      {
        NEXT_OPT();
        RProps pt = targets->getProps(lastTarget, PropsNoWarnRead);
        pt->appendStringArrayVal("to", curArg, PropsNoFlags);
      }
      else if (s->equals("-tn") == true)
      {
        NEXT_OPT();
        lastTarget = curArg;
        RProps pt = targets->getProps(lastTarget, PropsNoWarnRead);
        pt->setStringVal("tn", curArg, PropsNoFlags);
        basicprops->setStringVal("AMAKE_TARGET", s, PropsNoFlags);
      }
      else if (s->equals("-j") == true)
      {
        NEXT_OPT();
	int j = Integer::parseInt(curArg);
        basicprops->setIntVal("AMAKE_JOBCOUNT", j, PropsNoFlags);
	//System::out->println(RString("JC: ") + basicprops->getIntVal("AMAKE_JOBCOUNT"));
      }
      else if (s->equals("-tc") == true)
      {
        NEXT_OPT();
        targets->getProps(lastTarget, PropsNoWarnRead)->appendStringArrayVal("tc", curArg, PropsNoFlags);
      }
      else if (s->equals("-help") == true)
      {
        help();
        return 0;
      }
      else if (s->equals("-k") == true)
      {
        TaskManager::noFailOnBreak(true);
      }
      else if (s->equals("-dn") == true)
      {
        CppSourceDependTask::noSourceDeps = true;
      }
      else if (s->equals("-dd") == true)
      {
        CppSourceDependTask::onlyDirectIncludes = true;
      }
      else if (s->equals("-df") == true)
      {
        CppSourceDependTask::onlyDirectIncludes = false;
      }
      else if (s->equals("-dump-tasks") == true)
      {
        dumpdef = true;
      }
      else if (s->equals("-D") == true)
      {
        NEXT_OPT();
        unitConfig->addDefine(curArg);
      }
      else if (s->equals("-I") == true)
      {
        NEXT_OPT();
        unitConfig->addIncludePath(curArg);
      }
      else if (s->equals("-L") == true)
      {
        NEXT_OPT();
        unitConfig->addLibraryPath(curArg);
      }
      else if (s->equals("-PATH") == true)
      {
        NEXT_OPT();
        unitConfig->addExecPath(curArg);
      }
      else if (s->equals("-autoload") == true)
      {
        NEXT_OPT();
        int erg = autoload(curArg, basicprops);
        if (erg != 0)
          return erg;
      }
      /*else if (s->equals("-csfinclude") == true)
      {
        NEXT_OPT();
        basicprops->appendStringArrayVal("CSFINCLUDES", args[i]);
      }*/
      else if (s->equals("-dump-targets") == true)
      {
        PlattformSelectTask pst;
        RStringArray sa = pst.getAvailableTargets(basicprops);
        System::out->println("Available targets: " + sa->toString());
        return 0;
      }
      else if (s->equals("-dump-target") == true)
      {
        NEXT_OPT();
        RString tname = args[i];
        dumptarget = true;
        PlattformSelectTask platform;
        basicprops->setStringVal("AMAKE_TARGET", tname, PropsNoFlags);
        platform.doExecute("", basicprops);
        basicprops->dump();
        return 0;
      }
      else if (s->equals("-dump-sel-target") == true)
      {
        dumptarget = true;
      }
      else if (s->equals("-dump-tools") == true)
      {
        dumptools = true;
      }
      
      else
      {
        if (s->startsWith("-") == true)
        {
          help();
          return 1;
        }
        unspecs->append(s);
      }
    }


    PlattformSelectTask platform;
    platform.doExecute("", basicprops);
    if (dumptarget == true)
    {
      basicprops->dump();
      return 0;
    }
    if (buildfilename == Nil)
    {
      if (File("build.csf").exists() == true)
        buildfilename = "build.csf";
      else if (File("build.lsp").exists() == true)
        buildfilename = "build.lsp";
      else
        buildfilename = "build.csf";
    }
    if (File(buildfilename).exists() == false)
    {
      System::out->println("File not found: " + buildfilename);
      return 1;
    }
#if defined(AMAKE_WITH_LISP)
    ::acdk::lisp::RLispEnvironment lenv;
#endif
    if (buildfilename->endsWith(".csf") == true)
    {
      script = new Script(buildfilename);
      //int i;
      //std::cin >> i;
      if (script->readEval(basicprops) != 0)
        return 2;
    }
#if defined(AMAKE_WITH_LISP)
    else if (buildfilename->endsWith(".lsp") == true)
    {
      lenv = new ::acdk::lisp::LispEnvironment();
      lenv->init();
      lenv->bindGlobal("buildprops", new ::acdk::lisp::LispAtom(acdk::lang::dmi::ScriptVar(&basicprops)));

      ::acdk::io::File buildfile(buildfilename);

      RString perg = lenv->load(buildfilename);
      if (perg == Nil)
        return 1;
    }
#endif //defined(AMAKE_WITH_LISP)
    if (dumpdef == true)
    {
      RStringArray tasks = TaskManager::getTasks();
      for (int i = 0; i < tasks->length(); ++i)
      {
        RTask t = TaskManager::getTask(tasks[i]);
        RTaskInfo ti = t->getTaskInfo();
        System::out->println(
          "Task registered as: " + tasks[i] + "\n"
          "\tName: " + ti->getTaskName() + "\n"
          "\tDescription: " + ti->getDescription() + "\n"
          "\tTarget: " + ti->getTargetName() + "\n"
          "\tWorkDir: " + ti->getWorkingDir()
          );
        if (instanceof(t, AbstractTask) == true)
        {
          RTaskArray childs = RAbstractTask(t)->getChilds();
          for (int j = 0; j < childs->length(); ++j)
          {
            System::out->println("\tDepends on: " + childs[j]->getTaskInfo()->getTaskName());
          }
        }
      }
    }
    if (dumptools == true)
    {

    }

    {
      for (int i = 0; i < unspecs->length(); ++i)
      {
        RString s = unspecs[i];
        if (TaskManager::getTask(s) != Nil)
        {
          lastTarget = s;
          RProps pt = targets->getProps(lastTarget, PropsNoWarnRead);
          pt->setStringVal("tn", s, PropsNoFlags);
          //targets->append(s);
        }
        else if (isCompileType(basicprops, s) == true)
        {
          targets->getProps(lastTarget, PropsNoWarnRead)->appendStringArrayVal("to", s, PropsNoWarnRead);
        }
        else
        {
          targets->getProps(lastTarget, PropsNoWarnRead)->appendStringArrayVal("tc", s, PropsNoWarnRead);
        }
      }
    }
    if (targets->size(PropsNoFlags) == 0)
    {
      RProps defaulttarget = targets->getProps("default", PropsNoWarnRead);
      defaulttarget->setStringVal("tn", "default", PropsNoWarnRead);
      //defaulttarget->appendStringArrayVal("tc", "build", PropsNoWarnRead);
      return executeTask(basicprops, defaulttarget);
    }
    else
    {
      RStringArray tasknames = targets->getKeys(PropsNoFlags);
      int ret = 0;
      for (int i = 0; i < tasknames->length(); ++i)
      {
        ret |= executeTask(basicprops, targets->getProps(tasknames[i], PropsNoFlags));
      }
      return ret;
    }
  }
  } catch (RDmiException ex) {
    System::out->println("Unandled exeption at:");
    if (script != Nil)
      acdk::lang::System::err->println(Script::getScriptBackTrace());
    else
      ex->printStackTrace();
    System::out->println("Unandled dmi exeption: "  + ex->getMessage());
    
    return 1;
  } catch (RThrowable ex) {
    System::out->println("Unandled exeption at:");
    if (script != Nil)
      acdk::lang::System::err->println(Script::getScriptBackTrace());
    else
      ex->printStackTrace();
    System::out->println("\n:" + ex->getMessage());
    return 1;
  }
  return 0;
  }

};


} // namespace make
} // namespace acdk



int
main(int argc, char* argv[], char** envptr)
{
  int erg =  ::acdk::lang::System::main(acdk::make::Main::acdkmain, argc, argv, envptr);
  return erg;
}
