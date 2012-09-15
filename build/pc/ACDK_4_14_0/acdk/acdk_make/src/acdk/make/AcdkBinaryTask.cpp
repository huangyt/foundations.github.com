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


#include "AcdkLibTask.h"
#include "ShellExecuteTask.h"
#include "DirExistsTask.h"
#include "CompileTask.h"
#include "FileDependTask.h"
#include "FileDeleteTask.h"
#include "FileCopyTask.h"
#include "FileSet.h"
#include "TaskManager.h"
#include "JobExecuterTask.h"

#include <acdk/util/Arrays.h>
#include <acdk/lang/System.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/io/CharToByteWriter.h>
#include <acdk/locale/Encoding.h>
#include <acdk/util/StringTokenizer.h>


namespace acdk {
namespace make {

USING_CLASS(::acdk::io::, File);
using namespace acdk::cfgscript;

AcdkBinaryTask::AcdkBinaryTask(IN(RString) name, IN(RString) desc)
: AbstractTask(name, name, desc)
, _moduleProps(new Props(name, PropsNoParentWrite | PropsParentRead | PropsNoWarnRead))
, _modules(new StringArray(0))
, _sources(Nil) //will be set in getSourceSet
, _headers(Nil)
, _sourceAndHeader(new StringArray(0))
, _initialized(false)
, _baseSourceDir("src")
, _moduleConfigTasks(new TaskArray(0))
{
  ACDK_SAFE_CONSTRUCTOR();
  _baseDir = File::getCWD();
  TaskManager::registerTask(this);
}


void 
AcdkBinaryTask::addSource(IN(RString) source)
{
  if (source->endsWith(".def") == true)
    _defFile = source;
  else
    _sourceAndHeader->append(source);
  /*
  if (File(source).isDirectory() == true)
  {
    //RStringArray 
    _sources->addSpec(source + File::separator() + "*.cpp");
    _headers->addSpec(source + File::separator() + "*.h");
  } 
  else 
  {

    if (source->endsWith(".h") || source->endsWith(".hxx") || source->endsWith(".hpp") )
      _headers->addSpec(source);
    else if (source->endsWith(".def") == true)
      _defFile = source;
    else
      _sources->addSpec(source);
  }*/
}

RFileSet
AcdkBinaryTask::getSourceSet(IN(RProps) props)
{
  if (_sources != Nil)
    return _sources;
  return _sources = FileSet::createFileSpecs(_sourceAndHeader, props->getStringArrayVal("CCC_SOURCE_FILE_PATTERN"), false);
}

RFileSet 
AcdkBinaryTask::getHeaderSet(IN(RProps) props)
{
  if (_headers != Nil)
    return _headers;
  return _headers = FileSet::createFileSpecs(_sourceAndHeader, props->getStringArrayVal("CCC_HEADER_FILE_PATTERN"), false);
}

void 
AcdkBinaryTask::addModule(IN(RString) modpath)
{
  _modules->append(modpath);
  addSource(modpath);
}

void 
AcdkBinaryTask::addExtLib(IN(RString) lib)
{
   _moduleProps->appendStringArrayVal("CCC_EXT_LIBARIES_LIST", lib);
}

void 
AcdkBinaryTask::addExtLibs(IN(RStringArray) libs)
{
  for (int i = 0; i < libs->length(); ++i)
    addExtLib(libs[i]);
}

void 
AcdkBinaryTask::addExtIncludeDir(IN(RString) dir)
{
  _moduleProps->appendStringArrayVal("CCC_EXT_INCLUDE_LIST", dir);
}

void 
AcdkBinaryTask::addExtIncludeDirs(IN(RStringArray) dirs)
{
  for (int i = 0; i < dirs->length(); ++i)
    addExtIncludeDir(dirs[i]);
}

void 
AcdkBinaryTask::addDefine(IN(RString) key, IN(RString) val)
{
  RString v = key;
  if (val->equals("") == false)
    v = key + "=" + val;
  _moduleProps->appendStringArrayVal("CCC_EXT_DEFINES_LIST", v);
}

void 
AcdkBinaryTask::addLibraryPath(IN(RString) path)
{
  _moduleProps->appendStringArrayVal("CCC_EXT_LIBPATH_LIST", path);
}

void 
AcdkBinaryTask::addLibraryPaths(IN(RStringArray) paths)
{
  for (int i = 0; i < paths->length(); ++i)
    addLibraryPath(paths[i]);
}

void 
AcdkBinaryTask::setBaseDir(IN(RString) basedir)
{
  _baseDir = basedir;
#if defined(ACDK_OS_WIN32)
  _baseDir = _baseDir->replace('/', '\\');
#endif
}

void 
AcdkBinaryTask::addModuleConfigTask(IN(RString) taskname)
{
  RTask t = TaskManager::getTask(taskname);
  if (t == Nil)
  {
    ACDK_NLOG("acdk.make", Error, "Cannot find ModuleConfigTask: " + taskname);
    return;
  }
  _moduleConfigTasks->append(t);
}

RStringArray 
AcdkBinaryTask::expandSources(IN(RStringArray) sa)
{
  RStringArray erg = new StringArray(0);
  for (int i = 0; i < sa->length(); ++i)
  {
    RString tn = sa[i];
    ::acdk::io::File tf(tn);
    if (tf.isFile() == true && tn->endsWith(".cpp") || tn->endsWith(".c"))
    {
      erg->append(sa[i]);
    } else if (tf.isDirectory() == true)
    {
      RStringArray ta = tf.list(new ::acdk::io::GlobFilenameFilter("*.cpp"));
      int i;
      for (i = 0; i < ta->length(); ++i)
      {
        erg->append(tn + ::acdk::io::File::separator() + ta[i]);
      }
      ta = tf.list(new ::acdk::io::GlobFilenameFilter("*.c"));
      for (i = 0; i < ta->length(); ++i)
      {
        erg->append(tn + ::acdk::io::File::separator() + ta[i]);
      }
    }
  }
  return erg;
}


RString 
AcdkBinaryTask::getReleaseTag(IN(RProps) props)
{
  RStringArray tags = props->getStringArrayVal("COMPILE_TYPE", false);
  acdk::util::Arrays::sort(tags);
  StringBuffer sb;
  for (int i = 0; i < tags->length(); ++i)
  {
    RString s = tags[i];
    if (s->equals("LIB") == true ||
        s->equals("EXE") == true)
      continue;
    if (i > 0)
      sb.append("_");
    sb.append(tags[i]->toLowerCase());
  }
  return sb.toString();
}

void
AcdkBinaryTask::setupAcdkDeps(IN(RProps) props)
{
  ACDK_NLOG("acdk.make", Info, "initialize: " + _name);
  _initialized = true;
  RString objdir = _baseDir + ::acdk::io::File::separator() + 
                    "tobj" + ::acdk::io::File::separator() + 
                      _name + ::acdk::io::File::separator()
                      ;
  RString objdir2 = props->getStringVal("AMAKE_TARGET", PropsParentRead) + ::acdk::io::File::separator() + 
                      getReleaseTag(props);
  if (objdir2->startsWith("vc\\") == true)
  {
    props->setStringVal("CCC_DEBUG_BUILDTYPE_APPENDIX", "_d");
    props->setStringVal("CCC_BUILDTYPE_APPENDIX", "_d");
    props->setStringVal("CCC_RELEASE_BUILDTYPE_APPENDIX", "_r");
  }
  if (objdir2->equals("vc\\release") == true)
    objdir2 = "dsp_r";
  else if (objdir2->equals("vc\\debug") == true)
    objdir2 = "dsp_d";
  
  else if (objdir2->equals("linux\\debug") == true)
    objdir2 = "linux";
  else if (objdir2->equals("sunos\\debug") == true)
    objdir2 = "sunos-gcc";
  else if (objdir2->equals("bsd\\debug") == true)
    objdir2 = "bsd-gcc";
  props->setStringVal("OBJDIR", objdir + objdir2, PropsParentRead);
  //RStringArray ctypes = _moduleProps->getStringArrayVal("COMPILE_TYPE", false);
  

  //if (ctypes->length() == 0)
  if (props->containsInStringArrayVal("COMPILE_TYPE", "STATIC", PropsParentRead) == false &&
      props->containsInStringArrayVal("COMPILE_TYPE", "SHARED", PropsParentRead) == false)
  {
      props->appendStringArrayVal("COMPILE_TYPE", "SHARED", PropsParentRead | PropsNoParentWrite | PropsNoStringDups);
  }
  if (props->containsInStringArrayVal("COMPILE_TYPE", "BOEHMGC", PropsParentRead) == true)
  {
    RTask bcfg = TaskManager::getTask("amake_boehmgc_cfg");
    if (bcfg == Nil)
    {
      ACDK_NLOG("acdk.make", Error, "Cannot found amake_boehmgc_cfg task (should be defined in ACDK_TOOLS_HOME/amake_config.csf");
    }
    else
    {
      _moduleProps->addParentProps(props);
      bcfg->doExecute("", _moduleProps);
      _moduleProps->removeParentProps(props);
    }
  }
  if (_moduleConfigTasks->length() > 0) 
  {
    _moduleProps->addParentProps(props);
    _moduleProps->setObjectVal("task", this);
    for (int i = 0; i < _moduleConfigTasks->length(); ++i)
    {
      _moduleConfigTasks[i]->doExecute("", _moduleProps, TaskExecuteForce);
    }
    _moduleProps->removeParentProps(Nil);
    _moduleProps->unset("task");
  }
  RTask task = (RTask)props->getObjectVal("AcdkUnitConfigTask", PropsParentRead);
  task->doExecute("", props);
  props->merge(_moduleProps);
  RStringArray ctypes = props->getStringArrayVal("COMPILE_TYPE", PropsParentRead);

  bool changed = false;
  {
    for (int i = 0; i < ctypes->length(); ++i)
    {
      changed |= props->importNameSpace(ctypes[i]);
    }
  }
  

  RString acdkhome = props->getAcdkHome("ACDKHOME");
  RString dirsep = props->getStringVal("DIRSEP", PropsParentRead | PropsWarnRead);
  
  // set inlcude acdk_package/src
  RString tincl = acdk::io::File::getCWD() + dirsep + "src";
  props->setStringVal(".IncludeFile", tincl, 0);
  RString includes = props->eval(props->getStringVal("CCC_IncludeFileMask", PropsParentRead), 
                                  PropsEvalQuoteFileNameArgs | PropsEvalRecursive | PropsParentRead);
  props->setStringVal("CCC_ACDK_INCLUDES", includes, PropsNoFlags);
  props->appendStringArrayVal("CCC_INCLUDE_LIST", tincl, PropsParentRead | PropsNoParentWrite);
  
  // set ACDKHOME/include  
  tincl = acdkhome + dirsep + "include";
  props->setStringVal(".IncludeFile", tincl, 0);
  includes = props->eval(props->getStringVal("CCC_IncludeFileMask", PropsParentRead), 
                                  PropsEvalQuoteFileNameArgs | PropsEvalRecursive | PropsParentRead);
  props->appendStringVal("CCC_ACDK_INCLUDES", includes, " ", PropsNoFlags);
  props->appendStringArrayVal("CCC_INCLUDE_LIST", tincl, PropsParentRead | PropsNoParentWrite);
  
  RStringArray ackdliblist = props->getStringArrayVal("ACDKLIBS_LIST", PropsParentRead);
  props->setStringVal(".LinkPath", acdkhome + dirsep + "bin", PropsNoFlags);

  props->setStringVal("CCC_ACDK_LIBS", props->eval("${CCC_LinkPathMask}", PropsEvalRecursive | PropsParentRead), PropsNoFlags);
  
  //System::out->println(props->eval("1 CCC_ACDK_DEFINES: ${CCC_ACDK_DEFINES}"));
  for (int i = ackdliblist->length() - 1; i >= 0; --i)
  {
    RString alib = ackdliblist[i];
    RString adef = "USE_" + alib->toUpperCase() + "_LIB";
    props->setStringVal(".DefineToken", adef, PropsNoFlags);
    RString terg = props->eval("${CCC_DefineMask}", PropsEvalRecursive | PropsParentRead);
    props->appendStringVal("CCC_ACDK_DEFINES", terg, " ", PropsParentRead | PropsNoParentWrite);
    
    RString libn = alib;
    props->setStringVal(".LibFile", libn, PropsNoFlags);
    props->appendStringVal("CCC_ACDK_LIBS", props->eval("${CCC_LinkAcdkLibMask}", PropsEvalRecursive | PropsParentRead), " ", PropsParentRead | PropsNoParentWrite);
  }
  //System::out->println(props->eval("2 CCC_ACDK_DEFINES: ${CCC_ACDK_DEFINES}"));
  props->setStringVal(".DefineToken", "IN_" + _name->toUpperCase() + "_LIB", PropsNoFlags);
  RString terg = props->eval("${CCC_DefineMask}", PropsEvalRecursive | PropsParentRead);
  props->appendStringVal("CCC_ACDK_DEFINES", terg, " ", PropsParentRead | PropsNoParentWrite);
  //System::out->println(props->eval("3 CCC_ACDK_DEFINES: ${CCC_ACDK_DEFINES}"));
  props->setStringVal(".BinUnitName", _name, PropsNoParentWrite);

  if (props->hasValue("BINDIR", PropsParentRead) == false)
  {
    //System::out->println("Set BINDIR: " + props->getStringVal("ACDKHOME"));
    props->setStringVal("BINDIR", props->eval("${ACDKHOME}${DIRSEP}bin", PropsEvalRecursive | PropsParentRead), PropsNoFlags);
  }
  props->setStringVal("GENMAKE_PROJECTNAME", _name, PropsNoFlags);
  props->setStringVal("GENMAKE_OUTDIR", props->eval("${BINDIR}${DIRSEP}", PropsEvalRecursive | PropsParentRead), PropsNoFlags);
  props->setStringVal("GENMAKE_OBJDIR", props->getStringVal("OBJDIR", PropsParentRead), PropsNoFlags);

  {
    RStringArray defines = props->getStringArrayVal("CCC_EXT_DEFINES_LIST", PropsParentRead);
    Props tprops("", PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, props);
    for (int i = 0; i < defines->length(); ++i)
    {
      tprops.setStringVal(".DefineToken", defines[i]);
      props->appendStringVal("CCC_EXT_DEFINES", tprops.eval("${CCC_DefineMask}", PropsEvalRecursive), " ");
    }
  }
  {
    RStringArray includes = props->getStringArrayVal("CCC_EXT_INCLUDE_LIST", PropsParentRead);
    Props tprops("",  PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, props);
    for (int i = 0; i < includes->length(); ++i)
    {
      props->setStringVal(".IncludeFile", includes[i], PropsNoFlags);
      RString include = props->eval(props->getStringVal("CCC_IncludeFileMask", PropsParentRead), 
                                  PropsEvalQuoteFileNameArgs | PropsEvalRecursive | PropsParentRead);
      props->appendStringVal("CCC_EXT_INCLUDES", include, " ", PropsParentRead | PropsNoParentWrite);
      props->appendStringArrayVal("CCC_INCLUDE_LIST", includes[i], PropsParentRead | PropsNoParentWrite);
    }
  }
  {
    RStringArray includes = props->getStringArrayVal("CCC_EXT_LIBPATH_LIST", PropsParentRead);
    Props tprops("", PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, props);
    for (int i = 0; i < includes->length(); ++i)
    {
      props->setStringVal(".LinkPath", includes[i], PropsNoFlags);
      RString include = props->eval(props->getStringVal("CCC_LinkPathMask", PropsParentRead), 
                                  PropsEvalQuoteFileNameArgs | PropsEvalRecursive | PropsParentRead );
      props->appendStringVal("CCC_EXT_LDFLAGS", include, " ", PropsParentRead | PropsNoParentWrite);
    }
    
  }
  if (_defFile != Nil)
  {
    Props tprops("", PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, props);
    tprops.setStringVal(".DefFile", _defFile);
    RString defmask = tprops.eval("${CCC_LINK_WIN_DEF_MASK}",  PropsEvalQuoteFileNameArgs | PropsEvalRecursive | PropsParentRead);
    props->setStringVal("CCC_WIN_DEF_LDFLAGS", defmask);
  }
  //props->setStringVal("GENMAKE_LDFLAGS", props->getStringVal("CCC_LDFLAGS"));
}

//static
RString
getObjectFile(IN(RProps) props, IN(RString) source, IN(RString) objext)
{
  ::acdk::io::File tf(source);
  RString fname = tf.getName();
  if (fname->endsWith(".cpp") == true)
    fname = fname->substr(0, fname->length() - 4) + objext;
  else if (fname->endsWith(".c") == true)
    fname = fname->substr(0, fname->length() - 2) + objext;
  return fname;
}

#if defined(ACDK_OS_WIN32)
# define SHELLEXECLFAGS 0 //UseShell | UseFileRedirect
#else
# define SHELLEXECLFAGS 0
#endif

//virtual 
bool 
AcdkBinaryTask::execute(IN(RString) exec, IN(RProps) props)
{
  return _execute(exec, props, true, true);
}

RString join(IN(RStringArray) sa, IN(RString) delim)
{
  StringBuffer sb;
  for (int i = 0; i < sa->length(); ++i)
  {
    if (i > 0)
      sb.append(delim);
    sb.append(sa[i]);
  }
  return sb.toString();
}

void appendEnvPath(IN(RProps) props, IN(RString) key, IN(RString) val)
{
  if (props->hasValue(key) == true)
  {
    props->setStringVal(key, props->eval(val + "${SHELL_ENV_PATH_DIVIDER}" + props->getStringVal(key) ));
  }
  else
    props->setStringVal(key, val);
}

bool 
AcdkBinaryTask::_execute(IN(RString) exec, IN(RProps) props, bool executeParents, bool initialize)
{

  
  if (executeParents == true)
  {
    if (AbstractTask::execute(exec, props) == false)
      return false;
  }
  //_taskProps->setParentProps(props);
  RProps cprops = new Props(_name, PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, _taskProps);
  cprops->addParentProps(props);
  if (initialize == true)
    setupAcdkDeps(cprops);
  RString dirsep = props->getStringVal("DIRSEP", PropsParentRead | PropsWarnRead);
  
  ACDK_NLOG("acdk.make", Info, "execute: " + exec + " " + _name + " " + join(props->getStringArrayVal("COMPILE_TYPE", PropsParentRead), " "));

  if (exec->equals("help") == true)
  {
    System::out->println(
      "help       print this help\n"
      "compile    Compile sources and link binary\n"
      "build      metainfo and compile\n"
      "clean      remove object and executable files\n"
      "install    copy sources into ACDKHOME/include directory\n"
      "metainfo   update metainfo if needed\n"
      "fmetainfo  force update metainfo\n"
      "all        build and install\n"
      "rebuild    clean and all\n"
      "test       executes program\n"
    );
  }
// rebuild
  else if (exec->equals("rebuild") == true)
  {
    if (_execute("clean", cprops, false, false) == false)
      return false;
    return _execute("all", cprops, false, false);
  }
// all
  else if (exec->equals("all") == true)
  {
    if (_execute("build", cprops, false, false) == false)
      return false;
    return _execute("install", cprops, false, false);
  }
// build
  else if (exec->equals("build") == true)
  {
    if (_execute("install", cprops, false, false) == false)
      return false;
    if (_execute("metainfo", cprops, false, false) == false)
      return false;
    return _execute("compile", cprops, false, false);
  }
// metainfo
  else if (exec->equals("metainfo") == true || exec->equals("fmetainfo") == true)
  {
    bool force = false;
    if (exec->equals("fmetainfo") == true)
      force = true;

    if (cprops->hasValue("ACDKMC", PropsParentRead) == false)
    {
      ACDK_NLOG("acdk.make", Error, "AcdkBinaryTask: ACDKMC is not defined");
      return true;
    }
    for (int i = 0; i < _modules->length(); ++i)
    {
      RString module = _modules[i];
      if (force  == false)
      {
        File modfile(module);
        RString clazzfile = module + dirsep + File(module).getName() + "_metainf_base.cpp";
        if (FileDependTask(FileSet(module + dirsep + "*.h")
                                  .getFiles(), clazzfile)
                           .execute(Nil, props) == true)
          continue;
      }
      RString cmdline = cprops->getStringVal("ACDKMC", PropsParentRead) + " " + module;
      ACDK_NLOG("acdk.make", Info, "AcdkBinaryTask: execute: " + cmdline);

      ShellExecuteTask shelex("link", "Link binary", cmdline, SHELLEXECLFAGS);
      if (shelex.doExecute("exec", props) == false)
      {
        ACDK_NLOG("acdk.make", Error, "AcdkBinaryTask: ACDKMC failed on module " + module);
        return false;
      }
    }
    return true;
  }

// clean
  else if (exec->equals("clean") == true) 
  {
    RStringArray sources = getSourceSet(cprops)->getFiles();
    CompileTask compiletask;
    RString objext = cprops->getStringVal("CCC_OBJECT_FILE_SUFFIX", PropsParentRead);
    RStringArray objectFiles = new StringArray(0);
    for (int i = 0; i < sources->length(); ++i)
    {
      RString fname = getObjectFile(cprops, sources[i], objext);
      fname = cprops->eval("${OBJDIR}${DIRSEP}", PropsEvalRecursive | PropsParentRead)  + fname;
      FileDeleteTask fdt(fname);
      if (fdt.execute(Nil, cprops) == false)
        return false;
    }
    RString bintarget = cprops->eval("${BINDIR}${DIRSEP}${CCC_OUTFILE_NAME}", PropsEvalRecursive | PropsParentRead);
    if (FileDeleteTask(bintarget).execute(Nil, cprops) == false)
      return false;
    return true;
  } 

// compile
  else if (exec->equals("compile") == true) 
  {

    RStringArray sources = getSourceSet(cprops)->getFiles();
    
    RString objext = props->getStringVal("CCC_OBJECT_FILE_SUFFIX", PropsParentRead);
    RStringArray objectFiles = new StringArray(0);
    int jobjcount = 1;
    if (props->hasValue("AMAKE_JOBCOUNT", PropsParentRead) == true)
    {
      jobjcount = props->getIntVal("AMAKE_JOBCOUNT", PropsParentRead);
    }
    JobExecuterTask jt(jobjcount, TaskManager::noFailOnBreak() == false);
    for (int i = 0; i < sources->length(); ++i)
    {
      
      RProps compileprops = new Props("", PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, cprops);
      compileprops->setStringVal("SOURCEFILE", sources[i], PropsNoFlags);
      RString fname = getObjectFile(cprops, sources[i], objext);
      compileprops->setStringVal("OBJFILE", fname, PropsNoFlags);
      
      objectFiles->append(cprops->eval("${OBJDIR}${DIRSEP}" + fname, PropsEvalRecursive | PropsParentRead));
      RCompileTask compiletask = new CompileTask();
      jt.addTask(&compiletask, "compile", compileprops);
      //if (compiletask.doExecute("compile", compileprops) == false)
      //  return false;
    }
    if (jt.execute(Nil, props) == false)
    {
      return false;
    }
// link here
    {
      RStringArray libfiles = new StringArray(0);
      {
        Props tprops("", PropsParentRead | PropsNoWarnRead, cprops);
        RStringArray libs = _moduleProps->getStringArrayVal("ACDKLIBS_LIST"); 
        //::acdk::util::Arrays::append(libs, ); 
        for (int  i = 0; i < libs->length(); ++i)
        {
          RString libname = libs[i];
          tprops.setStringVal(".LibFile", libname, PropsNoFlags);
          RString libfile = tprops.eval("${BINDIR}${DIRSEP}${CCC_LinkAcdkLibMask}", PropsEvalRecursive | PropsParentRead);
          libfiles->append(libfile);
        }
      }
      {
        RStringArray libs = _moduleProps->getStringArrayVal("CCC_EXT_LIBARIES_LIST");
        Props tprops("", PropsParentRead | PropsNoWarnRead, cprops);
        for (int i = 0; i < libs->length(); ++i)
        {
          RString libname = libs[i];
          tprops.setStringVal(".LibFile", libname, PropsNoFlags);
          cprops->appendStringVal("CCC_PROJECT_LIBS", tprops.eval("${CCC_LinkLibMask}", PropsEvalRecursive | PropsParentRead), " ", PropsParentRead);
          RString t = cprops->getStringVal("CCC_PROJECT_LIBS");
        }
      }
              

      
      RProps linkprops = new Props("", PropsParentRead | PropsNoWarnRead, &cprops);
      linkprops->setStringArrayVal("OBJECTFILES", objectFiles, PropsNoFlags);
      
      ::acdk::io::File tf(linkprops->getStringVal("BINDIR", PropsParentRead));
      DirExistsTask dexists(&tf);
      dexists.execute(".", linkprops);
      
      RString bintarget = linkprops->eval("${BINDIR}${DIRSEP}${CCC_OUTFILE_NAME}", PropsEvalRecursive | PropsParentRead);
      FileDependTask fdeps(objectFiles, bintarget);
      FileDependTask fdeps2(libfiles, bintarget);
      if (fdeps.execute("", &cprops) == true && fdeps2.execute("", &cprops) == true)
      {
        ACDK_NLOG("acdk.make", Trace, "AcdkBinaryTask::execute: target is up to date: " + bintarget);
        return true;
      }
      RString linkcmd;
      if (cprops->containsInStringArrayVal("COMPILE_TYPE", "LIB", PropsParentRead) == true && 
          cprops->containsInStringArrayVal("COMPILE_TYPE", "STATIC", PropsParentRead) == true && 
          cprops->containsInStringArrayVal("COMPILE_TYPE", "EXE", PropsParentRead) == false)
      {
        linkcmd = linkprops->getEvaluatedStringVal("CCC_AR_MASK", PropsEvalRecursive | PropsParentRead);
      } 
      else
      {
        linkcmd = linkprops->getEvaluatedStringVal("CCC_LINK_MASK", PropsEvalRecursive | PropsParentRead);
      }
      
      ACDK_NLOG("acdk.make", Info, linkcmd);
      ShellExecuteTask shelex("link", "Link binary", linkcmd, SHELLEXECLFAGS);
      return shelex.doExecute("exec", props);
    }

  } 
// install
  else if (exec->equals("install") == true) 
  {
    RString include = props->getAcdkHome() + dirsep + "include";
    
    if (FileCopyTask(getHeaderSet(cprops), include, _baseSourceDir, FileCopyOnlyNewer | FileCopyWithAttributes).execute(Nil, props) == false)
      return false;
    if (FileCopyTask(getSourceSet(cprops), include, _baseSourceDir, FileCopyOnlyNewer | FileCopyWithAttributes).execute(Nil, props) == false)
      return false;
    
    return true;
  }
// test
  else if (exec->equals("test") == true)
  {
    if (props->containsInStringArrayVal("COMPILE_TYPE", "LIB", PropsParentRead) == true)
      return true;
    
    Props execprops("", PropsParentRead | PropsNoParentWrite | PropsNoWarnRead, cprops);
    appendEnvPath(&execprops, "LD_LIBRARY_PATH",  cprops->eval("${BINDIR}", PropsEvalRecursive));
    appendEnvPath(&execprops, "PATH",  cprops->eval("${BINDIR}", PropsEvalRecursive)); // needed by windows

    execprops.appendStringArrayVal("SHELL_EXPORT_ENV_LIST", "LD_LIBRARY_PATH");
    RStringArray additionalMods = _moduleProps->getStringArrayVal("CCC_EXT_LIBPATH_LIST");
    for (int i = 0; i < additionalMods->length(); ++i)
    {
      appendEnvPath(&execprops, "LD_LIBRARY_PATH",  additionalMods[i]);
      appendEnvPath(&execprops, "PATH",  additionalMods[i]);
    }
    execprops.setStringVal("ACDK_TOOLS_HOME",  cprops->eval("${ACDKHOME}", PropsEvalRecursive)); // needed by windows
    execprops.appendStringArrayVal("SHELL_EXPORT_ENV_LIST", "ACDK_TOOLS_HOME");

    RString bintarget = cprops->eval("${BINDIR}${DIRSEP}${CCC_OUTFILE_NAME}", PropsEvalRecursive | PropsParentRead);
    bintarget = bintarget + " -test-htmlreport";
    ShellExecuteTask shelex("test", "Test binary", bintarget, SHELLEXECLFAGS);
    if (shelex.doExecute("exec", &execprops) == false)
    {
      ACDK_NLOG("acdk.make", Error, "AcdkBinaryTask: test failed with " + bintarget);
      //return false;
      return true;
    }
    return true;
  }
  else if (exec->equals("genmake") == true)
  {
    return _genMake(cprops);
  }
  else 
  {
    ACDK_NLOG("acdk.make", Error, "AcdkBinaryTask::execute: unknown exec: " + exec);
    execute("help", props);
  }
  
  return false;
}

void 
AcdkBinaryTask::addCompileType(IN(RString) type)
{
  RStringArray sa = _moduleProps->getStringArrayVal("COMPILE_TYPE");
  if (type->equals("RELEASE") == true)
  {
    acdk::util::Arrays::removeFirstElement(sa, RString("DEBUG"));
  } 
  else if (type->equals("DEBUG") == true)
  {
    acdk::util::Arrays::removeFirstElement(sa, RString("RELEASE"));
  }
  else if (type->equals("STATIC") == true)
  {
    acdk::util::Arrays::removeFirstElement(sa, RString("SHARED"));
  }
  else if (type->equals("SHARED") == true)
  {
    acdk::util::Arrays::removeFirstElement(sa, RString("STATIC"));
  }
  //_moduleProps->setStringArrayVal("COMPILE_TYPE", sa);
  _moduleProps->appendStringArrayVal("COMPILE_TYPE", type, PropsParentRead | PropsNoParentWrite | PropsNoStringDups);

}

bool 
AcdkBinaryTask::_genMake(IN(RProps) props)
{
  _genMake(props, "dsp");
  
  //kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_text_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_boot_r.dll" ""

  return true;
}

//foreign 
bool 
AcdkBinaryTask::_genMake(IN(RProps) props, IN(RString) target)
{
  Props cprops("", PropsParentRead, props);
  RString fname = props->eval("${ACDK_TOOLS_HOME}${DIRSEP}acdk_make/cfg/lisp/targets/" + target + "/Makefile.template", PropsParentRead);
  File tf(fname);
  if (tf.exists() == false)
  {
    ACDK_NLOG("acdk.make", Error, "Cannot find template file: " + fname);
    return false;
  }
  acdk::io::FileReader fr(fname);
  RString cont = acdk::io::ByteToCharReader(&fr, 
                                             acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()).readString();
  //RString cont = rin.readIntoString();
  cprops.setObjectVal("ctask", this, PropsNoFlags);
  RString str = cprops.getStringVal("GENMAKE_PROJECTNAME", PropsParentRead);
  RString erg = cprops.eval(cont, PropsParentRead);
  RString outfname = _name + "." + target;
  acdk::io::FileWriter fout(outfname);
  acdk::io::CharToByteWriter cout(&fout);
  cout.writeString(erg);
  return true;
}

RString 
AcdkBinaryTask::getDspSources(IN(RProps) props)
{
  StringBuffer sb;
  RStringArray sources = getSourceSet(props)->getFiles();
  ::acdk::util::Arrays::sort(sources);
  RString ldir = "";
  RStringArray lparts = new StringArray(0);
  int i;
  for (i = 0; i < sources->length(); ++i)
  {
    RString fn = sources[0];
    File tf(fn);
    RString cdir = tf.getParent();
    RString fname = tf.getName();
    if (cdir->equals(ldir) == false)
    {
      RString cp = cdir->substr(File::getCWD()->length() + 1);
      if (cp->startsWith("src") == true)
        cp = cp->substr(4);
      else if (cp->startsWith("tests") == true)
        cp = cp->substr(6);
      RStringArray nparts = acdk::util::StringTokenizer(cp, File::separator()).allToken();
      int j  = lparts->length() - 1;
      for (; j >= 0; --j)
      {
        if (nparts->length() < j - 1)
        {
          sb.append("# end Group\n");
        } 
        else if (nparts[j]->equals(lparts[j]) == false)
        { 
          sb.append("# end Group\n");
        }
      }
      j += 1;
      for (int k = j; k < nparts->length(); ++k)
      {
        sb.append("# Begin Group \"" + nparts[k] + "\"\n# PROP Default_Filter \"\"\n");
      }
      lparts = nparts;
      ldir = cdir;
    }
    sb.append("\n# Begin Source File\nSOURCE=" + fn + "\n# End Source File\n");
  }
  for (i = 0; i < lparts->length(); ++i)
  {
    sb.append("# end Group\n");
  }
  return sb.toString();
}

} // namespace make
} // namespace acdk







