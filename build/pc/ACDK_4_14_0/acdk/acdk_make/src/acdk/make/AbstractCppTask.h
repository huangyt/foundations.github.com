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

#ifndef acdk_make_AbstractCppTask_h
#define acdk_make_AbstractCppTask_h

#include "AbstractTask.h"
#include "FileSet.h"

#include <acdk/util/Arrays.h>
#include <acdk/cfgscript/Props.h>

namespace acdk {
namespace make {

ACDK_DECL_CLASS(AbstractCppTask);

/**
  Base class for compiling cpp files
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC AbstractCppTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(AbstractCppTask)
public:
  /**
    unexpanded sources and directories
  */
  RStringArray _sourceAndHeader;
  //RString _baseDir;
  RTaskArray _moduleConfigTasks;
  RString _defFile;
  
  /*
  RFileSet _sources;
  RFileSet _headers;
  
  
  bool _initialized;
  /// relative base directory where hierarchy is starting
  RString _baseSourceDir;
  /// only win32 .def file
  */
  AbstractCppTask(IN(RString) name, IN(RString) desc);

  RTaskInfo getTaskInfo() 
  {
    return new TaskInfo(_name, _name, _description, _description + ": " + _name, ::acdk::util::logging::Info);
  }
  
  void addSource(IN(RString) source);

  void addSources(IN(RStringArray) sources)
  {
    for (int i = 0; i < sources->length(); ++i)
      addSource(sources[i]);
  }
  void addLib(IN(RString) lib);
  void addIncludeDir(IN(RString) dir);
  void addDefine(IN(RString) key, IN(RString) val = "");
  void addLibraryPath(IN(RString) path);
  /**
    return the file specifications for sources
  */
  RFileSet getSourceSet(IN(RProps) props);
  RFileSet getHeaderSet(IN(RProps) props);
  /**
    Possible targets are:
    DEBUG / RELEASE
    SHARED / STATIC
    SO / STATIC
  */
  void addCompileType(IN(RString) type);
  virtual bool execute(IN(RString) exec, IN(RProps) props);
  
  /**
    @param short directory names or file names
    @return the source file names with path names
  */
  static RStringArray expandSources(IN(RStringArray) sa);
  
  /**
    returns debug etc.
  */
  RString getReleaseTag(IN(RProps) props);
  /**
    basedir is used to specify base dir for object
    files.
  */
  void setBaseDir(IN(RString) basedir);
  void setObjBaseDir(IN(RString) basedir) { setBaseDir(basedir); }
  //void setBaseSourceDir(IN(RString) basedir) { _baseSourceDir = basedir; }
  //RString getDspSources(IN(RProps) props);

  /**
    Will overwritten by lib and exe
  */
  //virtual void setupAcdkDeps(IN(RProps) props);
  void addModuleConfigTask(IN(RString) taskname);
  RString getCompileCommandLine(IN(RProps) props);
  virtual void setupDeps(IN(RProps) props);

protected:
  foreign bool _execute(IN(RString) exec, IN(RProps) props, bool executeParents, bool initialize);
  /*
  
  foreign bool _genMake(IN(RProps) props);
  foreign bool _genMake(IN(RProps) props, IN(RString) target);
  */
};



} // namespace make
} // namespace acdk


#endif //acdk_make_AbstractCppTask_h
