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

#ifndef acdk_make_AcdkBinaryTask_h
#define acdk_make_AcdkBinaryTask_h

#include "AbstractTask.h"
#include "FileSet.h"

#include <acdk/util/Arrays.h>
#include <acdk/cfgscript/Props.h>

namespace acdk {
namespace make {

ACDK_DECL_CLASS(AcdkBinaryTask);

/**
  Creates a library or executable
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC AcdkBinaryTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(AcdkBinaryTask)
public:
  /**
    unexpanded sources and directories
  */
  RProps _moduleProps;
  RStringArray _modules;
  RFileSet _sources;
  RFileSet _headers;
  RStringArray _sourceAndHeader;
  bool _initialized;
  RString _baseDir;
  /// relative base directory where hierarchy is starting
  RString _baseSourceDir;
  RTaskArray _moduleConfigTasks;
  /// only win32 .def file
  RString _defFile;
  AcdkBinaryTask(IN(RString) name, IN(RString) desc);

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
  /** 
    directory contains a ACDK module
    with meta info.
    All *.h are added to headers
    All *.cpp are added to soruces
  */
  void addModule(IN(RString) modpath);
  void addModules(IN(RStringArray) mods)
  {
    for (int i = 0; i < mods->length(); ++i)
      addModule(mods[i]);
  }
  /**
    an acdk library has to following the standard 
    naming conventions of ACKD library.
    In case the library "my_package" will be added
    the sources of this binary will define USE_MY_PACKAGE_LIB
    will be defined
  */
  void addAcdkLib(IN(RString) lib)
  {
    _moduleProps->appendStringArrayVal("ACDKLIBS_LIST", lib, acdk::cfgscript::PropsParentRead | acdk::cfgscript::PropsNoWarnRead);
  }
  /**
    @see addAcdkLib
  */
  void addAcdkLibs(IN(RStringArray) libs)
  {
    RStringArray s = _moduleProps->getStringArrayVal("ACDKLIBS_LIST", acdk::cfgscript::PropsParentRead | acdk::cfgscript::PropsNoWarnRead);
    for (int i = 0; i < libs->length(); ++i)
      s->append(libs[i]);
  }
  RStringArray getAcdkLibs()
  {
    return _moduleProps->getStringArrayVal("ACDKLIBS_LIST", acdk::cfgscript::PropsParentRead | acdk::cfgscript::PropsNoWarnRead);
  }
  void addExtLib(IN(RString) lib);
  void addExtLibs(IN(RStringArray) libs);
  void addExtIncludeDir(IN(RString) dir);
  void addExtIncludeDirs(IN(RStringArray) dirs);
  void addDefine(IN(RString) key, IN(RString) val = "");
  void addLibraryPath(IN(RString) path);
  void addLibraryPaths(IN(RStringArray) paths);
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
  void setBaseSourceDir(IN(RString) basedir) { _baseSourceDir = basedir; }
  RString getDspSources(IN(RProps) props);

  /**
    Will overwritten by lib and exe
  */
  virtual void setupAcdkDeps(IN(RProps) props);
  void addModuleConfigTask(IN(RString) taskname);
protected:
  
  foreign bool _execute(IN(RString) exec, IN(RProps) props, bool executeParents, bool initialize);
  foreign bool _genMake(IN(RProps) props);
  foreign bool _genMake(IN(RProps) props, IN(RString) target);
};



} // namespace make
} // namespace acdk


#endif //acdk_make_AcdkBinaryTask_h
