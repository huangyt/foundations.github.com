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

#include "LookupFileTask.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace make {

using namespace acdk::cfgscript;

LookupFileTask::LookupFileTask(LookupFileType ft)
: AbstractTask("locate", "", "try to locate file")
, _filenames(new Props(PropsParentRead | PropsNoParentWrite | PropsNoWarnRead))
, _fileType(ft)
, _suggestDirs(new Props(PropsParentRead | PropsNoParentWrite | PropsNoWarnRead))
, _foundFiles(new StringArray(0))
, _foundBaseFiles(new StringArray(0))
{

}

LookupFileTask::LookupFileTask(IN(RString) filename, LookupFileType ft, IN(RStringArray) suggestPaths)
: AbstractTask("locate", filename, "try to locate file")
, _filenames(new Props(PropsParentRead | PropsNoParentWrite | PropsNoWarnRead))
, _fileType(ft)
, _suggestDirs(new Props(PropsParentRead | PropsNoParentWrite | PropsNoWarnRead))
, _foundFiles(new StringArray(0))
, _foundBaseFiles(new StringArray(0))
{
  _filenames->appendStringArrayVal("generic", filename);
  if (suggestPaths != Nil)
  {
    for (int i = 0; i < suggestPaths->length(); ++i)
      _suggestDirs->appendStringArrayVal("generic", suggestPaths[i]);
  }
}

RString 
LookupFileTask::fileTypeAsString()
{
  switch(_fileType)
  {
   case Executable: return "Executable";
   case AcdkExecutable: return "AcdkExecutable";
   case StaticLibrary: return "StaticLibrary";
   case SharedLibrary: return "SharedLibrary";
   case Header: return "Header";
   case PerlUnit: return "PerlUnit";
   case NormalFile: return "NormalFile";
   default: return "UnknownFileType";
  }
}

bool 
LookupFileTask::lookupFileInSuggestedDir(IN(RProps) props, IN(RString) dir, IN(RString) name)
{
  RString fname = name;
  switch(_fileType)
  {
  case Executable:
#ifdef ACDK_OS_WIN32
    if (name->endsWith(".exe") == false && name->endsWith(".EXE") == false)
      fname = name + ".exe";
#endif
    break;
  case StaticLibrary:
#if defined(ACDK_OS_WIN32)
    if (name->endsWith(".lib") == false && name->endsWith(".LIB") == false)
      fname = name + ".lib";
#elif defined(ACDK_OS_UNIX)
    if (name->endsWith(".a") == false )
      fname = name + ".a";
    if (fname->startsWith("lib") == false )
      fname = "lib" + fname;
#endif
    break;
  case SharedLibrary:
#if defined(ACDK_OS_WIN32)
    if (name->endsWith(".dll") == false && name->endsWith(".DLL") == false)
      fname = name + ".dll";
#elif defined(ACDK_OS_UNIX)
    if (name->endsWith(".so") == false )
      fname = name + ".so";
    if (name->startsWith("lib") == false )
      fname = "lib" + fname;
#endif
    break;
  case ImportLibrary:
#if defined(ACDK_OS_WIN32)
    if (name->endsWith(".lib") == false && name->endsWith(".LIB") == false)
      fname = name + ".lib";
#elif defined(ACDK_OS_UNIX)
    if (name->endsWith(".so") == false )
      fname = name + ".so";
    if (name->startsWith("lib") == false )
      fname = "lib" + fname;
#endif
    break;
  default:
    break;
  }
  ::acdk::io::File tf(dir, fname);
  if (tf.exists() == true)
  {
    RString s = tf.getCanonicalPath();
    ACDK_NLOG("acdk.make", Trace, "LooupFileTask: File exists: " + s);
    props->setStringVal("_located", s, PropsNoFlags);
    return true;
  }
  RString s = tf.getCanonicalPath();
  ACDK_NLOG("acdk.make", Trace, "LooupFileTask: File doesn't exists: " + s);
  return false;
}

bool 
LookupFileTask::lookupFileInSuggestedDirs(IN(RProps) props, IN(RStringArray) dirs, IN(RString) fname)
{
  bool found = false;
  for (int i = 0; i < dirs->length(); ++i)
  {
     if (lookupFileInSuggestedDir(props, dirs[i], fname) == true)
       found = true;
  }
  return found;
}

bool 
LookupFileTask::lookupFileInSuggestedDirs(IN(RProps) props, IN(RString) fname)
{
  RStringArray ptags = props->getStringArrayVal("AMAKE_TARGET_TAGS", PropsParentRead);
  bool found = false;
  for (int i = 0; i < ptags->length(); ++i)
  {
    RString tag = ptags[i];
    if (_suggestDirs->hasValue(tag, PropsParentRead) == true)
    {
      if (lookupFileInSuggestedDirs(props, _suggestDirs->getStringArrayVal(tag, PropsParentRead), fname) == true)
        found = true;
    }
  }
  return found;
}

RString 
LookupFileTask::foundParentDir()
{
  if (_foundFiles->length() == 0)
    return Nil;
  return acdk::io::File(_foundFiles[0]).getParent();
}

//virtual 
bool 
LookupFileTask::execute(IN(RString) exec, IN(RProps) props)
{
  RStringArray ptags = props->getStringArrayVal("AMAKE_TARGET_TAGS", PropsParentRead);
  bool found = false;
  if (ptags->length() == 0)
  {
    RString tag = "generic";
    RStringArray sa = _filenames->getStringArrayVal(tag, PropsParentRead);
    for (int j = 0; j < sa->length(); ++j)
    {
      if (_execute(exec, props, sa[j]) == true)
      {
        _foundFiles->append(props->getStringVal("_located", PropsNoFlags));
        _foundBaseFiles->append(sa[j]);
        found = true;
      }
    }
    return found;
  }
  for (int i = 0; i < ptags->length(); ++i)
  {
    RString tag = ptags[i];
    if (_filenames->hasValue(tag, PropsParentRead) == true || _filenames->hasValue("generic", PropsParentRead) == true)
    {
      if (_filenames->hasValue(tag, PropsParentRead) == false)
        tag = "generic";
      RStringArray sa = _filenames->getStringArrayVal(tag, PropsParentRead);
      for (int j = 0; j < sa->length(); ++j)
      {
        if (_execute(exec, props, sa[j]) == true)
        {
          _foundFiles->append(props->getStringVal("_located", PropsNoFlags));
          _foundBaseFiles->append(sa[j]);
          found = true;
        }
      }
    }
  }
  return found;
}

//virtual 
bool 
LookupFileTask::_execute(IN(RString) exec, IN(RProps) props, IN(RString) filename)
{
  //ACDK_NLOG("acdk.make", Error, SBSTR("LookupFileTask::_execute"));
  bool found = false;
  if (lookupFileInSuggestedDirs(props, filename) == true)
    found = true;

  switch(_fileType)
  {
    case AcdkExecutable:
    {
      
      //ACDK_NLOG("acdk.make", Warn, "ACDK_TOOLS_HOME=" + binp);
      RStringArray bal = props->getStringArrayVal("CCC_BUILDTYPE_APPENDIX_LIST", PropsParentRead);
      for (int i = 0; i < bal->length(); ++i)
      {
        RString ext = props->getStringVal("CCC_EXE_FILE_SUFFIX");
        //props->dump();
        RString fname = props->eval("${ACDK_TOOLS_HOME}${DIRSEP}bin${DIRSEP}" + filename + bal[i] + "${CCC_EXE_FILE_SUFFIX}", PropsParentRead);
        
        if (::acdk::io::File(fname).exists() == true)
        {
          props->setStringVal("_located", fname, PropsNoParentWrite);
          ACDK_NLOG("acdk.make", Trace, "LooupFileTask: File exists: " + fname);
          return true;
        }
        ACDK_NLOG("acdk.make", Trace, "LooupFileTask: File doesn't exists: " + fname);
      }
      break;
    }
    case Executable: 
    {
      RStringArray path = props->getStringArrayVal("ENV_PATH_LIST", PropsParentRead);
      //System::out->println(SBSTR("path length: " << path->length()));
      for (int i = 0; i < path->length(); ++i)
      {
        ACDK_NLOG("acdk.make", Debug, "Search in path: " + path[i]);
        if (lookupFileInSuggestedDir(props, path[i], filename) == true)
          found = true;
      }
      //ACDK_NLOG("acdk.make", Error, "Cannot locate " + fileTypeAsString() + ": " + _filename);
      break;
    }
    case StaticLibrary: 
    case ImportLibrary: 
    case SharedLibrary:
      // look in corresponding dirs
      break;
    default:
      ACDK_NLOG("acdk.make", Warn, "Unsupported locate filetype or cannot find file: " + fileTypeAsString());
      return false;
  }

  return found;
}



} // namespace make
} // namespace acdk

