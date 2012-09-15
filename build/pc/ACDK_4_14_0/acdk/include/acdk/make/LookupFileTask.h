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

#ifndef acdk_make_LookupFileTask_h
#define acdk_make_LookupFileTask_h

#include "AbstractTask.h"


namespace acdk {
namespace make {


enum LookupFileType
{
  NormalFile = 0,
  Executable = 1,
  StaticLibrary = 2,
  SharedLibrary = 3,
  Header = 4,
  PerlUnit = 5,
  AcdkExecutable = 6,
  ImportLibrary = 7
};
ACDK_DEF_LIB_ENUM(ACDK_ACDK_MAKE_PUBLIC, LookupFileType);

ACDK_DECL_CLASS(LookupFileTask);

/**
  Try to locate a file, executable, library, or header.
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC LookupFileTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(LookupFileTask)
public:
  RProps _filenames;
  LookupFileType _fileType;
  RProps _suggestDirs;
  RStringArray _foundFiles;
  RStringArray _foundBaseFiles;
  LookupFileTask(LookupFileType ft);
  LookupFileTask(IN(RString) filename, LookupFileType ft, IN(RStringArray) suggestPaths = Nil);
  void addFileName(IN(RString) platformtag, IN(RString) path)
  {
    _filenames->appendStringArrayVal(platformtag, path);
  }
  void addLookupPath(IN(RString) platformtag, IN(RString) path)
  {
    _suggestDirs->appendStringArrayVal(platformtag, path);
  }
  /**
    if file found, saves in "_located" prop
  */
  virtual bool execute(IN(RString) exec, IN(RProps) props);
  RString fileTypeAsString();
  bool lookupFileInSuggestedDirs(IN(RProps) props, IN(RStringArray) dirnames, IN(RString) fname);
  bool lookupFileInSuggestedDirs(IN(RProps) props, IN(RString) fname);
  bool lookupFileInSuggestedDir(IN(RProps) props, IN(RString) dir, IN(RString) name);

  /**
    returns found fully qualified path of the first found file
  */
  RString foundPath() { return _foundFiles->length() == 0 ? RString(Nil) : _foundFiles[0]; }
  RStringArray foundPathes() {  return _foundFiles; }
  /**
    returns the fully qualified parent directory name of the first found file
  */
  RString foundParentDir();
  /**
    return the base file name of first found file.
    In case of libraries without lib prefix and suffix.
  */
  RString foundFile() { return _foundBaseFiles->length() == 0 ? RString(Nil) : _foundBaseFiles[0]; }
  RStringArray foundFiles() { return _foundBaseFiles; }
private:
  foreign bool _execute(IN(RString) exec, IN(RProps) props, IN(RString) filename);
};


} // namespace make
} // namespace acdk


#endif //acdk_make_LookupFileTask_h
