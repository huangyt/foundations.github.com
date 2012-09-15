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

#ifndef acdk_make_FileSet_h
#define acdk_make_FileSet_h

#include "Task.h"

namespace acdk {
namespace make {




ACDK_DECL_CLASS(FileSet);
/**
  Creates a file set of given globbin pattern specs
*/
class ACDK_ACDK_MAKE_PUBLIC FileSet
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(FileSet)
public:
  RStringArray _fileSpecs;
  FileSet()
  : _fileSpecs(new StringArray(0))
  {
  }

  FileSet(IN(RString) spec)
  : _fileSpecs(new StringArray(1))
  {
    _fileSpecs[0] = spec;
  }
  FileSet(IN(RStringArray) specs)
  : _fileSpecs(specs)
  {
  }
  void addSpec(IN(RString) spec)
  {
    _fileSpecs->append(spec);
  }
  /**
    returns the the files
  */
  RStringArray getFiles();
  /**
    return a file set constructed by filesOrDirs with given file extensions
    @param alwaysAddFiles if filesOrDir is a file and alwaysAddFiles == true always
           add to file set, independed the file doesn't have the extension
  */
  static RFileSet createFileSpecs(IN(RStringArray) filesOrDirs, IN(RStringArray) filematchpattern, bool alwaysAddFiles = true);
private:
  void expand(IN(RString) spec, IN(RStringArray) erg);
  bool isPattern(IN(RString) spec);
  void expandPattern(IN(RString) pattern, IN(RStringArray) erg);
  void collectFiles(IN(acdk::io::RFile) dir, IN(acdk::io::RFilenameFilter) filter, IN(RStringArray) flist);
};



} // namespace make
} // namespace acdk


#endif //acdk_make_FileSet_h


