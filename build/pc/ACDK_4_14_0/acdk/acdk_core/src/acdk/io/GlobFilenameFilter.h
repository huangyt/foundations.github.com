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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/GlobFilenameFilter.h,v 1.11 2005/02/05 10:44:54 kommer Exp $

#ifndef acdk_io_GlobFilenameFilter_h
#define acdk_io_GlobFilenameFilter_h

#include "FilenameFilter.h"
#include "FileFilter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(GlobFilenameFilter);

/** 
  Provides simply Globbing with pattern like '*.h' 
  API: Java Extended<br>
  Using fnmatch on unix system, and a similar (propably less powerfull) matching function on NT
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:44:54 $
  
*/

class ACDK_CORE_PUBLIC GlobFilenameFilter 
: extends acdk::lang::Object
, implements FilenameFilter
, implements FileFilter

{
  ACDK_WITH_METAINFO(GlobFilenameFilter)
private:
  RString _pattern;
public:
  GlobFilenameFilter(IN(RString) pattern)
  : _pattern(pattern)
  {
  }
  // implement from FilenameFilter
  foreign virtual bool accept(IN(RFile) dir, IN(RString) name);
  foreign virtual bool accept(IN(RFile) path);
protected:
  static bool match(const char* pattern, const char* fname);
#if !defined(HAS_FNMATCH_H)

  /** a simple fnmatch clone 
      @param pattern The Pattern like "*.h"
      @param string A String to test, like "HeaderFile.h" 
      return Returns true if matching
  */
  static bool mm_fnmatch(const char *pattern, const char *string);

  /** helper for mm_fnmatch */
  static const char* mm_rangematch(const char *pattern, char test);
#endif 
};

} // io
} // Lang

#endif //acdk_io_GlobFilenameFilter_h

