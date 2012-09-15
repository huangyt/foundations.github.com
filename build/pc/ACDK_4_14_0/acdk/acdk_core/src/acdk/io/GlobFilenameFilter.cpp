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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/GlobFilenameFilter.cpp,v 1.11 2005/02/05 10:44:54 kommer Exp $




#include <acdk.h>
#include "GlobFilenameFilter.h"
#include "File.h"

#if defined(HAS_FNMATCH_H)
#include "fnmatch.h"
#endif

namespace acdk {
namespace io {

using namespace acdk::lang;


//virtual 
bool 
GlobFilenameFilter::accept(IN(RFile) dir, IN(RString) name)
{
  RString npattern = _pattern->convert(CCAscii);
  RString nname = name->convert(CCAscii);
  return match(npattern->c_str(), nname->c_str());
}

//foreign virtual 
bool 
GlobFilenameFilter::accept(IN(RFile) path)
{
  RString npattern = _pattern->convert(CCAscii);
  RString nname = path->getName()->convert(CCAscii);
  return match(npattern->c_str(), nname->c_str());
}


#if !defined(HAS_FNMATCH_H)
//static
const char *
GlobFilenameFilter::mm_rangematch(const char *pattern, char test)
{
  char c, c2;
  int negate, ok;
  if ((negate = (*pattern == '!')) != 0)
    ++pattern;
  
  //TODO: quoting

  for (ok = 0; (c = *pattern++) != ']';) {
    if (c == 0)
      return 0;    /* illegal pattern */
    if (*pattern == '-' && (c2 = pattern[1]) != 0 && c2 != ']') {
      if (c <= test && test <= c2)
        ok = 1;
      pattern += 2;
    } else if (c == test)
      ok = 1;
  }
  return (const char *)(ok == negate ? 0 : pattern);
}

//static
bool
GlobFilenameFilter::mm_fnmatch(const char *pattern, const char *string)
{
  register char c;
  char test;
  
  for (;;) {
    switch (c = *pattern++) {
    case 0:
      return(*string == 0);
    case '?':
      if ((test = *string++) == 0)
        return false;
      break;
    case '*':
      c = *pattern;
      while (c == '*')
        c = *++pattern;
      if (c == 0) 
        return 1;
      while ((test = *string) != 0) {
        if (mm_fnmatch(pattern, string))
          return true;
        ++string;
      }
      return 0;
    case '[':
      if ((test = *string++) == 0 )
        return 0;
      if ((pattern = mm_rangematch(pattern, test)) == 0)
        return 0;
      break;
    case '\\':
      if ((c = *pattern++) == 0) {
        c = '\\';
        --pattern;
      }
      if (c != *string++)
        return false;
      break;
    default:
      if (c != *string++)
        return false;
      break;
    }
  }
}
#endif //!defined(HAS_FNMATCH_H)


//static
bool 
GlobFilenameFilter::match(const char* pattern, const char* fname)
{
#if defined(HAS_FNMATCH_H)
  return fnmatch(pattern, fname, 0) == 0;
#else
  return mm_fnmatch(pattern, fname) == true;
#endif
}

} // io
} // acdk
