
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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileAbstractImpl.cpp,v 1.12 2005/03/08 12:45:36 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Math.h>
#include "FileAbstractImpl.h"

#include <acdk/util/StringTokenizer.h>

namespace acdk {
namespace io {


//virtual 
RString 
FileAbstractImpl::getParent()
{
  RString canon = getCanonicalPath();
  if (canon == Nil)
    return String::emptyString();
  int idx = canon->lastIndexOf(File::separator());
  if (idx == -1)
    return Nil;
  return canon->substr(0, idx);
}

RFile 
FileAbstractImpl::getParentFile()
{
  RString pstr = getParent();
  if (pstr == Nil)
    return Nil;
  return new File(pstr);
}



//virtual 
RFile 
FileAbstractImpl::makeChild(IN(RString) subfile)
{
  return new File(concatNames(getPath(), subfile));
}

static
void
File_removeElement(ObjectArrayImpl<RString>& ar, int idx, int &len)
{
  while (idx < len - 1) {
    ar[idx] = ar[idx + 1];
    ++idx;
  }
  --len;
  ar[len] = Nil;
}


//virtual 
RString 
FileAbstractImpl::getCanonicalPath()
{
  
   RString abspath = getAbsolutePath();//separator()
#ifdef ACDK_OS_WIN32
  abspath = abspath->replace('/', '\\'); // to avoid misunderstandings
#endif
  
  if (abspath->length() > 1 && abspath->charAt(abspath->length() - 1) == File::separatorChar())
    abspath = abspath->substr(0, abspath->length() - 1);
  if (abspath->indexOf('.') == -1 || abspath->indexOf(File::separatorChar()) == -1)
    return abspath;
  
  acdk::util::StringTokenizer st(abspath, File::separator());

  // ### this is buggy for valid unix-paths with multiple slashes as upper//lower
  
  int countelements = abspath->elementCount(File::separatorChar()) - 1;
  if (countelements == 0)
    return abspath;
  
  StringArray ar(countelements + 2);
  int curidx = 0;
  if (abspath->charAt(0) == File::separatorChar())
    curidx = 1;
  while (st.hasMoreTokens() == true) {
    ar[curidx++] = st.nextToken();
  }
  
  curidx = 0;
  int alen = ar.length();
  int oldlen = alen;
  while (curidx < alen) {
    if (ar[curidx] != Nil && ar[curidx]->equals((RString)".")) {
      File_removeElement(ar, curidx, alen);
    } if (ar[curidx] != Nil && ar[curidx]->equals((RString)"..")) {
      if (curidx > 0) {
        File_removeElement(ar, curidx, alen);
        File_removeElement(ar, --curidx, alen);

      } else
        ++curidx;
    } else
      ++curidx;
  }
  
  if (oldlen == alen) {
    return abspath;
  }
  StringBuffer sb(abspath->length() + 1);
  int start=0;
  if (abspath->charAt(0) == File::separatorChar()) {
    start = 1;
    sb.append(File::separatorChar());
  }
  
  for (int i = start; i < alen; i++) {
    if (ar[i] != Nil)
      sb.append(ar[i]);
    if (i < alen - 1)
      sb.append(File::separatorChar());
  }
  return sb.toString();
}

//virtual 
RString 
FileAbstractImpl::getAbsolutePath()
{
  if (isAbsolute() == true)
    return _path;
  return File::getCWD()  + File::separator() + _path;
}

//virtual 
RString 
FileAbstractImpl::getName()
{
  RString canpath = getCanonicalPath();
  int idx = canpath->lastIndexOf(File::separator());
  int altidx = -1;
  if (File::separatorChar() == '\\')
    altidx = canpath->lastIndexOf('/');
  if (idx == -1 && altidx == -1)
    return canpath;
  if (altidx != -1 && idx != -1)
    idx = Math::max(altidx, idx);
  else if (idx == -1)
    idx = altidx;
  return canpath->substr(idx + 1);
}

//virtual 
bool 
FileAbstractImpl::isAbsolute()
{
  #if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
  return _path->startsWith(File::separator()) == true;
#endif
#ifdef ACDK_OS_WIN32
  if (_path->length() < 2)
    return false;
  if (_path->charAt(1) == ':')
    return true;
  // network
  return _path->charAt(0) == '\\' && _path->charAt(1) == '\\';
#endif //ACDK_OS_WIN32
}

//static
RString
FileAbstractImpl::concatNames(IN(RString) parent, IN(RString) child)
{
  if (parent == Nil)
    return child;
  if (child == Nil)
    return parent;
  if (parent->endsWith(File::separator()))
    return parent +  child;
  else
    return parent + File::separator() + child;
}



} // io
} // acdk





