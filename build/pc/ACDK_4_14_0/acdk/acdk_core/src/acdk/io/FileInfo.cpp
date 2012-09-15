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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileInfo.cpp,v 1.7 2005/02/21 16:54:58 kommer Exp $


#include "FileInfo.h"
#include "File.h"

#include <acdk/util/SysDate.h>

namespace acdk {
namespace io {


FileInfo::FileInfo(IN(RString) path)
  : flags(0)
  , size(0)
  , created(0)
  , modified(0)
  , digest(0)
  {
    File tf(path);
    dir = tf.getParent();
    name = tf.getName();
  }

RString 
FileInfo::getPath() 
{
  if (name == Nil || name->length() == 0)
    return dir;
  if (dir == Nil || dir->length() == 0)
    return name;
  if (dir->equals("/") == true)
    return dir + name;
  return dir + File::separator() + name;
}

//static 
RString 
FileInfo::flagString(int flags)
{
  StringBuffer sb;
  if (flags & FileInfoExists)
    sb.append("E");
  else
    sb.append("N");
  if (flags & FileInfoIsFile)
    sb.append("F");
  else if (flags & FileInfoIsDir)
    sb.append("D");
  if (flags & FileInfoCanRead)
    sb.append("R");
  if (flags & FileInfoCanWrite)
    sb.append("W");
  if (flags & FileInfoCanExec)
    sb.append("X");
  if (flags & FileInfoIsHidden)
    sb.append("H");
  if (flags & FileInfoHasABit)
    sb.append("A");
  return sb.toString();
}

//static 
RString 
FileInfo::timeToString(jlong time)
{
  return acdk::util::SysDate(time).toString();
}

RString 
FileInfo::toString()
{
  StringBuffer sb;
  sb.append("\""); 
  if (dir != Nil)
  {
    sb.append(dir); 
    sb.append("/"); 
  }
  sb.append(name); sb.append("\""); 
  sb.append(" "); sb.append(size); 
  sb.append(" "); sb.append(flagString(flags)); 
  sb.append(" "); sb.append(timeToString(created)); 
  sb.append(" "); sb.append(timeToString(modified)); 
  //sb.append(" "); sb.append(digest); 
  return sb.toString();
}

void 
FileInfo::dump(IN(::acdk::io::RPrintWriter) out)
{
  out->printQuoted(dir + "/" + name); 
  out->print(" "); out->print(size); 
  out->print(" "); out->print(flags); 
  out->print(" "); out->print(created); 
  out->print(" "); out->print(modified); 
  out->print(" "); out->print(digest); 
}

} // namespace io
} // namespace acdk 



