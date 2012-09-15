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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/FileInfo.h,v 1.9 2005/02/05 10:45:30 kommer Exp $


#ifndef acdk_net_srsync_FileInfo_h
#define acdk_net_srsync_FileInfo_h

#include <acdk.h>

#include <acdk/io/Serializable.h>
#include <acdk/io/FileImpl.h>
#include <acdk/io/File.h>
#include <acdk/io/PrintWriter.h>

#include <acdk/util/SysDate.h>
#include "srfsys.h"

#include "FileInfo.h"


namespace acdk {
namespace net {
namespace srfsys {

USING_CLASS(::acdk::lang::, String);
USING_CLASS(::acdk::io::, File);

ACDK_DECL_CLASS(FileInfo);

const int FileExists    = 0x0001;
const int FileIsFile     = 0x0002;
const int FileIsDir     = 0x0004;
const int FileCanRead   = 0x0010;
const int FileCanWrite  = 0x0020;
const int FileCanExec   = 0x0040;
const int FileIsHidden  = 0x0100;
const int FileHasABit   = 0x0200; // dos specific

const int FileChecked   = 0x1000;

class ACDK_NET_SRFSYS_PUBLIC FileInfo
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable
, implements ::acdk::lang::Comparable
{
  ACDK_WITH_METAINFO(FileInfo)
public:
  RString dir;
  RString name;
  jlong size;
  jlong created;
  jlong modified;
  int flags;
  jlong digest;
  FileInfo()
  : flags(FileExists)
  , digest(0)
  {
  }
  FileInfo(IN(RFileInfo) other)
  : Object()
  , dir(other->dir)
  , name(other->name)
  , size(other->size)
  , created(other->created)
  , modified(other->modified)
  , flags(other->flags)
  , digest(other->digest)
  {
  }
  FileInfo(IN(RString) path)
  : Object()
  , size(0)
  , created(0)
  , modified(0)
  , flags(0)
  , digest(0)
  {
    File tf(path);
    dir = tf.getParent();
    name = tf.getName();
  }
  RString getPath() 
  {
    return dir + "/" + name;
  }
  int compareTo(IN(RFileInfo) other)
  {
    int t = dir->compareTo(other->dir);
    if (t != 0) return t;
    return name->compareTo(other->name);
  }
  int compareTo(IN(RObject) other)
  {
    return compareTo(RFileInfo(other));
  }
  bool exists() { return flags & FileExists; }
  void exists(bool b) { if (b) flags |= FileExists; else flags &= ~FileExists; }
  bool canRead() { return flags & FileCanRead; }
  void canRead(bool b) { if (b) flags |= FileCanRead; else flags &= ~FileCanRead; }
  bool canWrite() { return flags & FileCanWrite; }
  void canWrite(bool b) { if (b) flags |= FileCanWrite; else flags &= ~FileCanWrite; }
  bool isDirectory() { return flags & FileIsDir; }
  void isDirectory(bool b) { if (b) flags |= FileIsDir; else flags &= ~FileIsDir; }
  bool isFile() { return flags & FileIsFile; }
  void isFile(bool b) {  if (b) flags |= FileIsFile; else flags &= ~FileIsFile; }
  bool isHidden() { return flags & FileIsHidden; }
  void isHidden(bool b) { if (b) flags |= FileIsHidden; else flags &= ~FileIsHidden; }
  bool isChecked() { return flags & FileChecked; }
  void isChecked(bool b) { if (b) flags |= FileChecked; else flags &= ~FileChecked; }
  static RString flagString(int flags)
  {
    StringBuffer sb;
    if (flags & FileExists)
      sb.append("E");
    else
      sb.append("N");
    if (flags & FileIsFile)
      sb.append("F");
    else if (flags & FileIsDir)
      sb.append("D");
    if (flags & FileCanRead)
      sb.append("R");
    if (flags & FileCanWrite)
      sb.append("W");
    if (flags & FileCanExec)
      sb.append("X");
    if (flags & FileIsHidden)
      sb.append("H");
    if (flags & FileHasABit)
      sb.append("A");
    return sb.toString();
  }
  static RString timeToString(jlong time)
  {
    return acdk::util::SysDate(time).toString();
  }
  RString toString()
  {
    StringBuffer sb;
    sb.append("\""); sb.append(dir); sb.append("/"); sb.append(name); sb.append("\""); 
    sb.append(" "); sb.append(size); 
    sb.append(" "); sb.append(flagString(flags)); 
    sb.append(" "); sb.append(timeToString(created)); 
    sb.append(" "); sb.append(timeToString(modified)); 
    sb.append(" "); sb.append(digest); 
    return sb.toString();
  }
  void dump(IN(::acdk::io::RPrintWriter) out)
  {
    out->printQuoted(dir + "/" + name); 
    out->print(" "); out->print(size); 
    out->print(" "); out->print(flags); 
    out->print(" "); out->print(created); 
    out->print(" "); out->print(modified); 
    out->print(" "); out->print(digest); 
  }
  bool equals(IN(RFileInfo) other)
  {
    return dir->equals(other->dir) == true &&
           name->equals(other->name) == true;
  }
  bool equals(IN(RObject) other)
  {
    if (instanceof(other, FileInfo) == false)
      return false;
    return equals(RFileInfo(other));
  }

};


} // namespace srfsys
} // namespace net
} // namespace acdk 


#endif //acdk_net_srsync_FileInfo_h
