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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/File.cpp,v 1.25 2005/03/10 19:18:49 kommer Exp $





#include <acdk.h>

#include "File.h"
#include "FileStandardImpl.h"
#include "Writer.h"
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#  include <direct.h>
#  include <io.h>
#  include <windows.h>
#endif
#if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
#  include <dirent.h>
#endif
#if defined(ACDK_OS_UNIX)
#include <unistd.h>
#endif 


namespace acdk {
namespace io {

#if defined(ACDK_OS_WIN32)
const char File::_pathSeparatorChar =  ';';
const char File::_nameSeparatorChar = '\\';
#else //defined(ACDK_OS_WIN32)
const char File::_pathSeparatorChar =  ':';
const char File::_nameSeparatorChar = '/';
#endif //defined(ACDK_OS_WIN32)

//static 

RString 
File::pathSeparator() 
{ 
  static char buf[2]; buf[1] = 0; buf[0] = pathSeparatorChar();
  /*
#if defined(ACDK_OS_WIN32)
  static char ubuf[2]; buf[1] = 0; buf[0] = ':';
  if (System::_unixMode == true)
    return new String(ubuf);
#endif
    */
  return new String(buf);
}

//static 
RString 
File::separator() 
{ 
  static char buf[2]; buf[1] = 0; buf[0] = separatorChar();
  /*
#if defined(ACDK_OS_WIN32)
  static char ubuf[2]; buf[1] = 0; buf[0] = '/';
  if (System::_unixMode == true)
    return new String(ubuf);
#endif
    */
  return new String(buf);
}

//static
RString
File::endOfLine()
{
#if defined(ACDK_OS_WIN32)
  if (System::_unixMode == true)
    return new String("\n");
  return new String("\r\n");
#else
  return new String("\n");
#endif
}                                                                                           


/*
RFileImplFactoryArray getStaticFactory()
{
  static RFileImplFactoryArray _factories;
  if (_factories == Nil) {
    _factories = new FileImplFactoryArray(0);
  }
  return _factories;
}

//static 


//static 
void 
File::registerFileImpl(RFileImplFactory fimplfactory)
{
  RFileImplFactoryArray factories = getStaticFactory();
  factories->append(fimplfactory);
}
*/

//static 
RFileImpl 
File::getFileImpl(IN(RString) fname)
{
  RFileSystem fs = FileSystem::findFileSystem(fname);
  if (fs == Nil)
    return new FileStandardImpl(fname);
  return fs->getFileImpl(fname);
  /*reimplement 
  int l = fac->length();
  for (int i = 0; i < l; ++i)
  {
    if (fac[i]->handleFile(fname) == true)
      return fac[i]->create(fname);
  }
  */
  
}

File::File(IN(RString) path)
{
  _fileImpl = getFileImpl(path);
}


File::File(IN(RFile) parent, IN(RString) child)
{
  _fileImpl = parent->makeChild(child)->getFileImpl(); //## performance: may go directly over file system
}

File::File(IN(RString) parent, IN(RString) child)
{
  _fileImpl = File(parent).makeChild(child)->getFileImpl();
}

//static
RString
File::concat(IN(RString) parent, IN(RString) child)
{
  if (parent == Nil)
    return child;
  if (child == Nil)
    return parent;
  return File(parent).makeChild(child)->getPath();
  /*
  if (parent->endsWith(separator()))
    return parent +  child;
  else
    return parent + separator() + child;
    */
}




//static 
RString 
File::getCWD()
{
  char buffer[ACDK_MAX_PATH];
  if (getcwd(buffer, ACDK_MAX_PATH) == NULL)
    return Nil;
  return SCS(buffer);
}

//static 
bool
File::setCWD(IN(RString) newpath)
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  char buffer[ACDK_MAX_PATH];
  if (getcwd(buffer, ACDK_MAX_PATH) != NULL) {
    if (newpath->length() > 1 && newpath->charAt(0) != buffer[0]) {
      char cb[3]; cb[0] = newpath->charAt(0); cb[1] = ':'; cb[2] = '\0';
      chdir(cb);
    }
  }
#endif //defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  RString ascpath = newpath->convert(CCAscii);
  return chdir(ascpath->c_str()) == 0;

}


//static 
RFile 
File::getCWDFile()
{
  return new File(getCWD());
}


bool 
File::mkdirs(int mode)
{
  if (exists() == true)
    return true;
  if (mkdir(mode) == true)
    return true;
  RString fname = getCanonicalPath();
  int lpos = fname->lastIndexOf(separatorChar());
  if (lpos == 0 || lpos == -1) 
    return false;
  if (File(fname->substr(0, lpos)).mkdirs(mode) == false)
    return false;
  return mkdir(mode);
}

//static 
RFile 
File::createTempFile(IN(RString) prefix, IN(RString) suffix) 
{
  if (prefix == Nil || prefix->length() < 3)
    THROW1(IllegalArgumentException, "File::createTempFile: prefix needs at least 3 characters");
  RString suf = suffix;
  if (suf == Nil)
    suf = ".tmp";
  RString tdir = System::getProperty("TEMP");
  if (tdir == Nil || tdir->length() == 0)
    tdir = File::getCWD();
  return createTempFile(prefix, suf, new File(tdir));
}

//static 
RFile 
File::createTempFile(IN(RString) prefix, IN(RString) suffix, IN(RFile) directory)
{
  if (prefix == Nil || prefix->length() < 3)
    THROW1(IllegalArgumentException, "File::createTempFile: prefix needs at least 3 characters");
  RClass fileclass = File::GetClass();
  SYNCOBJECT(fileclass);
  
  RString suf = suffix;
  if (suf == Nil)
    suf = ".tmp";
  
  int c = 0;
  while (true)
  {
    RString fname = File::concat(directory->getCanonicalPath(), prefix + Integer::toString(c) + suf);
    RFile ret = new File(fname);
    if (ret->exists() == false)
    {
      ret->createNewFile();
      return ret;
    }
    ++c;
  }
  return Nil;
}


//static 
RFileArray
File::listRoots()
{
#if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32) //|| defined(ACDK_OS_WIN32)
# if defined(ACDK_OS_WIN32) // dead code
  if (System::_unixMode == true)
  {
# endif
  RFileArray roots = new FileArray(1);
  roots[0] = new File("/");
  return roots;
# if defined(ACDK_OS_WIN32)
  }
# endif
#endif

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  TCHAR buffer[260];
  RFileArray  roots = new FileArray(26);
  if (GetLogicalDriveStrings(260, buffer) == 0) 
  {
    roots->resize(0);
    return roots;
  }
  TCHAR* ptr = buffer;
  int i;
  for (i = 0; *ptr; i++) 
  {
    roots[i] = new File((uc2char*)ptr);
    ptr += tstrlen(ptr) + 1;
  }
  roots->resize(i);
  return roots;
#endif //defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
}


//foreign virtual 
int 
File::compareTo(IN(RObject) o)
{
  return compareTo((RFile)o);
}
  
//virtual 
int 
File::compareTo(IN(RFile) o)
{
  RString str1 = getCanonicalPath();
  RString str2 = o->getCanonicalPath();
#if defined(ACDK_OS_WIN32)
  if (str1->equalsIgnoreCase(str2) == true)
    return 0;
#endif
  return str1->compareTo(str2);

}

//foreign virtual 
bool 
File::equals(IN(RObject) o)
{
  if (instanceof(o, File) == false)
    return false;
  return compareTo((RFile)o) == 0;
}

//foreign 
RString 
File::toString()
{
  return _fileImpl->getPath();
}

//foreign virtual 
int 
File::hashCode()
{
  return getCanonicalPath()->hashCode();
}


void 
File::deleteOnExit()
{
  _throwNotImplementedYet("File::deleteOnExit()");
}

//virtual 
RReader 
File::getReader()
{
  return _fileImpl->getReader();
}

//virtual 
RWriter
File::getWriter()
{
  return _fileImpl->getWriter();
}


RString 
File::loadAscii()
{
  return new String(RcharArray(loadBinary()));
}

RbyteArray 
File::loadBinary()
{
  if (canRead() == false)
    return new byteArray(0);
  jlong l = length();
  RbyteArray ch = new byteArray(l);
  RReader fin = getReader();
  fin->read(ch, 0, ch->length());
  return ch;
}



} // io
} // acdk


