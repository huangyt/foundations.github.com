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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/SystemIntern.h,v 1.12 2005/04/09 19:26:51 kommer Exp $

#ifndef acdk_lang_SystemIntern_h
#define acdk_lang_SystemIntern_h
#if !defined(DOXYGENONLY)

namespace acdk {
namespace lang {

/** 
Holds the arguments from commandline without the acdk specific
    arguments. 
*/
foreign
class ACDK_CORE_PUBLIC ArgumentHolder
{
public:
  ArgumentHolder(IN(RStringArray) args)
  {
    if (args == Nil)
    {
      argc = 0;
      argv = new char*[1];
      argv[0] = 0;
      return;
    }
    argc = args->length();
    argv = new char*[argc + 1]; // one more for trailing NULL-pointer
    for (int i = 0; i < argc; i++) {
      RString sa = args[i];
      sa = sa->convert(CCAscii); // ### use a environment variable to determine external encoding
      int salen = sa->length();
      argv[i] = new char[salen + 1];
      
      const char* ptr = sa->c_str();
      strncpy(argv[i], ptr, salen);
      argv[i][salen] = 0;
    }
    argv[argc] = 0; // needed for some implementations (envp)
  }
  ~ArgumentHolder()
  {
    for (int i = 0; i < argc; i++) {
      delete [] argv[i];
      argv[i] = 0;
    }
    delete [] argv;
  }
  int& getArgc() { return argc; }
  char** getArgv() { return argv; }
private:
  int    argc;
  char** argv;
};

/**
  Same like ArgumentHolder but holds
  platform Unicode (16bit) arguments
*/
foreign
class ACDK_CORE_PUBLIC UcArgumentHolder
{
public:
  UcArgumentHolder(IN(RStringArray) args)
  {
    argc = args->length();
    argv = new ucchar*[argc + 1]; // one more for trailing NULL-pointer
    for (int i = 0; i < argc; i++) 
    {
      RString sa = args[i];
      sa = sa->convert(CCUcs2); 
      int salen = sa->length();
      argv[i] = new ucchar[salen + 1];
      
      const ucchar* ptr = sa->uc2c_str();
      memcpy(argv[i], ptr, salen * sizeof(ucchar));
      argv[i][salen] = 0;
    }
    argv[argc] = 0; // needed for some implementations (envp)
  }
  ~UcArgumentHolder()
  {
    for (int i = 0; i < argc; i++) {
      delete [] argv[i];
      argv[i] = 0;
    }
    delete [] argv;
  }
  int& getArgc() { return argc; }
  ucchar** getArgv() { return argv; }
private:
  int    argc;
  ucchar** argv;
};

/** @internal */
foreign
class ACDK_CORE_PUBLIC UcEnvBlockHolder
{
public:
  ucchar* block;
  int size;
  UcEnvBlockHolder(IN(RStringArray) args)
  : block(0)
  , size(0)
  {
    if (args == Nil || args->length() == 0)
      return;
    int i;
    int charCount = 0;
    for (i = 0; i < args->length(); ++i)
    {
      charCount += args[i]->length() + 1;
    }
    ++charCount;
    block = new ucchar[charCount];
    ucchar* bptr = block;
    for (i = 0; i < args->length(); ++i)
    {
      RString s= args[i];
      for (String::iterator it = s->begin(); it < s->end(); ++it)
      {
        *bptr++ = *it;
      }
      *bptr++ = 0;
    }
    *bptr++ = 0;
  }
  ~UcEnvBlockHolder()
  {
    delete block;
  }
};

/** @internal */
foreign
class ACDK_CORE_PUBLIC EnvBlockHolder
{
public:
  char* block;
  int size;
  EnvBlockHolder(IN(RStringArray) args)
  : block(0)
  , size(0)
  
  {
    if (args == Nil || args->length() == 0)
      return;
    int i;
    int charCount = 0;
    for (i = 0; i < args->length(); ++i)
    {
      charCount += args[i]->length() + 1;
    }
    ++charCount;
    block = new char[charCount];
    char* bptr = block;
    for (i = 0; i < args->length(); ++i)
    {
      RString s= args[i];
      for (String::iterator it = s->begin(); it < s->end(); ++it)
      {
        *bptr++ = *it;
      }
      *bptr++ = 0;
    }
    *bptr++ = 0;
  }
  ~EnvBlockHolder()
  {
    delete block;
  }
};


} // namespace lang
} // namespace acdk

#endif //!defined(DOXYGENONLY)
#endif //acdk_lang_SystemIntern_h

