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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_sharedlib.h,v 1.9 2005/03/14 12:14:09 kommer Exp $

#ifndef acdk_lang_sys_core_sharedlib_h
#define acdk_lang_sys_core_sharedlib_h

#include "../../Config.h"
#include "../../Platform.h"
#ifdef ACDK_OS_WIN32
# include <windows.h>
#endif //ACDK_OS_WIN32
#if defined(ACDK_OS_LINUX) || defined(ACDK_OS_SOLARIS) || defined(ACDK_OS_BSD) || defined(ACDK_OS_DARWIN)
# include <dlfcn.h>
#define ACDK_UNIX_DLFNC
#endif

#if defined(ACDK_OS_DARWIN)
void *darwin_dlopen(const char *path, int mode);
const char *darwin_dlerror(void);
int darwin_dlclose(void *handle);
void *darwin_dlsym(void *handle, const char *symbol);

#define dlerror darwin_dlerror
#define dlopen darwin_dlopen
#define dlclose darwin_dlclose
#define dlsym darwin_dlsym

#endif // defined(ACDK_OS_DARWIN)


namespace acdk {
namespace lang {
namespace sys {

class core_sharedlib
{
  void* _libReference;
public:
  core_sharedlib(const char* shlibname = 0)
  : _libReference(0)
  {
    if (shlibname != 0)
      loadLib(shlibname);
  }
  bool loaded() const { return _libReference != 0; }
  void loadLib(const char* shlibname)
  {
#if defined(ACDK_OS_WIN32) || defined(ACDK_OS_CYGWIN32)
#if defined(UNICODE)
    RString str = String(shlibname).convert(CCUcs2); 
    _libReference = (void*)LoadLibrary((wchar_t*)str->uc2c_str());
#else //defined(UNICODE)
    _libReference = (void*)LoadLibrary(shlibname);
#endif //defined(UNICODE)
#elif defined(ACDK_UNIX_DLFNC)
    _libReference = (void*)dlopen(shlibname, RTLD_LAZY | RTLD_GLOBAL);
    if (_libReference != 0)
      return;
    char buffer[1024];
    strncat(strncat(strncpy(buffer, "lib", 1024), shlibname, 1024), ".so", 1024);
   
    _libReference = (void*)dlopen(buffer, RTLD_LAZY | RTLD_GLOBAL);
   
#else //defined(ACDK_UNIX_DLFNC)
# error unsupported plattform
#endif
  }
  void unloadLib()
  {
    if (_libReference == 0)
      return ;

#if defined(ACDK_OS_WIN32) || defined(ACDK_OS_CYGWIN32)
    FreeLibrary((HMODULE)_libReference);
#elif defined(ACDK_UNIX_DLFNC)
    dlclose(_libReference);
#else //defined(ACDK_UNIX_DLFNC)
# error unsupported plattform
#endif
    _libReference = 0;
  }
  void* locateFunction(const char* funcname)
  {
    if (_libReference == 0)
      return 0;
#if defined(ACDK_OS_WIN32) || defined(ACDK_OS_CYGWIN32)
    return (void*)GetProcAddress((HMODULE)_libReference, funcname);
#elif defined(ACDK_UNIX_DLFNC)
    return dlsym(_libReference, funcname);
#else //defined(ACDK_UNIX_DLFNC)
# error unsupported plattform
#endif
  }
};


} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_sharedlib_h


