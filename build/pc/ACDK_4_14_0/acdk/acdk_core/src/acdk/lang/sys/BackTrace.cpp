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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/BackTrace.cpp,v 1.47 2005/04/30 23:08:36 kommer Exp $

#include "../../Config.h"
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
// windoof stuff is encapsulated in the core_winimage class
#include "core_winimage.h"

#endif //defined(ACDK_OS_WIN32)

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/locale/AsciiEncoding.h>

#include "BackTrace.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>


#if defined(ACDK_OS_SOLARIS)
#include <ucontext.h>
#include <sys/frame.h>
#include <dlfcn.h>
#include <setjmp.h>
#include <sys/procfs_isa.h>
#include <demangle.h>

#if defined(sparc) || defined(__sparc)
#  define FRAME_PTR_REGISTER REG_SP
#endif
#  if defined(i386) || defined(__i386)
#define FRAME_PTR_REGISTER EBP
#endif

#endif //defined(ACDK_OS_SOLARIS)

#if defined(ACDK_OS_LINUX)
#include <execinfo.h>
#endif

#if defined(__GNUG__) // does only work on linux
#if __GNUC__ >= 3
# define ACDK_HAS_CXXABI_H
#endif
//#define ACDK_HAS_LIBERTY_DEMANGLE
#ifdef ACDK_HAS_LIBERTY_DEMANGLE
// from demangle.h
#define DMGL_PARAMS	 (1 << 0)	/* Include function args */
#define DMGL_ANSI	 (1 << 1)	/* Include const, volatile, etc */
#define DMGL_JAVA	 (1 << 2)	/* Demangle as Java rather than C++. */

extern "C" char * cplus_demangle (const char *mangled, int options);
#elif defined(ACDK_HAS_CXXABI_H)
#include <cxxabi.h>
#endif //ACDK_HAS_LIBERTY_DEMANGLE
#endif

//#define LOCAL_DEBUG

#ifdef LOCAL_DEBUG
#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)
#else
#define DOUT(strexpr)
#endif

namespace acdk {
namespace lang {
namespace sys {

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#if defined(__BORLANDC__) // make troubles in destruction
core_winimage* BackTrace::_winimage = 0;
#else
core_scoped_ptr<core_winimage> BackTrace::_winimage;
#endif


//static
void BackTrace::init_winimage()
{
   if(_winimage == 0)
      _winimage = new core_winimage();
}
#endif //defined(ACDK_OS_WIN32)

BackTrace::BackTrace(int ignoreTopFrames)
: _ignoreTopFrames(ignoreTopFrames)
{
   _save_pcs();
}
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
BackTrace::BackTrace(_CONTEXT& ctx, int ignoreTopFrames)
: _ignoreTopFrames(ignoreTopFrames)
{
   _save_pcs(ctx);
}
#endif

RStackFrameArray 
BackTrace::getStackFrames()
{
   int offset = _ignoreTopFrames;
  if (_program_counters.size() - offset < 0)
    return new StackFrameArray(0);
  RStackFrameArray rets = new StackFrameArray(_program_counters.size() - offset);
  for (int i = 0; i < _program_counters.size() - offset; i++)
  {
    rets[i] = getStackFrame(_program_counters[i + offset]);
  }
  return rets;
}


void
gcc_demangle(RString text, RString& funcname, RString& libname)
{

  // /home/roger/artefaktur/acdk/bin/libacdk_tools_testunit.so(executeTests__Q44acdk5tools8testunit10TestRunner+0x356)
  if (text->indexOf('(') == -1 || text->indexOf(')') ==  -1) {
    funcname = text;
    libname = "";
    return;
  }
  libname = text->substr(0, text->indexOf('('));
  funcname = text->substr(text->indexOf('(') + 1, text->indexOf(')'));
  if (funcname->indexOf('+') == -1)
    return;
  funcname = funcname->substr(0, funcname->indexOf('+'));
#if defined(ACDK_HAS_LIBERTY_DEMANGLE)

  char* retptr = cplus_demangle (funcname->c_str(), DMGL_PARAMS /*| DMGL_JAVA*/ | DMGL_ANSI);
  funcname = acdk::locale::AsciiEncoding::encodeToString(retptr);
  free(retptr);
#elif defined(ACDK_HAS_CXXABI_H)
  
#if defined(ACDK_64_BIT_PTR)
  unsigned long int size = 0;
#else
  unsigned int size = 0;
#endif
  
  int status = 0;
  RString cfunc = funcname->convert(CCAscii);
  DOUT("BT: " << cfunc);
  const char* cptr = cfunc->c_str();
  char* retbuffer = ::abi::__cxa_demangle(cptr, 0, &size, &status);
  
  if (status == 0)
  {
    DOUT("BTDEM(" << status << "): " << cptr << "=>" << (const char*)retbuffer);
    //int l = strlen(//strnlen(cptr, size);
    //funcname = acdk::locale::AsciiEncoding::decodeToString((const byte*)retbuffer, l);
    funcname = SCS(retbuffer);
  }
  if (retbuffer != 0)
    free(retbuffer);
  
#endif //defined(ACDK_HAS_LIBERTY_DEMANGLE)
}


//static 
RStackFrame 
BackTrace::getStackFrame(void* pc)
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  init_winimage();
  core_winimage::SymbolResult res = _winimage->getSymbol(pc);
  core_winimage::FallbackInfoResult fallRes;
  _winimage->getFallbackInfo(pc, fallRes);
  RString module = fallRes.module;
  RString function = "";
  if (res.result) {
    function = res.symbol;
  } else {
    char buf[256];
    sprintf(buf, "0x%x", fallRes.offset);
    function = RString("<") + SCS(buf) + ">";
  }
  RString sourceName;
  int sourceLine = -1;
  _winimage->getSourceLine(pc, sourceName, sourceLine);
  return new NativeStackFrame((int)pc, function, module, sourceName, sourceLine);

#elif defined(ACDK_OS_LINUX)
  void* abuf[1];
  abuf[0] = pc;
  char** funcs = backtrace_symbols(abuf, 1);
  RString text = acdk::locale::AsciiEncoding::decodeToString((const byte*)*funcs);
  RString funcname = text;
  RString libname = "";
  gcc_demangle(text, funcname, libname);
  //System::out->println("Text: [" + text + "] funcname=[" + funcname + "] lib=[" + libname + "]");
  free(funcs);
  return new NativeStackFrame((int)pc, funcname, libname);
#elif defined(ACDK_OS_SOLARIS) && defined(__GNUG__)

  return new NativeStackFrame((int)pc, String::valueOf((int)pc), "<unknown>");
#elif defined(ACDK_OS_SOLARIS) // implicit worshop
   Dl_info info;
   const char* func = "??";
   const char* lib = "??";
   char cpp_func[1024];

   memset(cpp_func, 0, sizeof(cpp_func));
   memset(&info, 0, sizeof(info));
   if (dladdr(pc, & info) != 0)
   {
      lib = (char *)info.dli_fname;
      func = (char *)info.dli_sname;
   }

   cplus_demangle(func, cpp_func, sizeof(cpp_func));
   cpp_func[sizeof(cpp_func) - 1] = 0;
   return new NativeStackFrame((int)pc,
			     acdk::locale::AsciiEncoding::decodeToString((const byte*)cpp_func),
			     acdk::locale::AsciiEncoding::decodeToString((const byte*)lib));
#elif defined(ACDK_OS_BSD) || defined(ACDK_OS_DARWIN) || defined(ACDK_OS_CYGWIN32)
   return new NativeStackFrame(0, "", "");
#else
#error unkown OS
#endif //defined(ACDK_OS_SOLARIS)
}

#if defined(ACDK_OS_WIN32) 

void BackTrace::_save_pcs(_CONTEXT& ctx, bool replace)
{
  if (replace == true)
    _program_counters.resize(0);
  STACKFRAME stack;
  _winimage->initStackWalk(&ctx, &stack, false);
  for (int i = 0; _winimage->stackWalk(&ctx, &stack) == true; i++)
  {
    void* addr = reinterpret_cast<void*>(stack.AddrPC.Offset);
    //printf("ret: %i\n", (int)addr);
    if (0 != addr)
      _program_counters.add(addr);
  }
}
#endif
void BackTrace::_save_pcs()
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  init_winimage();
  CONTEXT context;
  STACKFRAME stack;
  context.ContextFlags = CONTEXT_FULL;
  HANDLE currentThread = GetCurrentThread();
  GetThreadContext(currentThread, &context);
  _winimage->initStackWalk(&context, &stack, false);
  for (int i = 0; _winimage->stackWalk(&context, &stack) == true; i++)
  {
    void* addr = reinterpret_cast<void*>(stack.AddrPC.Offset);
    //printf("ret: %i\n", (int)addr);
    if (0 != addr)
      _program_counters.add(addr);
  }
#elif defined(ACDK_OS_LINUX)
  void* adress_buffer[100];
  int len = backtrace(adress_buffer, 100);
  //sys::coreout << "backtrace len: " << len << sys::eofl;
  _program_counters.ensureCapacity(len);
  for (int i = 0; i < len; i++)
  {
    _program_counters.add(adress_buffer[i]);
  }
#elif defined(ACDK_OS_SOLARIS)
  void* savpc;
  struct frame* savfp;
  struct frame* fp;
  ucontext_t  u;

  getcontext(&u);
  fp = savfp = (struct frame*)(((struct frame *)u.uc_mcontext.gregs[FRAME_PTR_REGISTER])->fr_savfp);

  for (; fp && (savpc = (void*)fp->fr_savpc); fp = (struct frame*)fp->fr_savfp)
  {
    _program_counters.add(savpc);
  }
#endif
}

int 
BackTrace::fillBackTrace(int offset, void** pcBuff, int size)
{
 #if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  init_winimage();
  CONTEXT context;
  STACKFRAME stack;
  context.ContextFlags = CONTEXT_FULL;
  HANDLE currentThread = GetCurrentThread();
  GetThreadContext(currentThread, &context);
  _winimage->initStackWalk(&context, &stack, false);
  int i;
  for (i = 0; _winimage->stackWalk(&context, &stack) == true; i++)
  {
    if (i < offset)
      continue;
    if (i - offset >= size)
      break;
    pcBuff[i - offset] = reinterpret_cast<void*>(stack.AddrPC.Offset);
  }  
  return i - offset;
#elif defined(ACDK_OS_LINUX)
  void* adress_buffer[100];
  int len = backtrace(adress_buffer, 100);
  int i;
  for (i = 0; i < len; i++)
  {
    if (i < offset)
      continue;
    if (i - offset >= size)
      break;
    pcBuff[i - offset] = adress_buffer[i];
  }
  return i - offset;
#elif defined(ACDK_OS_SOLARIS)
  void* savpc;
  struct frame* savfp;
  struct frame* fp;
  ucontext_t  u;

  getcontext(&u);
  fp = savfp = (struct frame*)(((struct frame *)u.uc_mcontext.gregs[FRAME_PTR_REGISTER])->fr_savfp);
  int i = 0;
  for (; fp && (savpc = (void*)fp->fr_savpc); fp = (struct frame*)fp->fr_savfp, ++i)
    {
     if (i < offset)
      continue;
    if (i - offset >= size)
      break;
    pcBuff[i - offset]  =  savpc;
  }
  return i - offset;
#endif
}

} // namespace sys
} // namespace lang
} // namespace acdk

