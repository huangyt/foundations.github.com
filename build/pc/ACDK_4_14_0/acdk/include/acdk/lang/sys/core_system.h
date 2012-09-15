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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_system.h,v 1.13 2005/03/07 20:41:36 kommer Exp $

#ifndef acdk_lang_sys_core_system_h
#define acdk_lang_sys_core_system_h

#include "../../Config.h"


namespace acdk {
namespace lang {
namespace sys {


/**
   This class encapsulate some very basic 
   initializations.
   
*/

enum ExecutionState
{
  /**
     Executable is not finished initialized.
     Normally in this state,  constructors of static
     classes are called.
     Only limited usage of synchronization and allocators can be used.
  */
  BeforeMain = 0,

  /**
     The executable up and running
  */
  InMain = 1,
  /**
     The exutable is finished.
     This state can be occured in destructors of static classes.
  */
  AfterMain = 2,

  /**
     A signal handler is currently be called
  */
  InSignalHandler = 3

};


/**
  implements a basic system wrapper
*/
class ACDK_CORE_PUBLIC core_system
{
 public:

  typedef int (*MainFunction)(int argc, char* argv[], char* envp[]);
  /**
     this is the basic wrapper for main.
  */
  static ExecutionState execution_state;
  static int core_main(MainFunction, int argc, char* argv[], char* envp[]);


  
  static ExecutionState getState() { return execution_state; }
  static void setState(ExecutionState state);
  static bool inMain() { return execution_state == InMain; }
  static bool isInMain;
  static bool acdk_core_static_bound;
  typedef bool (*InitializeFunction)(void* arg);
  typedef void (*DeinitializeFunction)(void* arg);
  static void registerIntializer(InitializeFunction, void* arg = 0);
  static void registerDeintializer(DeinitializeFunction, void* arg = 0);
  /**
    return the fq executable name.
    if name contains '/' it looks relative to current directory
    if name is only a file name search in PATH
    if not found, return unmodified name
    @note if the process used chdir, relativ path will not be handled correctly
  */
  static RString fq_executable_filename(IN(RString) name);
  static bool intialize();
  static void deintialize();


  // this is used to identify stack-objects
// maybe it should return jlong, too

  static inline unsigned int _getSP()
  {
    unsigned int _esp_val;
#if defined(__BORLANDC__)
    int __bccGetSP();
    _esp_val = __bccGetSP();
#elif defined(_MSC_VER)
  __asm mov _esp_val, esp;
#elif defined(__GNUC__)
#if defined(__i386__)
  __asm__ __volatile__( "movl\t%%esp,%0\n\t" :"=r" (_esp_val));
#elif defined(__sparc__)
  __asm__ __volatile__( "mov\t%%sp,%0\n\t" :"=r" (_esp_val));
#else
  //#error HEAP-DETECTION CURRENTLY NOT SUPPORTED ON THIS PLATFORM!
  char val;
  _esp_val = ACDK_CAST_PTR2INT(&val);
#endif // __GNUC__
#else
  //#error HEAP-DETECTION CURRENTLY NOT SUPPORTED WITH THIS COMPILER!
    char val;
    _esp_val = (int)&val;
#endif
    return _esp_val;
  } 
  /**
    returns true, if the pointer is pointing into stack
  */
  static bool isPtrInStack(void* ptr);

};

struct ACDK_CORE_PUBLIC core_endline
{
};

extern ACDK_CORE_PUBLIC core_endline eofl;

/**
  used for output, if System::out/err is not available
*/
class ACDK_CORE_PUBLIC core_output 
{
public:
  core_output& operator<<(const char* text);
  core_output& operator<<(IN(RObject) obj);
  core_output& operator<<(IN(RString) text);
  core_output& operator<<(int i);
  core_output& operator<<(jlong i);
  core_output& operator<<(double d);
  core_output& operator<<(void* ptr);
  core_output& operator<<(const core_endline& oef);
  core_output& operator<<(bool val);
};

extern ACDK_CORE_PUBLIC core_output coreout;

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_system_h


