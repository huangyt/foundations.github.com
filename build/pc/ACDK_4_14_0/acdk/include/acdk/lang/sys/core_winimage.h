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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_winimage.h,v 1.12 2005/04/28 15:02:28 kommer Exp $


#ifndef acdk_lang_sys_core_winimage_h
#define acdk_lang_sys_core_winimage_h

#include <windows.h>
#include <imagehlp.h>
#include <acdk.h>

namespace acdk {
namespace lang {
namespace sys {

foreign
class core_winimage 
{
public:
  enum {
    SymbolMaxLength = 1024
  };
  struct SymbolResult 
  {
    bool result;
    RString symbol;
    long displacement;
  };
  struct FallbackInfoResult 
  {
    RString  module;
    long section;
    long offset;
  };
  core_winimage() throw();
  ~core_winimage() throw();
  
  bool isInited() { return inited; }
  
  bool initStackWalk(CONTEXT *context, STACKFRAME *stack, bool determineContext = true);
  
  bool stackWalk(CONTEXT *context, STACKFRAME *stack);
  
  SymbolResult getSymbol(void *addr);
  bool getSourceLine(void* ptr, OUT(RString) source, OUT(int) line);
  void getFallbackInfo(void *addr, FallbackInfoResult& fbir);
  
  private:
  bool GetLogicalAddress(PVOID addr, PTSTR szModule, DWORD len, 
                         DWORD& section, DWORD& offset);
    
  bool inited;
  bool initLib();
    
  typedef DWORD (__stdcall *SYMSETOPTIONSPROC)( DWORD );
  typedef DWORD (__stdcall *SYMGETOPTIONSPROC)( );
  typedef BOOL (__stdcall * SYMINITIALIZEPROC)( HANDLE, LPSTR, BOOL );
  typedef BOOL (__stdcall *SYMCLEANUPPROC)( HANDLE );
  typedef LPVOID (__stdcall *SYMFUNCTIONTABLEACCESSPROC)( HANDLE, DWORD );
  typedef DWORD (__stdcall *SYMGETMODULEBASEPROC)( HANDLE, DWORD );
  
  typedef BOOL (__stdcall * STACKWALKPROC) ( DWORD, HANDLE, HANDLE, LPSTACKFRAME, LPVOID,
    PREAD_PROCESS_MEMORY_ROUTINE,PFUNCTION_TABLE_ACCESS_ROUTINE, 
    PGET_MODULE_BASE_ROUTINE, PTRANSLATE_ADDRESS_ROUTINE );
  typedef BOOL (__stdcall *SYMGETSYMFROMADDRPROC) ( HANDLE, DWORD, PDWORD, PIMAGEHLP_SYMBOL );
  typedef DWORD (__stdcall *UNDECORATESYMBOLNAME)(LPSTR, LPSTR, DWORD, DWORD);
  typedef BOOL(__stdcall *SYMGETLINEFROMADDR)(HANDLE, DWORD, PDWORD, PIMAGEHLP_LINE);
    
  // all the functions pointers determined by the constructor
  SYMINITIALIZEPROC _SymInitialize;
  SYMCLEANUPPROC _SymCleanup;      
  STACKWALKPROC _StackWalk;
  SYMFUNCTIONTABLEACCESSPROC _SymFunctionTableAccess;
  SYMGETMODULEBASEPROC _SymGetModuleBase;
  SYMGETSYMFROMADDRPROC _SymGetSymFromAddr;
  SYMSETOPTIONSPROC _SymSetOptions;
  SYMGETOPTIONSPROC _SymGetOptions;
  UNDECORATESYMBOLNAME _UnDecorateSymbolName;
  SYMGETLINEFROMADDR _SymGetLineFromAddr;
  
};

} // namespace sys 
} // namespace lang 
} // namespace acdk 



#endif //acdk_lang_sys_core_winimage_h
