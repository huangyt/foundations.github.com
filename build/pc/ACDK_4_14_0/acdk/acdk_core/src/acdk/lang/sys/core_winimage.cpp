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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_winimage.cpp,v 1.21 2005/04/28 15:02:26 kommer Exp $

#include "../../Config.h"
#include "../../Platform.h"

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#include "core_winimage.h"
#endif
#include <algorithm>
#include <string.h>
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)


#include <acdk.h>

//#include <algorithm>

namespace acdk {
namespace lang {
namespace sys {

//#define ACDK_USE_PURIFY


core_winimage::core_winimage() throw()
: inited(false)
{
   inited = initLib();
}

core_winimage::~core_winimage() throw()
{
}

bool core_winimage::initLib()
{
#if defined(ACDK_USE_PURIFY)
  return false;
#endif
  HMODULE hModImagehlp = LoadLibrary(_T("IMAGEHLP.DLL"));
  
  if ( !hModImagehlp )        
    return false;
  
  _SymInitialize = (SYMINITIALIZEPROC)GetProcAddress( hModImagehlp,
    "SymInitialize" );
  if ( !_SymInitialize )        
    return false;
  _SymCleanup = (SYMCLEANUPPROC)GetProcAddress( hModImagehlp, "SymCleanup" );
  if ( !_SymCleanup )        
    return false;
  _StackWalk = (STACKWALKPROC)GetProcAddress( hModImagehlp, "StackWalk" );
  if ( !_StackWalk )        
    return false;
  _SymFunctionTableAccess = (SYMFUNCTIONTABLEACCESSPROC)
    GetProcAddress( hModImagehlp, "SymFunctionTableAccess" );
  if ( !_SymFunctionTableAccess )        
    return false;
  _SymGetModuleBase=(SYMGETMODULEBASEPROC)GetProcAddress( hModImagehlp,
    "SymGetModuleBase");
  if ( !_SymGetModuleBase )
    return false;
  _SymGetSymFromAddr=(SYMGETSYMFROMADDRPROC)GetProcAddress( hModImagehlp,
    "SymGetSymFromAddr" );
  if ( !_SymGetSymFromAddr )        
    return false;
  
  _SymGetOptions = (SYMGETOPTIONSPROC)GetProcAddress( hModImagehlp,
    "SymGetOptions");
  if ( !_SymGetOptions )
    return false;
  
  _SymSetOptions = (SYMSETOPTIONSPROC)GetProcAddress( hModImagehlp,
    "SymSetOptions");
  if ( !_SymSetOptions )
    return false;
  
  _UnDecorateSymbolName = (UNDECORATESYMBOLNAME)GetProcAddress( hModImagehlp,
    "UnDecorateSymbolName");
  if ( !_UnDecorateSymbolName )
    return false;
  _SymGetLineFromAddr = (SYMGETLINEFROMADDR)GetProcAddress( hModImagehlp, "SymGetLineFromAddr");
  if ( !_SymGetLineFromAddr)
    return false;
  const size_t TTBUFLEN = 4096;
  TCHAR *tt = new TCHAR[TTBUFLEN]; // this is a _sample_. you can do the error checking yourself.

  StringBuffer symSearchPath;
  if (GetCurrentDirectory(TTBUFLEN, tt )) 
  {
    symSearchPath.append(tt);
    symSearchPath.append(";");
  }
  

  if (GetModuleFileName( 0, tt, TTBUFLEN)) 
  {
    TCHAR *p;
    for ( p = tt + tstrlen( tt ) - 1; p >= tt; -- p )
    {
      if ( *p == '\\' || *p == '/' || *p == ':' )
        break;
    }
    if ( p != tt ){ 
      if ( *p == ':' )
        ++ p;
      *p = '\0';
      symSearchPath.append(tt);symSearchPath.append(";");
    }
  }
  
  
  if (GetEnvironmentVariable( _T("_NT_SYMBOL_PATH"), tt, TTBUFLEN)) 
  {
    symSearchPath.append(tt); symSearchPath.append(";");
  }

  
  if (GetEnvironmentVariable( _T("_NT_ALTERNATE_SYMBOL_PATH"), tt, TTBUFLEN)) 
  {
    symSearchPath.append(tt); symSearchPath.append(";");
  }
  

  if (GetEnvironmentVariable( _T("SYSTEMROOT"), tt, TTBUFLEN)) 
  {
    symSearchPath.append(tt); symSearchPath.append(";");
  }
  
  if (symSearchPath.length() > 0)
    symSearchPath = symSearchPath.substring(0, symSearchPath.length() - 1);
  
  
  RString nsmp = symSearchPath.toString()->convert(CCAscii);
  //printf("symbol search path: %s\n", tt);
  bool symInitialized = _SymInitialize( GetCurrentProcess(), (char*)nsmp->c_str(), TRUE ) != 0 ? true : false;;
  delete[] tt;
  
  if( ! symInitialized )
    return false;
  
  DWORD symOptions = _SymGetOptions();
  symOptions &= ~SYMOPT_UNDNAME;
  _SymSetOptions( symOptions );
  
  return true;        
}

bool core_winimage::initStackWalk(CONTEXT *context, 
                                 STACKFRAME *stack, 
                                 bool determineContext)
{
  if (inited == false)
    return false;
        HANDLE ht = GetCurrentThread();
        memset(stack, 0, sizeof(STACKFRAME));

   if(determineContext) 
   {
      context->ContextFlags = CONTEXT_FULL; 
      if (GetThreadContext(ht, context) == FALSE)
         return false; 
   }

   stack->AddrPC.Offset       = context->Eip;
   stack->AddrPC.Mode         = AddrModeFlat;
   stack->AddrStack.Offset    = context->Esp;
   stack->AddrStack.Mode      = AddrModeFlat;
   stack->AddrFrame.Offset    = context->Ebp;
   stack->AddrFrame.Mode      = AddrModeFlat;

   return true;
}

bool core_winimage::stackWalk(CONTEXT *context, STACKFRAME *stack)
{
  if (inited == false)
    return false;

  stack->AddrPC.Offset = 0;
  HANDLE hp = GetCurrentProcess();
  HANDLE ht = GetCurrentThread();

   bool ret = _StackWalk(IMAGE_FILE_MACHINE_I386, 
                         hp,
                         ht,
                         stack,
                         context,
                         NULL,
                         _SymFunctionTableAccess,
                         _SymGetModuleBase,
                         NULL) == TRUE;
  /*
   if (ret == false) 
   {

     LPVOID lpMsgBuf;
     DWORD le = GetLastError();
    FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,
                  le,
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                  (LPTSTR) &lpMsgBuf, 0, NULL );
    
      printf("GetLastError() = %d = %s\n", le, (char*)lpMsgBuf);
      LocalFree( lpMsgBuf );
   }
   */
   return ret;
}



void
core_winimage::getFallbackInfo(void *addr, core_winimage::FallbackInfoResult& res)
{
  DWORD section = 0;
  DWORD offset = 0;
  TCHAR szModule[MAX_PATH];
  szModule[0] = 0;
  GetLogicalAddress(addr, szModule, sizeof(szModule), section, offset );
  String s((uc2char*)szModule);
   int idx = s.lastIndexOf('\\');
   if(idx >= 0 && idx < s.length())  {
      idx++;
   } else {
      idx = 0;
   }

   res.module = new String((uc2char*)&szModule[idx]);
   res.section = section;
   res.offset = offset;
}

core_winimage::SymbolResult 
core_winimage::getSymbol(void *addr)
{
   
   BYTE symbolBuffer[sizeof(IMAGEHLP_SYMBOL) + SymbolMaxLength];
   memset(symbolBuffer, 0, sizeof(symbolBuffer));

   
   PIMAGEHLP_SYMBOL pSymbol = reinterpret_cast<PIMAGEHLP_SYMBOL>(symbolBuffer);
   pSymbol->SizeOfStruct = sizeof(IMAGEHLP_SYMBOL);
   pSymbol->MaxNameLength = SymbolMaxLength;                        
   
   DWORD symDisplacement = 0;  // Displacement of the input address,

   
   if ( _SymGetSymFromAddr(GetCurrentProcess(), 
                           reinterpret_cast<DWORD>(addr),
                           &symDisplacement, 
                           pSymbol) ) {
      SymbolResult res;
      res.result = true;
      
      char buf[4096];
      if(_UnDecorateSymbolName( pSymbol->Name, buf, sizeof(buf), 
                                UNDNAME_COMPLETE | UNDNAME_32_BIT_DECODE)) {
         res.symbol = SCS(buf);
      } else  {
         res.symbol = SCS(pSymbol->Name);
      }
      res.displacement = symDisplacement;
      return res;
   } else {
      SymbolResult res;
      res.result = false;
      return res;
   }
}

bool core_winimage::GetLogicalAddress(PVOID addr, 
                                     PTSTR szModule, 
                                     DWORD len, 
                                     DWORD& section, 
                                     DWORD& offset)
{
   
   MEMORY_BASIC_INFORMATION mbi;

   if (!VirtualQuery( addr, &mbi, sizeof(mbi)))
      return false;

   DWORD hMod = (DWORD)mbi.AllocationBase;
   if (!GetModuleFileName( (HMODULE)hMod, szModule, len ))
      return false;    

   PIMAGE_DOS_HEADER pDosHdr = (PIMAGE_DOS_HEADER)hMod;
   
   
   PIMAGE_NT_HEADERS pNtHdr = (PIMAGE_NT_HEADERS)(hMod + pDosHdr->e_lfanew);
   PIMAGE_SECTION_HEADER pSection = IMAGE_FIRST_SECTION( pNtHdr );
   DWORD rva = (DWORD)addr - hMod; 
   
   for ( unsigned i = 0; i < pNtHdr->FileHeader.NumberOfSections; i++, pSection++ )    
   {
      DWORD sectionStart = pSection->VirtualAddress;
      DWORD sectionEnd = sectionStart + 
        (pSection->SizeOfRawData < pSection->Misc.VirtualSize ? pSection->Misc.VirtualSize : pSection->SizeOfRawData);
      if ((rva >= sectionStart) && (rva <= sectionEnd)) {
         offset = rva - sectionStart;            
         return true;        
      }    
   }
   return false;   
}

class ToolHelp32Wrapper
{
  
};

bool 
core_winimage::getSourceLine(void* ptr, OUT(RString) source, OUT(int) line)
{

  IMAGEHLP_LINE lineInfo = { sizeof(IMAGEHLP_LINE) };
  DWORD dwLineDisplacement;
  if (_SymGetLineFromAddr(GetCurrentProcess(), (unsigned long)ptr,&dwLineDisplacement, &lineInfo ))
  {
    source = SCS(lineInfo.FileName);
    line = lineInfo.LineNumber;
    return true;
  }
  return false;
}

} // namespace sys 
} // namespace lang 
} // namespace acdk 


#endif
