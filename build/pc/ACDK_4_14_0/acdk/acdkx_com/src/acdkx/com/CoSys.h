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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/CoSys.h,v 1.16 2005/02/05 10:45:38 kommer Exp $


#ifndef acdkx_com_CoSys_h
#define acdkx_com_CoSys_h

#include <acdk.h>
#include <acdk/lang/String.h>
#include <acdk/lang/dmi/ScriptVar.h>

#include "Config.h"
#include <malloc.h>

inline wchar_t* convertAnsi2Unicode(const char* from, wchar_t* to, int len)
{
    if (0 == MultiByteToWideChar(CP_ACP, 0, from, len, to, len))
    {
      // check here
    }
    return to;
}

inline char* convertUnicode2Ansi(const wchar_t* from, char* to, int len)
{
  
    if (0 == WideCharToMultiByte(CP_ACP, 0, from, len, to, len, NULL, NULL))
    {
      // check here
    }
    return to;
}


#define CS2W(s) \
  ((s == 0) ? 0 : (_clength = strlen(s) + 1), convertAnsi2Unicode(s, (wchar_t*)core_alloca(_clength * 2), _clength))

#define S2W(s) \
  ((s == Nil) ? 0 : (uc2char*)s->uc2c_str())

#define W2CS(ws) \
  ((ws == 0) ? 0 : (_clength =  lstrlenW(ws) + 1), convertUnicode2Ansi(ws, (char*)core_alloca(_clength), _clength))

#define W2S(ws) new String(ws)

inline BSTR CS2BSTR(const char* cstr)  { int _clength; return (BSTR)::SysAllocString(CS2W(cstr)); } 
inline BSTR String2BSTR(IN(::acdk::lang::RString) str) { return ::SysAllocString((wchar_t*)str->convert(CCUcs2)->uc2c_str()); }


inline RString BSTR2S(BSTR ws) {  int _clength; return W2S(ws); }
  
#define S2OLE(s) ((OLECHAR*)S2W(s))
#define SC2OLE(s) SC2W(s)
#define OLE2S(bstr) W2S(bstr)
#define OLE2CS(bstr) W2CS(bstr)


#ifdef ACDK_OS_WIN32
# include <windows.h>
#endif

namespace acdkx {
namespace com {

void variants2ScriptVarArray(VARIANT* vars, int varcount, ::acdk::lang::dmi::ScriptVarArray& sv, bool unwrapAcdkObject);
::acdk::lang::dmi::ScriptVar variant2ScriptVar(const VARIANT& varg, bool unwrapAcdkObject);
void variants2ScriptVarArrayOut(VARIANT* vars, int varcount, acdk::lang::dmi::ScriptVarArray& sv, bool unwrapAcdkObject);
void variant2ScriptVarOut(const VARIANT& varg, acdk::lang::dmi::ScriptVar& sv, bool unwrapAcdkObjects);

void scriptVar2Variant(acdk::lang::dmi::ScriptVar& arg, VARIANT& dparam);
void scriptVarArray2Variants(acdk::lang::dmi::ScriptVarArray& args, VARIANT* dparams);
void scriptVar2Variant(acdk::lang::dmi::ScriptVar& arg, VARIANT& dparam);
void scriptVarArray2VariantsOut(acdk::lang::dmi::ScriptVarArray& args, VARIANT* dparams);

} // namespace com
} // namespace acdkx


#endif //acdkx_com_CoSys_h

