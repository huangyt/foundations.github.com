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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ClazzInfoInternals.h,v 1.17 2005/04/18 16:03:42 kommer Exp $

#ifndef acdk_lang_dmi_ClazzInfoInternals_h
#define acdk_lang_dmi_ClazzInfoInternals_h

#include "../Exception.h"

/* this file will be included in the unit_clazzinfo.cpp files */

#include "ClazzAttributesRes.h" 
#include <acdk/io/ObjectReader.h> // for instantiation purposes

#define ACDK_DMI_STANDARDDISPATCH_FORWARD() \
do { \
  bool __forwarded = false; \
  ::acdk::lang::dmi::StdDispatch* __forwardobj = this; \
  while ((__forwardobj = __forwardobj->getDmiTarget(__forwarded, clazzinfo)) != 0 && __forwarded == true) \
    ; \
  if (__forwarded == true) \
    return __forwardobj->standardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf); \
} while (false)


#define ACDK_PROXY_WITH_METAINFO(ClassName) \
  public: \
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); } \
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo(); \
static ::acdk::lang::RClass GetClass() { return ::acdk::lang::Class::getSingeltonClass(clazzInfo()); } \
private:   \
  friend struct ClassName##_DmiProxy_MetainfoWrapper;

namespace acdk {
namespace lang {
namespace dmi {
inline void getRef(ScriptVar& sv, bool& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getBoolRef();  }
/// @internal
inline void getRef(ScriptVar& sv, char& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getCharRef();  }
/// @internal
inline void getRef(ScriptVar& sv, ucchar& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getUcCharRef();  }
/// @internal
inline void getRef(ScriptVar& sv, byte& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getByteRef();  }
/// @internal
inline void getRef(ScriptVar& sv, short& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getShortRef();  }
/// @internal
inline void getRef(ScriptVar& sv, int& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getIntRef();  }
/// @internal
inline void getRef(ScriptVar& sv, jlong& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getLongRef();  }
/// @internal
inline void getRef(ScriptVar& sv, float& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getFloatRef();  }
/// @internal
inline void getRef(ScriptVar& sv, double& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getDoubleRef();  }
/// @internal
template <class T> 
inline void getRef(ScriptVar& sv, RefHolder<T>& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) 
{ 
  b = ::acdk::lang::getTypedObjectRef<RefHolder<T> >(sv);  
}
/// @internal
template <class T> 
inline void getRef(ScriptVar& sv, InterfaceHolder<T>& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) 
{ 
  b = ::acdk::lang::getTypedObjectRef<InterfaceHolder<T> >(sv); 
}
template <class T> 
inline void getRef(ScriptVar& sv, RObjectArrayImpl<T>& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) 
{ 
  b = ::acdk::lang::getTypedObjectRef<RObjectArrayImpl<T> >(sv); 
}

inline const ClazzInfo* clazzInfoOf(bool b) { return ClazzInfo::getBoolClazz(); }
inline const ClazzInfo* clazzInfoOf(char b) { return ClazzInfo::getCharClazz(); }
inline const ClazzInfo* clazzInfoOf(ucchar b) { return ClazzInfo::getUcCharClazz(); }
inline const ClazzInfo* clazzInfoOf(byte b) { return ClazzInfo::getByteClazz(); }
inline const ClazzInfo* clazzInfoOf(short b) { return ClazzInfo::getShortClazz(); }
inline const ClazzInfo* clazzInfoOf(int b) { return ClazzInfo::getIntClazz(); }
inline const ClazzInfo* clazzInfoOf(jlong b) { return ClazzInfo::getLongClazz(); }
inline const ClazzInfo* clazzInfoOf(float b) { return ClazzInfo::getFloatClazz(); }
inline const ClazzInfo* clazzInfoOf(double b) { return ClazzInfo::getDoubleClazz(); }

template <class T>
inline const ClazzInfo* clazzInfoOf(RefHolder<T>& b) { return T::clazzInfo(); }
template <class T>
inline const ClazzInfo* clazzInfoOf(InterfaceHolder<T>& b) { return T::clazzInfo(); }

/*
template <typename T>
inline T castTo(const ScriptVar& cv, DmiClient& dc, const ClazzMethodArgInfo* ai)
{
  T tmp;
  ScriptVar tcv = cv;
  dc.castTo(tcv, ai->type);
  get(tcv, tmp, dc.getScriptVarCastFlags(), ai->type);
  return tmp;
}

template <typename T>
inline T castTo(const ScriptVar& cv, DmiClient& dc, const ClazzInfo* type)
{
  T tmp;
  ScriptVar tcv = cv;
  dc.castTo(tcv, type);
  get(tcv, tmp, dc.getScriptVarCastFlags(), type);
  return tmp;
}
*/

/**
  represents a universal 0/Nil initializer
  used in template code to initialize value variable
*/
class UniversalNil
{
public:
  inline UniversalNil(){}
  inline operator bool() const { return false; }
  inline operator char() const { return 0; }
  inline operator ucchar() const { return 0; }
  inline operator byte() const { return 0; }
  inline operator short() const { return 0; }
  inline operator int() const { return 0; }
  inline operator jlong() const { return 0; }
  inline operator float() const { return 0.0; }
  inline operator double() const { return 0.0; }
  inline operator NilRef() const { return Nil; }
  inline operator RString() const { return Nil; }
  template <class T> inline operator RefHolder<T>() { return Nil; }
  template <class T> inline operator InterfaceHolder<T>() { return Nil; }
  template <class T> inline operator RBasicArray<T>() { return Nil; }
  template <class T> inline operator RObjectArrayImpl<T>() { return Nil; }
  template <class T, class OT> inline operator ThrowableHolder<T, OT>() { return Nil; }
  
};

/*
  cast a ScriptVar to a type t using the given DmiClient
*/
template <typename T>
inline T castTo(ScriptVar& cv, DmiClient& dc)
{
  //UniversalNil uNil;
  T tmp = UniversalNil();
  const ClazzInfo* type = clazzInfoOf(tmp);
  dc.castTo(cv, type);
  ::acdk::lang::dmi::get(cv, tmp, dc.getScriptVarCastFlags(), type);
  return tmp;
}




template <typename T>
inline T& castToObjectRef(ScriptVar& cv, DmiClient& dc)
{
  return ::acdk::lang::getTypedObjectRef<T>(cv);
}

inline bool& castToBoolRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getBoolRef();  
}

inline char& castToCharRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getCharRef();  
}
inline ucchar& castToUcCharRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getUcCharRef();  
}
inline byte& castToByteRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getByteRef();  
}

inline short& castToShortRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getShortRef();  
}

inline int& castToIntRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getIntRef();  
}

inline jlong& castToLongRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getLongRef();  
}

inline float& castToFloatRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getFloatRef();  
}


inline double& castToDoubleRef(ScriptVar& sv, DmiClient& dc) 
{ 
  return sv.getDoubleRef();  
}

/*
inline void getRef(ScriptVar& sv, ucchar& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getUcCharRef();  }
inline void getRef(ScriptVar& sv, byte& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getByteRef();  }
inline void getRef(ScriptVar& sv, short& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getShortRef();  }
inline void getRef(ScriptVar& sv, int& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getIntRef();  }
inline void getRef(ScriptVar& sv, jlong& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getLongRef();  }
inline void getRef(ScriptVar& sv, float& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getFloatRef();  }
inline void getRef(ScriptVar& sv, double& b, short castFlags = SVCastStdFlags, const ClazzInfo* type = 0) { b = sv.getDoubleRef();  }
*/

}
}
}

#endif //acdk_lang_dmi_ClazzInfoInternals_h

