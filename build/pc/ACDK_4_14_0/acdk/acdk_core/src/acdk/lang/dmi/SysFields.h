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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/SysFields.h,v 1.21 2005/03/07 13:53:14 kommer Exp $

#ifndef acdk_lang_dmi_SysFields_h
#define acdk_lang_dmi_SysFields_h


#include <acdk/lang/sys/core_vector.h>

//template <class T> class RObjectArrayImpl; 
template <class T> class RBasicArray; 
template <class T, class S> class RThrowableHolder;



namespace acdk {
namespace lang {
namespace dmi {

class ClazzFieldInfo;

class ScriptVar;

/** 
  Representing internal accessor to
  classes fields.
  Must not exported with ACDK_CORE_PUBLIC, because is 
  template
*/
foreign 
class SysField 
{
public:
  //int flags;
  //int offset;
  //char typeChar;
  enum FieldType {
    FT_Void,
    FT_Bool,
    FT_Char,
    FT_UcChar,
    //FT_Uc4Char,
    FT_Byte,
    FT_Short,
    FT_Int,
    //FT_Long,
    FT_JLong, // 64 bit
    FT_Float,
    FT_Double,
    FT_Object
  };

  union FieldContent 
  {
    void* vval;
    bool* zval;
    char* cval;
    ucchar* ucval;
    //uc4char* uc4val;
    byte* bval;
    short* sval;
    int* ival;
    //long* lval;
    float* fval;
    double* dval;
    jlong *jlval;
    RObject* oval;
    
  };
  //RObject* oval;
  //const char* name;
  FieldType type;
  FieldContent cont;
  ClazzFieldInfo* fieldInfo;

  ACDK_CORE_PUBLIC SysField()
  : type(FT_Void), 
    fieldInfo(0)
  {
    cont.jlval = 0;
  }
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const bool* ptr)
    : type(FT_Bool),
      fieldInfo(fi)
  {
    cont.zval = const_cast<bool*>(ptr);
  }
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const char* ptr)
    : type(FT_Char),
      fieldInfo(fi)
  {
    cont.cval = const_cast<char*>(ptr);
  }
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const ucchar* ptr)
    : type(FT_UcChar),
      fieldInfo(fi)
  {
    cont.ucval = const_cast<ucchar*>(ptr);
  }
  /*
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const uc4char* ptr)
    : type(FT_Uc4Char),
      fieldInfo(fi)
  {
    cont.uc4val = const_cast<uc4char*>(ptr);
  }*/
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const byte* ptr)
    : type(FT_Byte),
      fieldInfo(fi)
  {
    cont.bval = const_cast<byte*>(ptr);
  }
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const short* ptr)
    : type(FT_Short),
      fieldInfo(fi)
  {
    cont.sval = const_cast<short*>(ptr);
  }
  
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const int* ptr)
    : type(FT_Int),
      fieldInfo(fi)
      
  {
    cont.ival = const_cast<int*>(ptr);
  }
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const jlong* ptr)
    : type(FT_JLong),
      fieldInfo(fi)
  {
    cont.jlval = const_cast<jlong*>(ptr);
  }
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const float* ptr)
    : type(FT_Float),
      fieldInfo(fi)
  {
    cont.fval = const_cast<float*>(ptr);
  }
  ACDK_CORE_PUBLIC SysField(ClazzFieldInfo* fi, const double* ptr)
    : type(FT_Double),
      fieldInfo(fi)
  {
    cont.dval = const_cast<double*>(ptr);
  }
  template <class T>
  SysField(ClazzFieldInfo* fi, const RefHolder<T>* ptr, FieldType typ = FT_Object)
    : type(typ),
      fieldInfo(fi)
  {
    cont.oval = reinterpret_cast<RefHolder<Object>*>( const_cast<RefHolder<T>*>(ptr) );
  }
  template <class T>
  SysField(ClazzFieldInfo* fi, const InterfaceHolder<T>* ptr, FieldType typ = FT_Object)
    : type(typ),
      fieldInfo(fi)
  {
    cont.oval = reinterpret_cast<RefHolder<Object>*>( const_cast<InterfaceHolder<T>*>(ptr) );
  }
  /* RObjectArray not known here
  template <class T>
  SysField(ClazzFieldInfo* fi, const RObjectArrayImpl<T>* ptr, FieldType typ = FT_Object)
    : type(typ),
      fieldInfo(fi)
  {
    cont.oval = reinterpret_cast<RefHolder<Object>*>( const_cast<RObjectArrayImpl<T>*>(ptr) );
  }
  template <class T>
  SysField(ClazzFieldInfo* fi, const RBasicArray<T>* ptr, FieldType typ = FT_Object)
    : type(typ),
      fieldInfo(fi)
  {
    cont.oval = reinterpret_cast<RefHolder<Object>*>( const_cast<RBasicArray<T>*>(ptr) );
  }*/
  /** 
    this group set the the value in the field. 
    if type is wrong or no instance pointer throws exception 
  */
  ACDK_CORE_PUBLIC void set(bool val);
  ACDK_CORE_PUBLIC void set(char val);
  ACDK_CORE_PUBLIC void set(ucchar val);
  //ACDK_CORE_PUBLIC void set(uc4char val);
  ACDK_CORE_PUBLIC void set(byte val);
  ACDK_CORE_PUBLIC void set(short val);
  ACDK_CORE_PUBLIC void set(int val);
  ACDK_CORE_PUBLIC void set(jlong val);
  ACDK_CORE_PUBLIC void set(float val);
  ACDK_CORE_PUBLIC void set(double val);
  ACDK_CORE_PUBLIC void set(IN(RObject) val);
  /**
    @return the current field
    @param flags can be MiAiOut [| Modifier::inParam ] to
           to retrieve LValue
  */
  ACDK_CORE_PUBLIC ScriptVar getScriptVar(int flags);
  ACDK_CORE_PUBLIC void setScriptVar(const ScriptVar& var);
  /**
    @return an initialized SysField using ClazzFieldInfo as type and ScriptVar as value
  */
  ACDK_CORE_PUBLIC static SysField getField(const ClazzFieldInfo* fieldinfo, ScriptVar& sv);

};

typedef ::acdk::lang::sys::core_vector<SysField> SysFields;

} // dmi
} // lang
} // acdk

#endif //acdk_lang_dmi_SysFields_h

