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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/BasicArrayInl.h,v 1.5 2005/04/17 11:20:11 kommer Exp $

/*
has to included after Class.h
*/

#ifndef acdk_lang_sys_BasicArrayInl_h
#define acdk_lang_sys_BasicArrayInl_h

template <class T>
inline
acdk::lang::dmi::ClazzInfo*
BasicArray<T>::clazzInfo()
  {
      T dummy(0);
 #ifdef ACDK_ALT_CALL_TEMPLATED_FUNCTION
    return ::acdk::lang::Class::getSingeltonArrayClazz(::acdk::lang::dmi::ClazzInfo::getBasicTypeClazz<T>(dummy));
#else
    //return ::acdk::lang::Class::getSingeltonArrayClazz(::acdk::lang::dmi::ClazzInfo::getBasicTypeClazz(dummy));
    return ::acdk::lang::Class::getSingeltonArrayClazz(::acdk::lang::dmi::getBasicTypeClazz<T>(dummy));
#endif //ACDK_ALT_CALL_TEMPLATED_FUNCTION
  }

template <class T>
inline
::acdk::lang::RClass
BasicArray<T>::GetClass()
  {
    T dummy(0);
#ifdef ACDK_ALT_CALL_TEMPLATED_FUNCTION
    return ::acdk::lang::Class::getSingeltonArrayClass(::acdk::lang::dmi::ClazzInfo::getBasicTypeClazz<T>(dummy));
#else
    return ::acdk::lang::Class::getSingeltonArrayClass(::acdk::lang::dmi::getBasicTypeClazz<T>(dummy));
#endif //ACDK_ALT_CALL_TEMPLATED_FUNCTION
  }

template <class T>
inline
::acdk::lang::RClass
BasicArray<T>::getClass()
  {
    T dummy(0);
#ifdef ACDK_ALT_CALL_TEMPLATED_FUNCTION
    return ::acdk::lang::Class::getSingeltonArrayClass(::acdk::lang::dmi::ClazzInfo::getBasicTypeClazz<T>(dummy));
#else
    //return ::acdk::lang::Class::getSingeltonArrayClass(::acdk::lang::dmi::ClazzInfo::getBasicTypeClazz<T>(dummy));
  return ::acdk::lang::Class::getSingeltonArrayClass(::acdk::lang::dmi::getBasicTypeClazz<T>(dummy));
#endif //ACDK_ALT_CALL_TEMPLATED_FUNCTION
  }

template <class T>
inline
::acdk::lang::RClass
BasicArray<T>::getMemberClass()
  {
    T dummy(0);
#ifdef ACDK_ALT_CALL_TEMPLATED_FUNCTION
    return ::acdk::lang::Class::getSingeltonClass(::acdk::lang::dmi::ClazzInfo::getBasicTypeClazz<T>(dummy));
#else
    return ::acdk::lang::Class::getSingeltonClass(::acdk::lang::dmi::getBasicTypeClazz<T>(dummy));
#endif //ACDK_ALT_CALL_TEMPLATED_FUNCTION

  }

#if !defined(ACDK_NOMETAINFO)

EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, length);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, get);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, operator_bo_bc);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, getref);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, set);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, ensureCapacity);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, resize);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, append);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, insert);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, remove);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, findFirst);
EXTERN_EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, findLast);

template <class T>
inline
const ::acdk::lang::dmi::ClazzMethodInfo*
BasicArray<T>::lookupFunction(IN(acdk::lang::RString) fname_,
                                                           ::acdk::lang::dmi::ScriptVarArray& args,
                                                           int flags,
                                                            const ::acdk::lang::dmi::ClazzMethodInfo* methinf
                                                           )
  {
    if (flags & dmi::MiIvViaHash)
    {
      int hv = ACDK_CAST_PTR2INT(methinf);
      const acdk::lang::dmi::ClazzInfo* tclazz = clazzInfo();
      for (int i = 0; i < tclazz->getMethodsCount(); ++i)
      {
        if (tclazz->methods[i]->getMethodSignatureHashValue() == hv)
          return tclazz->methods[i];
      }
    }
    else
    {
      String& fname = *fname_;
     

      if (fname.equals(lit_length) == true)
        return &BasicArray_method_length_I;
      if (fname.equals(lit_get) == true)
        return &BasicArray_method_get_I_D;
      if (fname.equals(lit_operator_bo_bc) == true)
        return &BasicArray_operator_bo_bc_I_D;
      if (fname.equals(lit_getref) == true)
        return &BasicArray_method_getref_I_D;
      if (fname.equals(lit_set) == true)
        return &BasicArray_method_set_I_D_V;
      if (fname.equals(lit_ensureCapacity) == true)
        return &BasicArray_method_ensureCapacity_I_V;
      if (fname.equals(lit_resize) == true)
        return &BasicArray_method_resize_I_V;
      if (fname.equals(lit_append) == true)
        return &BasicArray_method_append_D_V;
      if (fname.equals(lit_insert) == true)
        return &BasicArray_methods_insert_I_D_V;
      if (fname.equals(lit_remove) == true)
        return &BasicArray_method_remove_I_V;
      if (fname.equals(lit_findFirst) == true)
        return &BasicArray_method_findFirst_D_I;
      if (fname.equals(lit_findLast) == true)
        return &BasicArray_method_findLast_D_I;
    }
    const acdk::lang::dmi::ClazzInfo* tclazz = clazzInfo();
    return ::acdk::lang::dmi::StdDispatch::lookupMethod( tclazz, fname_, args,  Nil
                                                        , ::acdk::lang::dmi::AcdkDmiClient::getDmiClient()
                                                        , ::acdk::lang::dmi::MiPublic);
  }

template <class T>
inline
const ::acdk::lang::dmi::ClazzMethodInfo*
BasicArray<T>::standardDispatch(IN(acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret,
                                                         ::acdk::lang::dmi::ScriptVarArray& args,
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs, int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    /*
    if (clazzinfo == 0)
      clazzinfo = clazzInfo();
    if (methinf == 0 || flags & dmi::MiIvViaHash)
      methinf = ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, dc, flags, methinf);
    */
    if (methinf == 0 || flags & dmi::MiIvViaHash)
      methinf = lookupFunction(fname, args, flags, methinf);
    if (methinf == 0)
    {
      ::acdk::lang::RString msg = ::acdk::lang::RString("Cannot find matching function for BaseArray::") + fname;
      ::acdk::lang::ObjectBase::_throwException(msg->c_str());
    }
    if (&BasicArray_method_length_I == methinf) {
      ret = length();
      return methinf;
    }
    if (&BasicArray_method_get_I_D == methinf) {
      ret = get(args[0].getIntVar());
      return methinf;
    }
    if (&BasicArray_method_getref_I_D == methinf || &BasicArray_operator_bo_bc_I_D == methinf)
    {
      ret = outOf(get(args[0].getIntVar()));
      return methinf;
    }
    if (&BasicArray_method_set_I_D_V == methinf) {
      T dummy(0);
      set(args[0].getIntVar(), args[1].getBasicType(dummy));
      return methinf;
    }
    if (&BasicArray_method_ensureCapacity_I_V == methinf) {
      ensureCapacity(args[0].getIntVar());
      return methinf;
    }
    if (&BasicArray_method_resize_I_V == methinf) {
      resize(args[0].getIntVar());
      return methinf;
    }
    if (&BasicArray_method_append_D_V == methinf) {
      T dummy(0);
      append(args[0].getBasicType(dummy));
      return methinf;
    }
    if (&BasicArray_methods_insert_I_D_V == methinf) {
      T dummy(0);
      insert(args[1].getIntVar(), args[1].getBasicType(dummy));
      return methinf;
    }
    if (&BasicArray_method_remove_I_V == methinf) {
      remove(args[0].getIntVar());
      return methinf;
    }
    if (&BasicArray_method_findFirst_D_I == methinf) 
    {
       T dummy(0);
       ret = inOf(findFirst(args[0].getBasicType(dummy)));
      return methinf;
    }
    if (&BasicArray_method_findLast_D_I == methinf) 
    {
       T dummy(0);
       ret = inOf(findLast(args[0].getBasicType(dummy)));
      return methinf;
    }
    if ((methinf = ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::standardDispatch(fname, ret, args, dc, namedArgs, flags, Object::clazzInfo(), methinf)) != 0)
      return methinf;
    return StandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  }
#endif //!defined(ACDK_NOMETAINFO)

#endif //acdk_lang_sys_BasicArray_h
