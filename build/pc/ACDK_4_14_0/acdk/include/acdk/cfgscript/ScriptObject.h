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

#ifndef acdk_cfgscript_ScriptObject_h
#define acdk_cfgscript_ScriptObject_h


#include "Props.h"
#include <acdk/io/CharReader.h>
#include <acdk/lang/ref/SharedOwning.h>

namespace acdk {
namespace cfgscript {

ACDK_DECL_CLASS(ScriptObject);


struct PEStack;
#if 0

ACDK_DECL_CLASS(ScriptFunction);

class ACDK_CFGSCRIPT_LIB_PUBLIC ScriptFunction
: extends ::acdk::lang::Object
{
public:
  /**
    @see acdk::lang::dmi::MetaInfoFlags
  */
  int flags;
  RString name;
  RStringArray args;
  
  RString fname;
  int startLine;
  // text to execute
  RString text;
  int readEval(IN(::acdk::io::RCharReader) in, PEStack& stack);
};

#endif //0

/**
  Simple Script Object
  @see gw_ref[acdk_cfgscript_hb].

*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiCiWeakBind))
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CFGSCRIPT_LIB_PUBLIC ScriptObject
: extends ::acdk::lang::Object
{
  //ACDK_WITH_METAINFO(ScriptObject)
private:
  bool _castRecGuard;
public:
  //int flags;
  //RString name;
  RProps _locals;
  RObjectArray _dmiProxies;
  //acdk::lang::ref::SharedOwning _sharedRefs;
protected:
  RObject _superObject;
  bool _finalizeCalled;
public:
  const acdk::lang::dmi::ClazzInfo* _scriptClazzInfo;
  /** holds the derived object if any */
  RScriptObject _derivedObject;
  //int _proxyCount;
  
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo();
  /**
    reimplemented from StdDispatch
    Will be called by a DmiProxy to choose native super implementation or to call overloaded script implementation
  */
  bool isDmiOverLoaded(const acdk::lang::dmi::ClazzInfo* ci, IN(RString) funcname, const acdk::lang::dmi::ClazzMethodInfo* mi, acdk::lang::dmi::ClazzMethodArgInfo** const args);
  foreign Object* getDmiTarget(bool& forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci) { forwarded = true; return this; }
public:
  ScriptObject(const acdk::lang::dmi::ClazzInfo* ci);
  ~ScriptObject();
  virtual Object* _cast(const ::acdk::lang::dmi::ClazzInfo* ci);
  virtual void getCollectableFields(FieldReferences& fields);
private:
  RScriptObject getMostDerived();
  virtual Object* _castFromTopMost(const ::acdk::lang::dmi::ClazzInfo* ci);
  virtual Object* _castFromObject(IN(RObject) o, const ::acdk::lang::dmi::ClazzInfo* ci);
public:
  bool _gc_releaseRef(bool force = false) const;
  void finalize();
  void setSuperObject(IN(RObject) obj);
  INOUT(RObject) getSuperObject() { return _superObject; }
  void setImplementation(IN(RScriptObject) impl);
  bool isDirectSuper(const ::acdk::lang::dmi::ClazzInfo* ci);
  /**
    read eval global module
  */
  int readEval(IN(::acdk::io::RCharReader) in);

   const acdk::lang::dmi::ClazzMethodInfo* 
  standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/);
private:
  const acdk::lang::dmi::ClazzMethodInfo* 
  standardDispatchFromTopMost(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/);
public:
  static const ::acdk::lang::dmi::ClazzMethodInfo* static_dispatch(IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
   static const ::acdk::lang::dmi::ClazzMethodInfo* dispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
  /*
  static const ::acdk::lang::dmi::ClazzMethodInfo* delegate_dispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
  */
  static const ::acdk::lang::dmi::ClazzMethodInfo* abstract_method_dispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
   static bool _isDmiProxyInterface(const acdk::lang::dmi::ClazzInfo* ci);
   static bool _isScriptInterface(const acdk::lang::dmi::ClazzInfo* ci);
   void _invokeFinalize();
   void _setFinalized();
   RObject  _findSuperByClazz(const acdk::lang::dmi::ClazzInfo* clazzinfo);

    
   
};




} // namespace cfgscript
} // namespace acdk 
  
#endif //acdk_cfgscript_ScriptObject_h
