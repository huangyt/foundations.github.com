// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_AalObject_h
#define acdk_aal_AalObject_h

#include "../aci/Compiler.h"
#include "../aci/EvalEnv.h"


namespace acdk {
namespace aal {

USING_CLASS(acdk::aci::, DClazzInfo);
USING_CLASS(acdk::aci::, DClazzFieldInfo);
USING_CLASS(acdk::aci::, EvalEnv);

struct AalObjectField
{
  const acdk::lang::dmi::ClazzFieldInfo* _fi;
  acdk::lang::dmi::ScriptVar _val;
  AalObjectField(const acdk::lang::dmi::ClazzFieldInfo* fi, const acdk::lang::dmi::ScriptVar& val = acdk::lang::dmi::ScriptVar())
  : _fi(fi)
  , _val(val)
  {
  }
};

ACDK_DECL_CLASS(AalObject);

/**
  Represents the DMI object for classes defined with AAL
*/
class ACDK_AAL_PUBLIC AalObject
: extends acdk::lang::Object
{
protected:
  RDClazzInfo _dclazzInfo;
  typedef AalObjectField FieldEntry;
  typedef acdk::lang::sys::core_vector<FieldEntry> FieldsMap;
  FieldsMap _fields;
  RObject _baseObject;
public:
  virtual acdk::lang::dmi::ClazzInfo* getClazzInfo() { return _dclazzInfo->getImplClazzInfo(); }
  ::acdk::lang::RClass getClass()  { return ::acdk::lang::Class::getSingeltonClass(getClazzInfo()); }
  //static ::acdk::lang::RClass GetClass() { 

  AalObject(IN(RDClazzInfo) dclazzInfo);
  virtual ::acdk::lang::dmi::SysFields getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz = 0);
  static const acdk::lang::dmi::ClazzMethodInfo* StandardDispatch(IN(RString) fname, 
                                                          acdk::lang::dmi::ScriptVar& ret, 
                                                          acdk::lang::dmi::ScriptVarArray& args, 
                                                          acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const acdk::lang::dmi::ClazzMethodInfo* methinf);
  virtual  const acdk::lang::dmi::ClazzMethodInfo* standardDispatch(IN(RString) fname, 
                                                          acdk::lang::dmi::ScriptVar& ret, 
                                                          acdk::lang::dmi::ScriptVarArray& args, 
                                                          acdk::lang::dmi::DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const acdk::lang::dmi::ClazzInfo* clazzinfo = 0,
                                                          const acdk::lang::dmi::ClazzMethodInfo* methinf = 0);
  static const ::acdk::lang::dmi::ClazzFieldInfo* FieldAccessor(
                                    ::acdk::lang::Object* This, 
                                      IN(acdk::lang::RString) fname, 
                                      ::acdk::lang::dmi::ScriptVar& var, 
                                      ::acdk::lang::dmi::DmiClient& dc,
                                      int flags,
                                      const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                      const ::acdk::lang::dmi::ClazzFieldInfo* fieldinf);
  static void executeFunctionCall(IN(acdk::aci::REvalEnv) env, IN(acdk::aci::RExecutableArray) oca, bool throwExceptions);
  
  static bool isAalClazz(const acdk::lang::dmi::ClazzInfo* ci);
  void setGetField(IN(acdk::lang::RString) fname, 
                                      ::acdk::lang::dmi::ScriptVar& var, 
                                      ::acdk::lang::dmi::DmiClient& dc,
                                      int flags,
                                      const ::acdk::lang::dmi::ClazzInfo* ci,
                                      const ::acdk::lang::dmi::ClazzFieldInfo* fi);
  
protected:
  void _initFields();
  foreign virtual void _initFields(const acdk::lang::dmi::ClazzInfo* ci);

};

/*
class ACDK_AAL_PUBLIC ExtendedAalObject
: extends AalObject
{
protected:
  RObject _baseObject;
public:
  ExtendedAalObject(IN(RDClazzInfo) dclazzInfo);

  static const acdk::lang::dmi::ClazzMethodInfo* StandardDispatch(const char* fname, 
                                                          acdk::lang::dmi::ScriptVar& ret, 
                                                          acdk::lang::dmi::ScriptVarArray& args, 
                                                          acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const acdk::lang::dmi::ClazzMethodInfo* methinf);

protected:
  foreign void _initFields(const acdk::lang::dmi::ClazzInfo* ci);
  void _initFields();
};

*/

} // aal
} // acdk


#endif //acdk_aal_AalObject_h

