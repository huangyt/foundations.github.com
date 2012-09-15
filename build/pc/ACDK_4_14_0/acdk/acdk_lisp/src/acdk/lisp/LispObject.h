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


#ifndef acdk_lisp_LispObject_h
#define acdk_lisp_LispObject_h

#include "LispCode.h"
#include "LispClass.h"


namespace acdk {
namespace lisp {


ACDK_DECL_CLASS(LispObject);

class ACDK_ACDK_LISP_PUBLIC LispObject
: extends LispVar
, implements Function
, implements ::acdk::io::Serializable
{
public:
  /// not using the standard meta info, becuase this class is used as a proxy 
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); } 
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return &_clazzInfo; } 
  virtual ::acdk::lang::RClass getClass() { return GetClass(); }
  static ::acdk::lang::RClass GetClass() { return ::acdk::lang::Class::getSingeltonClass(clazzInfo()); }
  static ::acdk::lang::RObject create_array(int length = 0) { return Nil; }
  static ::acdk::lang::RObject create_array_array(int firstLength = 0, int secondLength = 0) { return Nil; }
  virtual void getCollectableFields(FieldReferences& fields) { }
  virtual ::acdk::lang::dmi::SysFields getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz = 0) 
  { return ::acdk::lang::dmi::SysFields(); }
  static const ::acdk::lang::dmi::ClazzMethodInfo* dynamic_dispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
  static const ::acdk::lang::dmi::ClazzMethodInfo* static_dispatch(IN(RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
 
private:   
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo;
  
  static RLispList _definition;
  RLispClass _class;
  ::acdk::util::RHashMap _slots;
  /// initialize default values from Class defintion
  void initObject(IN(RLispEnvironment) env, IN(RLispClass) cls);
  // DMI interface
  void initObject(IN(RString) classname, IN(NamedArgs) args);
  bool setSlotByInitArg(IN(RLispEnvironment) env, IN(RLispClass) cls, IN(RString) initarg, IN(RLispVar) val);

public:
  /// for serialization
  static RObject create_instance() { return new LispObject(Nil); }
  /**
    DMI interface
  */
  LispObject(IN(RString) classname, IN(NamedArgs) args = NamedArgs());

  LispObject(IN(RLispEnvironment) env, IN(RLispClass) cls)
  : _class(cls)
  , _slots(new ::acdk::util::HashMap())
  {
    initObject(env, _class);
  }
  RLispClass getLispClass() { return _class; }
  RLispVar getSlot(IN(RString) str) 
  {
    return RLispVar(_slots->get(&str));
  }
  void setSlot(IN(RString) str, IN(RLispVar) lv)
  {
    _slots->put(&str, &lv);
  }
  bool setSlotByInitArg(IN(RLispEnvironment) env,IN(RString) initarg, IN(RLispVar) val)
  {
    return setSlotByInitArg(env, _class, initarg, val);
  }

  foreign virtual RString toString();
  foreign virtual RString toCode();
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc)
  {
    THROW1(CloneNotSupportedException, "LispObject::clone");
    return Nil;
  }

  // Function
  virtual RString functionName() { return "<LispObject>"; }
  /**
    args[0] == the symbol of function
    args[1 + n] == the arguments
  */
  virtual RLispVar eval(IN(RLispEnvironment) env, IN(RLispList) args);
  virtual RString getHelpText() { return "invokes a object method"; }
  virtual RLispList getDefinition();
  virtual RLispList getDeclDefinition() { return getDefinition(); }
};


} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispObject_h
