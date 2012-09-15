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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/CorObject.h,v 1.17 2005/03/14 17:59:14 kommer Exp $

#ifndef acdkx_orb_CorObject_h
#define acdkx_orb_CorObject_h

#include <acdk.h>
#include <acdkx/orb/orb.h>
#include <acdkx/orb/ServerDelegate.h>

#include <org/omg/CORBA/ServerRequest.h>
#include <org/omg/CORBA/portable/ResponseHandler.h>
#include <org/omg/CORBA/portable/RemarshalException.h>
#include <org/omg/CORBA/portable/ApplicationException.h>
#include "AcdkObjectInterface.h"

namespace acdkx {
namespace orb {

ACDK_DECL_CLASS(CorObject);

/**
  This class represents a Corba object used as generic client proxy object
*/
class ACDKX_ORB_PUBLIC CorObject
: extends ::acdkx::orb::ServerDelegate
, implements acdkx::orb::AcdkObjectInterface
{
protected:
  /** 
    used only in create_instance which itself needed for dynamic skeleton
  */
  CorObject();
  /**
    _objectClazzInfo represents the type library of
    the remote object
  */
  const ::acdk::lang::dmi::ClazzInfo* _objectClazzInfo;
  virtual bool isDmiOverLoaded(const acdk::lang::dmi::ClazzInfo* ci, IN(RString) funcname, const acdk::lang::dmi::ClazzMethodInfo* mi, acdk::lang::dmi::ClazzMethodArgInfo** const args);
public:
  static RObject create_instance() { return new CorObject(); }

  CorObject(IN(RString) stringifiedref);
  CorObject(IN(RObjectKey) objectkey, IN(RORB) orb);
  
  
  void setRemoteClazz(const ::acdk::lang::dmi::ClazzInfo* oclazz) { _objectClazzInfo =  oclazz; }
  void setRemoteClass(IN(RClass) cls) { setRemoteClazz(cls->objectClazzInfo()); }
  void setRemoteClass(IN(RString) classname) { setRemoteClass(Class::forName(classname)); }

  /** not using the standard meta info, becuase this class is used as a proxy */
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); } 
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return &_clazzInfo; } 
  virtual ::acdk::lang::RClass getClass() { return GetClass(); }
  static ::acdk::lang::RClass GetClass() { return ::acdk::lang::Class::getSingeltonClass(clazzInfo()); }
  static ::acdk::lang::RObject create_array(int length = 0) { return Nil; }
  static ::acdk::lang::RObject create_array_array(int firstLength = 0, int secondLength = 0) { return Nil; }
  virtual void getCollectableFields(FieldReferences& fields) { }
  virtual ::acdk::lang::dmi::SysFields getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz = 0) { return ::acdk::lang::dmi::SysFields(); }
  virtual  const ::acdk::lang::dmi::ClazzMethodInfo* standardDispatch(  IN(RString) fname, 
                                                          ::acdk::lang::dmi::ScriptVar& ret, 
                                                          ::acdk::lang::dmi::ScriptVarArray& args, 
                                                          ::acdk::lang::dmi::DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0,
                                                          const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0);
  static const ::acdk::lang::dmi::ClazzMethodInfo* StandardDispatch(  IN(RString) fname, 
                                                                      ::acdk::lang::dmi::ScriptVar& ret, 
                                                                      ::acdk::lang::dmi::ScriptVarArray& args, 
                                                                      ::acdk::lang::dmi::DmiClient& dc, 
                                                                      IN(::acdk::lang::RStringArray) namedArgs, 
                                                                      int flags, 
                                                                      const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0,
                                                                      const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0);
 
private:   
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo;
  
public:
  static ::acdk::lang::dmi::AcdkDmiClient _dmiClient;

  CorObject(RORB theorb, RObjectKey objKey)
  : ServerDelegate(theorb, objKey)
  , _objectClazzInfo(0)
  {
  }
  
  /** 
    @return true if this object is a ACDK object
            In this case the DMI over IIOP can be used

  */
  bool isAcdkObject();
  virtual acdk::lang::dmi::DmiClient& getDmiClient(){ return _dmiClient; }
  /**
    create a dynamic proxy for this object.
    Only possible, if the remote object is a ACDK class
    The returned object can be casted to a type of cls
    if no DmiProxy is available for this class method returns with Nil
  */
  RObject createDmiProxy(IN(RClass) cls);

  void setAcdkTypeLibrary(IN(RString) classname);

  // interface from AcdkObjectInterface
  virtual RObject get_cor_factory(IN(RString) classname);
  virtual RObject dyn_new(IN(RString) classname, IN(RString) constructor, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp);
  virtual RDmiObject dyn_invoke(IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp);
  virtual RDmiObject dyn_invoke_static(IN(RString) classname, IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp);
  virtual RDmiObject dyn_peek(IN(RString) membername);
  virtual RDmiObject dyn_peek_static(IN(RString) classname, IN(RString) membername);
  virtual void dyn_poke(IN(RString) membername, IN(RDmiObject) value);
  virtual void dyn_poke_static(IN(RString) classname, IN(RString) membername, IN(RDmiObject) value);
};

/** 
  use this to declare a generic interface proxy implementation
  The ClassName is an interface, which delegates all 
  calls to the dmi invoke interface
*/
#define ACDK_DECL_CORPROXY(ClassName, export) \
ACDK_DECL_CLASS(ClassName); \
class export ClassName##Proxy \
: extends ::acdkx::orb::CorObject \
, implements ClassName \
{ \
public: \
  IRObjectProxy(IN(::acdkx::orb::RCorObject) obj) \
  : ACDK_FQ_SUPER_QUALIFIER(::acdkx::orb::, CorObject)(obj->objectKey(), obj->orb()) \
{ \
    setRemoteClazz(ClassName ::clazzInfo()); \
} \
}

} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_CorObject_h
