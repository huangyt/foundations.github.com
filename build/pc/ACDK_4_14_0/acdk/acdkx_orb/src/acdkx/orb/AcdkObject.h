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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AcdkObject.h,v 1.14 2005/03/14 17:59:13 kommer Exp $

#ifndef acdkx_orb_AcdkObject_h
#define acdkx_orb_AcdkObject_h

#include <acdk.h>
#include <acdkx/orb/orb.h>
#include <acdkx/orb/ServerDelegate.h>

#include <org/omg/CORBA/ServerRequest.h>
#include <org/omg/CORBA/portable/ResponseHandler.h>
#include <org/omg/CORBA/portable/RemarshalException.h>
#include <org/omg/CORBA/portable/ApplicationException.h>
#include <org/omg/CORBA/portable/InvokeHandler.h>
#include <acdk/lang/dmi/DmiObject.h>

#include "AcdkObjectInterface.h"

namespace acdkx {
namespace orb {

USING_CLASS(::acdk::lang::dmi::, DmiObject);

ACDK_DECL_CLASS(AcdkObject);

/**
  This class is a server side CORBA DSI Wrapper to Acdk Objects
*/
class ACDKX_ORB_PUBLIC AcdkObject
: extends ::acdkx::orb::ServerDelegate
, implements ::org::omg::CORBA::portable::InvokeHandler
, implements ::acdkx::orb::AcdkObjectInterface
{
public: 
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); }
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return &_clazzInfo; } 
  virtual ::acdk::lang::RClass getClass(); 
  static ::acdk::lang::RClass GetClass(); 
  static ::acdk::lang::RObject create_instance();
  static ::acdk::lang::RObject create_array(int length = 0); 
  static ::acdk::lang::RObject create_array_array(int firstLength = 0, int secondLength = 0); 
  virtual void getCollectableFields(FieldReferences& fields); 
  virtual ::acdk::lang::dmi::SysFields getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz = 0); 
  virtual const ::acdk::lang::dmi::ClazzMethodInfo* standardDispatch(IN(RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0, const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0); 
  static const ::acdk::lang::dmi::ClazzMethodInfo* StandardDispatch(IN(RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0, const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0); 
private:   
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo; 
  
private:
  /** is not Nil in case of Proxy to acdk object */
  ::acdk::lang::RObject _acdkObject;
  /** is not Nil in case of factory class */
  const ::acdk::lang::dmi::ClazzInfo* _factoryClazz;
public:

  AcdkObject(IN(::acdk::lang::RObject) localObject);
  AcdkObject(const ::acdk::lang::dmi::ClazzInfo* factoryClazz);

  virtual ::org::omg::CORBA::portable::ROutputStream _invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& input, ::org::omg::CORBA::portable::ResponseHandler& handler) THROWS1(::org::omg::CORBA::RSystemException);
  
  /**
    Handles CORBA standard methods like _is_a and DMI over IIOP
  */
  ::org::omg::CORBA::portable::ROutputStream standard_invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& input, ::org::omg::CORBA::portable::ResponseHandler& handler)  THROWS1(::org::omg::CORBA::RSystemException);
  
  /**
    interface from AcdkObjectInterface
    They are not implemented, because already handled in standard_invoke()
  */
  virtual ::acdk::lang::RObject get_cor_factory(IN(RString) classname);
  virtual acdk::lang::RObject dyn_new(IN(RString) classname, IN(RString) constructor, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp);
  virtual RDmiObject dyn_invoke(IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp);
  virtual RDmiObject dyn_invoke_static(IN(RString) classname, IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp);
  virtual RDmiObject dyn_peek(IN(RString) membername);
  virtual RDmiObject dyn_peek_static(IN(RString) classname, IN(RString) membername);
  virtual void dyn_poke(IN(RString) membername, IN(RDmiObject) value);
  virtual void dyn_poke_static(IN(RString) classname, IN(RString) membername, IN(RDmiObject) value);
  /// using dii 
  ::org::omg::CORBA::portable::ROutputStream dii_invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& input, 
                                                        ::org::omg::CORBA::portable::ResponseHandler& handler);
};

} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_AcdkObject_h
