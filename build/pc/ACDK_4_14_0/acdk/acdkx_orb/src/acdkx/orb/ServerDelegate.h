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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/ServerDelegate.h,v 1.19 2005/02/05 10:45:39 kommer Exp $

#ifndef acdkx_orb_ServerDelegate_h
#define acdkx_orb_ServerDelegate_h

#include <acdkx/orb/orb.h>
#include <org/omg/CORBA/ORB.h>
#include <org/omg/CORBA/portable/Delegate.h>
#include <org/omg/CORBA/portable/ResponseHandler.h>

#include <acdkx/orb/AORB.h>
#include "OrbConnection.h"

namespace acdkx {
namespace orb {

USING_CLASS(::org::omg::CORBA::portable::, RemarshalException);
USING_CLASS(::org::omg::CORBA::portable::, ApplicationException);
USING_CLASS(::org::omg::CORBA::, NO_IMPLEMENT);

ACDK_DECL_CLASS(ServerDelegate);


/** 
  Implements the dynamic wrapper for CORBA interfaces
  
*/
class ACDKX_ORB_PUBLIC ServerDelegate
: extends ::org::omg::CORBA::portable::ObjectImpl,
  implements ::org::omg::CORBA::portable::Delegate
{
  ACDK_WITH_METAINFO(ServerDelegate)
private:
  ::org::omg::CORBA::RORB _orb;
  /** the remote this pointer */
  RObjectKey _objectKey;
  /** 
    used to store the current remote this pointer. 
    It only differs if Server replies with LOCATION_FORWARD message
    */
  RObjectKey _currentObjectKey;
  ROrbConnection _connection;
  /** this instance of ServerDelegate manages a client proxy */
  bool _isClient;
  /** 
    This server side instant should be permanent service and 
    will not be destroyed until ORB finished  
  */
  bool _permanent;
  
public:
  /** called if this is server-component */
  ServerDelegate();

  /** called if it is an client */
  ServerDelegate(IN(RString) theobjectref);

  ServerDelegate(IN(RORB) theOrb, IN(RString) theobjectref);

  ServerDelegate(IN(RORB) theOrb, IN(RObjectKey) theObject);

  ~ServerDelegate();
  virtual void finalize();
  RObjectKey objectKey() { return _currentObjectKey; }
  void setObjectKey(IN(RObjectKey) key) 
  { 
    _objectKey = _currentObjectKey = key; 
  }
    //::org::omg::CORBA::RInterfaceDef get_interface(::org::omg::CORBA::RObject self) = 0;
    
  virtual ::org::omg::CORBA::portable::RDelegate _get_delegate() { return this; }
  virtual void _set_delegate(IN(::org::omg::CORBA::portable::RDelegate) del) 
  { 
    THROW1(Exception, "ServerDelegate::_set_delegate() not supported");
  }

   //        InterfaceDef get_interface ();
  virtual bool is_nil();
  virtual ::org::omg::CORBA::RObject get_interface_def();
  /** does nothing, because internal reference counting will be used */
  virtual ::org::omg::CORBA::RObject duplicate();
  /** does nothing, because internal reference counting will be used */
  virtual void release();
  virtual bool is_a(IN(RString) logical_type_id);
  virtual bool non_existent();
  virtual bool is_equivalent(IN(::org::omg::CORBA::RObject) other_object);
  virtual int hash(int maximum);
  

  /** called by a stub to obtain an OutputStream for marshaling arguments. */
  ::org::omg::CORBA::portable::ROutputStream _request(IN(RString) operation, bool responseExpected);
  /** called by a stup */
  ::org::omg::CORBA::portable::RInputStream _invoke(IN(::org::omg::CORBA::portable::ROutputStream) output) 
    THROWS2(RApplicationException, RRemarshalException);
  virtual void _releaseReply(IN(::org::omg::CORBA::portable::RInputStream) input);
  virtual bool is_local();
  ::acdk::lang::Object* localObject() { return _currentObjectKey->localObject; }
  virtual RString toString();
  virtual ::org::omg::CORBA::RORB orb();

// Delegate
  virtual bool is_nil(::org::omg::CORBA::RObject self) { return is_nil(); }
  virtual ::org::omg::CORBA::RObject get_interface_def(IN(::org::omg::CORBA::RObject) self) 
  { 
    return get_interface_def(); 
  }
  virtual ::org::omg::CORBA::RObject duplicate(IN(::org::omg::CORBA::RObject) self)
  {
    return duplicate();
  }
  virtual void release(IN(::org::omg::CORBA::RObject) self) { release(); }
  virtual bool is_a(IN(::org::omg::CORBA::RObject) self, IN(RString) repository_id) { return is_a(repository_id); }
  virtual bool non_existent(IN(::org::omg::CORBA::RObject) self) { return non_existent(); }
  virtual bool is_equivalent(IN(::org::omg::CORBA::RObject) self, IN(::org::omg::CORBA::RObject) rhs) { return is_equivalent(rhs); }
  virtual int hash(IN(::org::omg::CORBA::RObject) self, int max) { return hash(max); }
  virtual ::org::omg::CORBA::portable::ROutputStream request(IN(::org::omg::CORBA::RObject) self,
                                                              IN(RString) operation,
                                                              bool responseExpected)
  {
    return _request(operation, responseExpected);
  }

  virtual ::org::omg::CORBA::portable::RInputStream orb_invoke(IN(::org::omg::CORBA::RObject) self,
                                                            IN(::org::omg::CORBA::portable::ROutputStream) os) 
    THROWS2(RApplicationException, RRemarshalException)
  {
    return _invoke(os);
  }

  virtual void releaseReply(IN(::org::omg::CORBA::RObject) self, IN(::org::omg::CORBA::portable::RInputStream) is)
  {
    _releaseReply(is);
  }
  
  virtual ::org::omg::CORBA::RORB orb(IN(::org::omg::CORBA::RObject) self)
  {
    return orb();
  }
  virtual bool is_local(IN(::org::omg::CORBA::RObject) self)  
  { 
    return _currentObjectKey->isLocal();
  }
  /**
    returns name of type id
  */
  virtual RString get_typeid();
  virtual RString toString(IN(::org::omg::CORBA::RObject) self) { return toString(); }
  virtual void write(::org::omg::CORBA::portable::OutputStream& out);
  static RServerDelegate read(::org::omg::CORBA::portable::InputStream& in, IN(RClass) clz = Nil);

  
  static RServerDelegate string_to_object(IN(RString) str);
  static RString object_to_string(IN(::org::omg::CORBA::RObject) obj);
  bool isClient() { return _isClient; }
  void isClient(bool isclient) { _isClient = isclient; }
};

} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_ServerDelegate_h
