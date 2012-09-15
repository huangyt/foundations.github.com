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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/ServerDelegate.cpp,v 1.19 2005/02/05 10:45:39 kommer Exp $

#include "ServerDelegate.h"
#include "RequestOutputStream.h"
#include "OrbConnectionMgr.h"

namespace acdkx {
namespace orb {

USING_CLASS(::org::omg::CORBA::, NO_IMPLEMENT);

using ::org::omg::CORBA::portable::ObjectImpl;

ServerDelegate::ServerDelegate()
: ObjectImpl(),
  _objectKey(),
  _currentObjectKey(),
  _isClient(false),
  _permanent(false)
{
  _objectKey = new ObjectKey(this);
  _currentObjectKey = _objectKey;
}

ServerDelegate::ServerDelegate(IN(RString) theobjectref)
: ObjectImpl(),
  _objectKey(ObjectKey::string_to_object(theobjectref)),
  _currentObjectKey(_objectKey),
  _isClient(true),
  _permanent(false)
{
}

ServerDelegate::ServerDelegate(IN(RORB) theOrb, IN(RString) theobjectref)
: ObjectImpl(),
  _orb(theOrb),
  _objectKey(new ObjectKey(theobjectref)),
  _currentObjectKey(_objectKey),
  _isClient(true),
  _permanent(false)
{
}

ServerDelegate::ServerDelegate(IN(RORB) theOrb, IN(RObjectKey) theObject)
: ObjectImpl(),
  _orb(theOrb),
  _objectKey(theObject),
  _currentObjectKey(_objectKey),
  _isClient(true),
  _permanent(false)
{
}

//virtual 
void 
ServerDelegate::finalize()
{
  if (_isClient == false)
    return;
  // ### send disconnect to server
}

ServerDelegate::~ServerDelegate()
{
}

//virtual 
bool 
ServerDelegate::is_nil()
{
  return _currentObjectKey->localObject == 0;
}

//virtual 
::org::omg::CORBA::RObject 
ServerDelegate::get_interface_def() 
{
  THROW0(NO_IMPLEMENT);
  return Nil;
}


//virtual 
::org::omg::CORBA::RObject 
ServerDelegate::duplicate()
{
  /* intentionally do nothing */
  return this;
}

//virtual 
void 
ServerDelegate::release()
{
  /* intentionally do nothing */
}

//virtual 
bool 
ServerDelegate::is_a(IN(RString) repository_id)
{
  return false;
}

//virtual 
bool 
ServerDelegate::non_existent()
{
  return false;
}

//virtual 
RString 
ServerDelegate::get_typeid() 
{ 
  return "IDL:" + getClass()->getName() + ":1.0"; 
}

//virtual 
bool 
ServerDelegate::is_equivalent(IN(::org::omg::CORBA::RObject) rhs)
{
  RServerDelegate other;
  if (instanceof(rhs, ServerDelegate) == true) {
    other = RServerDelegate(rhs);
  } else  if (instanceof(rhs, ::org::omg::CORBA::portable::ObjectImpl) == true) {
    ::org::omg::CORBA::portable::RDelegate td = ::org::omg::CORBA::portable::RObjectImpl(rhs)->_get_delegate();
    if (instanceof(td, ServerDelegate) == false)
      return false;
    other = RServerDelegate(td);
    
  } 
  if (other->_currentObjectKey == _currentObjectKey)
    return true;
  if (other->_currentObjectKey->object_key->equals((RObject)_currentObjectKey->object_key) == true)
    return true;
  return false;
}

//virtual 
int 
ServerDelegate::hash(int max)
{
  if (_currentObjectKey->isLocal() == true) {
    if (_currentObjectKey->localObject == 0)
      return 0;
    return ((::acdk::lang::Object*)_currentObjectKey->localObject)->hashCode() % max;
  }
  return (int)_currentObjectKey->localObject % max;
}

//virtual 
::org::omg::CORBA::portable::ROutputStream 
ServerDelegate::_request(IN(RString) operation, bool responseExpected) 
{
  RRequestOutputStream reqout = new RequestOutputStream(_objectKey, operation, responseExpected);
  reqout->setConnection(_connection);
  reqout->writeHeader();
  return &reqout;
}

//virtual 
::org::omg::CORBA::portable::RInputStream 
ServerDelegate::_invoke(IN(::org::omg::CORBA::portable::ROutputStream) os) 
                                               THROWS2(RApplicationException, RRemarshalException)
{
  RRequestOutputStream reqout = (RRequestOutputStream)os;
  try { 
    if (_connection == Nil)
    {
      _connection = OrbConnectionMgr::getMgr()->getConnection(this);
      reqout->setConnection(_connection);
    }
    ::org::omg::CORBA::portable::RInputStream inp = reqout->send();

    
    return inp;
  } catch (::org::omg::CORBA::portable::RRemarshalException ex) {
    RObjectKey okey = (RObjectKey)ex->objectKey();
    _currentObjectKey = okey;
    if (ex->replaceObjectKey() == true)
      _objectKey = _currentObjectKey;
    throw;
  }
}

//virtual 
void 
ServerDelegate::_releaseReply(IN(::org::omg::CORBA::portable::RInputStream) is) 
{
  _currentObjectKey = _objectKey;
}

//virtual 
bool 
ServerDelegate::is_local()
{
  return AORB::getAORB().ignoreLocal == false &&  _currentObjectKey->isLocal() == true;
}


//virtual 
::org::omg::CORBA::RORB 
ServerDelegate::orb() 
{
  return _orb;
}



//virtual 
RString 
ServerDelegate::toString()
{
  return _currentObjectKey->toString();
}


//virtual 
void 
ServerDelegate::write(::org::omg::CORBA::portable::OutputStream& out)
{
  _currentObjectKey->toIOR();
  _currentObjectKey->ior.write(out);
}

//static 
RServerDelegate 
ServerDelegate::read(::org::omg::CORBA::portable::InputStream& in, IN(RClass) clz /*= Nil*/)
{
  RObjectKey objectkey = new ObjectKey();
  objectkey->ior.read(in);
  objectkey->ior_inited = true;
  objectkey->fromIOR();
  objectkey->fromObjectKey();

  /* don't use, otherwise the wrong Proxy will be 
     created
  if (objectkey->isLocal() == true) 
  {
    RServerDelegate serverobj(objectkey->localObject);
    return serverobj;
  }
  */
  
  return AORB::createProxy(objectkey);
}

//static 
RServerDelegate 
ServerDelegate::string_to_object(IN(RString) str)
{
  RObjectKey key = ObjectKey::string_to_object(str);
  if (key->isLocal() == true && AORB::getAORB().ignoreLocal == false)
    return key->localObject;
  return AORB::createProxy(key);
}

//static 
RString 
ServerDelegate::object_to_string(IN(::org::omg::CORBA::RObject) obj)
{
  if (instanceof(obj, ServerDelegate) == false)
    THROW1(Exception, "can only handle Object of type ServerDelegate");

  return RServerDelegate(obj)->objectKey()->object_to_string();
}

} // namespace orb 
} // namespace acdkx 


