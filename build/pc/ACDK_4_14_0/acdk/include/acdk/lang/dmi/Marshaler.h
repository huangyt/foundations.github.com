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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/Marshaler.h,v 1.11 2005/03/06 11:57:19 kommer Exp $

#ifndef acdk_lang_dmi_Marshaler_h
#define acdk_lang_dmi_Marshaler_h

#include <acdk.h>
#include <acdk/lang/sys/RefHolderExt.h>

namespace acdk {
namespace lang {
namespace dmi {

/**
  Abstract base class for general purpose marshaling.

  @see StandardMarshaler For local standard marshaling  
*/
foreign 
class ACDK_CORE_PUBLIC Marshaler
{
public:
  /**
    The marshalign code passes this to the un/marshaler method
    to indicate, on what communcation side for which purpose the 
    argument should be marshaled.
  */
  enum MarshalMode
  {
    Unknown = 0,
    /** The client sends data */
    SendClient,
    /** The Server send data to the client */
    SendServer,
    /** The client receives data */
    ReceiveClient,
    /** The server receives Data */
    ReceiveServer,
    /** Client in and out data */
    SendReceiveClient,
    /** Server in and out data */
    SendReceiveServer
  };
  
  virtual ~Marshaler() {} 
  /**
    Marshals the given obj on basis of given class into the outstore.
    @param cls The Class, which should be used to marshal
    @param obj the Object instance, which should be marshaled.
    @param outstore the streamed storage
    @param mode On which side and for which purpose the method will be called
    @throw ObjectStreamException
  */
  virtual void marshal(IN(RClass) cls, IN(RObject) obj, byteArray& outstore, MarshalMode mode = Unknown) = 0;
  /**
    Unmarshals the on basis of given Class an object using instore.
    @param cls The template to use
    @param instore the buffer, from home to read
    @param mode On which side and for which purpose the method will be called
    @param cachedObject If the interface cache the object, it may can be reused by the marshaler
    @return The Object 
    @throw ObjectStreamException
  */
  virtual RObject unmarshal(IN(RClass) cls, byteArray& instore, MarshalMode mode = Unknown, IN(RObject) cachedObject = Nil) = 0;
  
};

/**
  If a BYVAL-call is used locally, the standard marshaler is used to make copies of the 
  objects.
*/
foreign 
class ACDK_CORE_PUBLIC StandardMarshaler
: public Marshaler
{
  RObject _obj;
public:
  
	virtual void marshal(IN(RClass) cls, IN(RObject) obj, byteArray& outstore, MarshalMode mode = Unknown);
	virtual RObject unmarshal(IN(RClass) cls, byteArray& instore, MarshalMode mode = Unknown, IN(RObject) cachedObject = Nil);
  static StandardMarshaler gInstance;
};

/**
  
*/
template <class T>
class TMarshaler
{
protected:
  byteArray _data;
  Marshaler& _marshaler;
  
public:
  TMarshaler(Marshaler& marshaler = StandardMarshaler::gInstance)
	: _data(0)
  , _marshaler(marshaler)
	{
	}
protected:
	~TMarshaler()
	{
	}
	RClass getTClass()
  {
    return ::acdk::lang::Class::getSingeltonClass(T::clazzInfo());
  }
};


/**
  Marshal Holder for sending data from Client to Server.
  T is type of the RefHolder family
  and contains and Objects which supports 
  serialization.
*/
 
template <class T>
class TSendMarshaler
: public TMarshaler<T>
{
  T _rh;
public:
  TSendMarshaler(Marshaler& marshaler = StandardMarshaler::gInstance)
  : TMarshaler<T>(marshaler)
  {
  }
	TSendMarshaler(const T& rh, Marshaler& marshaler = StandardMarshaler::gInstance)
  : TMarshaler<T>(marshaler)
  , _rh() // don't take copy, because otherwise it will not copied.
  {
    TSendMarshaler<T>::_marshaler.marshal(TSendMarshaler<T>::getTClass(), (::acdk::lang::RObject)rh, TSendMarshaler<T>::_data, Marshaler::SendClient);
	}
	~TSendMarshaler()
	{
		// nothing here
	}
  T getT()
  {
    if (_rh != Nil)
      return _rh;
    _rh = (T)TSendMarshaler<T>::_marshaler.unmarshal(TSendMarshaler<T>::getTClass(), TSendMarshaler<T>::_data, Marshaler::SendServer);
    return _rh;
  }
  T operator ()()
  {
    return TSendMarshaler<T>::getT(); 
  }
	operator T () 
	{ 
		return TSendMarshaler<T>::getT();
	}
  T operator->() { return operator T(); }
};



/**
  Marshal Holder for receiving data from server
  T is type of RefHolder family
*/
template <class T>
class TReceiveMarshaler
: public TMarshaler<T>
{
  T _serverObject;
  T& _clientObject;
public:	
  TReceiveMarshaler(T& rh, Marshaler& marshaler = StandardMarshaler::gInstance)
  : TMarshaler<T>(marshaler)
  , _serverObject()
  , _clientObject(rh)
  {
		
	}
// gcc 3.1 doesn't like this?
// private:
  TReceiveMarshaler(const TReceiveMarshaler<T>& other);
public:
	~TReceiveMarshaler()
	{
    if (_serverObject != Nil)
      TReceiveMarshaler<T>::_marshaler.marshal(TReceiveMarshaler<T>::getTClass(), (::acdk::lang::RObject)_serverObject, TReceiveMarshaler<T>::_data, Marshaler::ReceiveServer);
    else
      _clientObject = (T)TReceiveMarshaler<T>::_marshaler.unmarshal(TReceiveMarshaler<T>::getTClass(), TReceiveMarshaler<T>::_data, Marshaler::ReceiveClient, (::acdk::lang::RObject)_clientObject);
	}
  TReceiveMarshaler<T>& operator=(const T& other)
  {
    TReceiveMarshaler<T>::_marshaler.marshal(TReceiveMarshaler<T>::getTClass(), (::acdk::lang::RObject)other, TReceiveMarshaler<T>::_data, Marshaler::ReceiveServer);
    return *this;
  }
};


/**
  Marshal Holder for sending data from client to server and back
  T is type of RefHolder family
*/
template <class T>
class TSendReceiveMarshaler
: public TMarshaler<T>
{
  T& _rrh;
public:
  TSendReceiveMarshaler(T& rh, Marshaler& marshaler = StandardMarshaler::gInstance)
  : TMarshaler<T>(marshaler)
  , _rrh(rh)
  {
    TSendReceiveMarshaler<T>::_marshaler.marshal(TSendReceiveMarshaler<T>::getTClass(), (::acdk::lang::RObject)_rrh, TSendReceiveMarshaler<T>::_data, Marshaler::SendReceiveClient);
	}
private:
  TSendReceiveMarshaler(const TReceiveMarshaler<T>& other)
  : TMarshaler<T>()
  ,  _rrh(other._rrh)
  {
  }
public:
	~TSendReceiveMarshaler()
	{
    _rrh = (T)TSendReceiveMarshaler<T>::_marshaler.unmarshal(TSendReceiveMarshaler<T>::getTClass(), TSendReceiveMarshaler<T>::_data, Marshaler::SendReceiveClient, (::acdk::lang::RObject)_rrh);
	}
  operator T ()
  {
    return (T)TSendReceiveMarshaler<T>::_marshaler.unmarshal(TSendReceiveMarshaler<T>::getTClass(), TSendReceiveMarshaler<T>::_data, Marshaler::SendReceiveClient, (::acdk::lang::RObject)_rrh);
  }
  TSendReceiveMarshaler<T>& operator=(const T& other)
  {
    TSendReceiveMarshaler<T>::_marshaler.marshal(TSendReceiveMarshaler<T>::getTClass(), (::acdk::lang::RObject)other, TSendReceiveMarshaler<T>::_data, Marshaler::SendReceiveServer);
    return *this;
  }
};




} // namespace dmi
} // namespace lang
} // namespace acdk

#endif // acdk_lang_dmi_Marshaler_h

