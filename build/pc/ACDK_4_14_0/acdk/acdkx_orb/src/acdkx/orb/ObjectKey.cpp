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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/ObjectKey.cpp,v 1.23 2005/02/05 10:45:39 kommer Exp $

#include <acdk.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>
#include <acdk/net/InetAddress.h>
#include <acdk/net/Socket.h>
#include <acdk/net/ServerSocket.h>
#include <acdk/lang/Thread.h>

#include "AORB.h"
#include <org/omg/CORBA/OrbExceptions.h>
#include <org/omg/CORBA/GIOP/GIOP.h>
#include <org/omg/CORBA/IOP/IOP.h>
#include <org/omg/CORBA/IIOP/IIOP.h>

#include "CDRObjectReader.h"
#include "GIOPMessage.h"
#include "AServerRequestImpl.h"
#include "ServerDelegate.h"
#include <ctype.h>

namespace acdkx {
namespace orb {

USING_CLASS(::org::omg::CORBA::, ORB);
USING_CLASS(::acdk::net::, Socket);
USING_CLASS(::acdk::net::, ServerSocket);
USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Writer);

USING_CLASS(::acdk::io::, MemWriter);
USING_CLASS(::acdk::io::, MemReader);


ObjectKey::ObjectKey(IN(RString) str) // ## param str will be ignored?
: Object(),
  ior_inited(false),
  port(0),
  pid(-1),
  _isLocal(false),
  _isAcdkObject(false),
  _skel(0)
  
{
    
}


ObjectKey::ObjectKey(ServerDelegate* impl)
: Object(),
  ior_inited(false),
  port(0),
  pid(-1),
  localObject(impl),
  _isLocal(true),
  _isAcdkObject(false),
  _skel(0)
{
  /*
  doesn't work, because vtable dispatch doesn't work in constructors
  const ClazzInfo* clazz = impl->getClass()->objectClazzInfo();
  type_id = impl->getClass()->getName();
  */
  version.major = 1;
  version.minor = 2;
}


ObjectKey::ObjectKey(::org::omg::CORBA::GIOP::MessageHeader& messageHeader, ::org::omg::CORBA::GIOP::RequestHeader& requestHeader)
: Object(),
  ior_inited(false),
  port(0),
  pid(-1),
  localObject(0),
  _isLocal(false),
  _isAcdkObject(false),
  _skel(0)
{
  if (messageHeader.version.minor < 2) {
    object_key = core_octet_array_to_byteArray(requestHeader.object_key);
    fromObjectKey();
  } else {
    switch (requestHeader.target_address.addressingDisposition) {
    case ::org::omg::CORBA::GIOP::TargetAddress::KeyAddr:
      object_key = core_octet_array_to_byteArray(requestHeader.target_address.object_key);
      fromObjectKey();
      break;
    case ::org::omg::CORBA::GIOP::TargetAddress::ProfileAddr : 
      if (requestHeader.target_address.profile.tag != ::org::omg::CORBA::IOP::TAG_INTERNET_IOP)
        THROW1(Exception, "type of Address not supported");
      fromInterIOPData(requestHeader.target_address.profile.profile_data);
      break;
    case ::org::omg::CORBA::GIOP::TargetAddress::ReferenceAddr:
      ior = requestHeader.target_address.ior.ior;
      ior_inited = true;
      fromIOR();
      break;
    }
  }
}

bool 
ObjectKey::isNil() 
{
  return type_id == Nil || type_id->length() == 0;
}

RString
toHexaString(IN(RbyteArray) ca)
{
  char hex[] = 
  { 
    '0', '1', '2', '3', '4', '5', '6', '7', '8',
		'9', 'a', 'b', 'c', 'd', 'e', 'f' 
  };
  int count = ca->length();
  StringBuffer str(2 * count);
  for(int i = 0; i < count; i++) {	
	  str.append(hex[(byte)((ca[i] & 0xF0 ) >> 4)]) ;		
	  str.append(hex[(byte)(ca[i] & 0x0F)]);		
	}
	return str.toString();
}


inline 
byte
hex2byte (char c)
{
  if (isdigit(c))
    return (byte) (c - '0');
  else if (islower (c))
    return (byte) (10 + c - 'a');
  else
    return (byte) (10 + c - 'A');
}

//public 
RbyteArray
fromHexaString(IN(RString) str)
{
  const char* data = str->c_str();
  RbyteArray buffer = new byteArray(str->length() / 2);
  for(int i = 0, j = 0 ; i< str->length() / 2; i++) {	
    int high = hex2byte(data[j++]);
		int low = hex2byte(data[j++]);
	  buffer[i] = (byte)(16 * high + low);
  }
  return buffer;
}


RObject 
ObjectKey::getLocalObject()
{
  return localObject;
}

RString 
ObjectKey::object_to_string()
{
  MemWriter buffer;
  CDRObjectWriter oout(&buffer, AORB::getORB());
  oout.write_boolean(org::omg::CORBA::portable::naturualEndian == org::omg::CORBA::portable::LittleEndian);
  toIOR();
  ior.write(oout);
  RString tstr = new String(RcharArray(buffer.getBuffer()));
  
  RString hexa = toHexaString(buffer.getBuffer());;
  System::out->println("IOR:" + hexa );
  return "IOR:" + hexa;

}

//static 
RObjectKey
ObjectKey::string_to_object(IN(RString) stringified)
{
  RString str = stringified;
  if (str->startsWith("IOR:") == false) {
    THROW1(Exception, "unknown object reference: " + str); //### look which excpetion
  }
  str = str->substr(4);
  MemReader buffer(fromHexaString(str));
  CDRObjectReader oin(&buffer, ORB::init());
  if (oin.read_boolean() == true) 
    oin.setEndian(org::omg::CORBA::portable::LittleEndian);
  else
    oin.setEndian(org::omg::CORBA::portable::BigEndian);
  RObjectKey okey = new ObjectKey();
  okey->ior.read(oin);
  okey->ior_inited = true;
  okey->fromIOR();
  okey->fromObjectKey();
  return okey;
}

void 
ObjectKey::fromObjectKey()
{
  /*
  if (object_key == Nil)
    THROW1(Exception, "ObjectKey::fromObjectKey(): object_key is not set");
    */
  if (object_key == Nil) // isNil object??
    return;
  if (object_key.length() < 4)
    return;
  MemReader buffer(object_key);
  CDRObjectReader oin(&buffer, ORB::init());
  if (oin.read_char() != 'A')
    return; //### throw ex
  if (oin.read_char() != 'C')
    return;
  if (oin.read_char() != 'D')
    return;
  if (oin.read_char() != 'K')
    return;
  network = oin.read_string();
  port = oin.read_long();
  pid = oin.read_long();
  type_id = oin.read_string(); 
  int curid = Process::getProcessId();
  if (pid == curid)
    _isLocal = true;
  localObject = (ServerDelegate*)oin.read_long();
  _isAcdkObject = true;
}

void 
ObjectKey::toObjectKey()
{
  if (object_key != Nil)
    return;
  
  if (isLocal() == true) {
    MemWriter buffer;
    CDRObjectWriter oout(&buffer, ORB::init());
    oout.write_char_array("ACDK", 0, 4);
    network = AORB::getLocalHost(); 
    oout.write_string(network);
    port = AORB::getLocalPort();
    oout.write_long(port);
    pid = Process::getProcessId();
    oout.write_long(pid);
    type_id = localObject->get_typeid();
    oout.write_string(type_id);
    oout.write_long((int)localObject);
    object_key = buffer.getBuffer();
    _isAcdkObject = true;
  } 

}


void
ObjectKey::fromInterIOPData(sequence<octet>& profile_data)
{
   
  MemReader buffer(RbyteArray((const byte*)profile_data.data(), profile_data.size()));
  CDRObjectReader oin(&buffer, ORB::init());
  oin.setEndian(  oin.read_octet() == 1 
                ?  org::omg::CORBA::portable::LittleEndian
                :  org::omg::CORBA::portable::BigEndian
                );

  ::org::omg::CORBA::IIOP::ProfileBody pbody;
  pbody.read(oin);
  network = pbody.host;
  port = pbody.port;
  version.major = pbody.iiop_version.major;
  version.minor = pbody.iiop_version.minor;
  object_key = new byteArray(pbody.object_key.data(), pbody.object_key.size());
}

void
ObjectKey::fromIOR()
{
  if (ior_inited == false)
    THROW1(Exception, "called ObjectKey::fromIOR() but ior is not set");
  
  type_id = ior.type_id;
  /*
  if (type_id->startsWith("IDL:") == false || type_id->endsWith(":1.0") == false)
    THROW1_FQ(::org::omg::CORBA::, INV_OBJREF, "type_id has wrong format (should be \"IDL:type:1.0\"): " + type_id);
  */
  if (type_id->startsWith("IDL:") == true && type_id->endsWith(":1.0") == true)
  {
    type_id = type_id->substr(4);
    type_id = type_id->substr(0, type_id->length() - 4);
  }
  for (int i = 0; i < ior.profiles.size(); i++) {
    if (ior.profiles[i].tag == ::org::omg::CORBA::IOP::TAG_INTERNET_IOP) {
      fromInterIOPData(ior.profiles[i].profile_data);
      break;
    }
  }
}

void 
ObjectKey::toIOR()
{
  if (ior_inited == true)
    return;
  toObjectKey();
  
  ior.type_id = "IDL:" + type_id + ":1.0";

  ::org::omg::CORBA::IIOP::ProfileBody pbody;
  pbody.iiop_version.major = version.major;
  pbody.iiop_version.minor = version.minor;
  pbody.host = network;
  pbody.port = port;
  int i;

  for (i = 0; i < object_key->length(); i++) 
    pbody.object_key.push_back(object_key[i]);

  MemWriter buffer;
  CDRObjectWriter oout(&buffer, ORB::init());
  oout.setEndian(org::omg::CORBA::portable::naturualEndian);
  oout.write_boolean(org::omg::CORBA::portable::naturualEndian == org::omg::CORBA::portable::LittleEndian);
  pbody.write(oout);
  
  ::org::omg::CORBA::IOP::TaggedProfile pt;
  pt.tag = ::org::omg::CORBA::IOP::TAG_INTERNET_IOP;
  int lenght =  buffer.getBuffer()->length();
  const byte* data = buffer.getBuffer()->data();

  for (i = 0; i < lenght; i++)
    pt.profile_data.push_back(data[i]);
  ior.profiles.push_back(pt);
  
  ior_inited = true;
}

struct RepIdEntry
{
  const acdk::lang::dmi::ClazzInfo* type;
  const char* name;
  short vmajor;
  short vminor;
  RepIdEntry* next;
  RepIdEntry(const acdk::lang::dmi::ClazzInfo* ci, const char* nam, short maj, short mini)
  : type(ci)
  , name(nam)
  , vmajor(maj)
  , vminor(mini)
  , next(0)
  {
  }
};

static RepIdEntry* _root = 0;

//static 
void
ObjectKey::registerRepId(const acdk::lang::dmi::ClazzInfo* ci, const char* name, short major, short minor)
{
  RepIdEntry* ne = new RepIdEntry(ci, name, major, minor);
  ne->next = _root;
  _root = ne;
}



RString
ObjectKey_getFqClassName(IN(RString) str) // ### move
{
  if (str->equals("InterfaceRepository") == true)
    return "org/omg/CORBA/Repository";
  if (str->equals("NameService") == true)
    return "org/omg/CosNaming/NamingContext";
  
  RString ret = str->replace('.', '/');
  if (ret->startsWith("omg/org/") == true)
    ret = ret->replace("omg/org/", "org/omg/");
  RepIdEntry* re = _root;
  while (re != 0)
  {
    if (ret->equals(re->name) == true)
      return Class::getSingeltonClass(re->type)->getName();
    re = re->next;
  }
  return ret;
}

//static 
RString 
ObjectKey::classNameFromRepId(IN(RString) str)
{
  if (str->startsWith("IDL:") == false)
    return ObjectKey_getFqClassName(str);
  RString ret = str->substr(4);
  ret = ret->substr(0, ret->lastIndexOf(":"));
  return ObjectKey_getFqClassName(ret);
}



} // namespace orb
} // namespace acdkx
