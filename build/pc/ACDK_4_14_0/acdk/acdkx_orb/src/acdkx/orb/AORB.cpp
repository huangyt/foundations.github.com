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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AORB.cpp,v 1.36 2005/04/30 20:18:41 kommer Exp $

#include <acdk.h>
#include <acdk/lang/CmdLineParser.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/net/InetAddress.h>
#include <acdk/net/Socket.h>
#include <acdk/net/TCPSocket.h>
#include <acdk/net/SocketException.h>
#include <acdk/net/URL.h>

#include <acdk/lang/Thread.h>
#include <acdk/lang/Short.h>

#include "AORB.h"
#include <org/omg/CORBA/GIOP/GIOP.h>
#include <org/omg/CORBA/IIOP/IIOP.h>

#include "CDRObjectReader.h"
#include "GIOPMessage.h"
#include "AServerRequestImpl.h"
#include "ServerDelegate.h"
#include "CorObject.h"

namespace acdkx {
namespace orb {

#if defined(LOCAL_DEBUG)
# define DOUT(msg) sys::coreout << msg << sys::eofl
#else
# define DOUT(msg) do { } while(false)
#endif

USING_CLASS(::org::omg::CORBA::, ORB);
USING_CLASS(::acdk::net::, Socket);
USING_CLASS(::acdk::net::, ServerSocket);
USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Writer);

static SkelInfoClassesStruct* _proxyRoot = 0;
static SkelInfoClassesStruct* _proxyTail = 0;

RegisterSkelInfoClass::RegisterSkelInfoClass(SkelInfoClassesStruct* proxy)
: _proxy(proxy)
{
  if (_proxyRoot == 0) {
    _proxyTail = _proxyRoot = proxy;
  } else {
    _proxyTail->next = proxy;
    _proxyTail = proxy;
  }
}

RegisterSkelInfoClass::~RegisterSkelInfoClass()
{
  //### to implement
}



RServerDelegate 
AORB::createProxy(IN(RObjectKey) key)
{
  if (key->isNil() == true)
    return Nil;
  RString name = ObjectKey::classNameFromRepId(key->type_id);
  
  const char* clname = name->c_str();
  if (strchr(name->c_str(), '/') != 0)
    clname = strrchr(name->c_str(), '/') + 1;

  SkelInfoClassesStruct* proxy = _proxyRoot;
  if (strcmp(clname, "AcdkObject") == 0)
  {
    RServerDelegate sd = new CorObject(key, getORB());
    sd->isClient(true);
    return sd;
  }
  while (proxy != 0) {
    
    if (strcmp(clname, proxy->clazz->name) == 0) 
    {
      if (proxy->clazz->ns[0] != 0) {
        StringBuffer sb(proxy->clazz->ns);
        sb.append("/"); 
        sb.append(proxy->clazz->name);
        if (sb.toString()->equals(name) == false) {
          proxy = proxy->next;
          continue;
        }
      }
      
      RServerDelegate sd = (RServerDelegate)proxy->creator();
      sd->isClient(true);
      sd->setObjectKey(key);
      return sd;
    }
    proxy = proxy->next;
  }
  RCorObject cobj = new CorObject(key, getORB());
  
  try {
    if (name->length() > 0)
      cobj->setRemoteClass(Class::forName(name));
  } catch (...) {

  }
  return &cobj;
}


//static 
const acdk::lang::dmi::ClazzMethodInfo*
AORB::lookupMethod(IN(RString) method, const dmi::ClazzInfo* clazz)
{
  if (method->startsWith("__acdk_dmi_") == true)
  {
    // ### todo
  }
  const dmi::ClazzInfo* ci = clazz;
  int i = 0;
  while (ci->methods[i]) 
  {
    if (strcmp(ci->methods[i]->name, method->c_str()) == 0)
      return ci->methods[i];
    i++;
  }
  const dmi::ClazzMethodInfo* merg = 0;
  for (i = 0; ci->interfaces[i]; i++) 
  {
    ::acdk::lang::sys::core_vector<const acdk::lang::dmi::ClazzMethodInfo*> vec;
    findFunctions(ci->interfaces[i]->type, method, acdk::lang::dmi::MiPublic, vec);
    if (vec.size() < 1)
      return 0;
    return vec[1];
  }
  return 0;
}

//static
RAORB AORB::_theORB;


//static
int 
AORB::_lookupFreePort()
{
  //return 1234;
  return 0;
  /** not used
  int i = StdOrbPort + 1;
  return i;
  ::acdk::net::RInetAddress address = ::acdk::net::InetAddress::getLocalHost();
  while (true) {
    //::acdk::net::TCPSocket socket;
    //socket.create(false);
    try {
      //::acdk::net::ServerSocket socket(i);
      ::acdk::net::Socket sock(address, i);
      sock.close();
      //socket.connect(address, i);
      //socket.close();
      i++;
      continue;
    } catch(::acdk::net::RSocketException ) {
      break;
    }
  }
  System::out->println("Port: " + Integer::toString(i));
  return i;
  */
}

AORB::AORB()
: Thread(),
  _port(-1),
  _objects(new HashMap()),
  _objectsIds(new HashMap()),
  _shutdown(false),
  _treadGroup(new ThreadGroup("ORB")),
  _isServer(false),
  _serverSocket(Nil),
  ignoreLocal(false),
  ignoreDmiOverGIOP(false)
{
  _port = _lookupFreePort();
}



//static 
AORB& 
AORB::getAORB()
{
  if (_theORB != Nil)
    return *_theORB;
  _theORB = new AORB();
  System::registerStaticReference(_theORB);
  return *_theORB;
}

void 
AORB::init(IN(RStringArray) args, IN(acdk::util::RProperties) props)
{
  _properties = props;
  if (props == Nil)
    _properties = System::getProperties();

  ::acdk::io::File pfile(System::getProperty("ACDKHOME"), "acdkx_orb.cfg");
  if (pfile.exists() == true)
  {
    _properties->load(pfile.getReader());
  }
  CmdLineParser cparser;
  cparser.addOption("-ORBIIOPAddr", "acdkx.orb.ORBIIOPAddr", true, "Set the ORB server listner address");
  cparser.addOption("-ORBNamingAddr", "acdkx.orb.ORBNamingAddr", true, "Points to the address of the naming service");
  cparser.addOption("-ORBNamingIOR ", "acdkx.orb.ORBNamingIOR", true, "Points to the address of the naming service");
  cparser.parse(_properties, args, true, true);

  RString addr = _properties->getProperty("acdkx.orb.ORBIIOPAddr");
  if (addr == Nil || addr->length() == 0)
    addr = "inet:localhost:" + Integer::toString(_lookupFreePort());
  acdk::net::URL url(addr);
  _serverHost = url.getHost();
  _port = url.getPort();

  
}

int 
AORB::port() 
{ 
  return _port; 
}

//virtual 
RStringArray 
AORB::list_initial_services()
{

  return Nil;
}
  
//virtual 
::org::omg::CORBA::RObject 
AORB::resolve_initial_references(IN(RString) identifier) THROWS1(RInvalidName)
{
  if (identifier->equals("NameService") == true)
  {
    RString val = _properties->getProperty("acdkx.orb.ORBNamingAddr");
    if (val != Nil && val->length() > 0)
    {
      return string_to_object(val);
    }
  }
  THROW0_FQ(org::omg::CORBA::, InvalidName);
  return Nil;
}
  
//virtual 
void 
AORB::connect(IN(::org::omg::CORBA::RObject) obj)
{
  /** not needed?

  ObjectKey objkey("IIOP", acdk::net::InetAddress::getLocalHost()->toString(), _port, obj);
  RString objstring = objkey.toString();
  _objects->put(obj, objstring);
  _objectsIds->put(objstring, obj);
  */
}


//virtual 
RString 
AORB::object_to_string(IN(::org::omg::CORBA::RObject) obj)
{
  _initServer();
  return ServerDelegate::object_to_string(obj);
}


//virtual 
::org::omg::CORBA::RObject 
AORB::string_to_object(IN(RString) str)
{
  if (str->startsWith("corbaloc:") == true)
    return corbaloc_to_object(str);
  return &ServerDelegate::string_to_object(str);
}


::org::omg::CORBA::RObject 
AORB::corbaloc_to_object(IN(RString) str)
{
  if (str->startsWith("corbaloc:") == false)
    THROW2_FQ(org::omg::CORBA::, BAD_PARAM, 9, org::omg::CORBA::COMPLETED_NO);
  RString url = "";
  RString base = str->substr(strlen("corbaloc:"));
  int idx = base->indexOf('/');
  RString addr = base;
  if (idx != -1)
  {
    url = base->substr(idx + 1);
    addr = base->substr(0, idx);
    if (addr->charAt(0) == ':')
      addr = addr->substr(1);
    else if (addr->startsWith("iiop:") == true)
      addr = addr->substr(5);
  }
  RString version = "1.0";
  RString port = "2089";
  RString host = "localhost";
  // [ <version> `@' ] <host> [ `:' <port> ]
  if ((idx = addr->indexOf('@')) != -1)
  {
    version = addr->substr(0, idx);
    addr = addr->substr(idx + 1);
  }
  if ((idx = addr->indexOf(':')) != -1)
  {
    host = addr->substr(0, idx);
    port = addr->substr(idx + 1);
  }
  else
  {
    host = addr;
  }
  if ((idx = version->indexOf('.')) == -1)
    THROW2_FQ(org::omg::CORBA::, BAD_PARAM, 9, org::omg::CORBA::COMPLETED_NO);
  short majorversion = Short::parseShort(version->substr(0, idx));
  short minorversion = Short::parseShort(version->substr(idx + 1));
  RObjectKey objkey = new ObjectKey();
  objkey->type_id = url;
  objkey->object_key = url->getBytes();
  objkey->version.major = majorversion;
  objkey->version.minor = minorversion;
  objkey->protokoll = "inet";
  objkey->network = host;
  objkey->port = Short::parseShort(port);
  return &createProxy(objkey);
}


RString 
AORB::impl_is_ready(IN(::acdk::lang::RObject) obj)
{
  _initServer();
  RServerDelegate sd = RServerDelegate(obj);
  RObjectKey objkey = new ObjectKey(sd.iptr());
  _objects->put(obj, (RObject)objkey);
  _objects->put((RObject)objkey->objectId(), (RObject)objkey);
  return "";
}
  
void
AORB::_initServer()
{
  if (_serverSocket != Nil)
    return;
  if (_serverHost == Nil)
    _serverHost = AORB::getLocalHost();
  _serverSocket = new ServerSocket(_port, 30, acdk::net::InetAddress::getByName(_serverHost));
  _port = _serverSocket->getLocalPort();
  DOUT("AORB::_initServer: " << _serverSocket->toString()->c_str() << "; port=" << _port);
}

//virtual 
bool 
AORB::work_pending()
{
  
  return true;
}

//virtual 
void 
AORB::perform_work()
{
  
}


ACDK_DECL_CLASS(ClientConnection);
class ClientConnection 
: public ::acdk::lang::Thread
{
  RAORB _orb;
  RSocket _clientSocket;
public:
  ClientConnection(IN(RAORB) orb, IN(RSocket) clsock)
  : Thread(/*orb->threadGroup()*/),
    _orb(orb),
    _clientSocket(clsock)
  {
  }
  virtual void run();
};

//virtual 
void 
ClientConnection::run()
{
  //System::out->println("ClientConnection::run()");
  
  RReader in = _clientSocket->getInputStream();
  RWriter out = _clientSocket->getOutputStream();

  //RCDRObjectReader cdrin = new CDRObjectReader(in, (::org::omg::CORBA::RORB)_orb);
  try {
    do {
      RCDRObjectReader cdrin;
      RGIOPMessage mess = GIOPMessage::readMessage((::org::omg::CORBA::RORB)_orb, in, cdrin);
      RAServerRequestImpl serverrequest = new AServerRequestImpl((::org::omg::CORBA::RORB)_orb, 
                                                      (RGIOPRequestMessage)mess, cdrin, out);
      mess->handleCall(serverrequest);
    } while (true);
  } catch (RThrowable ex) {
    System::out->println(ex->getMessage());
  }
  //_clientSocket->close(); don't close it here, let client do
}


//virtual 
void 
AORB::run()
{
  if (_serverSocket == Nil)
    THROW1(Exception, "_serverSocket is not initialized! No Object registered?");
  //RServerSocket theSocket = new ServerSocket(_port);
  _isServer = true;
  //_serverSocket->setSoTimeout(1000);
  while (_doShutdown() == false) 
  {
    RSocket clsock = _serverSocket->accept();
    
    if (_doShutdown() == true)
      break;

    if (clsock)
    {
      RClientConnection client_thread = new ClientConnection(this, clsock);
      client_thread->start();
    }
  }
  _serverSocket->close();
  _isServer = false;
}

//virtual 
void 
AORB::shutdown(bool wait_for_completion)
{
  _shutdown = true;
  if (_isServer == true)
  {
    ::acdk::net::Socket tsock(true);
    tsock.connect(_serverSocket->getInetAddress()/*AORB::getLocalHost()*/, _serverSocket->getLocalPort(), 200);// to unblock accept  in AORB::run();
    
  }
  
  //_serverSocket->close();
  if (wait_for_completion == true)  // ### what to do
    ; // ??
}

//virtual 
void 
AORB::destroy()
{
}

//static 
void 
AORB::reset()
{
  if (_theORB == Nil)
    return;
  int rc = _theORB->refCount();
  if (rc != 1)
    System::out->println("**  WARN: AORB::reset(): Warning ORB is used somewhere");
  _theORB = Nil;
}

RObject 
AORB::object_key_to_object(sequence<octet>& object_key)
{
  RString str = new String((const char*)object_key.data(), object_key.size(), NormalSST | CCAscii);
  ObjectKey objkey(str);
  if (objkey.isLocal() == true)
    return objkey.getLocalObject();
  // return Skelleton here
  return Nil;
}

RObject 
AORB::resolve_object(AServerRequestImpl& req)
{
  if (req.inMessage()->header().version.minor < 2) {
    return object_key_to_object(req.inMessage()->requestHeader().object_key);
  } else {
    switch (req.inMessage()->requestHeader().target_address.addressingDisposition) {
    case org::omg::CORBA::GIOP::TargetAddress::KeyAddr:
      return object_key_to_object(req.inMessage()->requestHeader().target_address.object_key);
    case org::omg::CORBA::GIOP::TargetAddress::ProfileAddr :
      //profile.read(in);
      break;
    case org::omg::CORBA::GIOP::TargetAddress::ReferenceAddr:
      //ior.read(in);
      break;
    }
  }
  return Nil;
}

bool 
AORB::isOwnObjectId(IN(RObjectKey) key)
{
  if (key->port == _port &&
      key->network->equals(getLocalHost()))
      return true;
  /*
  if (_objectsIds->get(key->objectId()) != Nil)
    return true;
  */
  return false;
}

//static 
RString 
AORB::getLocalHost()
{
  return acdk::net::InetAddress::getLocalHost()->toString();
}

//static 
int 
AORB::getLocalPort()
{
  return getAORB().port();
}


} // namespace orb
} // namespace acdkx
