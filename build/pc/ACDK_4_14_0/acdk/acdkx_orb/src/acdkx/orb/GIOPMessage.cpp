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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/GIOPMessage.cpp,v 1.20 2005/02/05 10:45:39 kommer Exp $

#include <acdk.h>
#include <acdk/io/MemReader.h>
#include <acdk/util/logging/Log.h>
#include <acdk/text/Format.h>
#include <acdk/io/EOFException.h>

#include <org/omg/CORBA/portable/InvokeHandler.h>
#include "GIOPMessage.h"
#include "OrbConnection.h"
#include "ServerDelegate.h"
#include "AServerRequestImpl.h"

/*

class TestClass
{
  RInterface foo(IN(int) inparam, OUT(RString) outparam) THROWS1(RBlaBla);
};

void
TestClass_GIOP_Proxy
: public TestClass
{
  RInterface foo(IN(int) inparam, OUT(RString) outparam) THROWS1(RBlaBla)
  {
    GIOPRequestMessage reqmsg(_connection, "foo");
    reqmsg.write_header();
    reqmsg.out().write_long(inparam);
    GIOPReplyMessage reply;
    reqmsg.getReply(reply);
    if (reply.is_exeption() == true) {

    }
    RInterface __retvalue = reply.in().read_object();
    outparam = reply.in().read_string()
    return __retvalue;
  }
};

void
TestClass::dispatch_giop(GIOPRequestMessage& request, GIOPReplyMessage& reply)
  {
    lookupMethod("foo");
    if (minf == foo_method) {
      RString outparam;
      try {
        RInterface __retvalue = foo(request.in().read_long(), outparam);
        reply.out().write_object(__retvalue);
        reply.out().write_string(outparam);
      } catch (RBlaBla __ex) {
         
      } catch (RThrowable __ex) {
      }
    }
  }
*/

namespace acdkx {
namespace orb {

using namespace org::omg::CORBA;
using namespace org::omg::CORBA::portable;
using namespace org::omg::CORBA::GIOP;
using namespace org::omg::CORBA::IOP;

GIOPMessage::GIOPMessage(MsgType msgtype)
: ::acdk::lang::Object()
{
  _header.magic[0] = 'G'; _header.magic[1] = 'I'; _header.magic[2] = 'O'; _header.magic[3] = 'P'; 
  _header.version.major = 1;
  _header.version.minor = 2;
  _header.flag = (::org::omg::CORBA::portable::naturualEndian == ::org::omg::CORBA::portable::LittleEndian) ? 1 : 0;
  _header.message_type = msgtype;
  _header.message_size = (size_t)0;
}

::org::omg::CORBA::portable::Endian 
GIOPMessage::getEndian()
{
  if (_header.flag & 1)
    return ::org::omg::CORBA::portable::LittleEndian;
  return ::org::omg::CORBA::portable::BigEndian;
}

//static 
RGIOPMessage
GIOPMessage::readMessage(IN(::org::omg::CORBA::RORB) orb, IN(::acdk::io::RReader) realin, INOUT(RCDRObjectReader) cdrin)
{
  SYNCOBJECT(realin);
  RReader in = realin;
  
  RbyteArray buffer = new byteArray(sizeof(MessageHeader));
  int readedbytes = in->read(buffer, 0, sizeof(MessageHeader));
  if (readedbytes != sizeof(MessageHeader))
    THROW0_FQ(acdk::io::, EOFException);

  if (cdrin == Nil)
    cdrin = new CDRObjectReader(new acdk::io::MemReader(buffer), orb);
  else
    cdrin->setIn(new acdk::io::MemReader(buffer));

  //MessageHeader* msgHeader = reinterpret_cast<MessageHeader*>(buffer->data());
  //msgHeader->read(*cdrin);
  MessageHeader msgHeader;
  msgHeader.read(*cdrin);

  buffer->resize(sizeof(MessageHeader) + msgHeader.message_size);
  readedbytes = in->read(buffer, sizeof(MessageHeader), msgHeader.message_size);
  if (size_t(readedbytes) != msgHeader.message_size)
    THROW0_FQ(acdk::io::, EOFException);
 
  RGIOPMessage ret;
  switch (msgHeader.message_type) 
  {
  case Request :
    ret = &GIOPRequestMessage::readMessage(msgHeader, &cdrin);
    break;
  case Reply :
    ret = &GIOPReplyMessage::readMessage(msgHeader, &cdrin);
    break;
  case LocateRequest:
    ret = &GIOPLocateRequest::readMessage(msgHeader, &cdrin);
    break;
  default:
    THROW1(Exception, RString("unsupported GIOP Message") + (int)msgHeader.message_type);
    return Nil;
  }
  ACDK_LOG(Debug, "GIOP Message readed: \n" + acdk::text::Format::dumpbin(buffer, 16) + "\n");
  return ret;
}

/*
void arraycpy(sequence<octet>& to, RcharArray from)
{
  int len = from->length();
  to.reserve(len);
  to.reset_hard(len);
  for (int i = 0; i < len; i++)
    to[i] = from[i];
}
*/
GIOPRequestMessage::GIOPRequestMessage(int reqId, IN(RObjectKey) objKey, IN(RString) method, bool responseExpected)
: GIOPMessage(Request)  
{
  
  
  _header.version = objKey->version;
  _requestHeader.operation = method;
  _requestHeader.response_flags = responseExpected;
  _requestHeader.target_address.addressingDisposition = TargetAddress::ReferenceAddr;
  _requestHeader.target_address.ior.selected_profile_index = IOP::TAG_INTERNET_IOP;
  _requestHeader.target_address.ior.ior = objKey->getIOR();
   
  byteArray_to_core_octet_array(objKey->object_key, _requestHeader.object_key);
  _requestHeader.request_id = reqId;

}



//virtual
void 
GIOPRequestMessage::handleCall(IN(RAServerRequestImpl) req)
{
  
  RObjectKey objKey = new ObjectKey(_header, _requestHeader);
  if (AORB::getAORB().isOwnObjectId(objKey) == true)
    objKey->fromObjectKey();
  req->setObjectKey(objKey);

  ::acdk::lang::RObject lobj = objKey->localObject;
  ::org::omg::CORBA::portable::RInvokeHandler obj = ::org::omg::CORBA::portable::RInvokeHandler(lobj);
  if (operation()->equals("_is_a") == true) { //is_a
  
    /*_non_existent
    _interface
    get_interface - operation name is
    get_implementation - operation name is _implementation
     non_existent - operation name is _non_existent
*/
  }
  ::org::omg::CORBA::portable::ROutputStream outp = obj->_invoke(operation(), *req->in().iptr(), *req.iptr());
  req->sendReply(outp);

}

//static 
RGIOPRequestMessage
GIOPRequestMessage::readMessage(::org::omg::CORBA::GIOP::MessageHeader& hdr, 
                     IN(::org::omg::CORBA::portable::RInputStream) in)
{
  RGIOPRequestMessage rqmh = new GIOPRequestMessage(hdr);
  rqmh->read(*in.iptr());
  //ACDK_LOG(Debug, "Read GIOPRequestMessage:\n" + acdk::text::Format::dumpbin(
  return rqmh;
}


void 
GIOPRequestMessage::read(::org::omg::CORBA::portable::InputStream& in)
{
  _requestHeader.read(_header, in);
}

//static 
RGIOPReplyMessage
GIOPReplyMessage::readMessage(::org::omg::CORBA::GIOP::MessageHeader& hdr, 
                              IN(::org::omg::CORBA::portable::RInputStream) in)
{
  RGIOPReplyMessage rqmh = new GIOPReplyMessage(hdr);
  rqmh->read(*in.iptr());
  return rqmh;
}

//virtual 
void 
GIOPReplyMessage::handleCall(IN(RAServerRequestImpl) req)
{
  THROW0(Exception);
}



//////////////////////////////   GIOPLocateRequest ////////////////////////////////////////////////

//static 
RGIOPLocateRequest 
GIOPLocateRequest::readMessage(::org::omg::CORBA::GIOP::MessageHeader& hdr, 
                               IN(::org::omg::CORBA::portable::RInputStream) in)
{
  RGIOPLocateRequest rqmh = new GIOPLocateRequest(hdr);
  rqmh->read(*in.iptr());
  return rqmh;
}

//virtual 
void 
GIOPLocateRequest::handleCall(IN(RAServerRequestImpl) req)
{
  ObjectBase::_throwNotImplementedYet("GIOPLocateRequest::handleCall(RAServerRequestImpl req)");
}




//////////////////////////////   GIOPLocateReplyMessage ////////////////////////////////////////////////


//static 
RGIOPLocateReplyMessage 
GIOPLocateReplyMessage::readMessage(::org::omg::CORBA::GIOP::MessageHeader& hdr, 
                                    IN(::org::omg::CORBA::portable::RInputStream) in)
{
  RGIOPLocateReplyMessage rm = new GIOPLocateReplyMessage(hdr);
  rm->read(*in.iptr());
  return rm;
}


//virtual 
void 
GIOPLocateReplyMessage::handleCall(IN(RAServerRequestImpl) req)
{
  ObjectBase::_throwNotImplementedYet("GIOPLocateReplyMessage::handleCall(RAServerRequestImpl req)");
}

} // namespace orb 
} // namespace acdkx 



