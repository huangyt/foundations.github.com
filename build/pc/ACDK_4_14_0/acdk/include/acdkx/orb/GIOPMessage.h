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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/GIOPMessage.h,v 1.11 2005/02/05 10:45:39 kommer Exp $


#ifndef acdkx_orb_GIOPMessage_h
#define acdkx_orb_GIOPMessage_h

#include <acdk.h>
#include "orb.h"
#include <org/omg/CORBA/portable/InputStream.h>
#include <org/omg/CORBA/GIOP/GIOP.h>
#include "ObjectKey.h"

#include <acdk/io/MemWriter.h>

#include <acdk/net/Socket.h>
#include <acdk/net/InetAddress.h>

#include "AORB.h"
#include "GIOPMessage.h"
#include "CDRObjectWriter.h"
#include "CDRObjectReader.h"


namespace acdkx {
namespace orb {



ACDK_DECL_CLASS(GIOPMessage);



class ACDKX_ORB_PUBLIC GIOPMessage
: extends ::acdk::lang::Object
{
protected:
  org::omg::CORBA::GIOP::MessageHeader _header;
  /** sending client side */
  GIOPMessage(org::omg::CORBA::GIOP::MsgType msgtype);
  /** receiving server side */
  GIOPMessage(org::omg::CORBA::GIOP::MessageHeader& hdr)
  : _header(hdr)
  {
  }
public:
  org::omg::CORBA::GIOP::MessageHeader& header() { return _header; }
  static RGIOPMessage readMessage(IN(::org::omg::CORBA::RORB) orb, IN(::acdk::io::RReader) in, INOUT(RCDRObjectReader) cdrin);

  void writeHeader(::org::omg::CORBA::portable::OutputStream& out)
  {
    _header.write(out);
  }
  ::org::omg::CORBA::portable::Endian getEndian();
  virtual void handleCall(IN(RAServerRequestImpl) req) = 0;
};

ACDK_DECL_CLASS(GIOPRequestMessage);

class ACDKX_ORB_PUBLIC GIOPRequestMessage
: extends GIOPMessage
{
protected:
  org::omg::CORBA::GIOP::RequestHeader _requestHeader;
  /** for receiving server side */
  GIOPRequestMessage(org::omg::CORBA::GIOP::MessageHeader& hdr)
  : GIOPMessage(hdr)
  {
  }
public:
  /** for sending client side */
  GIOPRequestMessage(int reqId, IN(RObjectKey) objKey, IN(RString) method, bool responseExpected);
  
  static RGIOPRequestMessage readMessage(::org::omg::CORBA::GIOP::MessageHeader& hdr, 
                                         IN(::org::omg::CORBA::portable::RInputStream) in);
  void read(::org::omg::CORBA::portable::InputStream& in);
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    writeHeader(out);
    _requestHeader.write(_header, out);
  }
  virtual void handleCall(IN(RAServerRequestImpl) req);
  
  // accessors
  org::omg::CORBA::GIOP::RequestHeader& requestHeader() { return _requestHeader; }

  sequence<octet>& object_key() { return _requestHeader.object_key; }

  RString operation() { return _requestHeader.operation; }
  bool responseExpected() { return _requestHeader.responseExpected(); }
};


ACDK_DECL_CLASS(GIOPReplyMessage);

class ACDKX_ORB_PUBLIC GIOPReplyMessage
: extends GIOPMessage
{
protected:
  org::omg::CORBA::GIOP::ReplyHeader _replyHeader;
  /**
    Constructor for reading message
    */
  GIOPReplyMessage(org::omg::CORBA::GIOP::MessageHeader& hdr)
  : GIOPMessage(hdr)
  {
  }
public:
   /**
    Constructor for writing message
    */
  GIOPReplyMessage(GIOPRequestMessage& req, ::org::omg::CORBA::GIOP::ReplyStatusType replyStatus)
   : GIOPMessage(req.header())
  {
    _header.message_type = org::omg::CORBA::GIOP::Reply;
    _header.message_size = (unsigned)0;
    _replyHeader.reply_status = replyStatus;
    _replyHeader.request_id = req.requestHeader().request_id;
  }

  static RGIOPReplyMessage readMessage(::org::omg::CORBA::GIOP::MessageHeader& hdr, 
                                         IN(::org::omg::CORBA::portable::RInputStream) in);
  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    _replyHeader.read(_header, in);
  }
  void writeHeader(::org::omg::CORBA::portable::OutputStream& out)
  {
    GIOPMessage::writeHeader(out);
    _replyHeader.write(_header, out);
  }
  org::omg::CORBA::GIOP::ReplyHeader& replyHeader() { return _replyHeader; }
  virtual void handleCall(IN(RAServerRequestImpl) req);
};


ACDK_DECL_CLASS(GIOPLocateRequest);

class ACDKX_ORB_PUBLIC GIOPLocateRequest
: extends GIOPMessage
{
protected:
  org::omg::CORBA::GIOP::LocateRequestHeader _locateRequestHeader;
  /** for receiving server side */
  GIOPLocateRequest(org::omg::CORBA::GIOP::MessageHeader& hdr)
  : GIOPMessage(hdr)
  {
  }
public:
  /** for sending client side */
  //OPRequestMessage(int reqId, RObjectKey objKey, RString method, bool responseExpected);
  
  static RGIOPLocateRequest readMessage(::org::omg::CORBA::GIOP::MessageHeader& hdr, 
                                         IN(::org::omg::CORBA::portable::RInputStream) in);

  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    _locateRequestHeader.read(_header, in);
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    writeHeader(out);
    _locateRequestHeader.write(_header, out);
  }
  virtual void handleCall(IN(RAServerRequestImpl) req);
  
  // accessors
  org::omg::CORBA::GIOP::LocateRequestHeader& locateRequestHeader() { return _locateRequestHeader; }

  //sequence<octet>& object_key() { return _requestHeader.object_key; }

  //RString operation() { return _requestHeader.operation; }
  //bool responseExpected() { return _requestHeader.responseExpected(); }
};


ACDK_DECL_CLASS(GIOPLocateReplyMessage);

class ACDKX_ORB_PUBLIC GIOPLocateReplyMessage
: extends GIOPMessage
{
protected:
  org::omg::CORBA::GIOP::LocateReplyHeader _locateReplyHeader;
  /**
    Constructor for reading message
    */
  GIOPLocateReplyMessage(org::omg::CORBA::GIOP::MessageHeader& hdr)
  : GIOPMessage(hdr)
  {
  }
public:
   /**
    Constructor for writing message
    
  GIOPLocateReplyMessage(GIOPRequestMessage& req, ::org::omg::CORBA::GIOP::ReplyStatusType replyStatus)
   : GIOPMessage(req.header())
  {
    _header.message_type = org::omg::CORBA::GIOP::Reply;
    _header.message_size = -1;
    _replyHeader.reply_status = replyStatus;
    _replyHeader.request_id = req.requestHeader().request_id;
  }
  */
  static RGIOPLocateReplyMessage readMessage(::org::omg::CORBA::GIOP::MessageHeader& hdr, 
                                             IN(::org::omg::CORBA::portable::RInputStream) in);
  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    _locateReplyHeader.read(_header, in);
  }
  void writeHeader(::org::omg::CORBA::portable::OutputStream& out)
  {
    GIOPMessage::writeHeader(out);
    _locateReplyHeader.write(_header, out);
  }
  org::omg::CORBA::GIOP::LocateReplyHeader& locateReplyHeader() { return _locateReplyHeader; }
  virtual void handleCall(IN(RAServerRequestImpl) req);
};


} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_GIOPMessage_h

