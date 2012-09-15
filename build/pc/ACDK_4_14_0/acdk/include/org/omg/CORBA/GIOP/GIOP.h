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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/GIOP/GIOP.h,v 1.9 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_GIOP_GIOP_h
#define org_omg_CORBA_GIOP_GIOP_h

#include <org/omg/CORBA/IOP/IOP.h>

namespace org {
namespace omg {
namespace CORBA {
  /** 
  General Inter Orb Protokoll
  */
namespace GIOP {


class Version
{ 
public:
  octet major;
  octet minor;
  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    major = in.read_octet();
    minor = in.read_octet();
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_octet(major);
    out.write_octet(minor);
  }

};

enum MsgType
{
    Request = 0,
    Reply,
    CancelRequest,
    LocateRequest,
    LocateReply,
    CloseConnection,
    MessageError
};

class MessageHeader
{
public:
  char magic[4];
  Version version;
  octet flag;
  octet message_type;
  unsigned long message_size;
  void write(::org::omg::CORBA::portable::OutputStream& out);
  void read(::org::omg::CORBA::portable::InputStream& in);
}; 

typedef sequence<octet> Principal;

//1.2
class IORAddressingInfo
{
public:
  unsigned long selected_profile_index;
  IOP::IOR ior;
  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    selected_profile_index = in.read_long();
    ior.read(in);
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_long(selected_profile_index);
    ior.write(out);
  }
};

// only 1.2
class TargetAddress
{
public:
  enum AddressingDisposition 
  {
    KeyAddr = 0,
    ProfileAddr = 1,
    ReferenceAddr = 2
  };
  AddressingDisposition addressingDisposition;
  sequence<octet> object_key;
  IOP::TaggedProfile profile;
  IORAddressingInfo ior;

  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    addressingDisposition = (AddressingDisposition)in.read_short();
    switch (addressingDisposition) {
    case KeyAddr:
      IOP::read_octet_sequence(in, object_key);
      break;
    case ProfileAddr :
      profile.read(in);
      break;
    case ReferenceAddr:
      ior.read(in);
      break;
    }
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_short(addressingDisposition);
    switch (addressingDisposition) {
    case KeyAddr:
      IOP::write_octet_sequence(out, object_key);
      break;
    case ProfileAddr :
      profile.write(out);
      break;
    case ReferenceAddr:
      ior.write(out);
      break;
    }
  }
};

const octet ResponseExpected = 0x01;

class RequestHeader
{
public:
  org::omg::CORBA::IOP::ServiceContextList service_context;
  unsigned long request_id;
  octet response_flags;
  octet reserved[3];
  // 1.0 + 1.1
  sequence<octet> object_key;
  
  RString operation;
  // 1.0 + 1.1
  Principal requesting_principal;
  
  TargetAddress target_address;

  RequestHeader()
  {
    memset(reserved, 0, sizeof(reserved));
  }
  void read(MessageHeader& hdr, ::org::omg::CORBA::portable::InputStream& in)
  {
    if (hdr.version.minor < 2)
      IOP::read_t_sequence(in, service_context);
    request_id = in.read_long();
    response_flags = in.read_octet();
    if (hdr.version.minor > 0)
      in.read_char_array((char*)reserved, 0, 3);
    if (hdr.version.minor < 2)
      IOP::read_octet_sequence(in, object_key);
    else  // 1.2
      target_address.read(in);
    operation = in.read_string();
    if (hdr.version.minor < 2)
      IOP::read_octet_sequence(in, requesting_principal);
    else
      IOP::read_t_sequence(in, service_context);
  }
  void write(MessageHeader& hdr, ::org::omg::CORBA::portable::OutputStream& out)
  {
    if (hdr.version.minor < 2)
      IOP::write_t_sequence(out, service_context);
    out.write_long(request_id );
    out.write_octet(response_flags);
    if (hdr.version.minor > 0)
      out.write_char_array((char*)reserved, 0, 3);
    if (hdr.version.minor < 2)
      IOP::write_octet_sequence(out, object_key);
    else  // 1.2
      target_address.write(out);
    out.write_string(operation );
    if (hdr.version.minor < 2)
      IOP::write_octet_sequence(out, requesting_principal);
    else
      IOP::write_t_sequence(out, service_context);
  }
  bool responseExpected() { return (ResponseExpected & response_flags) == ResponseExpected; }
};


enum ReplyStatusType
{
  NO_EXCEPTION,
    USER_EXCEPTION,
    SYSTEM_EXCEPTION,
    LOCATION_FORWARD,
    LOCATION_FORWARD_PERM,
    NEEDS_ADDRESSING_MODE
};

class ReplyHeader
{
public:
  IOP::ServiceContextList service_context;
  unsigned long request_id;
  ReplyStatusType reply_status;
  void read(MessageHeader& hdr, ::org::omg::CORBA::portable::InputStream& in)
  {
    if (hdr.version.minor < 2) {
      IOP::read_t_sequence(in, service_context);
      request_id = in.read_long();
      reply_status = (ReplyStatusType)in.read_long();
    } else {
      request_id = in.read_long();
      reply_status = (ReplyStatusType)in.read_long();
      IOP::read_t_sequence(in, service_context);
    }
  }
  void write(MessageHeader& hdr, ::org::omg::CORBA::portable::OutputStream& out)
  {
    if (hdr.version.minor < 2) {
      IOP::write_t_sequence(out, service_context);
      out.write_long(request_id);
      out.write_long((int)reply_status);
    } else {
      out.write_long(request_id);
      out.write_long((int)reply_status);
      IOP::write_t_sequence(out, service_context);
    }
  }
};

class CancelRequestHeader
{
public:
  unsigned long request_id;
  void read(MessageHeader& hdr, ::org::omg::CORBA::portable::InputStream& in)
  {
    request_id = in.read_long();
  }
};

class LocateRequestHeader
{
public:
  unsigned long request_id;
  
  // 1.0 & 1.1
  sequence<octet> object_key;
  // 1.2
  TargetAddress target;
  void read(MessageHeader& hdr, ::org::omg::CORBA::portable::InputStream& in)
  {
    request_id = in.read_long();
    if (hdr.version.minor < 2) {
      IOP::read_octet_sequence(in, object_key);
    } else {
      target.read(in);
    }
  }
  void write(MessageHeader& hdr, ::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_long(request_id);
    if (hdr.version.minor < 2) {
      IOP::write_octet_sequence(out, object_key);
    } else {
      target.write(out);
    }
  }
};

enum LocateStatusType
{
  UNKNOWN_OBJECT,
  OBJECT_HERE,
  OBJECT_FORWARD
};

class LocateReplyHeader
{
public:
  unsigned long request_id;
  LocateStatusType locate_status;
  void read(MessageHeader& hdr, ::org::omg::CORBA::portable::InputStream& in)
  {
    request_id = in.read_long();
    locate_status = (LocateStatusType)in.read_long();
  }
  void write(MessageHeader& hdr, ::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_long(request_id);
    out.write_long(locate_status);
  }
};

} // namespace GIOP
} // namespace CORBA
} // namespace omg
} // namespace org

#endif //org_omg_CORBA_GIOP_GIOP_h
