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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/ReplyInputStream.cpp,v 1.9 2005/02/05 10:45:39 kommer Exp $


#include "ReplyInputStream.h"
#include <org/omg/CORBA/OrbExceptions.h>

namespace acdkx {
namespace orb {

void 
ReplyInputStream::testReplyStatus()
{
  switch(_replyMessage->replyHeader().reply_status) {
  case ::org::omg::CORBA::GIOP::NO_EXCEPTION :
    break;
  case ::org::omg::CORBA::GIOP::USER_EXCEPTION : 
  {
    RThrowable ex = read_exception();
    ex->throwException();

    //RString rep_id = read_string();
    //THROW2_FQ(::org::omg::CORBA::portable::, ApplicationException, this, rep_id);
    break;
  }
  case ::org::omg::CORBA::GIOP::SYSTEM_EXCEPTION : {
    
    RString rep_id = read_string();
    int minor_code = read_long();
    int completion = read_long();
    RString clsname = ObjectKey::classNameFromRepId(rep_id);
    org::omg::CORBA::RSystemException ex;
    try {
      ex = (org::omg::CORBA::RSystemException)Class::forName(clsname)->newInstance();
      ex->minor(minor_code);
      ex->completed(completion);
    } catch (RThrowable ex) {
      THROW2_FQ(::org::omg::CORBA::, UNKNOWN, minor_code, (org::omg::CORBA::CompletionStatus)completion);
    }
    ex->throwException();
    /* old code
    if (rep_id->equals("omg/org/CORBA/COMM_FAILURE") == true)
      THROW2_FQ(::org::omg::CORBA::, COMM_FAILURE, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/NO_IMPLEMENT") == true)
      THROW2_FQ(::org::omg::CORBA::, NO_IMPLEMENT, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/BAD_OPERATION") == true)
      THROW2_FQ(::org::omg::CORBA::, BAD_OPERATION, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/MARSHAL") == true)
      THROW2_FQ(::org::omg::CORBA::, BAD_OPERATION, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/BAD_CONTEXT") == true)
      THROW2_FQ(::org::omg::CORBA::, BAD_CONTEXT, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/NO_MEMORY") == true)
      THROW2_FQ(::org::omg::CORBA::, NO_MEMORY, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/NO_PERMISSION") == true)
      THROW2_FQ(::org::omg::CORBA::, NO_PERMISSION, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/NO_RESOURCES") == true)
      THROW2_FQ(::org::omg::CORBA::, NO_RESOURCES, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/NO_RESPONSE") == true)
      THROW2_FQ(::org::omg::CORBA::, NO_RESPONSE, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/OBJ_ADAPTER") == true)
      THROW2_FQ(::org::omg::CORBA::, OBJ_ADAPTER, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/OBJECT_NOT_EXIST") == true)
      THROW2_FQ(::org::omg::CORBA::, OBJECT_NOT_EXIST, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/PERSIST_STORE") == true)
      THROW2_FQ(::org::omg::CORBA::, PERSIST_STORE, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/TRANSACTION_REQUIRED") == true)
      THROW2_FQ(::org::omg::CORBA::, TRANSACTION_REQUIRED, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/TRANSACTION_ROLLEDBACK") == true)
      THROW2_FQ(::org::omg::CORBA::, TRANSACTION_ROLLEDBACK, minor_code, completion);
    if (rep_id->equals("omg/org/CORBA/TRANSIENT") == true)
      THROW2_FQ(::org::omg::CORBA::, TRANSIENT, minor_code, completion);
    THROW2_FQ(::org::omg::CORBA::, UNKNOWN, minor_code, completion);
    */
    break;
  }
  case ::org::omg::CORBA::GIOP::LOCATION_FORWARD : 
    // no break
  case ::org::omg::CORBA::GIOP::LOCATION_FORWARD_PERM : {
    RObjectKey okey = new ObjectKey();
    okey->ior.read(*this);
    okey->fromIOR();
    THROW2_FQ(::org::omg::CORBA::portable::, RemarshalException, (RObject)okey, 
        _replyMessage->replyHeader().reply_status == ::org::omg::CORBA::GIOP::LOCATION_FORWARD_PERM);
  }
  case ::org::omg::CORBA::GIOP::NEEDS_ADDRESSING_MODE : 
    THROW1(Exception, "Reply Status not supported: NEEDS_ADDRESSING_MODE");
    break;      
  }

  
}

void 
ReplyInputStream::readHeader()
{
  RCDRObjectReader oreader = this;
  RGIOPMessage giopmessage = GIOPMessage::readMessage(_orb, _in, oreader);
  if (instanceof(giopmessage, GIOPReplyMessage) == false)
    THROW1(Exception, "unexpected Message: " + giopmessage->getClass()->getName()); // ### conforming exception
  _replyMessage = (RGIOPReplyMessage)giopmessage;

}

} // namespace orb 
} // namespace acdkx 



