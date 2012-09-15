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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AServerRequestImpl.cpp,v 1.9 2005/02/05 10:45:39 kommer Exp $

#include <acdk.h>
#include <acdk/io/MemWriter.h>
#include "AServerRequestImpl.h"
#include "CDRObjectWriter.h"
#include "ReplyOutputStream.h"

namespace acdkx {
namespace orb {

USING_CLASS(::acdk::io::, MemWriter);

//virtual 
RString 
AServerRequestImpl::operation() 
{
  return _inMessage->requestHeader().operation;
}
  
//virtual 
::org::omg::CORBA::portable::ROutputStream 
AServerRequestImpl::createReply()
{
  RReplyOutputStream reply = new ReplyOutputStream(objectKey(), _out, _inMessage, ::org::omg::CORBA::GIOP::NO_EXCEPTION);
  reply->writeHeader();
  return &reply;
}

//virtual 
::org::omg::CORBA::portable::ROutputStream 
AServerRequestImpl::createExceptionReply()
{
  RReplyOutputStream reply = new ReplyOutputStream(objectKey(), _out, _inMessage, ::org::omg::CORBA::GIOP::USER_EXCEPTION);
  reply->writeHeader();
  return &reply;
}


void 
AServerRequestImpl::sendReply(IN(::org::omg::CORBA::portable::ROutputStream) out)
{
  RReplyOutputStream repl = (RReplyOutputStream)out;
  repl->send();
}

} // namespace orb 
} // namespace acdkx 

