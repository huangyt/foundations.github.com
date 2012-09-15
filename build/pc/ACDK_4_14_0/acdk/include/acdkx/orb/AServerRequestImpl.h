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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AServerRequestImpl.h,v 1.8 2005/03/31 17:50:21 kommer Exp $

#ifndef acdkx_orb_AServerRequestImpl_h
#define acdkx_orb_AServerRequestImpl_h

#include <acdk.h>
#include <acdkx/orb/orb.h>

#include <org/omg/CORBA/ServerRequest.h>
#include <org/omg/CORBA/portable/ResponseHandler.h>
#include <org/omg/CORBA/portable/RemarshalException.h>
#include <org/omg/CORBA/portable/ApplicationException.h>

#include "CDRObjectWriter.h"
#include "CDRObjectReader.h"

#include "AContextImpl.h"
#include "GIOPMessage.h"

namespace acdkx {
namespace orb {
USING_CLASS(::org::omg::CORBA::, ORB);
USING_CLASS(::org::omg::CORBA::portable::, RemarshalException);
USING_CLASS(::org::omg::CORBA::portable::, ApplicationException);
USING_CLASS(::org::omg::CORBA::, NO_IMPLEMENT);

ACDK_DECL_CLASS(AServerRequestImpl);

class ACDKX_ORB_PUBLIC AServerRequestImpl
: extends ::org::omg::CORBA::ServerRequest,
  implements ::org::omg::CORBA::portable::ResponseHandler
{
protected:
  RGIOPRequestMessage _inMessage;
  RCDRObjectReader _cdrIn;
  RWriter _out;	
  ::org::omg::CORBA::RContext _ctx;
  RORB _orb;
  RObjectKey _objectKey;
public:
  AServerRequestImpl(IN(RORB) theorb, IN(RGIOPRequestMessage) inmessage, IN(RCDRObjectReader) cdrIn, IN(RWriter) cout)
  : _inMessage(inmessage),
    _cdrIn(cdrIn),
    _out(cout),
    _ctx(new AContextImpl("root", Nil)),
    _orb(theorb)
  {
  }
  RObjectKey objectKey() { return _objectKey; }
  void setObjectKey(IN(RObjectKey) objKey) { _objectKey = objKey; }
  RGIOPRequestMessage inMessage() { return _inMessage; }
  virtual RString operation();
  
  virtual ::org::omg::CORBA::RContext ctx()
  {
    return _ctx;
  }
  RCDRObjectReader in() { return _cdrIn; }
  virtual ::org::omg::CORBA::portable::ROutputStream createReply();
  virtual ::org::omg::CORBA::portable::ROutputStream createExceptionReply();
  /** send the given reply back to sender */
  void sendReply(IN(::org::omg::CORBA::portable::ROutputStream) out);
};

} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_AServerRequestImpl_h
