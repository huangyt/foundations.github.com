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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/portable/ResponseHandler.h,v 1.6 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_portable_ResponseHandler_h
#define org_omg_CORBA_portable_ResponseHandler_h

#include "../CORBA.h"
#include "../ORB.h"
#include "OutputStream.h"

namespace org {
namespace omg {
namespace CORBA {
namespace portable {


ACDK_DECL_INTERFACE(ResponseHandler);

class ACDKX_ORB_PUBLIC ResponseHandler
      ACDK_INTERFACEBASE
{
public:

  /**
	 * Called by servant during a method invocation. The servant should call
	 * this method to create a reply marshal buffer if no exception occurred.
	 * 
	 * Return an OutputStream suitable for marshalling reply	 
	 */
  virtual ::org::omg::CORBA::portable::ROutputStream createReply() = 0;
	
	/**
	 * Called by servant during a method invocation. The servant should call this
	 * method to create a reply marshal buffer if a user exception occurred.
	 * 
	 * Returns an OutputStream suitable for marshalling the exception ID and the
	 * user exception body
	 */
	virtual ::org::omg::CORBA::portable::ROutputStream createExceptionReply() = 0;
};

} // namespace portable
} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_portable_ResponseHandler_h
