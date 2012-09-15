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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/Authenticator.cpp,v 1.7 2005/02/05 10:45:29 kommer Exp $


#include <acdk.h>
#include "Authenticator.h"
#include "InetAddress.h"

namespace acdk {
namespace net { 
  // public:
//  static 
void
Authenticator::setDefault(IN(RAuthenticator) def_auth)
{
  _default_authenticator = def_auth;

}

//  static 
RPasswordAuthentication
Authenticator::requestPasswordAuthentication(IN(RInetAddress) addr, int port, IN(RString) protocol, IN(RString) prompt, IN(RString) scheme)
{

  if (_default_authenticator == Nil)
    return(Nil);

  _default_authenticator->_addr = addr;
  _default_authenticator->_port = port;
  _default_authenticator->_protocol = protocol;
  _default_authenticator->_prompt = prompt;
  _default_authenticator->_scheme = scheme;

  return _default_authenticator->getPasswordAuthentication();

}

//  virtual 

// protected:





//  virtual 

// private:
//  static 
RAuthenticator Authenticator::_default_authenticator;






} // namespace acdk
} // namespace net
