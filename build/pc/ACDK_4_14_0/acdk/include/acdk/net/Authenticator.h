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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/Authenticator.h,v 1.10 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_Authenticator_h
#define acdk_net_Authenticator_h


#include "net.h"
#include "PasswordAuthentication.h"
#include "InetAddress.h"

namespace acdk {
namespace net {


ACDK_DECL_CLASS(Authenticator);


class ACDK_NET_PUBLIC Authenticator
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(Authenticator)
private:
  static RAuthenticator _default_authenticator;
  RInetAddress _addr;
  int _port;
  RString _protocol;
  RString _prompt;
  RString _scheme;
public:
  static void setDefault(IN(RAuthenticator) def_auth);
 

/*************************************************************************/

/**
  * This method is called whenever a username and password for a given
  * network operation is required.  First, a security check is made to see
  * if the caller has the "
  * permission.  If not, the method thows an exception.  If there is no
  * default Authenticator object, the method then returns null.  Otherwise,
  * the default authenticators's instance variables are initialized and
  * it's getPasswordAuthentication method is called to get the actual
  * authentication information to return.
  *
  * @param _addr The address requesting authentication
  * @param _port The _port requesting authentication
  * @param protocol The protocol requesting authentication
  * @param prompt The prompt to display to the user when requesting authentication info
  * @param scheme The authentication scheme in use
  * 
  * @return A PasswordAuthentication object with the user's authentication info
  *
  * @exception SecurityException If the caller does not have permission to perform this operation
  */ 

  static RPasswordAuthentication requestPasswordAuthentication(IN(RInetAddress) addr, int port, IN(RString) protocol, IN(RString) prompt, IN(RString) scheme);


/*************************************************************************/

/*
 * Constructors
 */

/**
  * Do nothing constructor for subclasses to call
  */

  Authenticator()
  {
  }
protected:


/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This method returns the address of the site that is requesting
  * authentication.
  *
  * @return The requesting site
  */

  RInetAddress getRequestingSite()
  {
    return(_addr);
  }


/*************************************************************************/

/**
  * This method returns the _port of the site that is requesting 
  * authentication.
  *
  * @return The requesting _port
  */

  int getRequestingPort()
  {
  return(_port);
  }


/*************************************************************************/

/**
  * This method returns the requesting protocol of the operation that is
  * requesting authentication
  *
  * @return The requesting protocol
  */

  RString getRequestingProtocol() { return _protocol; }


/*************************************************************************/

/**
  * Returns the prompt that should be used when requesting authentication 
  * information from the user
  * 
  * @return The user prompt
  */

  RString getRequestingPrompt() { return _prompt; }


/*************************************************************************/

/**
  * This method returns the authentication scheme in use
  *
  * @return The authentication scheme
  */

  RString getRequestingScheme() { return _scheme; }


/*************************************************************************/

/**
  * This method is called whenever a request for authentication is made.  It
  * can call the other getXXX methods to determine the information relevant
  * to this request.  Subclasses should override this method, which returns
  * null by default.
  *
  * @return The PasswordAuthentication information
  */
  virtual RPasswordAuthentication getPasswordAuthentication() { return Nil; }

};

} // namespace acdk
} // namespace net

#endif //acdk_net_Authenticator_h

