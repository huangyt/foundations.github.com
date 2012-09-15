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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/PasswordAuthentication.h,v 1.8 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_PasswordAuthentication_h
#define acdk_net_PasswordAuthentication_h

#include "Config.h"

namespace acdk {
namespace net {

ACDK_DECL_CLASS(PasswordAuthentication);

class ACDK_NET_PUBLIC PasswordAuthentication
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(PasswordAuthentication)
public:

  PasswordAuthentication(IN(RString) username, IN(RString) password)
  {
    this->username = username;
    this->password = password;
  }


/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the username associated with this object
  *
  * @return The username
  */

  virtual RString getUserName()
  {
    return(username);
  }

 
/*************************************************************************/

/**
  * Returns the password associated with this object
  *
  * @return The password
  */

  virtual RString getPassword()
  {
    return password;
  }

private:
/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * The username 
  */

  RString username;

  RString password;
};

} // namespace acdk
} // namespace net

#endif // acdk_net_PasswordAuthentication_h


