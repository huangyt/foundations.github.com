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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/InetAddress.cpp,v 1.19 2005/04/30 15:56:23 kommer Exp $


#include "netsysincl.h"

#include "InetAddress.h"

namespace acdk {
namespace net {


InetAddress::InetAddress(IN(RbyteArray) ipaddr, IN(RString) hostname)
: Object(),
  _ipNumber(),
  _hostname(hostname)
{
  _ipNumber = new byteArray(ipaddr->length());
  for (int i = 0; i < ipaddr->length(); i++)
    _ipNumber[i] = ipaddr[i];    
}

InetAddress::InetAddress(int ipaddr, IN(RString) hostname/* = Nil*/)
: Object(),
  _ipNumber(),
  _hostname(hostname)
{
  _ipNumber = new byteArray(4);
  _ipNumber[0] = ipaddr & 0x000F;
  _ipNumber[1] = ipaddr & 0x00F0;
  _ipNumber[2] = ipaddr & 0x0F00;
  _ipNumber[3] = ipaddr & 0xF000;
}


bool 
InetAddress::equals(IN(RObject) obj)
{
  if (instanceof(obj, InetAddress) == false)
    return false;
  return equals(RInetAddress(obj));
}
//virtual 
bool 
InetAddress::equals(IN(RInetAddress) other)
{
  if (other == Nil)
    return false;
  RbyteArray obytes = other->getAddress();
  if (obytes.length() != _ipNumber.length())
    return false;
  for (int i = 0; i < _ipNumber.length(); i++) {
    if (obytes[i] != _ipNumber[i])
       return false;
  }
  return true;
}

RbyteArray 
InetAddress::getAddress()
{
  return _ipNumber;
}

RString 
InetAddress::getHostAddress()
{
  return toString();
}

RString
InetAddress::getHostName()
{
  if (_hostname != Nil)
    return _hostname;
  if (hostent *host = gethostbyaddr( (const char *)_ipNumber->data(), _ipNumber->length(), AF_INET )) {
    _hostname = SCS(host->h_name); 
  }
  return _hostname;
}

//static 
RInetAddress 
InetAddress::getNullHost()
{
  RbyteArray addr = new byteArray(4);
  addr[0] = 0;
  addr[1] = 0;
  addr[2] = 0;
  addr[3] = 0;
  return new InetAddress(addr);
}

//static 
RInetAddress 
InetAddress::getWildcardHost()
{
  RbyteArray addr = new byteArray(4);
  addr[0] = 255;
  addr[1] = 255;
  addr[2] = 255;
  addr[3] = 255;
  return new InetAddress(addr);
}

RInetAddress 
InetAddress::getByName(IN(RString) host) 
{
  RbyteArray Address = new byteArray(4);
  Address[0] = 0;
  Address[1] = 0;
  Address[2] = 0;
  Address[3] = 0;
  RString nhost = host->convert(CCAscii);
  hostent *addr;
  if ((addr = gethostbyname( nhost->c_str())) != 0) 
  {
    for( int i=0;i<4;i++)
      Address[i]= addr->h_addr_list[0][i];
  }
  RInetAddress retval = new InetAddress( Address, host );
  return retval;
}

//static 
RInetAddress 
InetAddress::getAnyAddress()
{
#if !defined(INADDR_ANY)
# define INADDR_ANY 0x0
#endif
  static RInetAddress  anyAddress = new InetAddress(INADDR_ANY);
  return anyAddress;
}

RInetAddressArray
InetAddress::getAllByName(IN(RString) host)
{
  RInetAddressArray retval = new InetAddressArray(0);
  hostent *addr;
  RString nhost = host->convert(CCAscii);
  if ((addr = gethostbyname(nhost->c_str())) != 0) 
  {
    for (int n=0; addr->h_addr_list[n] != 0; n++ ) 
    {
      RbyteArray Address = new byteArray(4);
      for( int i=0;i<4;i++)
        Address[i]= addr->h_addr_list[n][i];
      RInetAddress nextaddr = new InetAddress(Address/*, host*/);
      retval->append(nextaddr);
    }
  }
  return retval;

}

bool 
InetAddress::isMulticastAddress()
{
   return ((_ipNumber[0] > 223) && (_ipNumber[0]  < 241));
}

//  virtual 
RString 
InetAddress::toString() 
{
  StringBuffer retval;
  for (int i = 0; i < _ipNumber->length(); i++) 
  {
    retval.append(_ipNumber[i]);
    if ((i+1) < _ipNumber->length()) 
      retval.append('.');
  }
  return retval.toString();
}

int
InetAddress::hashCode() 
{
  return *(int*)_ipNumber->data(); 
}

RString 
InetAddress::getLocalHostName()
{
  return "localhost";
}

RInetAddress 
InetAddress::getLocalHost() 
{
  return getByName(getLocalHostName());
}

//static 
RString 
InetAddress::getDefaultHostName()
{
  char hname[BUFSIZ];
  gethostname( hname, BUFSIZ );
  RString myName = SCS( hname );
  return myName;
}


} // namespace net 
} // namespace acdk 


