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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/SocketImpl.cpp,v 1.25 2005/03/31 11:08:59 kommer Exp $


#include <acdk.h>
#include <acdk/util/Date.h>
#include "netsysincl.h"
#include "InetAddress.h"


#if defined(ACDK_OS_UNIX)
# include <sys/time.h>

#if !defined(_SOCKLEN_T)
#if defined(ACDK_OS_SOLARIS)
//will be defined //typedef int socklen_t;
#else
typedef unsigned socklen_t;
#endif
#endif // SOCKLEN_T

#ifdef ACDK_OS_LINUX
#include <netinet/in.h>
#endif // ACDK_OS_LINUX
#if defined(ACDK_OS_BSD)
# include <netinet/in.h>
#endif //defined(ACDK_OS_BSD)
#endif // defined(ACDK_OS_UNIX)
#include "SocketImpl.h"


//#define DOUT(msg) System::out->println(SBSTR("TID: " << ThreadID::getCurrentThreadID().getId()  << ": " << msg))
#define DOUT(msg)

namespace acdk {
namespace net {

USING_CLASS(::acdk::util::, Date); 

SocketImpl::SocketImpl()
: Object(),
  fd(Nil),
  address(Nil),
  port(0),
  localport(0)
{

}

SocketImpl::~SocketImpl()
{

}

RString 
SocketImpl::toString()
{
  StringBuffer retval;
  if (address != Nil)
    retval.append( address->toString() );
  else
    retval.append("<addres=Nil>");
  retval.append( ":" );
  retval.append( port );
  return retval.toString();
}

enum ShutdownChanel
{
  ShutdownReceive = 0,
  ShutdownSend = 1,
  ShutdownBoth = 2
};

    // Places the input stream for this socket at "end of stream". 
void 
SocketImpl::shutdownInput() 
{
  DOUT("> shutdown(" << fd->c_fd() << ", ShutdownReceive)");
  int erg = shutdown(fd->c_fd(), ShutdownReceive);
  DOUT("< shutdown(" << fd->c_fd() << ", ShutdownReceive) = " << erg);
  // ### test erg;
}
 
// Disables the output stream for this socket. 
void 
SocketImpl::shutdownOutput() 
{
  DOUT("> shutdown(" << fd->c_fd() << ", ShutdownReceive)");
  int erg = shutdown(fd->c_fd(), ShutdownSend);
  DOUT("< shutdown(" << fd->c_fd() << ", ShutdownSend) = " << erg);
  // ### test erg;
}

#if !defined(SOL_IP)
# define SOL_IP IPPROTO_IP
#endif
#if !defined(IP_TOS)
# define IP_TOS 8
#endif


//virtual 
RObject 
SocketImpl::getSockOption(IN(::acdk::io::RFileDescriptor) fd, int optID)
{
  switch (optID) {
  case _TCP_NODELAY : {
    int ret = 0;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), IPPROTO_TCP, TCP_NODELAY, (char*)&ret, &len);
    return &Boolean::valueOf(ret != 0);
  }
  case _SO_BINDADDR :

    THROW1(UnsupportedOperationException, "SocketImpl::getOption(SO_BINDADDR) not implemented yet");
    break;
  case _IP_MULTICAST_IF:
    THROW1(UnsupportedOperationException, "SocketImpl::getOption(_IP_MULTICAST_IF) not supported");
    break;
  case _SO_LINGER: {
    linger ret;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_SOCKET, SO_LINGER, (char*)&ret, &len);
    if (ret.l_onoff == 0)
      return new Integer(0);
    return new Integer(ret.l_linger);
  }
  case _SO_REUSEADDR : {
    int ret = 0;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_SOCKET, SO_REUSEADDR, (char*)&ret, &len);
    return &Boolean::valueOf(ret != 0);
  }
  case _SO_SNDBUF : {
    int ret = 0;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_SOCKET, SO_SNDBUF, (char*)&ret, &len);
    return new Integer(ret);
  }
  case _SO_RCVBUF : {
    int ret = 0;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_SOCKET, SO_RCVBUF, (char*)&ret, &len);
    return new Integer(ret);
  }
  case _SO_RCVTIMEO: {
    struct timeval ret;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_SOCKET, SO_RCVTIMEO, (char*)&ret, &len);
    int iret = Date(ret.tv_sec, ret.tv_usec).getTime();
    return new Integer(iret);
    break;
  }
  case _SO_SNDTIMEO:{
    struct timeval ret;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_SOCKET, SO_SNDTIMEO, (char*)&ret, &len);
    int iret = Date(ret.tv_sec, ret.tv_usec).getTime();
    return new Integer(iret);
    break;
  }
  case _SO_TIMEOUT: {
    struct timeval ret;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_SOCKET, SO_RCVTIMEO, (char*)&ret, &len);
    int iret = Date(ret.tv_sec, ret.tv_usec).getTime();
    return new Integer(iret);
    break;
  }
  case _SO_BROADCAST: 
  {
    int ret = 0;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_SOCKET, SO_BROADCAST, (char*)&ret, &len);
    return &Boolean::valueOf(ret != 0);
  }
  case _IP_TOS:
  {
    int ret = 0;
    socklen_t len = sizeof(ret);
    int erg = getsockopt(fd->c_fd(), SOL_IP, IP_TOS, (char*)&ret, &len);
    return new Integer(ret);
  }
  default:
    THROW1(Exception, RString("Unknown Socket option: ") + optID);
    break;
  }
  return Nil;
}

//virtual 
void 
SocketImpl::setSockOption(IN(::acdk::io::RFileDescriptor) fd, int optId, IN(RObject) value)
{
  switch (optId) {
  case _TCP_NODELAY : {
    RBoolean b = (RBoolean)value;
    int ret = b->booleanValue() == true ? 1 : 0;
    socklen_t len = sizeof(ret);
    int erg = setsockopt(fd->c_fd(), IPPROTO_TCP, TCP_NODELAY, (char*)&ret, len);
    DOUT("setsockopt(" << fd->c_fd() << ", IPPROTO_TCP, TCP_NODELAY, " << ret << ") = " << erg);
    
    break;
  }
  case _SO_LINGER: {
    RInteger i = (RInteger)value;
    linger ret;
    socklen_t len = sizeof(ret);
    ret.l_onoff = i->intValue() > 0 ? 1 : 0;
    ret.l_linger = i->intValue();
    int erg = setsockopt(fd->c_fd(), SOL_SOCKET, SO_LINGER, (char*)&ret, len);
    DOUT("setsockopt(" << fd->c_fd() << ", SOL_SOCKET, SO_LINGER, " << i->intValue() << ") = " << erg);
    break;
  }
  case _SO_SNDBUF : {
    RInteger i = (RInteger)value;
    int ret = i->intValue();
    int len = sizeof(ret);
    int erg = setsockopt(fd->c_fd(), SOL_SOCKET, SO_SNDBUF, (char*)&ret, len);
    DOUT("setsockopt(" << fd->c_fd() << ", SOL_SOCKET, SO_SNDBUF, " << i->intValue() << ") = " << erg);
    break;
  }
  case _SO_REUSEADDR : {
    RBoolean b = (RBoolean)value;
    int ret = b->booleanValue() == true ? 1 : 0;
    int len = sizeof(ret);
    int erg = setsockopt(fd->c_fd(), SOL_SOCKET, SO_REUSEADDR, (char*)&ret, len);
    DOUT("setsockopt(" << fd->c_fd() << ", SOL_SOCKET, SO_REUSEADDR, " << ret << ") = " << erg);
      /*
#if defined(SO_REUSEPORT)
    ret = b->booleanValue() == true ? 1 : 0;
    erg = setsockopt(fd->c_fd(), SOL_SOCKET, SO_REUSEPORT, (char*)&ret, len);
    DOUT("setsockopt(" << fd->c_fd() << ", SOL_SOCKET, SO_REUSEPORT, " << ret << ") = " << erg);
#endif
      */
    break;
  }
  case _SO_RCVBUF : {
    RInteger i = (RInteger)value;
    int ret = i->intValue();
    int len = sizeof(ret);
    int erg = setsockopt(fd->c_fd(), SOL_SOCKET, SO_RCVBUF, (char*)&ret, len);
    DOUT("setsockopt(" << fd->c_fd() << ", SOL_SOCKET, SO_RCVBUF, " << ret << ") = " << erg);
    break;
  }
  case _SO_SNDTIMEO:
  case _SO_RCVTIMEO:
  case _SO_TIMEOUT: {
    RInteger b = (RInteger)value;
    Date d(b->intValue());
    timeval ret; 
    ret.tv_sec = d.getSecs();
    ret.tv_usec = d.getUSecs();
    int len = sizeof(ret);
    int erg;
    if (_SO_TIMEOUT == optId || _SO_SNDTIMEO == optId)
    {
      erg = setsockopt(fd->c_fd(), SOL_SOCKET, SO_SNDTIMEO, (char*)&ret, len);
      DOUT("setsockopt(" << fd->c_fd() << ", SOL_SOCKET, SO_SNDTIMEO, " << b->intValue() << ") = " << erg);
      
    }
    if (_SO_TIMEOUT == optId || _SO_RCVTIMEO == optId)
    {
      erg = setsockopt(fd->c_fd(), SOL_SOCKET, SO_RCVTIMEO, (char*)&ret, len);
      DOUT("setsockopt(" << fd->c_fd() << ", SOL_SOCKET, SO_RCVTIMEO, " << b->intValue() << ") = " << erg);
    }
    break;
  }
  case _SO_BROADCAST: 
  {
    RBoolean b = (RBoolean)value;
    int ret = b->booleanValue() == true ? 1 : 0;
    socklen_t len = sizeof(ret);
    int erg = setsockopt(fd->c_fd(), SOL_SOCKET, SO_BROADCAST, (char*)&ret, len);
    DOUT("setsockopt(" << fd->c_fd() << ", IPPROTO_TCP, TCP_NODELAY, " << ret << ") = " << erg);
    break;
  }
  case _IP_TOS:
  {
    RInteger i = (RInteger)value;
    int ret = i->intValue();
    int len = sizeof(ret);
    int erg = setsockopt(fd->c_fd(), SOL_IP, IP_TOS, (char*)&ret, len);
    DOUT("setsockopt(" << fd->c_fd() << ", SOL_SOCKET, SO_RCVBUF, " << ret << ") = " << erg);
    break;
  }
  default:
    THROW1(Exception, RString("Unknown Socket option: ") + optId);
    break;
  }
}


} // net
} // acdk
