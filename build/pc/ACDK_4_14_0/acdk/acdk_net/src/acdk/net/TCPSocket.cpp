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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/TCPSocket.cpp,v 1.48 2005/05/01 12:27:45 kommer Exp $




#include "netsysincl.h"


#if defined(ACDK_OS_UNIX)
#include <sys/time.h>
#include <sys/socket.h>
#endif

#include "string.h"
#include "TCPSocket.h"
#include "SocketException.h"


#include <acdk/lang/ClassCastException.h>
#include <acdk/io/IOException.h>
#include "SocketException.h"
#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/util/Date.h>

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)
# define DOUT(msg) System::out->println(SBSTR("TID: " << ThreadID::getCurrentThreadID().getId()  << ": " << msg))
#else
# define DOUT(msg)
#endif

#ifdef _eof
# undef _eof
#endif


#ifdef ACDK_OS_LINUX
# define RECV_OPTION MSG_NOSIGNAL
#else
# define RECV_OPTION 0
#endif

namespace acdk {
namespace net {

void ms2TimeVal(int ms, timeval& ret)
{
  acdk::util::Date d(ms);
  ret.tv_sec = d.getSecs();
  ret.tv_usec = d.getUSecs();
}


TCPSocket::TCPSocket()
: SocketImpl()
, _connected(false)
, _eof(false)
, _eofReturned(false)
, _bound(false)
{
  memset(&_address, 0, sizeof(_address));
  _address.sin_family = AF_INET;
}

TCPSocket::~TCPSocket()
{
  close();
}

/** @internal */
static RString getError()
{
#if defined(ACDK_OS_WIN32)
    return String::valueOf(WSAGetLastError());
#else
    return SCS(strerror(errno));
#endif
}

/** @internal */
static RString getError(int sockfd)
{
  int sockerr = 0;
  socklen_t socklen = sizeof(int);
  StringBuffer sb;
  if (getsockopt (sockfd, SOL_SOCKET, SO_ERROR, (char*)&sockerr, &socklen) == 0) 
  {
    sb.append(" SO_ERROR: [");
    sb.append(String::valueOf(sockerr));
    sb.append("]; ");
  } 
  else 
  {
    sb.append("(error internal: getsockopt(");
    sb.append(String::valueOf(sockfd));
    sb.append(") failed)");
  }
#if defined(ACDK_OS_WIN32)
    sb.append(String::valueOf(WSAGetLastError()));
#else
    sb.append("errno: ");
    sb.append(String::valueOf(errno));
    sb.append(": ");
    sb.append(strerror(errno));
#endif
    return sb.toString();
}

static RString getSockError(int error)
{
  StringBuffer sb;
  sb << "SO_ERROR: [" << error << "]; ";
#if !defined(ACDK_OS_WIN32)
  sb << strerror(error);
#endif
  return sb.toString();
}

//foreign static 
void 
TCPSocket::inetSocketAddress2native(IN(RInetSocketAddress) isa, sockaddr_in& _address)
{
  memset((char*)&_address, 0, sizeof(_address));
  _address.sin_family = AF_INET;
  _address.sin_port = htons(isa->getPort());
  _address.sin_addr.s_addr =* (u_long*) isa->getAddress()->getData();
}

//foreign static 
RInetSocketAddress 
TCPSocket::native2InetSocketAddress(const sockaddr_in& _address)
{
  RbyteArray _ipNumber = new byteArray(4);
  sockaddr *cliaddr = (sockaddr *)&_address;
  _ipNumber[0] = cliaddr->sa_data[2]; 
  _ipNumber[1] = cliaddr->sa_data[3];
  _ipNumber[2] = cliaddr->sa_data[4];
  _ipNumber[3] = cliaddr->sa_data[5];
  return new InetSocketAddress(new InetAddress(_ipNumber), ntohs(_address.sin_port));
}

// Creates either a stream or a datagram socket.
void
TCPSocket::create(bool stream)
{
  if (fd != Nil)
    return;
  int type = stream ? SOCK_STREAM : SOCK_DGRAM;
  int newfd;
  if (( newfd = socket( AF_INET, type, 0 )) < 0) {
    THROW1(SocketException, toString() + "; Create Socket Failed!: " +  getError());
  }
  DOUT(":: socket( AF_INET, type, 0 ) = " << newfd);
  fd = new FileDescriptor(newfd, O_RDWR);
}

static int getSockErr(int fd)
{
  socklen_t lon = sizeof(int); 
  int valopt = 0;
  getsockopt(fd, SOL_SOCKET, SO_ERROR, (char*)&valopt, &lon); 
  return valopt;
}

// Connects this socket to the specified port number on the specified host.
bool
TCPSocket::connect(IN(RInetAddress) address, int port, int timeOut)
{
  _address.sin_port = htons(port);
  _address.sin_addr.s_addr =* (u_long*) address->getAddress()->data();
  if (Nil == fd)
    create(true);
  if (timeOut != -1)
  {
    int ret;
#if !defined(ACDK_OS_WIN32)
    int soc = fd->c_fd();
    int arg = fcntl(soc, F_GETFL, NULL); 
    arg |= O_NONBLOCK; 
    fcntl(soc, F_SETFL, arg); 
    ret = ::connect(soc ,( struct sockaddr * ) &_address, sizeof( _address));
    if (ret == 0)
    {
       this->address = address;
       this->port = port;
       _connected = true;
       return true;
    } 
    else  if (ret < 0 ) 
    { 
      if (errno != EINPROGRESS) 
      { 
        THROW1(SocketException, toString() + "; Connect Socket Failed: [" + address->toString()
               + ":" + String::valueOf(port) + "]: " +  getError(fd->c_fd()));
      }
    }  
#endif
    timeval tv; 
    ms2TimeVal(timeOut, tv);
    fd_set fr, fw; 
    FD_ZERO(&fr);
    FD_ZERO(&fw);
    FD_SET(fd->c_fd(), &fr);
    FD_SET(fd->c_fd(), &fw);
    ret = select(fd->c_fd() + 1, 0, &fw,  0, &tv);
#if !defined(ACDK_OS_WIN32)
    if (ret > 0)
    {
      int valopt = getSockErr(fd->c_fd());
      if (valopt != 0) 
      {
        THROW1(SocketException, toString() + "; Connect Socket Failed: [" + address->toString()
                            + ":" + String::valueOf(port) + "]: " +  getSockError(valopt));
      }
    }
    arg = fcntl(soc, F_GETFL, NULL); 
    arg &= ~O_NONBLOCK; 
    fcntl(soc, F_SETFL, arg); 

#endif
    if (ret == 0)
      return false;
  }
  #if !defined(ACDK_OS_WIN32)
  else {
#endif
  DOUT("> ::connect(" + address->toString() + ", " + port + ")");
  int ret = ::connect( fd->c_fd(),( struct sockaddr * ) &_address, sizeof( _address));
  DOUT("< ::connect(" + address->toString() + ", " + port + ")");
  if ( ret < 0 ) 
  {
    THROW1(SocketException, toString() + "; Connect Socket Failed: [" + address->toString()
                            + ":" + String::valueOf(port) + "]: " +  getError(fd->c_fd()));
  }
 #if !defined(ACDK_OS_WIN32)
  }
#endif
  this->address = address;
  this->port = port;
  _connected = true;
  return true;
}



// Accepts a connection.
bool
TCPSocket::accept(IN(RSocketImpl) s, int timeOut)
{
  
  sockaddr cliaddr;
#if defined(ACDK_OS_LINUX) || defined(ACDK_OS_BSD) || defined(ACDK_OS_SOLARIS)
  typedef unsigned int sockl_t;
  /* doesn't work on Solaris machine on cf.sf.net
#elif defined(ACDK_OS_SOLARIS)
  typedef socketlen_t sockl_t
  */
#else
  typedef int sockl_t;
#endif
  
  sockl_t len = sizeof(cliaddr);
  if (timeOut != -1)
  {
    timeval tv; 
    ms2TimeVal(timeOut, tv);
    fd_set fs; //fs.fd_count = 1;
    FD_ZERO(&fs);
    FD_SET(fd->c_fd(), &fs);
    int ret = select(fd->c_fd() + 1, &fs, 0, 0, &tv);
    if (ret == 0)
      return false;
  }
  DOUT("> ::accept(" << s->toString() << ", FD=" << fd->c_fd() << ")");
  int newfd = ::accept( fd->c_fd(), &cliaddr, &len);
  DOUT("< ::accept(" + s->toString() + ", FD=" + fd->c_fd() + ")");
  if ( newfd < 0 ) 
  {
    THROW1(SocketException, toString() + "; Socket Accept Failed!");
  }
  RTCPSocket client = (RTCPSocket) s;
  client->fd = new FileDescriptor(newfd, O_RDWR);
  RbyteArray clientaddress = new byteArray(4);
  
  clientaddress[0] = cliaddr.sa_data[2]; 
  clientaddress[1] = cliaddr.sa_data[3];
  clientaddress[2] = cliaddr.sa_data[4];
  clientaddress[3] = cliaddr.sa_data[5];

  RInetAddress cadr = new InetAddress( clientaddress );

  client->address = cadr;
  client->port    = ntohs(((sockaddr_in*)(&cliaddr))->sin_port);
  client->_address = *((sockaddr_in*)(&cliaddr));
  client->_connected = true;
  return true;
}

// Returns the number of bytes that can be read from this socket without blocking.
// virtual
int
TCPSocket::available()
{
#ifdef ACDK_OS_WIN32
  unsigned long retval = 0;
  if (ioctlsocket(fd->c_fd(), FIONREAD, &retval) != 0)
    THROW1(SocketException, toString() + "; ioctlsocket failed: " +  getError(fd->c_fd()));
#else
  int retval = 0;
#if defined(FIONREAD) // may have solaris
  int erg = ioctl(fd->c_fd(), FIONREAD, &retval);
  if (erg < 0)
    THROW1(IOException, toString() + "; ioctl(,FIONREAD,) failed: " +  getError(fd->c_fd()));

#else
  struct timeval tm = {0,0};
  fd_set rd;
  FD_ZERO(&rd);
  FD_SET(fd->c_fd(), &rd);
  int erg = select(fd->c_fd() + 1, &rd, NULL, NULL, &tm);
  if (1 == erg)
    retval = 1;
  else
    retval = 0;
#endif
#endif
  return retval;
}

// Binds this socket to the specified port number on the specified host.
// virtual
void
TCPSocket::bind(IN(RInetAddress) host, int port)
{
  if (port == -1)
    port = 0;

  _address.sin_family = AF_INET;
  _address.sin_port = htons(port);
  if (host == Nil)
    _address.sin_addr.s_addr = INADDR_ANY;
  else
    _address.sin_addr.s_addr =* (u_long*) host->getAddress()->data();

  // We have to cast from sockaddr_in to sockkaddr here ( See Stevens Unix Network
  // Programming pg. 61
  DOUT("> ::bind(" + host->toString() + ", FD=" + fd->c_fd() + ")");
  int ret = ::bind( fd->c_fd(),( struct sockaddr * ) &_address, sizeof( _address));
  DOUT("< ::bind(" + host->toString() + ", FD=" + fd->c_fd() + ") = " << ret);
  if ( ret < 0 )
    THROW1(SocketException, toString() + "; Bind Socket Failed: " + getError(fd->c_fd()));

  _resolveLocalPortAddress();
  _bound = true;
}

// Closes this socket.
// virtual
void
TCPSocket::close()
{

  if (_connected == true)
  {
    flush();
    shutdownInput();
    shutdownOutput();
    _connected = false;
  }
  if (fd != Nil)
  {
     DOUT("::close(FD=" << fd->c_fd() << ")");
#if defined(ACDK_OS_WIN32)
#if !defined(SD_BOTH)
# define SD_RECEIVE      0x00
# define SD_SEND         0x01
# define SD_BOTH         0x02
#endif
  
  
  //::shutdown(fd->c_fd(), SD_BOTH);
   ::closesocket( fd->c_fd() );
 #else
  //::shutdown(fd->c_fd(), SHUT_RDWR);
  ::close( fd->c_fd() );
#endif
    fd = Nil;
  }
}


void 
TCPSocket::send(IN(RDatagramPacket) packet)
{
  RReadByteBuffer buf = Buffers::getNativeReadByteBuffer(&packet->getData());
  sockaddr_in address;

  inetSocketAddress2native(packet->getSocketAddress(), address);
  DOUT("> ::sendto(FD=" << fd->c_fd() << "; addres: " << packet->getSocketAddress()->toString() << ";buflen=" << buf->length() << ";buffer: " << Buffers::toString(&buf) << ")");
  int ret = sendto(fd->c_fd(), (char*)buf->begin(), buf->length(), 0, ( struct sockaddr * )&address, sizeof(address));
  DOUT("< ::sendto(FD=" << fd->c_fd() << "; addres: " << packet->getSocketAddress()->toString() << "; buf=" << Buffers::toString(&buf) << ") = " << ret);
  if ( ret < 0 )
    THROW1(SocketException, toString() + "; sending datagram failed: " + getError(fd->c_fd()));
}

void 
TCPSocket::receive(IN(RDatagramPacket) packet)
{
  RReadByteBuffer buf = Buffers::getNativeReadByteBuffer(&packet->getData());
  sockaddr_in address;
  memset(&address, 0, sizeof(address));
  socklen_t address_size = sizeof(address);
  DOUT("> ::recvfrom(FD=" << fd->c_fd() << "; addres: " << toString() << ";buflen=" << buf->length() << ")");
  int ret = recvfrom(fd->c_fd(), (char*)buf->begin(), buf->length(), 0, ( struct sockaddr * )&address, &address_size);
  DOUT("< ::recvfrom(FD=" << fd->c_fd() << "; addres: " << toString() << "; buf=" << Buffers::toString(&buf) << ") = " << ret);
  if ( ret < 0 )
    THROW1(SocketException, toString() + "; receiving datagram failed: " + getError(fd->c_fd()));

	packet->setLength(ret);
	packet->setSocketAddress(native2InetSocketAddress(address));
}

// Connects this socket to the specified port on the named host.
// virtual
bool
TCPSocket::connect(IN(RString) host, int port, int timeOut)
{
  return connect(InetAddress::getByName(host), port, timeOut);
}


RInetAddress
TCPSocket::getInetAddress()
{
  if (localport == 0)
    _resolveLocalPortAddress();
  return address;
}

int
TCPSocket::getLocalPort()
{
  if (localport == 0)
    _resolveLocalPortAddress();
  return localport;
}

void
TCPSocket::_resolveLocalPortAddress()
{
#if defined(ACDK_OS_WIN32)
  socklen_t sz = sizeof(_address);
  int ret = ::getsockname(fd->c_fd(), (sockaddr*)&_address, &sz);
  if (ret != 0)
    THROW1(SocketException, toString() + "; getsockname failed: " + getError(fd->c_fd()));
  localport = htons(_address.sin_port);
  RbyteArray ipNumber = new byteArray(4);
  ipNumber[0] = _address.sin_addr.S_un.S_un_b.s_b1;
  ipNumber[1] = _address.sin_addr.S_un.S_un_b.s_b2;
  ipNumber[2] = _address.sin_addr.S_un.S_un_b.s_b3;
  ipNumber[3] = _address.sin_addr.S_un.S_un_b.s_b4;
  address = new InetAddress(ipNumber);
#else
  socklen_t len = sizeof(_address);
  int ret = getsockname(fd->c_fd(), (sockaddr*)&_address, &len);
  if (ret != 0)
    THROW1(SocketException, toString() + "; getsockname failed: " + getError(fd->c_fd()));
  localport = htons(_address.sin_port);
  address = new InetAddress(_address.sin_addr.s_addr);
#endif

}

// Returns an input stream for this socket.
// virtual
RReader
TCPSocket::getInputStream()
{
  return this;
}

// Returns an output stream for this socket.
// virtual
RWriter TCPSocket::getOutputStream()
{
  return this;
}

// Sets the maximum queue length for incoming connection indications (a request to connect) to the count argument.
// virtual

void TCPSocket::listen(int backlog)
{
  int erg = 0;
  if ( (erg = ::listen(fd->c_fd(), backlog)) < 0 ) {
    THROW1(IOException, toString() + "; listen() Failed: " + getError(fd->c_fd()));
  }
  DOUT("::listen(" << fd->c_fd() << ", backlog: " <<  backlog << " ) = " << erg);
  _resolveLocalPortAddress();
}



jlong
TCPSocket::seek(SeekPos seekrel, jlong seekpos)
{
  THROW0(UnsupportedOperationException);
  return 0;
}

jlong
TCPSocket::skip(jlong n)
{
  byte buf[BUFSIZ];
  return read(buf,0,n);
}

int
TCPSocket::read()
{
  byte rd;
  int count;
  count = read((byte*)&rd,0,1);
  if (count == 1)
    return rd;
  return -1;
}

int
TCPSocket::read(IN(RbyteArray) buffer, int offset, int len )
{
  if (len == -1)
    len = buffer->length() - offset;
  buffer->ensureCapacity(len - offset);
  int retval = read( buffer->data(), offset, len);
  return retval;
}


int
TCPSocket::read(byte* buffer, int offset, int len)
{
  if (_eof == true)
  {
    if (_eofReturned == false)
    {
      _eofReturned = true;
      return -1;
    }
    THROW1(EOFException, toString());
  }
  byte* buffptr = buffer + offset;
  int rest = len;
  int readed = 0;
  int sumreaded = 0;
  while (rest > 0)
  {
    DOUT("> ::recv(" << fd->c_fd() << ", " << rest << ", " << RECV_OPTION << ")");
    readed = ::recv( fd->c_fd(), (char*)buffptr, rest, RECV_OPTION);
    DOUT("< ::recv(" << fd->c_fd() << ", " << rest << ", " << RECV_OPTION << ") = " << readed);
    if (readed < 0)
      THROW1(IOException, toString() + "; recv failed. " + getError(fd->c_fd()));
    if (readed == 0)
    {
      _eof = true;
      if (sumreaded != 0)
        return sumreaded;
      _eofReturned = true;
      return -1;
    }
    rest -= readed;
    sumreaded += readed;
    buffptr += readed;
  }
  return sumreaded;
  /*

  if( offset > 0) {
    byte buf[BUFSIZ];
    int skiped = 0;
    do {
      int readnum = (offset < BUFSIZE ? BUFSIZE : offset);
      int readed = ::recv( fd->c_fd(), (char*)buf, readnum, RECV_OPTION);
      skiped = readed;
    } while (skiped <
    if( offset < BUFSIZ ) {
      int o = ::recv( fd->c_fd(), (char*)buf, offset, 0
#if defined(ACDK_OS_LINUX)
        | MSG_NOSIGNAL  // prevent SIG_PIPE on failures
#endif
        );

      if (o != offset) {
        THROW1(IOException, toString() + "; Offset exceeded eof" + String::valueOf(offset));
      }
    } else {
      THROW1(IOException, toString() + "; Offset too large: " + String::valueOf(offset));
    }
  }
  if( len < 0 )
    len = BUFSIZ;
  int i = 0;
  do {
    i += recv( fd->c_fd(), (char*)buffer + i, len-i, 0
#if defined(ACDK_OS_LINUX)
        | MSG_NOSIGNAL  // prevent SIG_PIPE on failures
#endif
        );
  } while (i > 0 && i < len);

  if ( i < 0 )
    THROW1(IOException, toString() + "; recv failed. " + getError(fd->c_fd()));
  return i;
  */
}

#ifdef WIN32
typedef int socklen_t;
#endif

void
TCPSocket::write(const byte* cstr, int offset, int len)
{
  if ( len == -1 )
    len = strlen((char*)cstr);
  int i = 0;
  do {
    DOUT("> ::send(" << fd->c_fd() << ", len: " << len << ")");
    i = ::send( fd->c_fd(), (const char*)cstr , len, 0
#if defined(ACDK_OS_LINUX)
        | MSG_NOSIGNAL  // prevent SIG_PIPE on failures
#endif
        );
    DOUT("< ::send(" << fd->c_fd() << ", len: " << len << ") = " << i);
    if (i < 0)
      THROW1(SocketException, toString() + "; send failed. Reason :" + getError(fd->c_fd()));

    if (i < len) {
      cstr += i;
      len = len - i;
    } else
       break;
  } while (i < len);
}

void
TCPSocket::write(byte c)
{
  byte cbuf[2]; cbuf[1] = 0; cbuf[0] = c;
  write((const byte*)cbuf, 0, 1);
}

void
TCPSocket::write(IN(RbyteArray) ch, int offset , int len)
{
  write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
}

void
TCPSocket::flush()
{
}

void
TCPSocket::reset()
{
  THROW0(UnsupportedOperationException);
}

bool
TCPSocket::ready()
{
  return _connected;
}

void 
TCPSocket::setSoTimeout(int timeOut)
{
  //_timeOutSet = true;
  Integer it(timeOut);
  SocketImpl::setSockOption(fd, _SO_TIMEOUT, &it);
}

int 
TCPSocket::getSoTimeout()
{
  return RInteger(SocketImpl::getSockOption(fd, _SO_TIMEOUT))->intValue();
}
/* not needed
int 
TCPSocket::_setTimeOut(int timeOut)
{
  if (timeOut == -1)
    timeOut = 0;
  if (_timeOut == timeOut)
    return _timeOut;
  
  Integer it(timeOut);
  SocketImpl::setSockOption(fd, _SO_TIMEOUT, &it);
  int ret = _timeOut;
  _timeOut = timeOut;
  return ret;
}


void 
TCPSocket::_restoreTimeOut(int timeOut)
{
  if (_timeOutSet == false)
    return;
  Integer it(timeOut);
  SocketImpl::setSockOption(fd, _SO_TIMEOUT, &it);
  _timeOut = timeOut;
}
*/
int 
TCPSocket::getSetSocketErrno(int newErrno)
{
#if defined(ACDK_OS_WIN32)
  int ret = WSAGetLastError();
  if (newErrno != -1)
    WSASetLastError(newErrno);
  return ret;
#else
  int ret = errno;
  if (newErrno != -1)
    errno = newErrno;
  return ret;
#endif

}

} // net
} // acdk
