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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/DatagramPacket.h,v 1.1 2005/03/26 15:02:02 kommer Exp $

#ifndef acdk_net_DatagramPacket_h
#define acdk_net_DatagramPacket_h

#include "net.h"
#include "InetSocketAddress.h"
#include <acdk/lang/ByteBuffer.h>

namespace acdk {
namespace net {


ACDK_DECL_CLASS(DatagramPacket);

/** 

  API: Java with extentions
  @author Roger Rene Kommer
  @version $Revision: 1.1 $
  @date $Date: 2005/03/26 15:02:02 $
*/
class ACDK_NET_PUBLIC DatagramPacket 
: public acdk::lang::Object  
{
  ACDK_WITH_METAINFO(DatagramPacket)
protected:
  RInetSocketAddress _address;
  RReadWriteByteBuffer _buffer;
  
public:
  DatagramPacket(IN(RbyteArray) buf, int length)
  : _address(Nil)
  , _buffer(new ArrayReadWriteByteBuffer(buf, 0, length))
  {
  }
  DatagramPacket(IN(RReadWriteByteBuffer) buf)
  : _address(Nil)
  , _buffer(buf)
  {
  }
  DatagramPacket(IN(RbyteArray) buf, int length, IN(RInetAddress) address, int port)
  : _address(new InetSocketAddress(address, port))
  , _buffer(new ArrayReadWriteByteBuffer(buf, 0, length))
  {
  }
  DatagramPacket(IN(RReadWriteByteBuffer) buf, IN(RInetAddress) address, int port)
  : _address(new InetSocketAddress(address, port))
  , _buffer(buf)
  {
  }
  DatagramPacket(IN(RbyteArray) buf, int offset, int length)
  : _address(Nil)
  , _buffer(new ArrayReadWriteByteBuffer(buf, offset, length == -1 ? length : offset + length))
  {
  }
  DatagramPacket(IN(RbyteArray) buf, int offset, int length, IN(RInetAddress) address, int port)
  : _address(new InetSocketAddress(address, port))
  , _buffer(new ArrayReadWriteByteBuffer(buf, offset, length == -1 ? length : offset + length))
  {
  }
  DatagramPacket(IN(RbyteArray) buf, int offset, int length, IN(RInetSocketAddress) address) 
  : _address(address)
  , _buffer(new ArrayReadWriteByteBuffer(buf, offset, length == -1 ? length : offset + length))
  {
  }
  DatagramPacket(IN(RbyteArray) buf, int length, IN(RInetSocketAddress) address) 
  : _address(address)
  , _buffer(new ArrayReadWriteByteBuffer(buf, 0, length))
  {
  }
  DatagramPacket(IN(RReadWriteByteBuffer) buf, IN(RInetSocketAddress) address) 
    : _address(address)
    , _buffer(buf)
  {
  }

  RReadWriteByteBuffer getData() { return _buffer; }
  int getLength() { return _buffer->length(); }
  int getPort() 
  { 
    if (instanceof(_address, InetSocketAddress) == false)
      return 0;
    return RInetSocketAddress(_address)->getPort(); 
  }
  RInetSocketAddress getSocketAddress() { return _address; }
  //void setAddress(IN(RInetAddress) iaddr) { _address = new InetSocketAddress(iaddr); }
  void setData(IN(RbyteArray) buf) { _buffer = new ArrayReadWriteByteBuffer(buf); }
  void setData(IN(RbyteArray) buf, int offset, int length) { _buffer = new ArrayReadWriteByteBuffer(buf, offset, length == -1 ? length : offset + length); }
  void setData(IN(RReadWriteByteBuffer) buffer) { _buffer = buffer; }
  void setLength(int length) { _buffer = _buffer->createReadWriteSlice(0, length); }
  //void setPort(int iport) { _address->setPort(iport); }
  void setSocketAddress(IN(RInetSocketAddress) address) { _address = address; }
};

} // net
} // acdk

#endif // acdk_net_DatagramPacket_h

