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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/TransRateReader.h,v 1.9 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_TransRateReader_h
#define acdk_net_TransRateReader_h

#include "net.h"

#include <acdk/lang/sys/core_tick.h>
#include <acdk/io/AbstractFilterReader.h>

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;
using ::acdk::lang::sys::tick_t;
using ::acdk::lang::sys::core_tick;

ACDK_DECL_INTERFACE(TransListener);

/**
  Works as a Listener for transfer rate
*/
class ACDK_NET_PUBLIC TransListener
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(TransListener)
public:
  virtual bool listen(IN(byte) block, int block_size, int millisecs) = 0;
};


ACDK_DECL_CLASS(TransRateReader);

/** 
  A Reader Filter which limits the tranfer rate
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:29 $
*/


class ACDK_NET_PUBLIC TransRateReader 
: public acdk::io::AbstractFilterReader
{
  ACDK_WITH_METAINFO(TransRateReader)
public:
  int _bytePerSecondRate;
  int _bytesRead;
  int _bytesWhileSleepASec;
  foreign tick_t _lastReadTick;
  int _blockSize;
  RTransListener _listener;
  TransRateReader(IN(::acdk::io::RReader) in, int bytesPerSecond)
  : AbstractFilterReader(in)
  , _bytePerSecondRate(bytesPerSecond)
  , _bytesRead(0)
  , _lastReadTick(0)
  , _blockSize(1024)
  {
    
  }
  virtual int read()
  {
    if (_bytePerSecondRate == 0)
      return AbstractFilterReader::read();

    if (core_tick::millisecsSince(_lastReadTick) > 1000)
    {
      _lastReadTick = 0;
      _bytesRead = 0;
    }
    ++_bytesRead;
    return AbstractFilterReader::read();
  }
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1)
  {
    
    int reallen = len;
    if (reallen == -1)
      reallen = buffer->length() - offset;
    return read(buffer->data(), offset, reallen);
  }
  foreign virtual int read(byte* buffer, int offset, int len);

  void setBytesPerSecondRate(int newrate)
  {
    _bytePerSecondRate = newrate;
  }

};

} // net
} // acdk

#endif // acdk_net_TransRateReader_h

