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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/TransRateWriter.h,v 1.9 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_TransRateWriter_h
#define acdk_net_TransRateWriter_h

#include "net.h"

#include "TransRateReader.h"
#include <acdk/io/AbstractFilterWriter.h>

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;
using ::acdk::lang::sys::tick_t;
using ::acdk::lang::sys::core_tick;

ACDK_DECL_CLASS(TransRateWriter);

/** 
  A Writer Filter which limits the tranfer rate.
  If bytesPerSecond == 0 no limitaton will be used.

  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:29 $
*/


class ACDK_NET_PUBLIC TransRateWriter 
: public acdk::io::AbstractFilterWriter
{
  ACDK_WITH_METAINFO(TransRateWriter)
public:
  int _bytePerSecondRate;
  int _bytesWritten;
  int _bytesWhileSleepASec;
  foreign tick_t _lastWriteTick;
  int _blockSize;
  RTransListener _listener;
  TransRateWriter(IN(::acdk::io::RWriter) out, int bytesPerSecond)
  : AbstractFilterWriter(out)
  , _bytePerSecondRate(bytesPerSecond)
  , _bytesWritten(0)
  , _lastWriteTick(0)
  , _blockSize(1024)
  {
    
  }
  foreign virtual void write(byte c)
  {
    if (_bytePerSecondRate == 0)
    {
      AbstractFilterWriter::write(c);
      return;
    }
    if (core_tick::millisecsSince(_lastWriteTick) > 1000)
    {
      _lastWriteTick = 0;
      _bytesWritten = 0;
    }
    ++_bytesWritten;
    AbstractFilterWriter::write(c);
  }
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1)
  {
    if (len == -1)
      len = ch->length() - offset;
    if (ch->length() < offset + len)
      THROW0(ArrayIndexOutOfBoundsException);
    write(ch->data(), offset, len);
  }
  foreign virtual void write(const byte* cstr, int offset, int len);
  void setBytesPerSecondRate(int newrate)
  {
    _bytePerSecondRate = newrate;
  }
};

} // net
} // acdk

#endif // acdk_net_TransRateWriter_h

