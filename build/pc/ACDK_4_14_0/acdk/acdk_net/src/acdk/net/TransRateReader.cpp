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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/TransRateReader.cpp,v 1.8 2005/02/05 10:45:29 kommer Exp $


#include "TransRateReader.h"
#include <acdk/lang/Thread.h>

namespace acdk {
namespace net {


//foreign virtual 
int 
TransRateReader::read(byte* buffer, int offset, int len)
{
  if (_bytePerSecondRate == 0)
      return AbstractFilterReader::read(buffer, offset, len);

  int sumreaded = 0;  
  buffer += offset;
  if (core_tick::millisecsSince(_lastReadTick) > 1000)
  {
    _lastReadTick = 0;
    _bytesRead = 0;
  }
  if (len < 1024)
    return AbstractFilterReader::read(buffer, 0, len);
  int rest = len;
  
  int timereaded = 0;
  _lastReadTick = core_tick::now();
  tick_t lastreptick = core_tick::now();
  while (rest > 0)
  {
    int toRead = _blockSize < rest ? _blockSize : rest;
    int readed = AbstractFilterReader::read(buffer, 0, toRead);
    if (readed < _blockSize)
    {
      sumreaded += readed;
      return sumreaded;
    }
    sumreaded += readed;
    timereaded += readed;
    int resttosec = core_tick::millisecsSince(_lastReadTick);
    
    if (timereaded > _bytePerSecondRate &&  resttosec < 1000)
    {
      if (_listener != Nil)
      {
        if (_listener->listen(*buffer, timereaded,  core_tick::millisecsSince(lastreptick)) == false)
          return sumreaded;
        lastreptick = core_tick::now();
      }
      Thread::sleep(1000 - resttosec);
      timereaded = 0;
      _lastReadTick = core_tick::now();
    }
    buffer += readed;
    rest -= readed;
  }
  return sumreaded;
}


} // net
} // acdk



