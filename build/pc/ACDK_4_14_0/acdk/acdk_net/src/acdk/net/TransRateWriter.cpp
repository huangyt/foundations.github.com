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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/TransRateWriter.cpp,v 1.8 2005/02/05 10:45:29 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>

#include "TransRateWriter.h"

namespace acdk {
namespace net {


//foreign virtual 
void
TransRateWriter::write(const byte* cstr, int offset, int len)
{
  if (_bytePerSecondRate == 0)
  {
    AbstractFilterWriter::write(cstr, offset, len);
    return;
  }

  int sumwritten = 0;  
  const byte* buffer = cstr + offset;
  if (core_tick::millisecsSince(_lastWriteTick) > 1000)
  {
    _lastWriteTick = 0;
    _bytesWritten = 0;
  }
  if (len < 1024)
  {
    AbstractFilterWriter::write(buffer, 0, len);
    return;
  }
  int rest = len;
  
  int timewritten = 0;
  _lastWriteTick = core_tick::now();
  tick_t lastreptick = core_tick::now();
  while (rest > 0)
  {
    int toWrite = (rest < _blockSize ? rest : _blockSize);
    AbstractFilterWriter::write(buffer, 0, toWrite);
    if (toWrite < _blockSize)
    {
      sumwritten += toWrite;
      return;
    }
    sumwritten += toWrite;
    timewritten += toWrite;
    int resttosec = core_tick::millisecsSince(_lastWriteTick);
    
    if (timewritten > _bytePerSecondRate &&  resttosec < 1000)
    {
      if (_listener != Nil)
      {
        if (_listener->listen(*buffer, timewritten,  core_tick::millisecsSince(lastreptick)) == false)
          return ;
        lastreptick = core_tick::now();
      }
      Thread::sleep(1000 - resttosec);
      timewritten = 0;
      _lastWriteTick = core_tick::now();
    }
    buffer += _blockSize;
    rest -= _blockSize;
  }
}
  



} // net
} // acdk



