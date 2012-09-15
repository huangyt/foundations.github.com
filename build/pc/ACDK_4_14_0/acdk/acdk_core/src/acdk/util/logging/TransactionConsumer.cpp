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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/TransactionConsumer.cpp,v 1.5 2005/02/05 10:45:08 kommer Exp $

#include <acdk.h>
#include "Log.h"

#include "TransactionConsumer.h"

namespace acdk {
namespace util {
namespace logging {


void
TransactionConsumer::commit()
{
  SYNCTHIS();
  _queues->remove(_queues->length() - 1);
  if (_queues->length() == 0)
    _queues->append(new LogRecordArray(0));
}

void
TransactionConsumer::rollback()
{
  SYNCTHIS();
  for (int i = 0; i < _queues->length(); ++i)
  {
    for (int j = 0; j < _queues[i]->length(); ++j)
    {
      _consumer->publish(_queues[i][j]);
    }
  }
  _queues = new LogRecordArrayArray(1);
  _queues[0] = new LogRecordArray(0);
}


TransactionFrame::TransactionFrame(INP(RString) logname, INP(RString) msg)
: _commited(false)
, _logname(logname)
, _msg(msg)
, _startTime(acdk::lang::sys::core_tick::now())
{
  ACDK_NLOG(logname, TransBegin, "Start LogTrans: " << msg);
}

void 
TransactionFrame::commit() 
{ 
  if (_commited == true)
    return;
  ACDK_NLOG(_logname, TransCommit, "Commit LogTrans (" << acdk::lang::sys::core_tick::millisecsSince(_startTime) << " ms):" + _msg);
  _commited = true; 
}

TransactionFrame::~TransactionFrame()
{
  if (_commited == true)
    return;
  ACDK_NLOG(_logname, TransRollback, "Failed LogTrans (" << acdk::lang::sys::core_tick::millisecsSince(_startTime) << " ms):" + _msg);
}

/*
//foreign virtual 
void 
FileConsumer::configure(IN(RString) propnameprefix, IN(RProperties) props)
{
  AbstractLogConsumer::configure(propnameprefix, props);
  _fname = props->getProperty(propnameprefix + ".filename");
  RString appendstr = props->getProperty(propnameprefix + ".append");
  bool append = true;
  if (appendstr == Nil || Boolean::getBoolean(appendstr) == true)
    append  = true;
  else
    append = false;
  _writer = new ::acdk::io::CharToByteWriter(new ::acdk::io::FileWriter(_fname, append));
}
*/

} // namespace logging
} // namespace util
} // namespace acdk


