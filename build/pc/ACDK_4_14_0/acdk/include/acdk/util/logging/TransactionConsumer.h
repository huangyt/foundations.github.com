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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/TransactionConsumer.h,v 1.6 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_TransactionConsumer_h
#define acdk_util_logging_TransactionConsumer_h

#include "AbstractLogConsumer.h"
#include <acdk/lang/System.h>
#include <acdk/lang/sys/core_tick.h>
#include <acdk/io/FileWriter.h>

namespace acdk {
namespace util {
namespace logging {


ACDK_DECL_CLASS(TransactionConsumer);

ACDK_DECL_ARRAY(LogRecordArray);

/**
  The TransactionConsumer wrapps the logging
  into a transaction. From a begin of a logging transaction all log records
  will be kept in the memory. If the transaction will be commited all
  records of the transaction will be discarged. If the the transaction
  will be roll back all log records of the transaction will be written
  to the underlying consumer.
  The TransactionConsumer is usefull not to waste the log files
  with superflous information, but in case of an error situation
  give supporters full trace information about the situation.
  @author Roger Rene Kommer
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC TransactionConsumer
: extends AbstractLogConsumer
{
  ACDK_WITH_METAINFO(TransactionConsumer)
protected:
  RLogConsumer _consumer;
  RLogRecordArrayArray _queues;
  /**
    if LogLevel is >- _rollBackLogLevel
    rollback will be called and recorded
    LogRecors will be written to underlying consumer
  */
  int _rollBackLogLevel;
  

public:
  TransactionConsumer(IN(RLogConsumer) consumer, int rollBackMinLevel)
  : AbstractLogConsumer()
  , _consumer(consumer)
  , _queues(new LogRecordArrayArray(1))
  , _rollBackLogLevel(rollBackMinLevel)
  {
    _queues[0] = new LogRecordArray(0);
  }
  foreign void publish(IN(RLogRecord) rec) 
  {
    int rl = rec->level;
    if (rl == TransCommit)
    {
      commit();
    }
    else if (rl == TransBegin)
    {
      begin();
      _queues[_queues->length() - 1]->append(rec);
    }
    else if (rl == TransRollback || rl >= _rollBackLogLevel)
    {
      _queues[_queues->length() - 1]->append(rec);
      rollback();
    }
    else
    {
      _queues[_queues->length() - 1]->append(rec);
    }
  }
  /**
    Start new sub transaction.
    should be closed by commit
  */
  void begin()
  {
    _queues->append(new LogRecordArray(0));
  }
  /**
    throws away all log records of this level
  */
  void commit();
  /**
    write all LogRecors into underlying consumer
  */
  void rollback();
  //foreign virtual void configure(IN(RString) propnameprefix, IN(RProperties) props);
};

/**
  Helper class to deal with TransactionConsumer
  Used by the macros ACDK_TRANSLOG_BEGIN
  and ACDK_TRANSLOG_COMMIT
*/
foreign class ACDK_CORE_PUBLIC TransactionFrame
{
  bool _commited;
  RString _logname;
  RString _msg;
  acdk::lang::sys::tick_t _startTime;
public:
  TransactionFrame(IN(RString) logname, IN(RString) msg);
  ~TransactionFrame();
  void commit();
  
};

/**
  Starts a log transaction
  @see TransactionConsumer
*/
#define ACDK_TRANSLOG_BEGIN(logname, msg) \
  ::acdk::util::logging::TransactionFrame _log_transaction(logname, msg)

/**
  Ends a log transaction
*/
#define ACDK_TRANSLOG_COMMIT() \
  _log_transaction.commit()

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_TransactionConsumer_h
