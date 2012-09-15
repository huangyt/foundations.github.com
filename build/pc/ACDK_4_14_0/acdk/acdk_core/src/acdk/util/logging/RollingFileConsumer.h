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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/RollingFileConsumer.h,v 1.6 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_RollingFileConsumer_h
#define acdk_util_logging_RollingFileConsumer_h

#include "FileConsumer.h"

namespace acdk {
namespace util {
namespace logging {


ACDK_DECL_CLASS(RollingFileConsumer);
/*
  Writes into a rolling log files.
  A RollingLogFileConsumer can be configured
  with following parameters:
  - FileNameBase: 
    A FileNameBase where Filename will be 
    generated as <FileNameBase>.<number>.log
    The underlying FileSystem has to support write() and
    renameTo().
  - MaxRecordNum (_maxRecordNum):
    How many records should be written into one log file
  - MaxBackupIndex (_maxBackupIndex):
    How many backup files of the log files should be managed.
  
  In case in a Logfile the maximum records are written a the rollOver
  method moves the log files following way, asuming MaxBackupIndex is 3:
  
  new file        -> becomes MyLogFile.1.log
  MyLogFile.1.log -> becomes MyLogFile.2.log
  MyLogFile.2.log -> becomes MyLogFile.3.log
  MyLogFile.3.log -> throw away.

  @author Roger Rene Kommer
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC RollingFileConsumer
: extends FileConsumer
{
  ACDK_WITH_METAINFO(RollingFileConsumer)
protected:
  int _maxBackupIndex;
  int _maxRecordNum;
  int _curRecordNum;
  int _flushLevel;
public:
  RollingFileConsumer(INP(RFormatter) formatter = Nil, int minll = AllSys, int maxll = None, bool buffered = false)
  : FileConsumer(formatter, minll, maxll, buffered)
  , _maxBackupIndex(0)
  , _maxRecordNum(0)
  , _curRecordNum(0)
  , _flushLevel(Note)
  {
  }
  /**
    @see FileConsumer::FileConsumer
  */
  RollingFileConsumer(INP(RString) fnameBase, int maxBackupIndex, int maxRecordNum, INP(RFormatter) formatter = Nil, int minll = All, int maxll = None, bool buffered = false)
  : FileConsumer(fnameBase, formatter, minll, maxll, buffered)
  , _maxBackupIndex(maxBackupIndex)
  , _maxRecordNum(maxRecordNum)
  , _curRecordNum(0)
  , _flushLevel(Note)
  {
  }
  foreign void publish(INP(RLogRecord) rec) 
  {
    if (_buffered == true && (rec->level == TransCommit || rec->level >= _flushLevel))
      _writer->flush();

    if (wantPublish(rec) == false) 
      return;
    
    SYNCTHIS();
    if (_writer == Nil || (++_curRecordNum > _maxRecordNum))
      rollOver();
    getFormatter()->format(rec, &_writer);
    
    
  }
  int getFlushLevel() { return _flushLevel; }
  void setFlushLevel(int level) { _flushLevel = level; }
  void rollOver();
  foreign virtual void configure(IN(RString) propnameprefix, IN(RProperties) props);
};

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_RollingFileConsumer_h
