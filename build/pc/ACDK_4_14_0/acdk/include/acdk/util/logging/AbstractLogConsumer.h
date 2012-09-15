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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/AbstractLogConsumer.h,v 1.11 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_AbstractLogConsumer_h
#define acdk_util_logging_AbstractLogConsumer_h

#include "LogConsumer.h"
#include "LogManager.h"

namespace acdk {
namespace util {
namespace logging {



ACDK_DECL_CLASS(AbstractLogConsumer);
/**
  Abstract/incomplete implementation of the LogConsumer interface
*/
class ACDK_CORE_PUBLIC AbstractLogConsumer
: extends ::acdk::lang::Object
, implements LogConsumer
{
  ACDK_WITH_METAINFO(AbstractLogConsumer)
protected:
  RFormatter _formatter;
  int minLogLevel;
  int maxLogLevel;
public:
  AbstractLogConsumer(IN(RFormatter) formatter = Nil, int minll = AllSys, int maxll = None)
  : Object()
  , _formatter(formatter)
  , minLogLevel(minll)
  , maxLogLevel(maxll)
  {
  }

  foreign virtual void configure(IN(RString) propnameprefix, IN(RProperties) props);
  foreign virtual void publish(IN(RLogRecord) rec) = 0;

  virtual int getMinLevel() { return minLogLevel; }
  virtual void setMinLevel(int mn) { minLogLevel = mn; }
  virtual int getMaxLevel() { return maxLogLevel; }
  virtual void setMaxLevel(int mn) { maxLogLevel = mn; }
  RFormatter getFormatter()
  { 
    if (_formatter == Nil)
      return LogManager::getStandardFormatter();
    return _formatter; 
  }
  void setFormatter(INP(RFormatter) formatter) { _formatter = formatter; }

  inline bool wantPublish(IN(RLogRecord) rec) { return rec->level >= minLogLevel && rec->level <= maxLogLevel; }
  
};

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_AbstractLogConsumer_h
