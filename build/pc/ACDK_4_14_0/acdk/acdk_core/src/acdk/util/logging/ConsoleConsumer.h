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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/ConsoleConsumer.h,v 1.10 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_ConsoleConsumer_h
#define acdk_util_logging_ConsoleConsumer_h

#include "AbstractLogConsumer.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace util {
namespace logging {


ACDK_DECL_CLASS(ConsoleConsumer);

/**
  Write the logs into the standard or error ouput console.
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC ConsoleConsumer
: extends AbstractLogConsumer
{
  ACDK_WITH_METAINFO(ConsoleConsumer)
protected:
  bool _logToStdErr;
public:
  /**
    @param formatter which format to log
    @param minll minimum loglevel
    @param maxll maximum loglevel
    @param stdErr if true writes to System::err, else to System::out
  */
  ConsoleConsumer(IN(RFormatter) formatter = Nil, int minll = AllSys, int maxll = None, bool stdErr = false)
    : AbstractLogConsumer(formatter, minll, maxll)
    , _logToStdErr(stdErr)
  {
  }
  foreign void publish(IN(RLogRecord) rec) 
  {
    if (wantPublish(rec) == false) 
      return;
    if (_logToStdErr == true)
    {
      _formatter->format(rec, &::acdk::lang::System::err);
      ::acdk::lang::System::err->flush();
    }
    else
    {
       _formatter->format(rec, &::acdk::lang::System::out);
      ::acdk::lang::System::out->flush();
    }
  }
  foreign virtual void configure(IN(RString) propnameprefix, IN(RProperties) props)
  {
    AbstractLogConsumer::configure(propnameprefix, props);
  }
};

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_ConsoleConsumer_h
