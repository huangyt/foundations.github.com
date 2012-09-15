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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/LogConsumer.h,v 1.13 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_util_logging_LogConsumer_h
#define acdk_util_logging_LogConsumer_h

#include "LogRecord.h"
#include "Formatter.h"
#include "StdFormatter.h"

#include <acdk/util/Properties.h>

namespace acdk {
namespace util {
namespace logging {



ACDK_DECL_INTERFACE(LogConsumer);


/**
  same role as java.util.logging.Handler
  or org.apache.log4j.Appender
*/
class ACDK_CORE_PUBLIC LogConsumer
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(LogConsumer)
public:
  virtual int getMinLevel() = 0;
  virtual void setMinLevel(int mn) = 0;
  virtual int getMaxLevel() = 0;
  virtual void setMaxLevel(int mn) = 0;
  virtual RFormatter getFormatter()  = 0;
  virtual void setFormatter(INP(RFormatter) formatter) = 0;
  /**
    Will be called by the LogManager to self-configuration this LogConsumer.
    To access a custom configure value 'foo' just read it with the 
    the prefix propnameprefix + "foo"
  */
  virtual void configure(IN(RString) propnameprefix, IN(RProperties) props) = 0;
  /**
    Process the LogRecord.
  */
  virtual void publish(IN(RLogRecord) rec) = 0;
};


} // namespace logging
} // namespace util
} // namespace acdk

#if !defined(DOXYGENONLY)
inline
NamedLogArgs& 
operator<<(NamedLogArgs& nla, INP(::acdk::util::RProperties) cls)
{
  ::acdk::util::RIterator it = cls->propertyNames();
  while (it->hasNext() == true)
  {
    ::acdk::lang::RString key = (::acdk::lang::RString)it->next();
    nla << LOG_SPV(key, cls->getProperty(key));
  }
  return nla;
}
#endif //!defined(DOXYGENONLY)

#endif //acdk_util_logging_LogConsumer_h
